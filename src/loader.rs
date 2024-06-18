// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! The loader is responsible for loading documents.

use std::collections::HashMap;
use std::fs;
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::{env, path};

use crate::abstraction;
use crate::ast;
use crate::cli::Target;
use crate::cst;
use crate::error::{Error, Result};
use crate::eval::Evaluator;
use crate::lexer;
use crate::parser;
use crate::pprint::{self, concat};
use crate::runtime::{Env, Value};
use crate::source::{Doc, DocId, Span};
use crate::tracer::Tracer;
use crate::typecheck::{self, TypeChecker};

/// An owned document.
///
/// `Document` is to [`Doc`] what `String` is to `&str`.
pub struct Document {
    /// A friendly name for the source, usually the file path.
    name: String,

    /// The document contents.
    data: String,

    /// The span of the final expression, if known.
    ///
    /// If we haven't yet parsed the document, then this is the full span, but
    /// if a more precise span is known, then that is usually what we want to
    /// blame global errors (e.g. failure to export as json) on.
    span: Span,
}

impl Document {
    pub fn as_doc(&self) -> Doc {
        Doc {
            name: &self.name,
            data: &self.data,
            span: self.span,
        }
    }
}

#[derive(Debug)]
pub struct PathLookup {
    /// A friendly name displayed to the user.
    ///
    /// This is the path relative to the working directory if possible.
    pub name: String,

    /// The absolute path on the file system to load the data from.
    pub path: PathBuf,
}

/// A filesystem resolves import paths to file contents.
///
/// Importing is split into two stages: first we resolve a path that is
/// referenced from a given document to an absolute path and enforce sandbox
/// policies; then we load from the absolute path.
///
/// NOTE: This design is vulnerable to a TOCTOU issue. Say we canonicalized the
/// path previously and verified that importing it is allowed by the sandbox
/// policy. But now that we are about to open the file, the same path could be
/// a symlink to some file that is *not* allowed by the sandbox policy. Fixing
/// this properly is not possible with the filesystem API in Rust's standard
/// library, it will probably involve using pathfds which are Linux-specific.
/// So fixing this will involve a lot of non-portable unsafe code for an attack
/// that is super specific, and even then, the worst you could do is read a file
/// ... so I am not going to bother handling this properly at this time.
pub trait Filesystem {
    /// Return where to load `path` when imported from file `from`.
    fn resolve(&self, path: &str, from: &str) -> Result<PathLookup>;

    /// Return where to load `path` when that was a CLI argument.
    fn resolve_entrypoint(&self, path: &str) -> Result<PathLookup>;

    /// Return where to write `path` when that was a CLI argument.
    fn resolve_output(&self, path: &str) -> PathBuf;

    /// Load a resolved path from the filesystem.
    fn load(&self, path: PathLookup) -> Result<Document>;

    /// Return `path`, but relative to the working directory, if possible.
    ///
    /// If the path lies outside of the working directory, return the original.
    fn get_relative_path<'a>(&self, path: &'a Path) -> &'a Path;
}

/// A dummy filesystem impl to use during initialization.
///
/// This resolves a circular dependency in the error type: to be able to print
/// errors, we need a loader (because errors can reference spans from documents).
/// To have a loader, we need a filesystem. But initializing the filesystem
/// could throw an IO error. There is no actual circular dependency here because
/// the IO error does not reference a document in the loader, but we still need
/// to break the cycle for the type system.
struct PanicFilesystem;

// coverage:off -- Panic file system should not be called in correct code.
impl Filesystem for PanicFilesystem {
    fn resolve(&self, _: &str, _: &str) -> Result<PathLookup> {
        panic!("Should have initialized the filesystem to a real one before resolving.")
    }
    fn resolve_entrypoint(&self, _: &str) -> Result<PathLookup> {
        panic!("Should have initialized the filesystem to a real one before resolving.")
    }
    fn resolve_output(&self, _: &str) -> PathBuf {
        panic!("Should have initialized the filesystem to a real one before resolving.")
    }
    fn load(&self, _: PathLookup) -> Result<Document> {
        panic!("Should have initialized the filesystem to a real one before loading.")
    }
    fn get_relative_path<'a>(&self, _: &'a Path) -> &'a Path {
        panic!("Should have initialized the filesystem to a real one before resolving.")
    }
}
// coverage:on

/// Filesystem that fails to load anything.
///
/// Intended for use by the fuzzer.
pub struct VoidFilesystem;

// coverage:off -- Void filesystem is only used by the fuzzer, not production code.
impl Filesystem for VoidFilesystem {
    fn resolve(&self, _: &str, _: &str) -> Result<PathLookup> {
        Error::new("Void filesystem does not load files.").err()
    }
    fn resolve_entrypoint(&self, _: &str) -> Result<PathLookup> {
        Error::new("Void filesystem does not load files.").err()
    }
    fn resolve_output(&self, _: &str) -> PathBuf {
        panic!("Void filesystem should not be used for output paths.");
    }
    fn load(&self, _: PathLookup) -> Result<Document> {
        Error::new("Void filesystem does not load files.").err()
    }
    fn get_relative_path<'a>(&self, _: &'a Path) -> &'a Path {
        // It's okay to panic here, `get_relative_path` is only used in features
        // that are not used by the fuzzer.
        panic!("Void filesystem does not relativize paths.")
    }
}
// coverage:on

/// The policy about which documents can be loaded from the filesystem.
#[derive(Copy, Clone, Debug, Default, Eq, PartialEq)]
pub enum SandboxMode {
    #[default]
    Workdir,
    Unrestricted,
}

/// Access the real filesystem, but in a potentially sandboxed manner.
#[derive(Debug)]
pub struct SandboxFilesystem {
    mode: SandboxMode,
    workdir: PathBuf,
}

impl SandboxFilesystem {
    pub fn new(mode: SandboxMode, workdir: Option<&str>) -> io::Result<SandboxFilesystem> {
        let workdir = match workdir {
            Some(d) => PathBuf::from(d),
            None => env::current_dir()?,
        };
        let workdir = fs::canonicalize(workdir)?;
        let result = SandboxFilesystem { mode, workdir };
        Ok(result)
    }

    /// Apply path resolution for an absolute but not yet canonicalized path.
    pub fn resolve_absolute(
        &self,
        path_buf: PathBuf,
        sandbox_mode: SandboxMode,
    ) -> Result<PathLookup> {
        // Before we do any sandboxing checks, resolve the file to an absolute
        // path, following symlinks.
        let path_buf = fs::canonicalize(&path_buf).map_err(|err| {
            let fname = path_buf.to_string_lossy().into_owned();
            Error::new(concat! {
                "Failed to access path '"
                pprint::Doc::highlight(&fname).into_owned()
                "': "
                err.to_string()
            })
        })?;

        match sandbox_mode {
            SandboxMode::Unrestricted => {
                // Any path is allowed, nothing to verify.
            }
            SandboxMode::Workdir => {
                if !path_buf.starts_with(&self.workdir) {
                    let fname = path_buf.to_string_lossy().into_owned();
                    let workdir_name = self.workdir.to_string_lossy().into_owned();
                    let mut err = Error::new(concat! {
                        "Sandbox policy '"
                        pprint::Doc::highlight("workdir")
                        "' does not allow loading '"
                        pprint::Doc::highlight(&fname).into_owned()
                        "' because it lies outside of '"
                        pprint::Doc::highlight(&workdir_name).into_owned()
                        "'."
                    });
                    let mut base_dir = self.workdir.clone();
                    while !path_buf.starts_with(&base_dir) {
                        base_dir.pop();
                    }
                    let base_dir_name = base_dir.to_string_lossy().into_owned();
                    err.set_help(concat! {
                        "Try executing from '"
                        pprint::Doc::highlight(&base_dir_name).into_owned()
                        "' or use '"
                        pprint::Doc::highlight("--sandbox=unrestricted")
                        "'."
                    });
                    return err.err();
                }
            }
        }

        let friendly_name = if path_buf.starts_with(&self.workdir) {
            let mut result = String::new();
            let mut self_components = path_buf.components();
            // Skip the shared prefix. Note, the zip order is important. If we
            // put self_components firsts, the zip will consume one past the
            // length of `components`. If we put components first, the zip gets
            // a None there, and then we can still call `self_components.next`.
            for _ in self.workdir.components().zip(&mut self_components) {}
            // Then add the path relative to the working directory.
            for component in self_components {
                if !result.is_empty() {
                    result.push('/');
                }
                match component {
                    path::Component::Normal(p) => result.push_str(&p.to_string_lossy()),
                    _ => panic!("Canonicalization and prefix removal should have prevented this."),
                }
            }
            result
        } else {
            // If the path is outside the working directory, we reference it by
            // absolute path.
            path_buf.to_string_lossy().into_owned()
        };

        let result = PathLookup {
            name: friendly_name,
            path: path_buf,
        };
        Ok(result)
    }
}

impl Filesystem for SandboxFilesystem {
    fn resolve(&self, path: &str, from: &str) -> Result<PathLookup> {
        let mut path_buf = self.workdir.clone();

        if let Some(relative_to_workdir) = path.strip_prefix("//") {
            // The path is relative to the working directory.
            path_buf.push(Path::new(relative_to_workdir));
        } else if path.starts_with('/') {
            return Error::new("Importing absolute paths is not allowed.").err();
        } else {
            // The path is relative to the `from` file.
            path_buf.push(from);
            path_buf.pop();
            path_buf.push(path);
        }

        self.resolve_absolute(path_buf, self.mode)
    }

    fn resolve_entrypoint(&self, path: &str) -> Result<PathLookup> {
        // Making the path relative to the workdir is the same for in/outputs.
        let path_buf = self.resolve_output(path);

        // The entrypoint is specified on the command line and therefore
        // implicitly trusted, it's okay for it to lie outside of the working
        // directory.
        self.resolve_absolute(path_buf, SandboxMode::Unrestricted)
    }

    fn resolve_output(&self, path: &str) -> PathBuf {
        if path.starts_with('/') {
            path.into()
        } else {
            // The path is relative to the working directory.
            let mut path_buf = self.workdir.clone();
            path_buf.push(path);
            path_buf
        }
    }

    fn load(&self, path: PathLookup) -> Result<Document> {
        let buf = fs::read_to_string(&path.path).map_err(|err| {
            let fname = path.path.to_string_lossy().into_owned();
            Error::new(concat! {
                "Failed to read from file '"
                pprint::Doc::highlight(&fname).into_owned()
                "': "
                err.to_string()
            })
        })?;

        let doc = Document {
            name: path.name,
            data: buf,
            // This span is a placeholder that is overwritten later when we push.
            span: Span::new(DocId(0), 0, 0),
        };

        Ok(doc)
    }

    fn get_relative_path<'a>(&self, path: &'a Path) -> &'a Path {
        match path.strip_prefix(&self.workdir) {
            Ok(p) => p,
            Err(..) => path,
        }
    }
}

pub struct Loader {
    documents: Vec<Document>,

    /// For documents loaded from files, their document id.
    ///
    /// This enables us to avoid loading the same file twice.
    loaded_files: HashMap<PathBuf, DocId>,

    filesystem: Box<dyn Filesystem>,
}

impl Loader {
    pub fn new() -> Loader {
        Loader {
            documents: Vec::new(),
            loaded_files: HashMap::new(),
            filesystem: Box::new(PanicFilesystem),
        }
    }

    /// Set the filesystem access handler.
    pub fn set_filesystem(&mut self, filesystem: Box<dyn Filesystem>) {
        self.filesystem = filesystem;
    }

    /// Enable filesystem access with the given sandbox mode.
    pub fn initialize_filesystem(
        &mut self,
        mode: SandboxMode,
        workdir: Option<&str>,
    ) -> Result<()> {
        let sandbox_fs = SandboxFilesystem::new(mode, workdir).map_err(|err| {
            Error::new(concat! {
                "Failed to initialize filesystem access layer: "
                err.to_string()
            })
        })?;
        self.set_filesystem(Box::new(sandbox_fs));
        Ok(())
    }

    /// Resolve a path specified on the CLI so it respects the workdir.
    pub fn resolve_cli_output_path(&self, path: &str) -> PathBuf {
        self.filesystem.resolve_output(path)
    }

    /// Borrow all documents.
    pub fn as_inputs(&self) -> Vec<Doc> {
        self.documents.iter().map(Document::as_doc).collect()
    }

    /// Borrow a document.
    pub fn get_doc(&self, id: DocId) -> Doc {
        self.documents[id.0 as usize].as_doc()
    }

    /// Return the span of the document's body expression if known.
    ///
    /// This span is known only after parsing. Before that, this returns the
    /// span of the full document.
    pub fn get_span(&self, id: DocId) -> Span {
        self.get_doc(id).span
    }

    /// Lex the given document and return its tokens.
    pub fn get_tokens(&self, id: DocId) -> Result<Vec<lexer::Lexeme>> {
        let doc = self.get_doc(id);
        let tokens = lexer::lex(id, doc.data)?;
        Ok(tokens)
    }

    /// Parse the given document and return its Concrete Syntax Tree.
    pub fn get_cst(&mut self, id: DocId) -> Result<cst::Expr> {
        let doc = self.get_doc(id);
        let tokens = self.get_tokens(id)?;
        let (doc_span, expr) = parser::parse(id, doc.data, &tokens)?;

        // After parsing we have a more precise span for the document's body
        // expression, store it so we can later use it to blame errors on.
        self.documents[id.0 as usize].span = doc_span;

        Ok(expr)
    }

    /// Parse the given document and return its Abstract Syntax Tree.
    ///
    /// This is the AST before typecheking.
    pub fn get_unchecked_ast(&mut self, id: DocId) -> Result<ast::Expr> {
        let cst = self.get_cst(id)?;
        let doc = self.get_doc(id);
        let ast = abstraction::abstract_expr(doc.data, &cst)?;
        Ok(ast)
    }

    /// Parse and typecheck the document, return the checked Abstract Syntax Tree.
    pub fn get_typechecked_ast(
        &mut self,
        env: &mut typecheck::Env,
        id: DocId,
    ) -> Result<ast::Expr> {
        // The typechecker needs a span to blame type errors on, we put in the
        // entire document. It is not going to blame any type errors on this
        // span, because we check `Type::Any` which any value satisfies. If we
        // want to typecheck through imports, we would need an expected type and
        // span from the import site.
        let span = self.get_span(id);
        let mut ast = self.get_unchecked_ast(id)?;
        let mut checker = TypeChecker::new(env);
        checker.check_expr(typecheck::type_any(), span, &mut ast)?;
        Ok(ast)
    }

    /// Evaluate the given document and return the resulting value.
    pub fn evaluate(
        &mut self,
        type_env: &mut typecheck::Env,
        value_env: &mut Env,
        id: DocId,
        tracer: &mut dyn Tracer,
    ) -> Result<Value> {
        let mut evaluator = Evaluator::new(self, tracer);
        evaluator.eval_doc(type_env, value_env, id)
    }

    /// Push a document and set its span to the full document.
    ///
    /// We set the span here because the span contains the document id, which is
    /// only known after we push the document.
    fn push(&mut self, mut document: Document) -> DocId {
        let n = self.documents.len();
        let id = DocId(n.try_into().expect("Cannot load that many documents!"));
        document.span = Span::new(id, 0, document.data.len());
        self.documents.push(document);
        id
    }

    /// Load stdin into a new document.
    pub fn load_stdin(&mut self) -> Result<DocId> {
        let mut buf = String::new();
        io::stdin()
            .read_to_string(&mut buf)
            .map_err(|err| Error::new(format!("Failed to read from stdin: {}.", err)))?;
        let doc = Document {
            name: "stdin".to_string(),
            data: buf,
            // This span is a placeholder that is overwritten by `push`.
            span: Span::new(DocId(0), 0, 0),
        };
        Ok(self.push(doc))
    }

    /// Load a path that is referenced in the document with name `from`.
    pub fn load_path(&mut self, path: &str, from: Option<DocId>) -> Result<DocId> {
        let from_path = match from {
            Some(id) => self.get_doc(id).name,
            None => "",
        };
        let resolved = self.filesystem.resolve(path, from_path)?;
        assert!(!resolved.name.is_empty());
        self.load_file(resolved)
    }

    /// Load a file into a new document.
    pub fn load_file(&mut self, path: PathLookup) -> Result<DocId> {
        // Avoid loading the same file twice if we already loaded it. This is
        // needed in particular to be able to detect circular imports, because
        // we detect those based on document id.
        if let Some(id) = self.loaded_files.get(&path.path) {
            return Ok(*id);
        }

        let path_buf = path.path.clone();
        let doc = self.filesystem.load(path)?;
        let id = self.push(doc);
        self.loaded_files.insert(path_buf, id);

        Ok(id)
    }

    /// Load a string into a new document.
    pub fn load_string(&mut self, data: String) -> DocId {
        let doc = Document {
            name: "input".to_string(),
            data,
            // This span is a placeholder that is overwritten by `push`.
            span: Span::new(DocId(0), 0, 0),
        };
        self.push(doc)
    }

    /// Load the file with the given name, or stdin.
    pub fn load_cli_target(&mut self, target: &Target) -> Result<DocId> {
        match target {
            Target::File(fname) => {
                let path = self.filesystem.resolve_entrypoint(&fname)?;
                self.load_file(path)
            }
            Target::Stdin => self.load_stdin(),
            Target::StdinDefault => {
                // We are in the case where the input is stdin, but stdin was
                // selected implitictly, through the absence of a file argument.
                // If stdin is a TTY, and the user doesn't know that the
                // application defaulted to stdin and is waiting for input, then
                // it looks like the application hangs. Print a note to stderr
                // to educate the user.
                use std::io::IsTerminal;
                if std::io::stdin().is_terminal() {
                    eprintln!(
                        "No input file was specified, defaulting to stdin. See also --help.\n\
                        To silence this note, select stdin explicitly with '-'.\n\
                        Waiting for input ..."
                    );
                }
                self.load_stdin()
            }
        }
    }

    #[cfg(unix)]
    fn write_depfile_impl(&self, target_path: &Path, depfile_path: &Path) -> io::Result<()> {
        use std::io::Write;
        use std::os::unix::ffi::OsStrExt;
        let f = std::fs::File::create(depfile_path)?;
        let mut w = std::io::BufWriter::new(f);
        let rel_target = self.filesystem.get_relative_path(target_path);
        w.write_all(rel_target.as_os_str().as_bytes())?;
        w.write_all(b":")?;
        for (path, _doc_id) in self.loaded_files.iter() {
            let rel_path = self.filesystem.get_relative_path(path);
            w.write_all(b" ")?;
            w.write_all(rel_path.as_os_str().as_bytes())?;
        }
        w.write_all(b"\n")?;
        Ok(())
    }

    #[cfg(unix)]
    pub fn write_depfile(
        &self,
        target: &crate::cli::OutputTarget,
        depfile_path: &str,
    ) -> Result<()> {
        use crate::cli::OutputTarget;
        // The depfile output path is specified on the CLI, so we resolve it
        // to make it respect --directory.
        let resolved_depfile = self.resolve_cli_output_path(depfile_path);
        let resolved_target = match target {
            OutputTarget::File(path) => self.resolve_cli_output_path(path),
            OutputTarget::Stdout => {
                return Error::new(concat! {
                    "To use "
                    crate::pprint::Doc::highlight("--output-depfile")
                    ", "
                    crate::pprint::Doc::highlight("--output")
                    " is required."
                })
                .err()
            }
        };

        self.write_depfile_impl(&resolved_target, &resolved_depfile)
            .map_err(|err| Error::new(format!("Failed to write depfile: {}.", err)).into())
    }
}
