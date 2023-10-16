// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! The loader is responsible for loading documents.

use std::fs;
use std::io::{self, Read};
use std::path::Path;
use std::rc::Rc;

use crate::abstraction;
use crate::ast;
use crate::cli::Target;
use crate::cst;
use crate::error::{Error, Result};
use crate::eval;
use crate::lexer;
use crate::markup::Markup;
use crate::parser;
use crate::pprint::{self, concat};
use crate::runtime::{Env, Value};
use crate::source::{Doc, DocId, Span};

/// An owned document.
///
/// `Document` is to [`Doc`] what `String` is to `&str`.
struct Document {
    /// A friendly name for the source, usually the file path.
    name: String,
    /// The document contents.
    data: String,
}

impl Document {
    pub fn as_doc(&self) -> Doc {
        Doc {
            name: &self.name,
            data: &self.data,
        }
    }
}

#[derive(Default)]
pub struct Loader {
    documents: Vec<Document>,
}

impl Loader {
    pub fn new() -> Loader {
        Loader::default()
    }

    /// Borrow all documents.
    pub fn as_inputs(&self) -> Vec<Doc> {
        self.documents.iter().map(Document::as_doc).collect()
    }

    /// Borrow a document.
    pub fn get_doc(&self, id: DocId) -> Doc {
        self.documents[id.0 as usize].as_doc()
    }

    /// Return the span that covers the entire document.
    pub fn get_span(&self, id: DocId) -> Span {
        Span::new(id, 0, self.documents[id.0 as usize].data.len())
    }

    /// Lex the given document and return its tokens.
    pub fn get_tokens(&self, id: DocId) -> Result<Vec<lexer::Lexeme>> {
        let doc = self.get_doc(id);
        let tokens = lexer::lex(id, doc.data)?;
        Ok(tokens)
    }

    /// Parse the given document and return its Concrete Syntax Tree.
    pub fn get_cst(&self, id: DocId) -> Result<cst::Prefixed<cst::Expr>> {
        let doc = self.get_doc(id);
        let tokens = self.get_tokens(id)?;
        let (_doc_span, expr) = parser::parse(id, doc.data, &tokens)?;
        Ok(expr)
    }

    /// Parse the given document and return its Abstract Syntax Tree.
    pub fn get_ast(&self, id: DocId) -> Result<ast::Expr> {
        let doc = self.get_doc(id);
        let cst = self.get_cst(id)?;
        let ast = abstraction::abstract_expr(doc.data, &cst)?;
        Ok(ast)
    }

    /// Evaluate the given document and return the resulting value.
    pub fn evaluate(&self, id: DocId, env: &mut Env) -> Result<Rc<Value>> {
        let expr = self.get_ast(id)?;
        let result = eval::eval(env, &expr)?;
        Ok(result)
    }

    fn push(&mut self, document: Document) -> DocId {
        let n = self.documents.len();
        self.documents.push(document);
        DocId(n.try_into().expect("Cannot load that many documents!"))
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
        };
        Ok(self.push(doc))
    }

    /// Load a file into a new document.
    pub fn load_file<P: AsRef<Path>>(&mut self, path: P) -> Result<DocId> {
        // TODO: Deduplicate, don't load the same document twice.
        let buf = fs::read_to_string(&path).map_err(|err| {
            let fname = path.as_ref().to_string_lossy().into_owned();
            Error::new(concat! {
                "Failed to read from file '"
                pprint::Doc::from(fname).with_markup(Markup::Highlight)
                "': "
                err.to_string()
            })
        })?;
        let doc = Document {
            // TODO: Canonicalize all paths to be relative to the working directory.
            name: path.as_ref().to_string_lossy().into_owned(),
            data: buf,
        };
        Ok(self.push(doc))
    }

    /// Load a string into a new document.
    pub fn load_string(&mut self, data: String) -> DocId {
        let doc = Document {
            name: "input".to_string(),
            data,
        };
        self.push(doc)
    }

    /// Load the file with the given name, or stdin.
    pub fn load_cli_target(&mut self, target: Target) -> Result<DocId> {
        match target {
            Target::File(fname) => self.load_file(fname),
            Target::Stdin => self.load_stdin(),
        }
    }
}
