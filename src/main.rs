use rcl::runtime::Env;
use rcl::source::{DocId, Document, Inputs};

fn main_tags(inputs: &Inputs) -> rcl::error::Result<()> {
    for (i, doc) in inputs.iter().enumerate() {
        let id = DocId(i as u32);
        let tokens = rcl::lexer::lex(id, doc.data)?;
        for (token, span) in &tokens {
            eprintln!("{span:?} {token:?}");
        }

        let cst = rcl::parser::parse(id, doc.data)?;
        eprintln!("{cst:#?}");

        let ast = rcl::abstraction::abstract_expr(doc.data, &cst);
        eprintln!("{ast:#?}");

        let mut env = Env::new();
        let val = rcl::eval::eval(&mut env, &ast)?;

        let mut val_json = String::new();
        rcl::json::format_json(val.as_ref(), &mut val_json)?;
        println!("{}", val_json);
    }

    Ok(())
}

struct Data {
    path: String,
    data: String,
}

fn main() {
    let mut inputs_owned = Vec::new();
    for fname in std::env::args().skip(1) {
        let data = std::fs::read_to_string(&fname).expect("Failed to load example.");
        let doc = Data {
            path: fname,
            data: data,
        };
        inputs_owned.push(doc);
    };
    let inputs: Vec<_> = inputs_owned.iter().map(|d| Document { path: &d.path, data: &d.data }).collect();
    if let Err(err) = main_tags(&inputs) {
        err.print(&inputs);
    }
}
