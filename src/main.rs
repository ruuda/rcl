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

fn main() {
    let fname = "examples/tags.rcl";
    let data = std::fs::read_to_string(fname).expect("Failed to load example.");
    let doc = Document {
        path: fname,
        data: &data,
    };
    let inputs = [doc];
    if let Err(err) = main_tags(&inputs) {
        err.print(&inputs);
    }
}
