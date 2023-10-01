#![no_main]

//! This fuzzer test that the lexer and `is_identifier` functions agree.

use libfuzzer_sys::fuzz_target;

use rcl::error::Result;
use rcl::lexer::{Token, lex};
use rcl::source::DocId;
use rcl::string::is_identifier;

fn fuzz_main(input: &str) -> Result<()> {
    let tokens = lex(DocId(0), input)?;
    let lex_is_identifier = true
        && tokens.len() == 1
        && tokens[0].0 == Token::Ident
        && tokens[0].1.start() == 0
        && tokens[0].1.end() == input.len();
    assert_eq!(is_identifier(input), lex_is_identifier);
    Ok(())
}

fuzz_target!(|input: &str| {
    let _ = fuzz_main(input);
});
