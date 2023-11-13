mod tokenizer;
mod parser;

use tokenizer::{Tokenizer};
use parser::{Parser};

// Can't parse i = 5;

fn main() {
    let source_code_path = "C:/dev/hydrogen/test.hy";
    let tokenizer = Tokenizer::new(source_code_path);

    let mut parser = Parser::new(tokenizer);
    parser.parse();
}