use std::io::{self, Write};

use clap::Parser;
use wren::compiler::{Lexer, TokenKind};

/// Simple program to greet a person
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {}

fn repl() {
    println!("Wren {}", wren::VERSION);

    let mut buf = String::new();
    let stdin = io::stdin();

    loop {
        buf.clear();
        print!("> ");
        let _ = io::stdout().flush();
        stdin.read_line(&mut buf).expect("read stdin");

        let mut lexer = Lexer::from_source(buf.as_str());
        loop {
            match lexer.next_token() {
                Ok(token) => {
                    print!("{:>4}:{:<4} {:?}", token.span.pos, token.span.end(), token.kind);

                    match token.kind {
                        TokenKind::Number => {
                            println!(" {}", token.num);
                        }
                        TokenKind::End => {
                            println!("");
                            break;
                        }
                        _ => {
                            println!("");
                        }
                    }
                }
                Err(err) => eprintln!("{err}"),
            }
        }
    }
}

fn main() {
    repl()
}
