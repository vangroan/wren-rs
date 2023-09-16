use super::super::lexer::Lexer;
use crate::compiler::span::Span;
use crate::compiler::token::TokenKind;

#[test]
fn test_line_comment() {
    env_logger::init();

    let mut lexer = Lexer::from_source(
        r"
        //--------
        ",
    );

    assert_eq!(lexer.next_token_tuple(), (Span::new(0, 1), TokenKind::Newline));
    assert_eq!(lexer.next_token_tuple(), (Span::new(9, 11), TokenKind::Comment));
}

#[test]
fn test_line_comment_eof() {
    let mut lexer = Lexer::from_source(r"//--------");

    assert_eq!(lexer.next_token_tuple(), (Span::new(0, 10), TokenKind::Comment));
}

#[test]
fn test_block_comment() {
    let mut lexer = Lexer::from_source(
        r#"
        /* ----------
         * -        -
         * ---------- */
        "#,
    );

    assert_eq!(lexer.next_token_tuple(), (Span::new(0, 1), TokenKind::Newline));
    assert_eq!(lexer.next_token_tuple(), (Span::new(9, 60), TokenKind::BlockComment));
    assert_eq!(lexer.next_token_tuple(), (Span::new(69, 1), TokenKind::Newline));
}

#[test]
fn test_nested_block_comment() {
    let mut lexer = Lexer::from_source(r"/* /* /* */ */ */ //-");

    assert_eq!(lexer.next_token_tuple(), (Span::new(0, 17), TokenKind::BlockComment));
    assert_eq!(lexer.next_token_tuple(), (Span::new(18, 3), TokenKind::Comment));
}

#[test]
fn test_shebang_skip() {
    let mut lexer = Lexer::from_source(include_str!("test_shebang.wren"));

    assert_eq!(lexer.next_token_tuple(), (Span::new(18, 4), TokenKind::Comment));
}
