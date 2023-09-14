use super::super::lexer::Lexer;
use crate::compiler::span::Span;
use crate::compiler::token::TokenKind;

#[test]
fn test_block_comment() {
    let mut lexer = Lexer::from_source(
        r#"
    /* ----------
     * -        -
     * ---------- */
    "#,
    );

    assert_eq!(
        lexer.next_token().unwrap().to_tuple(),
        (Span::new(0, 1), TokenKind::Newline)
    );
}
