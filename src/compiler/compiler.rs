// TODO: Remove when compiler implementation is done
#![allow(dead_code)]

use crate::compiler::token::{KeywordKind, Token, TokenExt, TokenKind};
use crate::error::{CompileError, ErrorKind, WrenError, WrenResult};
use crate::opcode::{Arity, Op};
use crate::symbol::SymbolTable;
use std::convert::Infallible;

use super::lexer::Lexer;
use crate::value::{new_handle, Handle, ObjFn, ObjModule};

pub struct WrenCompiler<'src, 'sym> {
    lexer: Lexer<'src>,

    method_names: &'sym mut SymbolTable,

    /// FIXME: Circular dependency between Compiler and VM
    ///
    /// The parser and compiler need the current VM to:
    ///
    /// - Create new functions, the basic unit of code.
    /// - Allocate new strings.
    /// - Push new sub-compilers onto the stack.
    /// - Use compiler state as GC roots.
    /// vm: &'wren WrenVm;

    /// The compiler for the function enclosing this one,
    /// or `None` if it's the top module level.
    // parent: Option<Box<WrenCompiler<'src>>>,

    /// If this is a compiler for a method, then we keep
    /// track of the class enclosing it.
    enclosing_class: Option<ClassInfo>,

    /// The function being compiled.
    func: Option<ObjFn>,

    /// The last token that was received from the lexer.
    token: Option<Token>,

    /// Collected document comments.
    doc_comments: Vec<Token>,
}

/// Bookkeeping information for compiling a class definition.
struct ClassInfo {}

impl<'src, 'sym> WrenCompiler<'src, 'sym> {
    pub fn new(source: &'src str, method_names: &'sym mut SymbolTable) -> Self {
        Self {
            lexer: Lexer::from_source(source),
            method_names,
            // parent: None,
            enclosing_class: None,
            func: None,
            token: None,
            doc_comments: Vec::new(),
        }
    }

    // FIXME: Private ObjFn type in public compile() signature.
    pub(crate) fn compile(&mut self, _module: Handle<ObjModule>, is_expression: bool) -> WrenResult<Handle<ObjFn>> {
        self.func = Some(ObjFn::new());

        // Initialise.
        self.next_token()?;
        self.ignore_newlines()?;

        if is_expression {
            todo!("expression parsing");
        } else {
            self.compile_def_stmts()?;
            self.emit_op(Op::EndModule);
        }

        self.emit_op(Op::Return);
        self.end_compiler()
    }

    fn end_compiler(&mut self) -> WrenResult<Handle<ObjFn>> {
        // Mark the end of the bytecode. Since it may contain multiple early returns,
        // we can't rely on Op::Return to tell us we're at the end.
        self.emit_op(Op::End);

        Ok(new_handle(self.func.take().unwrap()))
    }

    fn has_token(&self) -> bool {
        match self.token.kind() {
            Some(TokenKind::End) | None => false,
            Some(_) => true,
        }
    }

    fn next_token(&mut self) -> WrenResult<()> {
        loop {
            match self.lexer.next_token() {
                Ok(token) => {
                    // Collect document comments, which will implicitly belong to
                    // the token following the last comment line.
                    match token.kind {
                        TokenKind::DocComment => {
                            self.doc_comments.push(token);
                            continue;
                        }
                        TokenKind::Comment | TokenKind::BlockComment => {
                            continue;
                        }
                        _ => {
                            self.token = Some(token);
                            return Ok(());
                        }
                    }
                }
                Err(err) => {
                    self.token = None;
                    return Err(err);
                }
            }
        }
    }

    fn try_token(&self) -> WrenResult<&Token> {
        match &self.token {
            Some(token) => Ok(token),
            None => Err(WrenError {
                kind: ErrorKind::Compile(CompileError::UnexpectedEndOfTokens),
            }),
        }
    }
}

/// Functions that compile tokens.
impl<'src, 'sym> WrenCompiler<'src, 'sym> {
    fn ignore_newlines(&mut self) -> WrenResult<()> {
        while let Some(token) = &self.token {
            if token.kind == TokenKind::Newline {
                self.next_token()?;
            } else {
                break;
            }
        }

        Ok(())
    }

    fn compile_def_stmts(&mut self) -> WrenResult<()> {
        while self.has_token() {
            println!("has_token: {:?}", self.try_token());
            self.compile_def_stmt()?;
        }

        Ok(())
    }

    fn compile_def_stmt(&mut self) -> WrenResult<()> {
        use KeywordKind::*;
        use TokenKind::*;

        let token = self.try_token()?;

        // TODO: Compile attributes starting with hash (#)

        match token.kind {
            Keyword(Class) => self.compile_class(),
            Keyword(Foreign) => self.compile_foreign(),
            Keyword(Import) => self.compile_import(),
            Keyword(Var) => self.compile_var(),
            _ => self.compile_stmt(),
        }
    }

    /// Compile a simple statement.
    fn compile_stmt(&mut self) -> WrenResult<()> {
        use KeywordKind::*;
        use TokenKind::*;

        let token = self.try_token()?;

        match token.kind {
            Keyword(Return) => self.compile_return(),
            Keyword(kw) => panic!("unexpected keyword: {kw:?}"),
            _ => self.compile_expr(),
        }
    }

    fn compile_class(&mut self) -> WrenResult<()> {
        todo!()
    }

    fn compile_foreign(&mut self) -> WrenResult<()> {
        todo!()
    }

    fn compile_import(&mut self) -> WrenResult<()> {
        todo!()
    }

    fn compile_var(&mut self) -> WrenResult<()> {
        todo!()
    }

    /// Compile a `return` statement.
    fn compile_return(&mut self) -> WrenResult<()> {
        debug_assert_eq!(self.token.keyword(), Some(KeywordKind::Return));

        self.next_token()?;

        let token = self.try_token()?;
        match token.kind {
            TokenKind::Newline => {
                // TODO: If there's no expression after return, initializers should return 'this' and regular methods should return null.
                Ok(())
            }
            _ => {
                // TODO: A constructor cannot return a value.
                self.compile_expr()?;
                self.emit_op(Op::Return);
                Ok(())
            }
        }
    }
}

// -------------------------------------------------------------------------------------------------
// Expression parser.

/// Token precedence.
#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq)]
#[rustfmt::skip]
enum Precedence {
    /// Tokens that terminate an expression
    /// should have a precedence of `None`.
    None = 0,
    Lowest = 1,
    Assignment = 2,    // =
    Conditional = 3,   // ?:
    LogicalOr = 4,     // ||
    LogicalAnd = 5,    // &&
    Equality = 6,      // == !=
    Is = 7,            // is
    Comparison = 8,    // < > <= >=
    BitwiseOr = 9,     // |
    BitwiseXor = 10,   // ^
    BitwiseAnd = 11,   // &
    BitwiseShift = 12, // << >>
    Range = 13,        // .. ...
    Term = 14,         // + -
    Factor = 15,       // * / %
    Unary = 16,        // - ! ~
    Call = 17,         // . () []
    Primary = 18,
}

impl Precedence {
    #[inline(always)]
    fn as_i32(&self) -> i32 {
        *self as i32
    }

    /// Get the precedence of the given token type in the context
    /// of the expression parser.
    #[rustfmt::skip]
    fn of(kind: TokenKind) -> Precedence {
        use TokenKind::*;

        match kind {
            Number | Name  => Precedence::Lowest,
            Plus   | Minus => Precedence::Term,
            Star   | Slash => Precedence::Factor,
                       Eq  => Precedence::Assignment,
            EqEq | BangEq  => Precedence::Equality,

            Dot  | LeftParen | LeftBracket => Precedence::Call,

            // Terminators
            RightParen | RightBracket => Precedence::None,
            Comma => Precedence::None,
            _     => Precedence::None,
        }
    }
}

impl TryFrom<i32> for Precedence {
    type Error = Infallible;

    #[rustfmt::skip]
    fn try_from(value: i32) -> Result<Self, Self::Error> {
        use Precedence as P;
        match value {
            0  => Ok(P::None),
            1  => Ok(P::Lowest),
            2  => Ok(P::Assignment),
            3  => Ok(P::Conditional),
            4  => Ok(P::LogicalOr),
            5  => Ok(P::LogicalAnd),
            6  => Ok(P::Equality),
            7  => Ok(P::Is),
            8  => Ok(P::Comparison),
            9  => Ok(P::BitwiseOr),
            10 => Ok(P::BitwiseXor),
            11 => Ok(P::BitwiseAnd),
            12 => Ok(P::BitwiseShift),
            13 => Ok(P::Range),
            14 => Ok(P::Term),
            15 => Ok(P::Factor),
            16 => Ok(P::Unary),
            17 => Ok(P::Call),
            18 => Ok(P::Primary),
            _  => Ok(P::None),
        }
    }
}

impl std::fmt::Display for Precedence {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.as_i32(), f)
    }
}

impl std::ops::Add<i32> for Precedence {
    type Output = Precedence;

    fn add(self, rhs: i32) -> Self::Output {
        Precedence::try_from(self.as_i32() + rhs).unwrap()
    }
}

/// Associativity is the precedence tie-breaker.
#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Associativity {
    Left,
    Right,
}

impl Associativity {
    /// Determine the associativity of the given token kind.
    fn of(token_ty: TokenKind) -> Associativity {
        if token_ty == TokenKind::Eq {
            Associativity::Right
        } else {
            Associativity::Left
        }
    }

    fn is_left(&self) -> bool {
        *self == Associativity::Left
    }
}

/// Functions that form the Pratt-parser.
impl<'src, 'sym> WrenCompiler<'src, 'sym> {
    fn compile_expr(&mut self) -> WrenResult<()> {
        self.compile_expr_precedence(Precedence::Lowest)
    }

    fn compile_expr_precedence(&mut self, precedence: Precedence) -> WrenResult<()> {
        println!("compile_expr_precedence {:?} {precedence:?}", self.try_token());
        let kind = self.try_token()?.kind;

        // TODO: Handle assignment expression.
        self.compile_prefix()?;

        println!(
            "infix token: {:?}; precedence {:?} <= {:?}",
            self.try_token()?,
            precedence,
            Precedence::of(self.try_token()?.kind)
        );
        while precedence <= Precedence::of(self.try_token()?.kind) {
            println!("infix loop for {kind:?} {precedence:?}");
            self.compile_infix()?;
        }

        println!("compile_expr_precedence return");
        Ok(())
    }

    fn compile_prefix(&mut self) -> WrenResult<()> {
        use TokenKind::*;
        let token = self.try_token()?;
        println!("prefix token: {:?}", token.kind);

        match token.kind {
            Number => self.compile_number_literal(),
            _ => todo!("compile_prefix"),
        }
    }

    fn compile_number_literal(&mut self) -> WrenResult<()> {
        debug_assert_eq!(self.token.kind(), Some(TokenKind::Number));
        let token = self.try_token()?;
        println!("compile_number_literal {:?}", token);
        let value = token.value.num().unwrap().into();
        let constant_id = self.func.as_mut().unwrap().intern_constant(value);
        self.emit_op(Op::Constant(constant_id));
        self.next_token()?;
        Ok(())
    }

    fn compile_infix(&mut self) -> WrenResult<()> {
        let kind = self.try_token()?.kind;
        let precedence = Precedence::of(kind);
        println!("compile_infix token {kind:?}");

        // An infix operator cannot end an expression.
        self.ignore_newlines()?;

        // Compile the right-hand side.
        self.next_token()?;
        self.compile_expr_precedence(precedence + 1)?;

        // Call the operator on the left-hand side's class.
        let method_name =
            token_method_name(kind).ok_or_else(|| WrenError::new_compile(CompileError::InvalidOperator))?;
        let signature = Signature {
            name: method_name.to_string(),
            kind: SignatureKind::Method,
            arity: Arity::new(1),
        };
        self.compile_call_signature(signature)
    }

    fn compile_call_signature(&mut self, signature: Signature) -> WrenResult<()> {
        println!("compile_call_signature {:?}", signature);
        let symbol = self
            .method_names
            .resolve(signature.name.as_str())
            .ok_or_else(|| WrenError::new_compile(CompileError::SymbolNotFound))?;
        self.emit_op(Op::Call(signature.arity, symbol));
        Ok(())
    }
}

fn token_method_name(kind: TokenKind) -> Option<&'static str> {
    use TokenKind::*;
    println!("token_method_name({kind:?})");
    match kind {
        Plus => Some("+"),
        Minus => Some("-"),
        Slash => Some("/"),
        Star => Some("*"),
        _ => None,
    }
}

#[derive(Debug)]
enum SignatureKind {
    Method,
}

#[derive(Debug)]
struct Signature {
    name: String,
    kind: SignatureKind,
    arity: Arity,
}

// -------------------------------------------------------------------------------------------------
/// Functions that emit bytecode instructions.
impl<'src, 'sym> WrenCompiler<'src, 'sym> {
    /// Emit the given instruction to the current function.
    ///
    /// # Panics
    ///
    /// Will panic if there is no current function.
    fn emit_op(&mut self, op: Op) {
        let func = self.func.as_mut().expect("compiler has no current function");
        // TODO: Line number from lexer
        func.push_op(op, 0);
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::value::new_handle;

    #[test]
    fn test_expr() {
        let mut method_names = SymbolTable::new();
        let mut compiler = WrenCompiler::new("return 7", &mut method_names);
        let module = new_handle(ObjModule::new("main"));

        let obj_fn = compiler.compile(module, false).unwrap();
    }
}
