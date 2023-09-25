// TODO: Remove when compiler implementation is done
#![allow(dead_code)]
use crate::compiler::token::KeywordKind;
use crate::compiler::{Token, TokenKind};
use crate::error::{ErrorKind, WrenError, WrenResult};
use crate::opcode::Op;
use std::rc::Rc;

use super::lexer::Lexer;
use crate::value::{Handle, ObjFn, ObjModule};

pub struct WrenCompiler<'src> {
    lexer: Lexer<'src>,

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
    parent: Option<Box<WrenCompiler<'src>>>,

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

impl<'src> WrenCompiler<'src> {
    pub fn new(source: &'src str) -> Self {
        Self {
            lexer: Lexer::from_source(source),
            parent: None,
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

        todo!()
    }

    fn has_token(&self) -> bool {
        self.token.is_some()
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
                kind: ErrorKind::Compile,
            }),
        }
    }
}

/// Functions that compile tokens.
impl<'src> WrenCompiler<'src> {
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
        todo!()
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
}

/// Functions that emit bytecode instructions.
impl<'src> WrenCompiler<'src> {
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
