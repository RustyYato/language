use std::cell::{Cell, RefCell};

use crate::{
    rowan_ext::{self, Builder, NodeKind},
    tokens::{Keyword, Symbol, Token, TokenKind},
};

#[derive(Default)]
struct Parser<'tokens, 'input> {
    tokens: Cell<&'tokens [Token<'input>]>,
    builder: Builder,
    errors: RefCell<Vec<Error>>,
}

pub enum Error {}

pub fn parse(input: &[Token<'_>]) -> (rowan_ext::Parse, Vec<Error>) {
    let parser = Parser {
        tokens: Cell::new(input),
        ..Default::default()
    };

    let finish = parser.builder.start_node(NodeKind::File);
    parser.parse_scope_contents();
    drop(finish);

    (parser.builder.finish(), parser.errors.into_inner())
}

impl<'input> Parser<'_, 'input> {
    fn skip_ws(&self) {
        let pos = self
            .tokens
            .get()
            .iter()
            .take_while(|token| {
                matches!(
                    token.kind,
                    TokenKind::BlockComment | TokenKind::LineComment | TokenKind::WhiteSpace
                )
            })
            .inspect(|&&token| self.builder.token(token))
            .count();
        self.tokens.set(&self.tokens.get()[pos..]);
    }

    fn peek(&self) -> Option<TokenKind> {
        self.skip_ws();
        self.tokens.get().get(0).map(|tok| tok.kind)
    }

    fn tok(&self, kind: impl Into<TokenKind>) {
        let kind = kind.into();
        match self.try_tok_imp(kind) {
            Ok(()) => (),
            Err(Some(token)) if token.kind == TokenKind::Error => self.builder.custom_error(|| self.eat()),
            Err(_) => self.builder.error(),
        }
    }

    fn try_tok(&self, kind: impl Into<TokenKind>) -> bool { self.try_tok_imp(kind).is_ok() }

    fn try_tok_imp(&self, kind: impl Into<TokenKind>) -> Result<(), Option<Token<'input>>> {
        let kind = kind.into();
        self.skip_ws();
        match self.tokens.get() {
            [first, ..] if first.kind == kind => {
                self.eat();
                Ok(())
            }
            [first, ..] => Err(Some(*first)),
            [] => Err(None),
        }
    }

    #[track_caller]
    fn eat(&self) {
        match self.tokens.get() {
            [first, tokens @ ..] => {
                self.tokens.set(tokens);
                self.builder.token(*first);
            }
            [] => unreachable!(),
        }
    }

    fn force_tok(&self, kind: impl Clone + IntoIterator<Item = impl Into<TokenKind>>) -> Option<usize> {
        let kind = kind;
        let start = self.builder.checkpoint();
        let mut error = false;
        let pos = loop {
            let peek = self.peek();
            match peek {
                Some(k) => {
                    if let Some(pos) = kind.clone().into_iter().map(Into::into).position(|x| x == k) {
                        break Some(pos)
                    } else {
                        self.eat();
                        error = true;
                    }
                }
                None => {
                    error = true;
                    break None
                }
            }
        };
        if error {
            self.builder.error_at(start);
        }
        self.eat();
        pos
    }

    fn parse_scope_contents(&self) -> bool {
        loop {
            self.skip_ws();
            match self.peek() {
                Some(TokenKind::Keyword(Keyword::Let)) => self.parse_let(),
                Some(TokenKind::Keyword(Keyword::Loop)) => self.parse_loop(),
                Some(TokenKind::Keyword(Keyword::If)) => self.parse_if(),
                Some(TokenKind::Symbol(Symbol::SemiColon)) => self.eat(),
                _ => {
                    if self.is_expression() {
                        self.parse_expression();
                        if self.peek() != Some(Symbol::EndCurly.into()) {
                            break Some(1)
                                != self.force_tok(
                                    std::iter::once(Symbol::SemiColon).chain(std::iter::once(Symbol::EndCurly)),
                                )
                        }
                    } else {
                        break true
                    }
                }
            }
        }
    }

    fn parse_let(&self) {
        self.skip_ws();
        let _node_let = self.builder.start_node(NodeKind::Let);
        self.tok(Keyword::Let);
        self.try_tok(Keyword::Mut);
        self.parse_pattern();
        if self.peek() == Some(TokenKind::Symbol(Symbol::Colon)) {
            self.eat();
            self.parse_type();
        }
        self.tok(Symbol::Assign);
        self.parse_expression();
        self.force_tok(Some(Symbol::SemiColon));
    }

    fn parse_loop(&self) {
        self.skip_ws();
        let _node_loop = self.builder.start_node(NodeKind::Loop);
        self.tok(Keyword::Loop);
        self.parse_scope();
    }

    fn parse_if(&self) {
        self.skip_ws();
        let _node_if = self.builder.start_node(NodeKind::If);
        let _node_then = self.builder.start_node(NodeKind::If);
        self.tok(Keyword::If);
        self.parse_expression();
        self.parse_scope();
        drop(_node_then);
        while self.peek() == Some(Keyword::Else.into()) {
            let start = self.builder.checkpoint();
            self.eat();
            if self.peek() == Some(Keyword::If.into()) {
                let _node_else_if = self.builder.start_node_at(start, NodeKind::ElseIf);
                self.eat();
                self.parse_expression();
                self.parse_scope();
            } else {
                let _node_else = self.builder.start_node_at(start, NodeKind::Else);
                self.parse_scope();
            }
        }
    }

    fn parse_scope(&self) {
        self.skip_ws();
        let _node_loop = self.builder.start_node(NodeKind::Scope);
        self.tok(Symbol::StartCurly);
        if self.parse_scope_contents() {
            self.force_tok(Some(Symbol::EndCurly));
        }
    }

    fn parse_pattern(&self) {
        self.skip_ws();
        let _node_pattern = self.builder.start_node(NodeKind::Pattern);
        self.tok(TokenKind::Ident);
    }

    fn is_expression(&self) -> bool {
        matches!(
            self.peek(),
            Some(TokenKind::Integer)
                | Some(TokenKind::Number)
                | Some(TokenKind::Ident)
                | Some(TokenKind::Keyword(Keyword::Break))
                | Some(TokenKind::Keyword(Keyword::Continue))
        )
    }

    fn parse_expression(&self) {
        if self.is_expression() {
            let _node_expr = self.builder.start_node(NodeKind::Expression);
            self.eat();
        } else {
            self.builder.error();
        }
    }

    fn parse_type(&self) {
        self.skip_ws();
        let _node_pattern = self.builder.start_node(NodeKind::Type);
        self.tok(TokenKind::Ident);
    }
}
