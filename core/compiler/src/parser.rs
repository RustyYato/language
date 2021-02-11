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

    let finish = parser.builder.start_node(NodeKind::Package);
    parser.parse();
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

    pub fn peek(&self) -> Option<TokenKind> { self.tokens.get().get(0).map(|tok| tok.kind) }

    pub fn tok(&self, kind: impl Into<TokenKind>) -> Option<Token<'input>> {
        let kind = kind.into();
        let tok = self.try_tok(kind);
        if tok.is_none() {
            self.builder.error_token(kind);
        }
        tok
    }

    pub fn try_tok(&self, kind: impl Into<TokenKind>) -> Option<Token<'input>> {
        let kind = kind.into();
        self.skip_ws();
        match self.tokens.get() {
            [first, tokens @ ..] if first.kind == kind => {
                self.tokens.set(tokens);
                self.builder.token(*first);
                Some(*first)
            }
            _ => None,
        }
    }

    pub fn parse(&self) -> Option<()> {
        self.parse_let()?;
        Some(())
    }

    pub fn parse_let(&self) -> Option<()> {
        let _let_node = self.builder.start_node(NodeKind::Let);
        self.tok(Keyword::Let)?;
        self.try_tok(Keyword::Mut);
        self.tok(TokenKind::Ident)?;
        self.tok(Symbol::Assign)?;
        self.tok(TokenKind::Integer)?;
        self.tok(Symbol::SemiColon)?;
        Some(())
    }
}
