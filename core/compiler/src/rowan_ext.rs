use crate::tokens::{Token, TokenKind};
use std::{
    cell::{Cell, RefCell},
    fmt::{Debug, Display, Formatter},
};

pub type SyntaxNode = rowan::SyntaxNode<Language>;
pub type SyntaxToken = rowan::SyntaxToken<Language>;
pub type SyntaxElement = rowan::SyntaxElement<Language>;

impl From<rowan::SyntaxKind> for SyntaxKind {
    fn from(rowan::SyntaxKind(kind): rowan::SyntaxKind) -> Self {
        let [kind, encoding] = u16::to_le_bytes(kind);
        match kind {
            0 => SyntaxKind::Token(TokenKind::decode(encoding)),
            1 => SyntaxKind::Node(NodeKind::decode(encoding)),
            _ => SyntaxKind::Error,
        }
    }
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        rowan::SyntaxKind(match kind {
            SyntaxKind::Token(token) => u16::from_le_bytes([0, token.encode()]),
            SyntaxKind::Node(node) => u16::from_le_bytes([1, node as u8]),
            SyntaxKind::Error => u16::from_le_bytes([2, 0]),
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Language;

impl rowan::Language for Language {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind { raw.into() }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind { kind.into() }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SyntaxKind {
    Token(TokenKind),
    Node(NodeKind),
    Error,
}

macro_rules! nodes {
    ($($node:ident)*) => {
        #[repr(u8)]
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub enum NodeKind {
            $($node),*
        }

        impl NodeKind {
            fn decode(encoding: u8) -> Self {
                [$(Self::$node),*][encoding as usize]
            }
        }
    };
}

nodes! {
    Package
    Let
}

#[derive(Default)]
pub struct Builder {
    inner: RefCell<rowan::GreenNodeBuilder<'static>>,
}

pub struct Finisher<'a> {
    builder: &'a Builder,
}

#[derive(Clone)]
pub struct Parse {
    root: rowan::GreenNode,
}

pub struct Syntax {
    root: SyntaxNode,
}

impl Parse {
    pub fn syntax(&self) -> Syntax {
        Syntax {
            root: SyntaxNode::new_root(self.root.clone()),
        }
    }
}

impl Drop for Finisher<'_> {
    fn drop(&mut self) { self.builder.finish_node() }
}

impl Builder {
    pub fn finish(self) -> Parse {
        Parse {
            root: self.inner.into_inner().finish(),
        }
    }

    pub fn finish_node(&self) { self.inner.borrow_mut().finish_node() }

    pub fn checkpoint(&self) -> rowan::Checkpoint { self.inner.borrow().checkpoint() }

    pub fn start_node(&self, node: NodeKind) -> Finisher {
        self.inner.borrow_mut().start_node(SyntaxKind::Node(node).into());
        Finisher { builder: self }
    }

    pub fn start_node_at(&self, checkpoint: rowan::Checkpoint, node: NodeKind) {
        self.inner
            .borrow_mut()
            .start_node_at(checkpoint, SyntaxKind::Node(node).into())
    }

    pub fn token(&self, token: Token<'_>) {
        self.inner
            .borrow_mut()
            .token(SyntaxKind::Token(token.kind).into(), token.lexeme)
    }

    pub fn error(&self) {
        let mut inner = self.inner.borrow_mut();
        inner.start_node(SyntaxKind::Error.into());
        inner.finish_node();
    }

    pub fn error_token(&self, token: TokenKind) {
        let mut inner = self.inner.borrow_mut();
        inner.start_node(SyntaxKind::Error.into());
        inner.start_node(SyntaxKind::Token(token).into());
        inner.finish_node();
        inner.finish_node();
    }

    pub fn error_node(&self, node: NodeKind) {
        let mut inner = self.inner.borrow_mut();
        inner.start_node(SyntaxKind::Error.into());
        inner.start_node(SyntaxKind::Node(node).into());
        inner.finish_node();
        inner.finish_node();
    }
}

impl Display for Syntax {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result { Debug::fmt(self, f) }
}

impl Debug for Syntax {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        thread_local! {
            static TABS: Cell<u32> = Cell::new(0);
        }

        struct DecOnDrop;

        impl Drop for DecOnDrop {
            fn drop(&mut self) { TABS.with(|tabs| tabs.set(tabs.get() - 1)); }
        }

        fn tabs() -> u32 { TABS.with(Cell::get) }
        fn write_tabs(f: &mut Formatter<'_>) -> std::fmt::Result {
            let tabs = tabs();
            for _ in 0..tabs {
                write!(f, "  ")?
            }
            Ok(())
        }

        fn debug_node(node: SyntaxNode, f: &mut Formatter<'_>) -> std::fmt::Result {
            write_tabs(f)?;
            writeln!(f, "{:?}", node)?;
            TABS.with(|tabs| tabs.set(tabs.get() + 1));
            let _dec = DecOnDrop;

            for children in node.children_with_tokens() {
                match children {
                    rowan::NodeOrToken::Node(node) => {
                        debug_node(node, f)?;
                    }
                    rowan::NodeOrToken::Token(token) => {
                        debug_token(token, f)?;
                    }
                }
            }

            Ok(())
        }

        fn debug_token(token: SyntaxToken, f: &mut Formatter<'_>) -> std::fmt::Result {
            write_tabs(f)?;
            writeln!(f, "{:?}", token)?;
            Ok(())
        }

        debug_node(self.root.clone(), f)
    }
}
