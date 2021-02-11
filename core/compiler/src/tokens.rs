use std::ops::Range;

use dec::base::{error, Parse};
use error::{CaptureInput, PResult};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Token<'input> {
    pub lexeme: &'input str,
    pub kind: TokenKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    Ident,
    WhiteSpace,
    Integer,
    Number,
    LineComment,
    BlockComment,
    Error,
    Keyword(Keyword),
    Symbol(Symbol),
}

impl From<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Self {
        Span {
            start: range.start,
            end: range.end,
        }
    }
}

impl Span {
    pub fn range(self) -> Range<usize> { self.start..self.end }

    pub fn to(self, other: Self) -> Self {
        debug_assert!(self.start <= self.end);
        debug_assert!(other.start <= other.end);
        debug_assert!(self.end <= other.start);

        Self {
            start: self.start,
            end: other.end,
        }
    }
}

impl TokenKind {
    pub fn encode(self) -> u8 {
        match self {
            Self::Ident => 0,
            Self::WhiteSpace => 1,
            Self::Integer => 2,
            Self::Number => 3,
            Self::LineComment => 4,
            Self::BlockComment => 5,
            Self::Error => 6,
            Self::Keyword(keyword) => keyword as u8 + 7,
            Self::Symbol(symbol) => symbol as u8 + 7 + Keyword::COUNT,
        }
    }

    pub fn decode(encoding: u8) -> Self {
        const KEYWORD_END: u8 = 6 + Keyword::COUNT;
        match encoding {
            0 => Self::Ident,
            1 => Self::WhiteSpace,
            2 => Self::Integer,
            3 => Self::Number,
            4 => Self::LineComment,
            5 => Self::BlockComment,
            6 => Self::Error,
            7..=KEYWORD_END => Self::Keyword(Keyword::decode(encoding - 7)),
            _ => Self::Symbol(Symbol::decode(encoding - 7 - Keyword::COUNT)),
        }
    }
}

macro_rules! symbol {
    ($($name:ident($symbol:literal))*) => {
        #[repr(u8)]
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub enum Symbol {
            $($name),*
        }

        impl From<Symbol> for TokenKind {
            fn from(kw: Symbol) -> Self {
                Self::Symbol(kw)
            }
        }

        impl Symbol {
            fn decode(encoding: u8) -> Self {
                [$(Self::$name),*][encoding as usize]
            }
        }

        pub(crate) struct AnySymbol;
        impl<'input> dec::base::Tag<&'input str> for AnySymbol {
            type Output = Symbol;

            #[cfg(not(feature = "phf"))]
            fn parse_tag(&self, input: &'input str) -> PResult<&'input str, Self::Output, CaptureInput<&'input str>> {
                $(if let Some(input) = input.strip_prefix($symbol) {
                    return Ok((input, Symbol::$name))
                })*

                Err(dec::base::error::Error::Error(CaptureInput(input)))
            }

            #[cfg(feature = "phf")]
            fn parse_tag(&self, input: &'input str) -> PResult<&'input str, Self::Output, CaptureInput<&'input str>> {
                use phf::Map;

                static SYMBOLS: Map<&'static str, Symbol> = phf::phf_map! {
                    $($symbol => Symbol::$name),*
                };

                const MAX_CUT: usize = {
                    let mut max = 0;
                    $(if max < $symbol.len() {
                        max = $symbol.len()
                    })*
                    max
                };

                for i in (1..MAX_CUT + 1).rev() {
                    if let Some(sym) = input.get(..i) {
                        if let Some(&symbol) = SYMBOLS.get(sym) {
                            return Ok((&input[i..], symbol))
                        }
                    }
                }

                Err(dec::base::error::Error::Error(CaptureInput(input)))
            }
        }

        impl<'input> dec::base::Tag<&'input str> for Symbol {
            type Output = Self;

            fn parse_tag(&self, input: &'input str) -> PResult<&'input str, Self::Output, CaptureInput<&'input str>> {
                match self {
                    $(Self::$name => if let Some(input) = input.strip_prefix($symbol) {
                        return Ok((input, *self))
                    })*
                }

                Err(dec::base::error::Error::Error(CaptureInput(input)))
            }
        }
    };
}

macro_rules! keyword {
    ($($name:ident($keyword:ident))*) => {
        #[repr(u8)]
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub enum Keyword {
            $($name),*
        }

        impl From<Keyword> for TokenKind {
            fn from(kw: Keyword) -> Self {
                Self::Keyword(kw)
            }
        }

        impl Keyword {
            const COUNT: u8 = [$(stringify!($keyword)),*].len() as u8;

            fn decode(encoding: u8) -> Self {
                [$(Self::$name),*][encoding as usize]
            }
        }

        impl core::str::FromStr for Keyword {
            type Err = ();

            fn from_str(input: &str) -> Result<Self, Self::Err> {
                match input {
                    $( stringify!($keyword) => Ok(Self::$name), )*
                    _ => Err(())
                }
            }
        }
    };
}

symbol! {
    Equal("==")
    NotEqual("!=")
    LessEqual("<=")
    GreaterEqual(">=")

    Plus("+")
    Hyphen("-")
    Asterix("*")
    ForSlash("/")
    BackSlash("\\")

    StartCurly("{")
    EndCurly("}")
    StartParen("(")
    EndParen(")")
    StartSquare("[")
    EndSquare("]")
    Assign("=")
    Bang("!")
    Less("<")
    Greater(">")
    Colon(":")
    Comma(",")
    SemiColon(";")
}

keyword! {
    Let(let)
    Mut(mut)

    Loop(loop)
    Break(break)
    Continue(continue)

    If(if)
    Else(else)
    Match(match)
    Return(return)

    Struct(struct)
    Union(union)
    Enum(enum)
}

impl<'input, E: dec::base::error::ParseError<&'input str>> dec::base::ParseOnce<&'input str, E> for Symbol {
    type Output = Self;

    fn parse_once(self, input: &'input str) -> PResult<&'input str, Self::Output, E> {
        dec::tag::tag(self).parse(input)
    }
}

impl<'input, E: dec::base::error::ParseError<&'input str>> dec::base::ParseMut<&'input str, E> for Symbol {
    fn parse_mut(&mut self, input: &'input str) -> PResult<&'input str, Self::Output, E> {
        dec::tag::tag(*self).parse(input)
    }
}

impl<'input, E: dec::base::error::ParseError<&'input str>> dec::base::Parse<&'input str, E> for Symbol {
    fn parse(&self, input: &'input str) -> PResult<&'input str, Self::Output, E> { dec::tag::tag(*self).parse(input) }
}
