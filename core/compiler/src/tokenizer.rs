use dec::base::{
    error::{Error, ErrorKind, PResult, ParseError},
    InputSplit, ParseOnce, Tag,
};

use crate::tokens::{Token, TokenKind};

pub fn tokenize<'input, E: ParseError<&'input str>>(input: &'input str) -> PResult<&'input str, Vec<Token>, E> {
    dec::seq::many0(token).parse_once(input)
}

pub fn token<'input, E: ParseError<&'input str>>(input: &'input str) -> PResult<&'input str, Token<'input>, E> {
    let (input, (lexeme, kind)) = dec::combinator::recognize(token_kind).parse_once(input)?;
    Ok((input, Token { lexeme, kind }))
}

pub fn token_kind<'input, E: ParseError<&'input str>>(input: &'input str) -> PResult<&'input str, TokenKind, E> {
    match input.find(|x: char| !x.is_whitespace()) {
        Some(0) => (),
        Some(at) => {
            let input = input.advance(at).unwrap();

            return Ok((input, TokenKind::WhiteSpace))
        }
        None => {
            if !input.is_empty() {
                let input = InputSplit::advance(input, input.len()).unwrap();
                return Ok((input, TokenKind::WhiteSpace))
            }
        }
    }

    if let Ok((new_input, _)) = "/*".parse_tag(input) {
        enum State {
            Nothing,
            LastSlash(usize),
            LastAsterix(usize),
        }

        let input_bytes = new_input.as_bytes();

        let mut state = State::Nothing;
        let mut stack: u64 = 0;

        let mut comment_end = None;

        for current in memchr::memchr2_iter(b'/', b'*', input_bytes) {
            match (input_bytes[current], state) {
                (b'/', State::LastAsterix(pos)) if current == pos => {
                    stack += 1;
                    state = State::Nothing;
                }
                (b'/', _) => state = State::LastSlash(current + 1),
                (b'*', State::LastSlash(pos)) if current == pos => {
                    if let Some(s) = stack.checked_sub(1) {
                        stack = s;
                        state = State::Nothing;
                    } else {
                        comment_end = Some(current);
                        break
                    }
                }
                (b'*', _) => state = State::LastAsterix(current + 1),
                _ => unreachable!(),
            }
        }

        return if let Some(comment_end) = comment_end {
            let input = input.advance(comment_end).unwrap();
            Ok((input, TokenKind::BlockComment))
        } else {
            Err(Error::Error(E::from_input_kind(
                input,
                ErrorKind::Custom("Unexpected EOF"),
            )))
        }
    }

    if input.starts_with("//") {
        let len = input.lines().next().unwrap().len();
        let input = input.advance(len).unwrap();

        return Ok((input, TokenKind::LineComment))
    }

    if input.starts_with(char::is_alphabetic) {
        let len = input.len();
        let end = input
            .char_indices()
            .find_map(|(i, c)| if c.is_alphanumeric() { None } else { Some(i) })
            .unwrap_or(len);
        let input = input.advance(end).unwrap();

        Ok((input, TokenKind::Ident))
    } else {
        let number = dec::map::map(
            dec::seq::snd(
                parse_digits,
                dec::combinator::opt(dec::seq::fst(dec::tag::tag("."), parse_digits)),
            ),
            |has_dot: Option<_>| {
                if has_dot.is_some() {
                    TokenKind::Number
                } else {
                    TokenKind::Integer
                }
            },
        );

        let symbol = dec::map::map(dec::tag::tag(crate::tokens::AnySymbol), TokenKind::Symbol);

        dec::branch::any((number, symbol)).parse_once(input)
    }
}

pub fn parse_digits<'input, E: ParseError<&'input str>>(input: &'input str) -> PResult<&'input str, (), E> {
    let trimmed = input.trim_start_matches(|c: char| c.is_ascii_digit());
    if input.len() == trimmed.len() {
        Err(Error::Error(E::from_input_kind(
            input,
            ErrorKind::Custom("No integer found"),
        )))
    } else {
        Ok((trimmed, ()))
    }
}
