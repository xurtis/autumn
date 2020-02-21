//! Parser abstrctions

pub mod combinators;
mod location;
mod parse;
pub mod parsers;

pub use location::{new_location, path_location, Location, Meta, Span};
pub use parse::{List, ParseResult, Parser};

pub mod prelude {
    pub use crate::combinators::{BoxedParserExt, ParserExt};
    pub use crate::parsers::*;
    pub use crate::{new_location, path_location, List, Meta, ParseResult, Parser, Span};
}

#[cfg(test)]
mod tests {
    use crate::prelude::*;

    const VALID_TOKENS: &'static [&'static str] = &["A", "ABC", "ABC123", "_ABC123"];

    fn token_prefix<L: Span>(source: &str, location: L) -> ParseResult<List<char>, L> {
        alphabetic.or(character('_')).parse(source, location)
    }

    fn token_suffix<L: Span>(source: &str, location: L) -> ParseResult<List<char>, L> {
        alphabetic
            .or(digit)
            .or(character('_'))
            .parse(source, location)
    }

    fn token<L: Span>(source: &str, location: L) -> ParseResult<String, L> {
        token_prefix
            .and(token_suffix.multiple().maybe())
            .map(|s| s.to_string())
            .parse(source, location)
    }

    fn space<L: Span>(source: &str, location: L) -> ParseResult<List<char>, L> {
        whitespace
            .and(whitespace.multiple().maybe())
            .parse(source, location)
    }

    #[test]
    fn simple_tokens() {
        for valid_token in VALID_TOKENS {
            println!("\n{:#?}", valid_token);
            let result = token.end().parse(valid_token, new_location());
            assert!(result.is_success());
            for value in result.values() {
                println!("{:?}", *value);
            }
        }
    }

    const VALID_SEQUENCES: &'static [&'static str] =
        &["The quick brown fox", "jumped over the lazy dog"];

    fn sequence<L: Span>(source: &str, location: L) -> ParseResult<List<String>, L> {
        fn token_list<L: Span>(source: &str, location: L) -> ParseResult<List<String>, L> {
            token(source, location).map(&List::single)
        }

        token_list
            .and(space.skip(token_list).multiple().maybe())
            .maybe()
            .drop(space.maybe())
            .end()
            .parse(source, location)
    }

    #[test]
    fn token_sequence() {
        println!();

        for token_sequence in VALID_SEQUENCES {
            println!("\n{:#?}", token_sequence);
            let result = sequence(token_sequence, new_location());
            assert!(result.is_success());
            for value in result.values() {
                println!("{:?}", *value);
            }
        }
    }
}
