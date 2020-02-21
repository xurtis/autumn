//! Parser abstrctions

pub mod combinators;
mod location;
mod parse;
pub mod parsers;
pub mod token;

pub use combinators::{BoxedParserExt, ParserExt};
pub use location::{new_location, path_location, Location, Meta, Span};
pub use parse::{parse, List, ParseResult, Parser};

#[cfg(test)]
mod tests {
    use crate::parsers::*;
    use crate::*;

    use crate::location::{new_location, Meta, Span};

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

    const S_EXPRESSIONS: &'static [&'static str] = &[
        "one",
        "1",
        "1.0",
        "( one)",
        "( one two three)",
        "( one 2 3.0 \"four\")",
        "(one (one two) (two three) \"four five\")",
    ];

    #[derive(Debug, Clone)]
    enum SExpression<L> {
        Atom(String),
        Integer(i32),
        Float(f32),
        String(String),
        List(List<Meta<SExpression<L>, L>>),
    }

    fn expr_space<L: Span>(source: &str, location: L) -> ParseResult<List<char>, L> {
        whitespace.multiple().parse(source, location)
    }

    fn atom_prefix<L: Span>() -> impl Parser<List<char>, L> {
        alphabetic.or(character('_').or(character('-')))
    }

    fn atom_char<L: Span>() -> impl Parser<List<char>, L> {
        atom_prefix().or(digit)
    }

    fn atom<L: Span>(source: &str, location: L) -> ParseResult<SExpression<L>, L> {
        atom_prefix()
            .and(atom_char().multiple().maybe())
            .map(|s| s.to_string())
            .map(SExpression::Atom)
            .parse(source, location)
    }

    fn integer<L: Span>(source: &str, location: L) -> ParseResult<SExpression<L>, L> {
        digit
            .multiple()
            .map(|i| i.to_string().parse().unwrap())
            .map(SExpression::Integer)
            .parse(source, location)
    }

    fn float<L: Span>(source: &str, location: L) -> ParseResult<SExpression<L>, L> {
        digit
            .multiple()
            .and(character('.').and(digit.multiple().maybe()))
            .map(|i| i.to_string().parse().unwrap())
            .map(SExpression::Float)
            .parse(source, location)
    }

    fn string<L: Span>(source: &str, location: L) -> ParseResult<SExpression<L>, L> {
        character('"')
            .skip(
                any_character
                    .condition(|c| c.clone().all(|c| *c != '\"'))
                    .or(character('\\').and(character('"')))
                    .multiple()
                    .maybe()
                    .drop(character('"')),
            )
            .map(|s| s.to_string())
            .map(SExpression::String)
            .parse(source, location)
    }

    fn list<L: Span>(source: &str, location: L) -> ParseResult<SExpression<L>, L> {
        fn expression_list<L: Span>(
            source: &str,
            location: L,
        ) -> ParseResult<List<Meta<SExpression<L>, L>>, L> {
            sexpression.meta().map(List::single).parse(source, location)
        }

        character('(')
            .and(expr_space.maybe())
            .skip(
                expression_list
                    .and(expr_space.skip(expression_list).multiple().maybe())
                    .drop(expr_space.maybe())
                    .maybe()
                    .drop(expr_space.maybe().and(character(')'))),
            )
            .map(|l| l.reverse())
            .map(SExpression::List)
            .parse(source, location)
    }

    fn sexpression<L: Span>(source: &str, location: L) -> ParseResult<SExpression<L>, L> {
        list.or(atom)
            .or(string)
            .or(float)
            .or(integer)
            .parse(source, location)
    }

    #[test]
    fn s_expressions() {
        println!();
        for expression in S_EXPRESSIONS {
            println!("\n{:#?}", expression);
            let result = sexpression.end().parse(expression, new_location());
            assert!(result.is_success());
            for value in result.values() {
                println!("{:#?}", *value);
            }
        }
    }
}
