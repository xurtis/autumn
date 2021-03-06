//! Autumn is a library for building recursive descent parsers using combinators.
//! It provides a set of basic parsers for parsing individual characters and common groupings of
//! characters, primitives to collect parsed values, and combinators for combining and modifying
//! parsers.
//!
//! Getting started
//! ===============
//!
//! A parser is an item that implements the [`Parser`](trait.Parser.html) trait. The most simple
//! way to implement the trait is with a function given the following signature:
//!
//! ```rust
//! # use autumn::prelude::*;
//! fn parser(source: &str, location: Span) -> ParseResult<String> {
//!     /* ... */
//! #   unimplemented!()
//! }
//! ```
//!
//! This is a function that takes an input string slice its location in some original source text
//! and will parse a `String` value starting at that location.
//!
//! A simple identifier parser
//! --------------------------
//!
//! The following is an example that implements a parser of C-like identifiers. These begin with an
//! ASCII letter or underscore and are followed by any number of ASCII digits, letters, or
//! underscores.
//!
//! ```rust
//! # use autumn::prelude::*;
//! /// Parse a single alphabetic character or underscore
//! fn identifier_prefix(source: &str, location: Span) -> ParseResult<Span> {
//!     alphabetic.or("_").parse(source, location)
//! }
//!
//! /// Parse a zero or more letters, digits, and underscores
//! fn identifier_body(source: &str, location: Span) -> ParseResult<Span> {
//!     identifier_prefix.or(digit).multiple().maybe().parse(source, location)
//! }
//!
//! /// Accumulate the characters for a single identifier into a string
//! fn identifier(source: &str, location: Span) -> ParseResult<String> {
//!     identifier_prefix
//!         .and(identifier_body)
//!         .copy_string()
//!         .parse(source, location)
//! }
//! # for code in &["hello", "world", "_underscore", "_with_numb3r5"] {
//! #     // Parse each string as a single identifier, ensure the whole string is consumed
//! #     let result = parse(identifier, code);
//! #     assert!(result.is_success());
//! #     assert!(result.single_parse());
//! #     let value = result.values().next().unwrap();
//! #     assert_eq!(value, code);
//! # }
//! # for code in &["12l", "5.3", "function(12)", "1 + 2"] {
//! #     // Parse each string as a single identifier, ensure the whole string is consumed
//! #     let result = parse(identifier, code);
//! #     assert!(!result.is_success());
//! # }
//! ```
//!
//! As the functions above each implement the [`Parser`](trait.Parser.html) trait,
//! [combinators](combinators/trait.ParserExt.html) can be used directly on the functions.
//!
//! A number of the most common combinators are shown in the example above.
//!
//!  * [`or`](combinators/trait.ParserExt.html#method.or) will take a parser and _additionally_
//!    try an alternative parser at the same location. This can produce a result from both parsers
//!    if they ar successful.
//!
//!  * [`and`](combinators/trait.ParserExt.html#method.and) will take a parser that produces a
//!    [`List`](list/struct.List.html) and append the result of another parser that produces the
//!    same type of list.
//!
//!  * [`multiple`](combinators/trait.ParserExt.html#method.multiple) will take a parser that
//!    produces a [`List`](list/struct.List.html) and attempt to apply that parser one or more
//!    times in succession.
//!
//!  * [`maybe`](combinators/trait.ParserExt.html#method.maybe) will take a parser that produces a
//!    [`List`](list/struct.List.html) and attempt to apply that parser zero or one times. When
//!    using both [`multiple`](combinators/trait.ParserExt.html#method.multiple) and
//!    [`maybe`](combinators/trait.ParserExt.html#method.maybe) to achieve zero or more
//!    repetitions, `multiple().maybe()` must be used; `maybe().multiple()` can find an infinite
//!    number of ways to apply any parser on even an empty string.
//!
//!  * [`map`](combinators/trait.ParserExt.html#method.map) can be used to transform the type
//!    produced by a particular parser.
//!
//!  * [`end`](combinators/trait.ParserExt.html#method.end) will only produce a successful parse if
//!    there is no input remaining after the parser.
//!
//! A few of the provided [parsers](parsers/index.html) have also been used above.
//!
//!  * [`alphabetic`](parsers/fn.alphabetic.html) will parse a single character that is an ASCII
//!    alphabetic character and produce a [`List<char>`](list/struct.List.html).
//!
//!  * [`digit`](parsers/fn.digit.html) will parse a single character that is an ASCII digit
//!    character and produce a [`List<char>`](list/struct.List.html).
//!
//!  * Any `&str` or `String` can be used to parse itself and produce a
//!    [`List<char>`](list/struct.List.html).
//!
//! When invoking a parser the source must be provided as a string slice and the current position
//! must be provided as a [`Span`](trait.Span.html). An intial span can be provided for the start
//! of any string using [`new_location`](fn.new_location.html) but a span associated with a path to
//! a file should be specified using [`path_location`](fn.path_location.html).
//!
//! The [`ParseResult`](struct.ParseResult.html) produced by a parser contains all valid parsings
//! of the input string.
//!
//! If an ambiguous parser is constructed, every unique manner in which that parser could be used
//! to process the input will be produced;
//! [`single_parse`](struct.ParseResult.html#method.single_parse) can be used to check that only a
//! single valid parse was produced.
//!
//! Every successful parse is encapsulated in a [`Meta`](struct.Meta.html) that associates it with
//! a [`Span`](trait.Span.html) that reflects the range within the source that contains the
//! characters parsed for that result. The [`meta`](struct.MetaMap.html) combinator can be used to
//! obtain the location associated with a value during parsing.
//!
//! Errors
//! ======
//!
//! A parser may also need to produce errors while parsing. The parser will discard errors that are
//! not associated with a valid parse so *some* value must be associated with the error and the
//! error will be associated with a particular location in the parse.
//!
//! If there are errors associated with a [`ParseResult`]() their type must be passed as the third
//! type argument to the type constructor.
//!
//! If any valid parse of the source produced errors
//! [`is_success`](struct.ParseResult.html#method.is_success) will return `false`.
//!
//! The following example will either parse the first five letters of the alphabet in lower-case or
//! will produce an error associated with the alphabetic characters starting at the same location.
//!
//! ```rust
//! # use autumn::prelude::*;
//! /// Parses the first 5 letters of the alphabet in order
//! fn alphabet_parse() -> impl Parser<Span, &'static str> {
//!     "abcde"
//!         .on_none(
//!             alphabetic
//!                 .multiple()
//!                 .maybe()
//!                 .and_then(|text| error(text, "Not in alphabetical order"))
//!         )
//! }
//! ```
//!
//! The [`on_none`](combinators/trait.ParserExt.html#method.on_none) combinator can be used to
//! provide an alternative parser that is used only if the original parser was unable to find any
//! valid parse (including parsers that resulted in an error).
//!
//! The [`on_failure`](combinators/trait.ParserExt.html#method.on_failure) combinator can be used
//! to provide an alternative parser that is used when the original parser produced an error or if
//! no valid parse was found or if a valid parse associated with an error was found.
//!
//! The [`error`](parsers/fn.error.html) parser will produce a valid parse with the provided value
//! but will also produce the provided error. This allows parsing to continue and find further
//! errors.
//!
//! The [`ParseResult`](struct.ParseResult.html) produced by a parser contains all errors produced
//! by all valid parsings of the input. Each [`error`](parsers/fn.error.html) will be associated
//! with the location in the source where the error was produced.
//!
//! Exceptions
//! ----------
//!
//! Exceptions can be used to associate errors with an exact span of input rather than a single
//! location. The [`throw`](parsers/fn.throw.html) parser can be used exactly like the
//! [`error`](parsers/fn.error.html) parser except it creates an _exception_, rather than an error,
//! at the same location. The [`catch`](combinators/trait.ParserExt.html#method.catch) combinator
//! can then be used to convert the exception into an error and extend the start of the span
//! associated with the error all the way back to the start of the original parse.
//!
//! The following example shows how the error produced by the `alphabet_parse` function above can
//! be associated with the span of source code that was consumed to produce the error.
//!
//! ```rust
//! # use autumn::prelude::*;
//! /// Parses the first 5 letters of the alphabet in order
//! fn alphabet_parse() -> impl Parser<Span, &'static str> {
//!     "abcde"
//!         .on_none(
//!             alphabetic
//!                 .multiple()
//!                 .maybe()
//!                 .and_then(|text| throw(text, "Not in alphabetical order"))
//!         )
//!         .catch()
//! }
//! ```

pub mod combinators;
mod location;
mod parse;
pub mod parsers;

pub use location::{new_location, path_location, Location, Meta, Span};
pub use parse::{list, Concat, ParseResult, Parser};

/// Common items from the library used when building parsers
pub mod prelude {
    pub use crate::combinators::{
        BoxedParserExt, ConcatParserExt, ListParserExt, ParserExt, TextParserExt,
    };
    pub use crate::list::List;
    pub use crate::parse;
    pub use crate::parsers::*;
    pub use crate::{new_location, path_location, Concat, Meta, ParseResult, Parser, Span};
}

/// Parse a source text using a given parser
pub fn parse<T, E, P: Parser<T, E>>(parser: P, source: &str) -> ParseResult<T, E> {
    use combinators::ParserExt;
    parser.end().parse(source, new_location())
}

#[cfg(test)]
mod tests {
    use crate::prelude::*;

    const VALID_TOKENS: &'static [&'static str] = &["A", "ABC", "ABC123", "_ABC123"];

    fn token_prefix(source: &str, location: Span) -> ParseResult<Span> {
        alphabetic.or("_").parse(source, location)
    }

    fn token_suffix(source: &str, location: Span) -> ParseResult<Span> {
        alphabetic.or(digit).or("_").parse(source, location)
    }

    fn token(source: &str, location: Span) -> ParseResult<String> {
        token_prefix
            .and(token_suffix.multiple().maybe())
            .copy_string()
            .parse(source, location)
    }

    fn space(source: &str, location: Span) -> ParseResult<Span> {
        whitespace
            .and(whitespace.multiple().maybe())
            .parse(source, location)
    }

    #[test]
    fn simple_tokens() {
        for valid_token in VALID_TOKENS {
            println!("\n{:#?}", valid_token);
            let result = parse(token, valid_token);
            assert!(result.is_success());
            for value in result.values() {
                println!("{:?}", *value);
            }
        }
    }

    const VALID_SEQUENCES: &'static [&'static str] =
        &["The quick brown fox", "jumped over the lazy dog"];

    fn sequence(source: &str, location: Span) -> ParseResult<Vec<String>> {
        fn token_list(source: &str, location: Span) -> ParseResult<List<String>> {
            token.to_list().parse(source, location)
        }

        token_list
            .and(space.skip(token_list).multiple().maybe())
            .maybe()
            .drop(space.maybe())
            .end()
            .collect()
            .parse(source, location)
    }

    #[test]
    fn token_sequence() {
        println!();

        for token_sequence in VALID_SEQUENCES {
            println!("\n{:#?}", token_sequence);
            let result = parse(sequence, token_sequence);
            assert!(result.is_success());
            for value in result.values() {
                println!("{:?}", *value);
            }
        }
    }
}
