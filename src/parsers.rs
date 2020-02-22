//! Commonly used parsers that can be combined with each other
//!
//! The functions here are either parsers in their own right or functions that produce parsers as
//! their output.
//!
//! Specifying characters
//! =====================
//!
//! The following parsers are provided that parse a single character matching a certain category of
//! characters. They all produce a [`List<char>`](../struct.List.html) of the character they match.
//!
//!  * [`any_character`](fn.any_character.html) will match any Unicode character.
//!  * [`alphabetic`](fn.alphabetic.html) will match an ASCII alphabetic character.
//!  * [`alphanumeric`](fn.alphanumeric.html) will match an ASCII alphanumeric character.
//!  * [`digit`](fn.digit.html) will match an ASCII decimal digit.
//!  * [`whitespace`](fn.whitespace.html) will match any Unicode whitespace character.
//!    [`space`](fn.space.html) is also provided to match one or more Unicode whitespace
//!    characters.
//!
//! These can be combined to produce many basic parsers.
//!
//! ```rust
//! # use autumn::prelude::*;
//! /// Parses C-like identifiers
//! fn identifier<L: Span>(source: &str, location: L) -> ParseResult<String, L> {
//!     alphabetic
//!         .or("_")
//!         .and(alphanumeric.or("_").multiple().maybe())
//!         .map(|s| s.to_string())
//!         .parse(source, location)
//! }
//!
//! /// Parses integers
//! fn integer<L: Span>(source: &str, location: L) -> ParseResult<String, L> {
//!     digit.multiple().map(|s| s.to_string()).parse(source, location)
//! }
//!
//! /// Parses float literals
//! fn float<L: Span>(source: &str, location: L) -> ParseResult<String, L> {
//!     digit
//!         .multiple()
//!         .and(".".and(digit.multiple()).maybe())
//!         .or(".".and(digit.multiple()))
//!         .map(|s| s.to_string())
//!         .parse(source, location)
//! }
//! ```
//!
//! Specific characters or strings
//! ==============================
//!
//! To parse a specific literal character or string, the corresponding `&str` or `String`
//! can be used directly as a parser. These types will all parse themselves from the input and
//! produce a [`List<char>`](../struct.List.html) corresponding to the matched characters.
//!
//! The [`empty`](fn.empty.html) parser is also provided to parse no characters and produce an
//! empty [`List<char>`](../struct.List.html).
//!
//! ```rust
//! # use autumn::prelude::*;
//! /// Parse C18 storage class specifiers
//! fn storage_class<L: Span>(source: &str, location: L) -> ParseResult<String, L> {
//!     "auto"
//!         .or("extern")
//!         .or("register")
//!         .or("static")
//!         .or("typedef")
//!         .or("_Thread_local")
//!         .map(|s| s.to_string())
//!         .parse(source, location)
//! }
//! ```
//!
//! Parsing a character rather than a list of characters
//! ----------------------------------------------------
//!
//! The [`character`](fn.character.html) parser consumes any Unicode character and
//! produces a `char` rather than a [`List<char>`](../struct.List.html). The
//! [`condition`](../combinators/trait.ParserExt.html#method.condition) combinator can be used to
//! restrict which character is matched.
//!
//! ```rust
//! # use autumn::prelude::*;
//! /// Parse simple single-character operators
//! fn operator<L: Span>(source: &str, location: L) -> ParseResult<char, L> {
//!     '+'.or('-').or('*').or('/').or('%').parse(source, location)
//! }
//! ```
//!
//! Closures as parsers
//! ===================
//!
//! Although the Rust compiler will quite happily accept any function with the appropriate
//! signature as a parser it requires additional help to determine that closures may also be
//! parsers. The [`closure`](fn.closure.html) simply ensures that a closure is appropriately
//! considered a parser.
//!
//! ```rust
//! # use autumn::prelude::*;
//! /// Parse an exact number of a specific character
//! fn counted<L: Span>(character: char, count: usize) -> impl Parser<List<char>, L> {
//!     closure(move |source, location| {
//!         if count > 0 {
//!             character
//!                 .map(List::single)
//!                 .and(counted(character, count - 1))
//!                 .parse(source, location)
//!         } else {
//!             empty.parse(source, location)
//!         }
//!     })
//! }
//! ```
//!
//! Parsers that produce errors
//! ===========================
//!
//! The [`error`](fn.error.html) and [`throw`](fn.throw.html) functions can be used when a
//! parse output needs to be checked to determine if the value is correct or a parse occurs that
//! relates to an error in the source. The [`value`](fn.value.html) parser can be used to produce a
//! matching parser if a check shows that no error has occurred.
//!
//! ```rust
//! # use autumn::prelude::*;
//! #[derive(Clone)]
//! struct InvalidIdentifier(String);
//!
//! /// Parses C-like identifiers
//! fn identifier<L: Span>(
//!     source: &str,
//!     location: L,
//! ) -> ParseResult<Option<String>, L, InvalidIdentifier> {
//!     alphabetic
//!         .or("_")
//!         .and(alphanumeric.or("_").multiple())
//!         .map(|s| Some(s.to_string()))
//!         .on_none(
//!             character
//!                 .condition(|c| !c.is_whitespace())
//!                 .map(List::single)
//!                 .multiple()
//!                 .map(|s| s.to_string())
//!                 .and_then(|identifier| throw(None, InvalidIdentifier(identifier)))
//!         )
//!         .catch()
//!         .parse(source, location)
//! }
//! ```

use crate::combinators::{Boxed, BoxedParserExt, ListParserExt, ParserExt};
use crate::location::Span;
use crate::{List, ParseResult, Parser};

/// Creates a parser that consumes no input and produces the given value as the result of parsing
///
/// See [parsers that produce errors](index.html#parsers-that-produce-errors).
///
/// This can be used if a particular value will be produced within an
/// [`and_then`](../combinators/trait.ParserExt.html#method.and_then) combinator.
///
/// ```rust
/// # use autumn::prelude::*;
/// fn alphabet<L: Span>(source: &str, location: L) -> ParseResult<String, L, &'static str> {
///     "abcde"
///         .and(digit)
///         .and_then(|text| {
///             let text = text.to_string();
///             if text.ends_with("0") {
///                 error(text, "Token must not end with 0")
///             } else {
///                 value(text)
///             }
///         })
///         .parse(source, location)
/// }
/// ```
pub fn value<'p, T: Clone + 'p, L: Span + 'p, E: 'p>(value: T) -> Boxed<dyn Parser<T, L, E> + 'p> {
    closure(move |source, location| success(value.clone(), source, location)).boxed()
}

/// Creates a parser that consumes no input and produces the given error as the result of parsing
///
/// See [parsers that produce errors](index.html#parsers-that-produce-errors).
///
/// This can be used if a particular value will be produced within an
/// [`and_then`](../combinators/trait.ParserExt.html#method.and_then) combinator.
///
/// ```rust
/// # use autumn::prelude::*;
/// fn alphabet<L: Span>(source: &str, location: L) -> ParseResult<String, L, &'static str> {
///     "abcde"
///         .and(digit)
///         .and_then(|text| {
///             let text = text.to_string();
///             if text.ends_with("0") {
///                 error(text, "Token must not end with 0")
///             } else {
///                 value(text)
///             }
///         })
///         .parse(source, location)
/// }
/// ```
pub fn error<'p, T: Clone + 'p, L: Span + 'p, E: Clone + 'p>(
    value: T,
    error: E,
) -> Boxed<dyn Parser<T, L, E> + 'p> {
    closure(move |source, location| failure(value.clone(), error.clone(), source, location)).boxed()
}

/// Creates a parser that consumes no input and produces the given error as an exception
///
/// See [parsers that produce errors](index.html#parsers-that-produce-errors).
///
/// This can be used if a particular value will be produced within an
/// [`and_then`](../combinators/trait.ParserExt.html#method.and_then) combinator. The exception
/// produced can be turned into an error with the
/// [`catch`](../combinators/trait.ParserExt.html#method.catch) combinator.
///
/// ```rust
/// # use autumn::prelude::*;
/// fn alphabet<L: Span>(source: &str, location: L) -> ParseResult<String, L, &'static str> {
///     "abcde"
///         .and(digit)
///         .and_then(|text| {
///             let text = text.to_string();
///             if text.ends_with("0") {
///                 throw(text, "Token must not end with 0")
///             } else {
///                 value(text)
///             }
///         })
///         .catch()
///         .parse(source, location)
/// }
/// ```
pub fn throw<'p, T: Clone + 'p, L: Span + 'p, E: Clone + 'p>(
    value: T,
    error: E,
) -> Boxed<dyn Parser<T, L, E> + 'p> {
    closure(move |source, location| exception(value.clone(), error.clone(), source, location))
        .boxed()
}

fn success<T, L: Span, E>(value: T, source: &str, location: L) -> ParseResult<T, L, E> {
    ParseResult::success(value, source, location)
}

fn failure<'s, T, L: Span, E>(
    value: T,
    error: E,
    source: &'s str,
    location: L,
) -> ParseResult<'s, T, L, E> {
    ParseResult::error(value, error, source, location)
}

fn exception<'s, T, L: Span, E>(
    value: T,
    error: E,
    source: &'s str,
    location: L,
) -> ParseResult<'s, T, L, E> {
    ParseResult::exception(value, error, source, location)
}

/// A parser that consumes no input and produces an empty [`List<char>`](../struct.List.html)
pub fn empty<T, L: Span, E>(source: &str, location: L) -> ParseResult<List<T>, L, E> {
    ParseResult::success(List::new(), source, location)
}


/// Converts a parser-like closure into an actual parser
///
/// Although the Rust compiler will quite happily accept any function with the appropriate
/// signature as a parser it requires additional help to determine that closures may also be
/// parsers. The [`closure`](fn.closure.html) simply ensures that a closure is appropriately
/// considered a parser.
///
/// ```rust
/// # use autumn::prelude::*;
/// /// Parse an exact number of a specific character
/// fn counted<L: Span>(character: char, count: usize) -> impl Parser<List<char>, L> {
///     closure(move |source, location| {
///         if count > 0 {
///             character
///                 .map(List::single)
///                 .and(counted(character, count - 1))
///                 .parse(source, location)
///         } else {
///             empty.parse(source, location)
///         }
///     })
/// }
/// ```
pub fn closure<F, T, L, E>(function: F) -> impl Parser<T, L, E>
where
    F: for<'s> Fn(&'s str, L) -> ParseResult<'s, T, L, E>,
{
    function
}

/// Parsers a single character from the input as an element of a
/// [`List<char>`](../struct.List.html)
///
/// See [specifying characters](index.html#specifying-characters).
pub fn any_character<L: Span, E>(source: &str, mut location: L) -> ParseResult<List<char>, L, E> {
    if let Some((_, next)) = source.char_indices().next() {
        location.after(next);
        ParseResult::success(List::new().push(next), &source[next.len_utf8()..], location)
    } else {
        ParseResult::none(location)
    }
}

/// Parses a single character from the input
pub fn character<L: Span, E>(source: &str, mut location: L) -> ParseResult<char, L, E> {
    if let Some((_, next)) = source.char_indices().next() {
        location.after(next);
        ParseResult::success(next, &source[next.len_utf8()..], location)
    } else {
        ParseResult::none(location)
    }
}

fn char_condition<'s, L: Span, E>(
    condition: &impl Fn(char) -> bool,
    source: &'s str,
    location: L,
) -> ParseResult<'s, List<char>, L, E> {
    character
        .condition(|c| condition(*c))
        .map(List::single)
        .parse(source, location)
}

impl<L: Span, E> Parser<char, L, E> for char {
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, char, L, E> {
        let expected = *self;
        character
            .condition(move |c| *c == expected)
            .parse(source, location)
    }
}

impl<L: Span, E> Parser<char, L, E> for &char {
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, char, L, E> {
        (*self).parse(source, location)
    }
}

/// Parses a single ASCII digit character from the input
///
/// See [specifying characters](index.html#specifying-characters).
pub fn digit<L: Span, E>(source: &str, location: L) -> ParseResult<List<char>, L, E> {
    char_condition(&|c| c.is_ascii_digit(), source, location)
}


/// Parses a single ASCII alphabetic character from the input
///
/// See [specifying characters](index.html#specifying-characters).
pub fn alphabetic<L: Span, E>(source: &str, location: L) -> ParseResult<List<char>, L, E> {
    char_condition(&|c| c.is_ascii_alphabetic(), source, location)
}


/// Parses a single ASCII alphabetic or digit character from the input
///
/// See [specifying characters](index.html#specifying-characters).
pub fn alphanumeric<L: Span, E>(source: &str, location: L) -> ParseResult<List<char>, L, E> {
    char_condition(&|c| c.is_ascii_alphanumeric(), source, location)
}


/// Parses a single Unicode whitespace character from the input
///
/// See [specifying characters](index.html#specifying-characters).
pub fn whitespace<L: Span, E>(source: &str, location: L) -> ParseResult<List<char>, L, E> {
    char_condition(&|c| c.is_whitespace(), source, location)
}

/// Parses one or more Unicode whitespace characters from the input
///
/// See [specifying characters](index.html#specifying-characters).
pub fn space<L: Span, E>(source: &str, location: L) -> ParseResult<List<char>, L, E> {
    whitespace.multiple().parse(source, location)
}

fn exact_rec<'s, L: Span, E>(
    exact: &str,
    source: &'s str,
    location: L,
) -> ParseResult<'s, List<char>, L, E> {
    if let Some(next) = exact.chars().next() {
        let remaining = &exact[next.len_utf8()..];
        next.map(List::single)
            .parse(source, location)
            .and_then(&|parsed, source, location| {
                exact_rec::<_, E>(remaining, source, location)
                    .map(&|remaining| parsed.concat(&remaining))
            })
    } else {
        empty.parse(source, location)
    }
}

impl<L: Span, E> Parser<List<char>, L, E> for str {
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, List<char>, L, E> {
        exact_rec(self, source, location)
    }
}

impl<L: Span, E> Parser<List<char>, L, E> for &str {
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, List<char>, L, E> {
        exact_rec(self, source, location)
    }
}

impl<L: Span, E> Parser<List<char>, L, E> for String {
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, List<char>, L, E> {
        exact_rec(self, source, location)
    }
}
