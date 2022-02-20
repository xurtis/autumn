//! Traits for combining and modifying parsers
//!
//! Combinators provide ways of modifying the behavior of parsers and combining parsers that is
//! consistent across all parser implementations.
//!
//! Parsing sequences
//! =================
//!
//! Parsing sequences of characters or values from input is done using parsers that produce a
//! [`Concat`](../trait.Concat.html) and the [`ConcatParserExt`](trait.ConcatParserExt.html) trait that is
//! implemented for all such parsers.
//!
//! The [`ConcatParserExt`](trait.ConcatParserExt.html) provides three combinators for manipulating
//! parsers over sequences of values:
//!
//!  * [`maybe`](trait.ConcatParserExt.html#method.maybe) repeats a parser zero or one times,
//!  * [`multiple`](trait.ConcatParserExt.html#method.multiple) repeats a parser one or more times,
//!    and
//!  * [`and`](trait.ConcatParserExt.html#method.and) uses two parsers in sequence, concatenating
//!    their results into a single list.
//!
//! ```rust
//! # use autumn::prelude::*;
//! /// Parses C-like identifiers
//! fn identifier(source: &str, location: Span) -> ParseResult<String> {
//!     alphabetic
//!         .or("_")
//!         .and(alphanumeric.or("_").multiple().maybe())
//!         .copy_string()
//!         .parse(source, location)
//! }
//! ```
//!
//! When using both [`multiple`](trait.ParserExt.html#method.multiple) and
//! [`maybe`](trait.ParserExt.html#method.maybe) to achieve zero or more repetitions,
//! `multiple().maybe()` must be used; `maybe().multiple()` can find an infinite number of ways to
//! apply any parser on even an empty string.
//!
//! [`and`](trait.ConcatParserExt.html#method.and) and
//! [`multiple`](trait.ConcatParserExt.html#method.multiple) when used with parsers producing a
//! [`Span`](../struct.Span.html) must ensure that all spans are adjacent (don't use
//! [`drop`](trait.ParserExt.html#method.drop) or [`skip`](trait.ParserExt.html#method.skip) to
//! join a pair of [`Span`](../struct.Span.html) results that are not continuous).
//!
//! Multiple potential parsers
//! ==========================
//!
//! The [`or`](trait.ParserExt.html#method.or) will take two parsers and attempt to evaluate both
//! of them returning any successful results produced by either of them. This can include duplicate
//! results if a given parse would be acceptable for either parser.
//!
//! Changing the result of a parser
//! ===============================
//!
//! There are to ways in which the result of a parser can be changed:
//!
//!  * the output value and type can be simply mapped to a new value with a new type using the
//!    [`map`](trait.ParserExt.html#method.map) operation, and
//!  * changing a successful (or already failing) parse into a potentially failing parse with the
//!    [`and_then`](trait.ParserExt.html#method.and_then) operation.
//!
//! ```rust
//! # use autumn::prelude::*;
//! #[derive(Clone)]
//! struct InvalidIdentifier(String);
//!
//! /// Parses C-like identifiers
//! fn identifier(
//!     source: &str,
//!     location: Span,
//! ) -> ParseResult<Option<String>, InvalidIdentifier> {
//!     alphabetic
//!         .or("_")
//!         .and(alphanumeric.or("_").multiple())
//!         .copy_string()
//!         .map(Some)
//!         .on_none(
//!             any_character
//!                 .str_condition(|s| !s.chars().any(char::is_whitespace))
//!                 .multiple()
//!                 .copy_string()
//!                 .and_then(|identifier| throw(None, InvalidIdentifier(identifier)))
//!         )
//!         .catch()
//!         .parse(source, location)
//! }
//! ```
//!
//! Discarding the result of a parser
//! =================================
//!
//! When using two parsers sequentially you may wish to discard the result of either sequence
//! rather than concatenate the results.
//!
//! The [`skip`](trait.ParserExt.html#method.skip) method can be used to combine two parsers,
//! discarding the result of the first and keeping the result of the second.
//!
//! The [`drop`](trait.ParserExt.html#method.drop) method can be used to combine two parsers,
//! keeping the result of the first and discarding the result of the second.
//!
//! ```rust
//! # use autumn::prelude::*;
//! // Produces "right"
//! fn right() -> impl Parser<String> {
//!     "left".skip("right").copy_string()
//! }
//! let skipped = parse(right(), "leftright");
//! # assert_eq!(skipped.values().next().unwrap(), "right");
//!
//! // Produces "left"
//! fn left() -> impl Parser<String> {
//!     "left".drop("right").copy_string()
//! }
//! let dropped = parse(left(), "leftright");
//! # assert_eq!(dropped.values().next().unwrap(), "left");
//! ```
//!
//! Adding conditions to a parser
//! =============================
//!
//! Two methods are provided to require the result of a parser to meet a certain condition:
//!
//!  * [`condition`](trait.ParserExt.html#method.condition) allows a certain condition to be
//!    specified as a function or closure, and
//!  * [`matching`](trait.ParserExt.html#method.matching) allows a value to be provided which the
//!    result must match.
//!
//! Ensuring a parser consumes all input
//! ====================================
//!
//! The [`end`](trait.ParserExt.html#method.end) can be used on any parser to ensure that after any
//! successful parse there is no text left un-parsed in the source. If not used, invoking a parser
//! directly can produce all prefixes that are themselves a valid parse of the source string.
//!
//! Finding the source location for the result of a parser
//! ======================================================
//!
//! When parsing a structured text input a parser may wish to know the location of a segment that
//! was parsed to produce a result. The [`meta`](trait.ParserExt.html#method.meta) method will map
//! a parser such that its result is wrapped in a [`Meta`](../struct.Meta.html), binding it with
//! a [`Span`](trait.Span.html) specifying the location in the source of the parsed text.
//!
//! ```rust
//! # use autumn::prelude::*;
//! /// Parses integers
//! fn integer(source: &str, location: Span) -> ParseResult<String> {
//!     digit.multiple().copy_string().parse(source, location)
//! }
//!
//! /// Parse a list of integers and get the source location of each integer
//! fn integer_list(source: &str, location: Span) -> ParseResult<Vec<Meta<String, Span>>> {
//!     integer
//!         .meta()
//!         .delimited_by(",".maybe_space_after(), ..)
//!         .surrounded_by("[".maybe_space_after(), "]")
//!         .collect()
//!         .parse(source, location)
//! }
//! # assert!(parse(integer_list, "[]").is_success());
//! # assert!(parse(integer_list, "[1, 2]").is_success());
//! # assert!(parse(integer_list, "[1, 2, 3]").is_success());
//! # assert!(parse(integer_list, "[1, 2, 3, 4, ]").is_success());
//! ```
//!
//! Matching parser types with dynamic dispatch
//! ===========================================
//!
//! There are some cases where a parser may need to produce one of many values within the argument
//! to the [`and_then`](trait.ParserExt.html#method.and_then) method. In this case it helps to be
//! able to unify the types of the values with [dynamic
//! dispatch](https://doc.rust-lang.org/book/ch17-02-trait-objects.html#trait-objects-perform-dynamic-dispatch).
//!
//! This can be achieved simply with the [`boxed`](trait.BoxedParserExt.html#method.boxed) method.
//!
//! The [`value`](../parsers/fn.value.html), [`error`](../parsers/fn.error.html), and
//! [`throw`](../parsers/fn.throw.html) functions all produce boxed parsers with types that will
//! match as they are commonly used in this case.
//!
//! ```rust
//! # use autumn::prelude::*;
//! fn alphabet(source: &str, location: Span) -> ParseResult<String, &'static str> {
//!     "abcde"
//!         .and(digit)
//!         .copy_string()
//!         .and_then(|text| {
//!             let text = text;
//!             if text.ends_with("0") {
//!                 throw(text, "Token must not end with 0")
//!             } else {
//!                 value(text)
//!             }
//!         })
//!         .catch()
//!         .parse(source, location)
//! }
//! ```

use crate::location::{Meta, Span};
use crate::parse::{list::List, Concat, ParseResult, Parser};
use crate::parsers::{empty, space, value};
use std::iter::FromIterator;
use std::marker::PhantomData;
use std::ops::{Bound, RangeBounds};

/// Combinators that can be used on all parsers
pub trait ParserExt<T, E>: Parser<T, E> + Sized {
    /// Evaluate two alternative parsers producing all successful parses from both
    ///
    /// ```rust
    /// # use autumn::prelude::*;
    /// fn alpha_or_beta() -> impl Parser<String> {
    ///     "alpha".or("beta").copy_string()
    /// }
    /// ```
    fn or<P: Parser<T, E>>(self, other: P) -> Or<Self, P, E> {
        Or(self, other, PhantomData)
    }

    /// Map the output of a parser to a new value and type
    ///
    /// ```rust
    /// # use autumn::prelude::*;
    /// use std::str::FromStr;
    /// fn integer() -> impl Parser<i32> {
    ///     digit.multiple().copy_string().map(|i| FromStr::from_str(&i).unwrap())
    /// }
    /// ```
    fn map<V, F: Fn(T) -> V>(self, map: F) -> Map<Self, F, T, E> {
        Map(self, map, PhantomData)
    }

    /// Apply an additional parser dependant on the result of the preceding parser
    ///
    /// ```rust
    /// # use autumn::prelude::*;
    /// use std::str::FromStr;
    /// enum Token {
    ///     Identifier(String),
    ///     Literal(i32),
    /// }
    /// use Token::*;
    ///
    /// fn identifier() -> impl Parser<String> {
    ///     alphabetic.and(alphanumeric.multiple().maybe()).copy_string()
    /// }
    ///
    /// fn literal() -> impl Parser<i32> {
    ///     "-".maybe()
    ///         .and(digit.multiple())
    ///         .copy_string()
    ///         .map(|i| FromStr::from_str(&i).unwrap())
    /// }
    ///
    /// fn tagged_token() -> impl Parser<Token> {
    ///     "identifier"
    ///         .or("literal")
    ///         .drop(space)
    ///         .copy_string()
    ///         .and_then(|parsed| {
    ///             match parsed.as_str() {
    ///                 "identifier" => identifier().map(Identifier).boxed(),
    ///                 "literal" => literal().map(Literal).boxed(),
    ///                 _ => unreachable!()
    ///             }
    ///         })
    /// }
    /// ```
    fn and_then<V, Q: Parser<V, E>, F: Fn(T) -> Q>(self, map: F) -> AndThen<Self, F, T, E> {
        AndThen(self, map, PhantomData)
    }

    /// Repeatedly apply a parser using it to extend the previous results of the parse
    ///
    /// This allows for a form of 'left recursion' where a parser rule includes itself as its first
    /// parser.
    ///
    /// ```rust
    /// # use autumn::prelude::*;
    /// fn identifier() -> impl Parser<String> {
    ///     alphabetic.and(alphanumeric.repeated(..)).copy_string()
    /// }
    ///
    /// fn concat() -> impl Parser<String> {
    ///     identifier()
    ///         .fold(|left| {
    ///             "##".maybe_space_around()
    ///                 .skip(identifier())
    ///                 .map(move |right| {
    ///                     let mut left = left.clone();
    ///                     left.push_str(&right);
    ///                     left
    ///                 })
    ///         })
    /// }
    /// # assert!(parse(concat(), "left").single_parse());
    /// # assert!(parse(concat(), "left ## right").single_parse());
    /// # assert!(parse(concat(), "left ## middle ## right").single_parse());
    /// ```
    fn fold<P: Parser<T, E>, F: Fn(T) -> P>(self, fold: F) -> Fold<Self, F, E> {
        Fold(self, fold, PhantomData)
    }

    /// Apply an alternative parser if the preceding parser produces errors or no successful parses
    ///
    /// ```rust
    /// # use autumn::prelude::*;
    /// fn failing_parser() -> impl Parser<String, &'static str> {
    ///     "hello"
    ///         .and("world")
    ///         .copy_string()
    ///         .on_none(error(String::new(), "Not hello world"))
    ///         .on_failure(error(String::new(), "Caught error"))
    /// }
    /// ```
    fn on_failure<P: Parser<T, E>>(self, other: P) -> OnFailure<Self, P, E> {
        OnFailure(self, other, PhantomData)
    }

    /// Apply an alternative parser if the preceding parser produces no successful parses
    ///
    /// ```rust
    /// # use autumn::prelude::*;
    /// fn parse_handle_none() -> impl Parser<String, &'static str> {
    ///     "hello"
    ///         .and("world")
    ///         .copy_string()
    ///         .on_none(error(String::new(), "Not hello world"))
    /// }
    /// ```
    fn on_none<P: Parser<T, E>>(self, other: P) -> OnNone<Self, P, E> {
        OnNone(self, other, PhantomData)
    }

    /// Apply an additional parser and discard the result
    ///
    /// ```rust
    /// # use autumn::prelude::*;
    /// fn drop_trailing_whitespace() -> impl Parser<String> {
    ///     "hello".drop(space).copy_string()
    /// }
    /// ```
    fn drop<V, P: Parser<V, E>>(self, other: P) -> Drop<Self, P, V, E> {
        Drop(self, other, PhantomData)
    }

    /// Apply an additional parser and discard any result
    ///
    /// ```rust
    /// # use autumn::prelude::*;
    /// fn drop_trailing_whitespace() -> impl Parser<String> {
    ///     "hello".maybe_drop(space).copy_string()
    /// }
    /// ```
    fn maybe_drop<V, P: Parser<V, E>>(self, other: P) -> MaybeDrop<Self, P, V, E> {
        MaybeDrop(self, other, PhantomData)
    }

    /// Apply an additional parser and take its output instead
    ///
    /// ```rust
    /// # use autumn::prelude::*;
    /// fn drop_leading_whitespace() -> impl Parser<String> {
    ///     space.skip("hello").copy_string()
    /// }
    /// ```
    fn skip<V, P: Parser<V, E>>(self, keep: P) -> Skip<Self, P, T, E> {
        Skip(self, keep, PhantomData)
    }

    /// Only parse successfully if the output of the parser satisfies a given condition
    ///
    /// ```rust
    /// # use autumn::prelude::*;
    /// use std::str::FromStr;
    /// fn fourty_two() -> impl Parser<i32> {
    ///     digit
    ///         .multiple()
    ///         .copy_string()
    ///         .map(|i| FromStr::from_str(&i).unwrap())
    ///         .condition(|i| *i == 42)
    /// }
    /// ```
    fn condition<F: Fn(&T) -> bool>(self, condition: F) -> Condition<Self, F, E> {
        Condition(self, condition, PhantomData)
    }

    /// Only parse successfully if there is no text remaining in the input after a parse
    ///
    /// ```rust
    /// # use autumn::prelude::*;
    /// fn single_line() -> impl Parser<String> {
    ///     any_character
    ///         .str_condition(|s| !s.chars().any(|c| c == '\n'))
    ///         .drop("\n")
    ///         .copy_string()
    ///         .end()
    /// }
    /// ```
    fn end(self) -> End<Self, E> {
        End(self, PhantomData)
    }

    /// Transform all exceptions in the result into errors, moving the start of the associated
    /// range to the start of the given parser.
    ///
    /// See [exceptions](../index.html#exceptions).
    fn catch(self) -> Catch<Self, E> {
        Catch(self, PhantomData)
    }

    /// Wrap the output of a parser with the location of the input parsed
    ///
    /// ```rust
    /// # use autumn::prelude::*;
    /// fn identifier_location() -> impl Parser<Meta<String, Span>> {
    ///     alphabetic
    ///         .and(alphanumeric.multiple().maybe())
    ///         .copy_string()
    ///         .meta()
    /// }
    /// ```
    fn meta(self) -> MetaMap<Self, E> {
        MetaMap(self, PhantomData)
    }

    /// Wrap the value of a parser in a list
    ///
    /// ```rust
    /// # use autumn::prelude::*;
    /// use std::str::FromStr;
    /// fn parse_integers() -> impl Parser<List<i32>> {
    ///     "-".maybe()
    ///         .and(digit.multiple())
    ///         .copy_string()
    ///         .map(|s| FromStr::from_str(&s).unwrap())
    ///         .to_list()
    ///         .multiple()
    /// }
    /// ```
    fn to_list(self) -> ListMap<Self, E> {
        ListMap(self, PhantomData)
    }

    /// Repeat the parser a given number of times with the provided delimeter with an optional
    /// trailing delimeter
    ///
    /// ```rust
    /// # use autumn::prelude::*;
    /// use std::str::FromStr;
    /// fn integer() -> impl Parser<i32> {
    ///     "-".maybe()
    ///         .and(digit.multiple())
    ///         .copy_string()
    ///         .map(|s| FromStr::from_str(&s).unwrap())
    /// }
    ///
    /// fn integer_list() -> impl Parser<List<i32>> {
    ///     integer()
    ///         .maybe_space_after()
    ///         .delimited_by(",".maybe_space_after(), ..)
    ///         .surrounded_by("[".maybe_space_after(), "]")
    /// }
    /// # assert!(parse(integer_list(), "[1,2,3,4]").single_parse());
    /// # assert!(parse(integer_list(), "[ 1 , 2 , 3 , 4]").single_parse());
    /// # assert!(parse(integer_list(), "[1,2,3,4,]").single_parse());
    /// # assert!(parse(integer_list(), "[ 1 , 2 , 3 , 4 , ]").single_parse());
    /// ```
    fn delimited_by<D, P: Parser<D, E>, R: RangeBounds<usize>>(
        self,
        delimiter: P,
        repetitions: R,
    ) -> DelimitedBy<Self, P, D, R, E> {
        DelimitedBy(self, delimiter, repetitions, PhantomData)
    }

    /// Repeat the parser a given number of times with the provided delimeter
    ///
    /// ```rust
    /// # use autumn::prelude::*;
    /// use std::str::FromStr;
    /// fn integer() -> impl Parser<i32> {
    ///     "-".maybe()
    ///         .and(digit.multiple())
    ///         .copy_string()
    ///         .map(|s| FromStr::from_str(&s).unwrap())
    /// }
    ///
    /// fn integer_list() -> impl Parser<List<i32>> {
    ///     integer()
    ///         .maybe_space_after()
    ///         .strictly_delimited_by(",".maybe_space_after(), ..)
    ///         .surrounded_by("[".maybe_space_after(), "]")
    /// }
    /// # assert!(parse(integer_list(), "[1,2,3,4]").single_parse());
    /// # assert!(parse(integer_list(), "[ 1 , 2 , 3 , 4]").single_parse());
    /// # assert!(parse(integer_list(), "[1,2,3,4,]").is_none());
    /// ```
    fn strictly_delimited_by<D, P: Parser<D, E>, R: RangeBounds<usize>>(
        self,
        delimiter: P,
        repetitions: R,
    ) -> StrictlyDelimitedBy<Self, P, D, R, E> {
        StrictlyDelimitedBy(self, delimiter, repetitions, PhantomData)
    }

    /// Enclose the parser in the given open and close delimeters
    ///
    /// ```rust
    /// # use autumn::prelude::*;
    /// use std::str::FromStr;
    /// fn integer() -> impl Parser<i32> {
    ///     "-".maybe()
    ///         .and(digit.multiple())
    ///         .copy_string()
    ///         .map(|s| FromStr::from_str(&s).unwrap())
    /// }
    ///
    /// fn integer_list() -> impl Parser<List<i32>> {
    ///     integer()
    ///         .maybe_space_after()
    ///         .strictly_delimited_by(",".maybe_space_after(), ..)
    ///         .surrounded_by("[".maybe_space_after(), "]")
    /// }
    /// # assert!(parse(integer_list(), "[1,2,3,4]").single_parse());
    /// # assert!(parse(integer_list(), "[ 1 , 2 , 3 , 4]").single_parse());
    /// # assert!(parse(integer_list(), "[1,2,3,4,]").is_none());
    /// ```
    fn surrounded_by<O: Clone, C: Clone, OP: Parser<O, E>, CP: Parser<C, E>>(
        self,
        open: OP,
        close: CP,
    ) -> SurroundedBy<Self, OP, CP, O, C, E> {
        SurroundedBy(self, open, close, PhantomData)
    }

    /// Parse two parsers and collect them in a tuple
    ///
    /// ```rust
    /// # use autumn::prelude::*;
    /// use std::collections::HashSet;
    /// fn operator<O: Clone, A: Clone, B: Clone>(
    ///     operator: impl Parser<O>,
    ///     left: impl Parser<A>,
    ///     right: impl Parser<B>,
    /// ) -> impl Parser<(A, B)> {
    ///     left.drop(operator.maybe_space_around()).pair(right)
    /// }
    /// ```
    fn pair<B, P: Parser<B, E>>(self, other: P) -> Pair<Self, P, E> {
        Pair(self, other, PhantomData)
    }

    /// Ignore the space that always appears after the parser
    ///
    /// ```rust
    /// # use autumn::prelude::*;
    /// use std::collections::HashSet;
    /// fn item() -> impl Parser<String> {
    ///     alphanumeric.multiple().copy_string()
    /// }
    ///
    /// fn set_decl() -> impl Parser<HashSet<String>> {
    ///     item()
    ///         .delimited_by(space, ..)
    ///         .surrounded_by(
    ///             "set".space_after().drop("{".maybe_space_after()),
    ///             "}"
    ///         )
    ///         .collect()
    /// }
    /// # assert!(parse(set_decl(), "set{}").is_none());
    /// # assert!(parse(set_decl(), "set {}").single_parse());
    /// # assert!(parse(set_decl(), "set { }").single_parse());
    /// # assert!(parse(set_decl(), "set {one two three}").single_parse());
    /// # assert!(parse(set_decl(), "set { one two three }").single_parse());
    /// ```
    fn space_after(self) -> SpaceAfter<Self, E> {
        SpaceAfter(self, PhantomData)
    }

    /// Ignore the space that might appear after the parser
    ///
    /// ```rust
    /// # use autumn::prelude::*;
    /// use std::collections::HashSet;
    /// fn item() -> impl Parser<String> {
    ///     alphanumeric.multiple().copy_string()
    /// }
    ///
    /// fn set_decl() -> impl Parser<HashSet<String>> {
    ///     item()
    ///         .delimited_by(space, ..)
    ///         .surrounded_by(
    ///             "set".maybe_space_after().drop("{".maybe_space_after()),
    ///             "}"
    ///         )
    ///         .collect()
    /// }
    /// # assert!(parse(set_decl(), "set{}").single_parse());
    /// # assert!(parse(set_decl(), "set { }").single_parse());
    /// # assert!(parse(set_decl(), "set {one two three}").single_parse());
    /// # assert!(parse(set_decl(), "set { one two three }").single_parse());
    fn maybe_space_after(self) -> MaybeSpaceAfter<Self, E> {
        MaybeSpaceAfter(self, PhantomData)
    }

    /// Ignore the space that always appears before the parser
    fn space_before(self) -> SpaceBefore<Self, E> {
        SpaceBefore(self, PhantomData)
    }

    /// Ignore the space that might appear before the parser
    fn maybe_space_before(self) -> MaybeSpaceBefore<Self, E> {
        MaybeSpaceBefore(self, PhantomData)
    }

    /// Ignore the space that always appears before and after the parser
    fn space_around(self) -> SpaceAround<Self, E> {
        SpaceAround(self, PhantomData)
    }

    /// Ignore the space that might appear before or after the parser
    ///
    /// ```rust
    /// # use autumn::prelude::*;
    /// use std::collections::HashSet;
    /// fn operator<O: Clone, A: Clone, B: Clone>(
    ///     left: impl Parser<A>,
    ///     operator: impl Parser<O>,
    ///     right: impl Parser<B>,
    /// ) -> impl Parser<(A, B)> {
    ///     left.drop(operator.maybe_space_around()).pair(right)
    /// }
    /// # assert!(parse(operator("left", "+", "right"), "left+right").single_parse());
    /// # assert!(parse(operator("left", "+", "right"), "left +right").single_parse());
    /// # assert!(parse(operator("left", "+", "right"), "left+ right").single_parse());
    /// # assert!(parse(operator("left", "+", "right"), "left + right").single_parse());
    /// ```
    fn maybe_space_around(self) -> MaybeSpaceAround<Self, E> {
        MaybeSpaceAround(self, PhantomData)
    }
}

impl<T, E, P: Parser<T, E>> ParserExt<T, E> for P {}

/// Combinators on parsers that produce a [`Concat`](../trait.Concat.html)
///
/// Parsing sequences of characters or values from input is done using parsers that produce a
/// [`Concat`](../trait.Concat.html) and the [`ConcatParserExt`](trait.ConcatParserExt.html) trait that
/// is implemented for all such parsers.
///
/// The [`ConcatParserExt`](trait.ConcatParserExt.html) provides three combinators for manipulating
/// parsers over sequences of values:
///
///  * [`maybe`](trait.ConcatParserExt.html#method.maybe) repeats a parser zero or one times,
///  * [`multiple`](trait.ConcatParserExt.html#method.multiple) repeats a parser one or more times,
///    and
///  * [`and`](trait.ConcatParserExt.html#method.and) uses two parsers in sequence, concatenating
///    their results into a single list.
///
/// ```rust
/// # use autumn::prelude::*;
/// /// Parses C-like identifiers
/// fn identifier(source: &str, location: Span) -> ParseResult<String> {
///     alphabetic
///         .or("_")
///         .and(alphanumeric.or("_").multiple().maybe())
///         .copy_string()
///         .parse(source, location)
/// }
/// ```
///
/// When using both [`multiple`](trait.ParserExt.html#method.multiple) and
/// [`maybe`](trait.ParserExt.html#method.maybe) to achieve zero or more repetitions,
/// `multiple().maybe()` must be used; `maybe().multiple()` can find an infinite number of ways to
/// apply any parser on even an empty string.
///
/// The characters or values read from input are pushed into the [`List`](../list/struct.List.html)
/// in last-in first-out (LIFO) order, so the list must be
/// [reversed](../list/struct.List.html#method.reverse) in order to iterate over the items in the
/// order they were added.
pub trait ConcatParserExt<T: Concat + Clone, E>: Parser<T, E> + Sized {
    /// Repeat a parser one or more times
    ///
    /// ```rust
    /// # use autumn::prelude::*;
    /// fn scream_parser() -> impl Parser<Span> {
    ///     "a".multiple().and("h".multiple()).and("!")
    /// }
    /// ```
    fn multiple(self) -> Multiple<Self, E> {
        Multiple(self, PhantomData)
    }

    /// Use a parser once or not at all
    ///
    /// ```rust
    /// # use autumn::prelude::*;
    /// fn nondeterministic_parser() -> impl Parser<Span> {
    ///     "non".maybe().and("deterministic")
    /// }
    /// ```
    fn maybe(self) -> Maybe<Self, E> {
        Maybe(self, PhantomData)
    }

    /// Use one parser after another parser
    ///
    /// ```rust
    /// # use autumn::prelude::*;
    /// fn simple_identifier() -> impl Parser<Span> {
    ///     alphabetic.and(digit)
    /// }
    /// ```
    fn and<P: Parser<T, E>>(self, other: P) -> And<Self, P, E> {
        And(self, other, PhantomData)
    }

    /// Repeat a parser for a specified number of times
    ///
    /// ```rust
    /// # use autumn::prelude::*;
    /// fn scream_parser() -> impl Parser<Span> {
    ///     "a".multiple().and("h".repeated(1..3)).and("!")
    /// }
    /// # assert!(parse(scream_parser(), "a!").is_none());
    /// # assert!(parse(scream_parser(), "ah!").single_parse());
    /// # assert!(parse(scream_parser(), "ahh!").single_parse());
    /// # assert!(parse(scream_parser(), "ahhh!").is_none());
    /// ```
    fn repeated<R: RangeBounds<usize> + Clone>(self, repetitions: R) -> Repeat<Self, R, E> {
        Repeat(self, repetitions, PhantomData)
    }
}

impl<T: Concat + Clone, E, P: Parser<T, E>> ConcatParserExt<T, E> for P {}

/// Combinators on parsers that produce a [`List`](../list/struct.List.html)
///
/// Sequences parsed into a [`List`](../list/struct.List.html) with only a single valid parse can
/// be collected into any container implementing `FromIterator`.
pub trait ListParserExt<T, E>: Parser<List<T>, E> + Sized {
    /// Collect a unqiely parsed list into a different kind of container
    ///
    /// Panics
    /// ======
    ///
    /// If the list, elements of the list, or a subsequence of the list has been copied or if the
    /// list is the result of a parser with more than a single unique parse then this operation
    /// will panic.
    ///
    /// ```rust
    /// # use autumn::prelude::*;
    /// fn token() -> impl Parser<String> {
    ///     alphabetic.and(alphanumeric).copy_string()
    /// }
    ///
    /// fn token_sequence() -> impl Parser<Vec<String>> {
    ///     token().to_list().multiple().collect()
    /// }
    /// ```
    fn collect<I: FromIterator<T>>(self) -> Collect<Self, T, I, E> {
        Collect(self, PhantomData)
    }
}

impl<T, E, P: Parser<List<T>, E>> ListParserExt<T, E> for P {}

/// Boxing parsers for [dynamic
/// dispatch](https://doc.rust-lang.org/book/ch17-02-trait-objects.html#trait-objects-perform-dynamic-dispatch)
///
/// There are some cases where a parser may need to produce one of many values within the argument
/// to the [`and_then`](trait.ParserExt.html#method.and_then) method. In this case it helps to be
/// able to unify the types of the values with [dynamic
/// dispatch](https://doc.rust-lang.org/book/ch17-02-trait-objects.html#trait-objects-perform-dynamic-dispatch).
///
/// This can be achieved simply with the [`boxed`](trait.BoxedParserExt.html#method.boxed) method.
///
/// The [`value`](../parsers/fn.value.html), [`error`](../parsers/fn.error.html), and
/// [`throw`](../parsers/fn.throw.html) functions all produce boxed parsers with types that will
/// match as they are commonly used in this case.
///
/// ```rust
/// # use autumn::prelude::*;
/// fn alphabet(source: &str, location: Span) -> ParseResult<String, &'static str> {
///     "abcde"
///         .and(digit)
///         .copy_string()
///         .and_then(|text| {
///             let text = text;
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
pub trait BoxedParserExt<'p, T, E>: Parser<T, E> + Sized + 'p {
    /// Convert a parser into a dynamically dispatched parser
    ///
    /// ```rust
    /// # use autumn::prelude::*;
    /// fn parser() -> impl Parser<Span> {
    ///     "a".and("b".or("c")).boxed()
    /// }
    /// ```
    fn boxed(self) -> Boxed<dyn Parser<T, E> + 'p> {
        Boxed::new(self)
    }

    /// Convert a parser into a dynamically dispatched parser
    ///
    /// ```rust
    /// # use autumn::prelude::*;
    /// fn parser() -> impl Parser<Span> {
    ///     "a".and("b".or("c")).boxed()
    /// }
    /// ```
    fn ref_parser(&'p self) -> Referenced<'p, Self, E> {
        Referenced(self, PhantomData)
    }
}

impl<'p, T: 'p, E: 'p, P: Parser<T, E> + 'p> BoxedParserExt<'p, T, E> for P {}

/// Combinators on parser that produce a list of characters
pub trait TextParserExt<E>: Parser<Span, E> + Sized {
    /// Convert the list of characters into a string within a parser
    ///
    /// ```rust
    /// # use autumn::prelude::*;
    /// fn identifier() -> impl Parser<String> {
    ///     alphabetic.and(alphanumeric.multiple().maybe()).copy_string()
    /// }
    /// ```
    fn copy_string(self) -> StringMap<Self, E> {
        StringMap(self, PhantomData)
    }

    /// Only parse successfully if there is no text remaining in the input after a parse
    ///
    /// ```rust
    /// # use autumn::prelude::*;
    /// fn single_line() -> impl Parser<String> {
    ///     any_character
    ///         .str_condition(|s| !s.chars().any(|c| c == '\n'))
    ///         .drop("\n")
    ///         .copy_string()
    ///         .end()
    /// }
    /// ```
    fn str_condition<F: Fn(&str) -> bool>(self, f: F) -> StrCondition<Self, F, E> {
        StrCondition(self, f, PhantomData)
    }
}

impl<E, P: Parser<Span, E>> TextParserExt<E> for P {}

/// The result of the [`multiple`](trait.ConcatParserExt.html#method.multiple) function in the
/// [`ConcatParserExt`](trait.ConcatParserExt.html) trait
pub struct Multiple<P, E>(P, PhantomData<E>);

impl<T: Concat + Clone, E, P: Parser<T, E>> Parser<T, E> for Multiple<P, E> {
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, T, E> {
        self.0
            .parse(source, location)
            .and_then(&|parsed, source, mut location| {
                self.parse(source, location.take())
                    .map(&|tail| parsed.clone().concat(tail))
                    .or(ParseResult::success(parsed, source, location))
            })
    }
}

/// The result of the [`repeated`](trait.ConcatParserExt.html#method.repeated) function in the
/// [`ConcatParserExt`](trait.ConcatParserExt.html) trait
pub struct Repeat<P, R, E>(P, R, PhantomData<E>);

#[derive(Clone)]
struct Counter<P>(P, usize);

impl<P: Concat> Concat for Counter<P> {
    fn empty() -> Self {
        Counter(P::empty(), 0)
    }

    fn empty_at(location: Span) -> Self {
        Counter(P::empty_at(location), 0)
    }

    fn concat(self, other: Self) -> Self {
        Counter(self.0.concat(other.0), self.1 + other.1)
    }
}

impl<T: Concat + Clone, E, R: RangeBounds<usize>, P: Parser<T, E>> Parser<T, E>
    for Repeat<P, R, E>
{
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, T, E> {
        self.0
            .ref_parser()
            .map(|parsed| Counter(parsed, 1))
            .multiple()
            .maybe()
            .parse(source, location)
            .and_then(&|Counter(parsed, length), source, location| {
                let long_enough = match self.1.start_bound() {
                    // Not enough for the minimum length
                    Bound::Included(minimum) if length < *minimum => ParseResult::none(location),
                    Bound::Excluded(minimum) if length <= *minimum => ParseResult::none(location),
                    // No minimum bound
                    _ => ParseResult::success(parsed.clone(), source, location),
                };
                match self.1.end_bound() {
                    // Too many elements
                    Bound::Included(maximum) if length > *maximum => ParseResult::none(location),
                    Bound::Excluded(maximum) if length >= *maximum => ParseResult::none(location),
                    // Could take more elements
                    _ => long_enough,
                }
            })
    }
}

/// The result of the [`maybe`](trait.ConcatParserExt.html#method.maybe) function in the
/// [`ConcatParserExt`](trait.ConcatParserExt.html) trait
pub struct Maybe<P, E>(P, PhantomData<E>);

impl<T: Concat, E, P: Parser<T, E>> Parser<T, E> for Maybe<P, E> {
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, T, E> {
        self.0
            .parse(source, location.clone())
            .or(empty(source, location))
    }
}

/// The result of the [`condition`](trait.ParserExt.html#method.condition) function in the
/// [`ParserExt`](trait.ParserExt.html) trait
pub struct Condition<P, F, E>(P, F, PhantomData<E>);

impl<T, E, P: Parser<T, E>, F: Fn(&T) -> bool> Parser<T, E> for Condition<P, F, E> {
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, T, E> {
        self.0
            .parse(source, location)
            .and_then(&|parsed, source, location| {
                if self.1(&parsed) {
                    ParseResult::success(parsed, source, location)
                } else {
                    ParseResult::none(location)
                }
            })
    }
}

/// The result of the [`matching`](trait.ParserExt.html#method.matching) function in the
/// [`ParserExt`](trait.ParserExt.html) trait
pub struct Matching<P, F, E>(P, F, PhantomData<E>);

impl<T: PartialEq<V>, V, E, P: Parser<T, E>> Parser<T, E> for Matching<P, V, E> {
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, T, E> {
        self.0
            .parse(source, location)
            .and_then(&|parsed, source, location| {
                if parsed == self.1 {
                    ParseResult::success(parsed, source, location)
                } else {
                    ParseResult::none(location)
                }
            })
    }
}

/// The result of the [`or`](trait.ParserExt.html#method.or) function in the
/// [`ParserExt`](trait.ParserExt.html) trait
pub struct Or<A, B, E>(A, B, PhantomData<E>);

impl<A, B, T: Clone, E> Parser<T, E> for Or<A, B, E>
where
    A: Parser<T, E>,
    B: Parser<T, E>,
{
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, T, E> {
        self.0
            .parse(source, location.clone())
            .or(self.1.parse(source, location))
    }
}

/// The result of the [`and`](trait.ConcatParserExt.html#method.and) function in the
/// [`ConcatParserExt`](trait.ConcatParserExt.html) trait
pub struct And<A, B, E>(A, B, PhantomData<E>);

impl<A, B, T, E> Parser<T, E> for And<A, B, E>
where
    T: Concat + Clone,
    A: Parser<T, E>,
    B: Parser<T, E>,
{
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, T, E> {
        self.0
            .parse(source, location)
            .and_then(&|prefix, source, location| {
                self.1
                    .parse(source, location)
                    .map(&|suffix| prefix.clone().concat(suffix))
            })
    }
}

/// The result of the [`map`](trait.ParserExt.html#method.map) function in the
/// [`ParserExt`](trait.ParserExt.html) trait
pub struct Map<P, F, T, E>(P, F, PhantomData<(T, E)>);

impl<P, F, T, E, V> Parser<V, E> for Map<P, F, T, E>
where
    P: Parser<T, E>,
    F: Fn(T) -> V,
{
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, V, E> {
        self.0.parse(source, location).map(&self.1)
    }
}

/// The result of the [`and_then`](trait.ParserExt.html#method.and_then) function in the
/// [`ParserExt`](trait.ParserExt.html) trait
pub struct AndThen<P, F, T, E>(P, F, PhantomData<(T, E)>);

impl<P, F, T, E, Q, V> Parser<V, E> for AndThen<P, F, T, E>
where
    P: Parser<T, E>,
    Q: Parser<V, E>,
    F: Fn(T) -> Q,
{
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, V, E> {
        self.0
            .parse(source, location)
            .and_then(&|value, source, location| (self.1)(value).parse(source, location))
    }
}

/// The result of the [`repeat`](trait.ParserExt.html#method.repeat) function in the
/// [`ParserExt`](trait.ParserExt.html) trait
pub struct Fold<P, F, E>(P, F, PhantomData<E>);

impl<P, F, T, E, Q> Parser<T, E> for Fold<P, F, E>
where
    T: Clone,
    P: Parser<T, E>,
    Q: Parser<T, E>,
    F: Fn(T) -> Q + Clone,
{
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, T, E> {
        self.0
            .ref_parser()
            .and_then(|parsed| {
                (self.1)(parsed.clone())
                    .fold(self.1.clone())
                    .or(value(parsed))
            })
            .parse(source, location)
    }
}

/// The result of the [`on_failure`](trait.ParserExt.html#method.on_failure) function in the
/// [`ParserExt`](trait.ParserExt.html) trait
pub struct OnFailure<A, B, E>(A, B, PhantomData<E>);

impl<A, B, T: Clone, E> Parser<T, E> for OnFailure<A, B, E>
where
    A: Parser<T, E>,
    B: Parser<T, E>,
{
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, T, E> {
        let parse = self.0.parse(source, location.clone());
        if parse.is_success() {
            parse
        } else {
            parse.or(self.1.parse(source, location))
        }
    }
}

/// The result of the [`on_none`](trait.ParserExt.html#method.on_none) function in the
/// [`ParserExt`](trait.ParserExt.html) trait
pub struct OnNone<A, B, E>(A, B, PhantomData<E>);

impl<A, B, T: Clone, E> Parser<T, E> for OnNone<A, B, E>
where
    A: Parser<T, E>,
    B: Parser<T, E>,
{
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, T, E> {
        let parse = self.0.parse(source, location.clone());
        if parse.is_none() {
            parse.or(self.1.parse(source, location))
        } else {
            parse
        }
    }
}

/// The result of the [`drop`](trait.ParserExt.html#method.drop) function in the
/// [`ParserExt`](trait.ParserExt.html) trait
pub struct Drop<A, B, V, E>(A, B, PhantomData<(V, E)>);

impl<A, B, T: Clone, V, E> Parser<T, E> for Drop<A, B, V, E>
where
    A: Parser<T, E>,
    B: Parser<V, E>,
{
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, T, E> {
        self.0
            .parse(source, location)
            .and_then(&|keep, source, location| {
                self.1.parse(source, location).map(&|_| keep.clone())
            })
    }
}

/// The result of the [`maybe_drop`](trait.ParserExt.html#method.maybe_drop) function in the
/// [`ParserExt`](trait.ParserExt.html) trait
pub struct MaybeDrop<A, B, V, E>(A, B, PhantomData<(V, E)>);

impl<A, B, T: Clone, V, E> Parser<T, E> for MaybeDrop<A, B, V, E>
where
    A: Parser<T, E>,
    B: Parser<V, E>,
{
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, T, E> {
        self.0
            .parse(source, location)
            .and_then(&|keep, source, location| {
                self.1
                    .parse(source, location)
                    .map(&|_| keep.clone())
                    .or(ParseResult::success(keep, source, location))
            })
    }
}

/// The result of the [`skip`](trait.ParserExt.html#method.skip) function in the
/// [`ParserExt`](trait.ParserExt.html) trait
pub struct Skip<P, Q, T, E>(P, Q, PhantomData<(T, E)>);

impl<P, Q, T, V, E> Parser<V, E> for Skip<P, Q, T, E>
where
    P: Parser<T, E>,
    Q: Parser<V, E>,
{
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, V, E> {
        self.0
            .parse(source, location)
            .and_then(&|_, source, location| self.1.parse(source, location))
    }
}

/// The result of the [`boxed`](trait.BoxedParserExt.html#method.boxed) function in the
/// [`BoxedParserExt`](trait.BoxedParserExt.html) trait
pub struct Boxed<P: ?Sized>(Box<P>);

impl<'p, T, E> Boxed<dyn Parser<T, E> + 'p> {
    pub(crate) fn new<P: Parser<T, E> + 'p>(parser: P) -> Self {
        let boxed: Box<dyn Parser<T, E> + 'p> = Box::new(parser);
        Boxed(boxed)
    }
}

impl<'p, T, E> Parser<T, E> for Boxed<dyn Parser<T, E> + 'p> {
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, T, E> {
        self.0.parse(source, location)
    }
}

/// The result of the [`ref_parser`](trait.BoxedParserExt.html#method.ref_parser) function in the
/// [`BoxedParserExt`](trait.BoxedParserExt.html) trait
pub struct Referenced<'p, P, E>(&'p P, PhantomData<E>);

impl<'p, T, E, P: Parser<T, E>> Parser<T, E> for Referenced<'p, P, E> {
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, T, E> {
        self.0.parse(source, location)
    }
}

/// The result of the [`end`](trait.ParserExt.html#method.end) function in the
/// [`ParserExt`](trait.ParserExt.html) trait
pub struct End<P, E>(P, PhantomData<E>);

impl<T, E, P: Parser<T, E>> Parser<T, E> for End<P, E> {
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, T, E> {
        self.0
            .parse(source, location)
            .and_then(&|value, source, location| {
                if source.len() == 0 {
                    ParseResult::success(value, source, location)
                } else {
                    ParseResult::none(location)
                }
            })
    }
}

/// The result of the [`catch`](trait.ParserExt.html#method.catch) function in the
/// [`ParserExt`](trait.ParserExt.html) trait
pub struct Catch<P, E>(P, PhantomData<E>);

impl<T, E, P: Parser<T, E>> Parser<T, E> for Catch<P, E> {
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, T, E> {
        self.0.parse(source, location).catch()
    }
}

/// The result of the [`meta`](trait.ParserExt.html#method.meta) function in the
/// [`ParserExt`](trait.ParserExt.html) trait
pub struct MetaMap<P, E>(P, PhantomData<E>);

impl<T, E, P: Parser<T, E>> Parser<Meta<T, Span>, E> for MetaMap<P, E> {
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, Meta<T, Span>, E> {
        self.0.parse(source, location).meta()
    }
}

/// The result of the [`to_list`](trait.ParserExt.html#method.to_list) function in the
/// [`ParserExt`](trait.ParserExt.html) trait
pub struct ListMap<P, E>(P, PhantomData<E>);

impl<T, E, P: Parser<T, E>> Parser<List<T>, E> for ListMap<P, E> {
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, List<T>, E> {
        self.0.parse(source, location).map(&List::single)
    }
}

/// The result of the [`delimited_by`](trait.ParserExt.html#method.delimited_by) function in the
/// [`ParserExt`](trait.ParserExt.html) trait
pub struct DelimitedBy<P, S, D, R, E>(P, S, R, PhantomData<(D, E)>);

impl<T, D, E, R, P, S> Parser<List<T>, E> for DelimitedBy<P, S, D, R, E>
where
    T: Clone,
    R: RangeBounds<usize> + Clone,
    P: Parser<T, E>,
    S: Parser<D, E>,
{
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, List<T>, E> {
        self.0
            .ref_parser()
            .drop(self.1.ref_parser())
            .to_list()
            .multiple()
            .maybe()
            .and(
                self.0
                    .ref_parser()
                    .to_list()
                    .maybe_drop(self.1.ref_parser()),
            )
            .maybe()
            .parse(source, location)
            .and_then(&|parsed, source, location| {
                let length = parsed.length();
                let long_enough = match self.2.start_bound() {
                    // Not enough for the minimum length
                    Bound::Included(minimum) if length < *minimum => ParseResult::none(location),
                    Bound::Excluded(minimum) if length <= *minimum => ParseResult::none(location),
                    // No minimum bound
                    _ => ParseResult::success(parsed.clone(), source, location),
                };
                match self.2.end_bound() {
                    // Too many elements
                    Bound::Included(maximum) if length > *maximum => ParseResult::none(location),
                    Bound::Excluded(maximum) if length >= *maximum => ParseResult::none(location),
                    // Could take more elements
                    _ => long_enough,
                }
            })
    }
}

/// The result of the [`strictly_delimited_by`](trait.ParserExt.html#method.strictly_delimited_by)
/// function in the [`ParserExt`](trait.ParserExt.html) trait
pub struct StrictlyDelimitedBy<P, S, D, R, E>(P, S, R, PhantomData<(D, E)>);

impl<T, D, E, R, P, S> Parser<List<T>, E> for StrictlyDelimitedBy<P, S, D, R, E>
where
    T: Clone,
    R: RangeBounds<usize> + Clone,
    P: Parser<T, E>,
    S: Parser<D, E>,
{
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, List<T>, E> {
        self.0
            .ref_parser()
            .drop(self.1.ref_parser())
            .to_list()
            .multiple()
            .maybe()
            .and(self.0.ref_parser().to_list())
            .maybe()
            .parse(source, location)
            .and_then(&|parsed, source, location| {
                let length = parsed.length();
                let long_enough = match self.2.start_bound() {
                    // Not enough for the minimum length
                    Bound::Included(minimum) if length < *minimum => ParseResult::none(location),
                    Bound::Excluded(minimum) if length <= *minimum => ParseResult::none(location),
                    // No minimum bound
                    _ => ParseResult::success(parsed.clone(), source, location),
                };
                match self.2.end_bound() {
                    // Too many elements
                    Bound::Included(maximum) if length > *maximum => ParseResult::none(location),
                    Bound::Excluded(maximum) if length >= *maximum => ParseResult::none(location),
                    // Could take more elements
                    _ => long_enough,
                }
            })
    }
}

/// The result of the [`surrounded_by`](trait.ParserExt.html#method.surrounded_by)
/// function in the [`ParserExt`](trait.ParserExt.html) trait
pub struct SurroundedBy<P, OP, CP, O, C, E>(P, OP, CP, PhantomData<(O, C, E)>);

impl<T, O, C, P, OP, CP, E> Parser<T, E> for SurroundedBy<P, OP, CP, O, C, E>
where
    T: Clone,
    P: Parser<T, E>,
    OP: Parser<C, E>,
    CP: Parser<C, E>,
{
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, T, E> {
        self.1
            .ref_parser()
            .skip(self.0.ref_parser())
            .drop(self.2.ref_parser())
            .parse(source, location)
    }
}

/// The result of the [`copy_string`](trait.TextParserExt.html#method.copy_string) function in the
/// [`TextParserExt`](trait.TextParserExt.html) trait
pub struct StringMap<P, E>(P, PhantomData<E>);

impl<E, P: Parser<Span, E>> Parser<String, E> for StringMap<P, E> {
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, String, E> {
        self.0
            .parse(source, location)
            .map(&|s| s.subslice(&location, source).to_owned())
    }
}

/// The result of the [`str_condition`](trait.TextParserExt.html#method.str_condition) function in
/// the [`TextParserExt`](trait.TextParserExt.html) trait
pub struct StrCondition<P, F, E>(P, F, PhantomData<E>);

impl<E, P: Parser<Span, E>, F: Fn(&str) -> bool> Parser<Span, E> for StrCondition<P, F, E> {
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, Span, E> {
        self.0
            .parse(source, location)
            .and_then(&|parsed, remaining, remaining_location| {
                if self.1(parsed.subslice(&location, source)) {
                    ParseResult::success(parsed, remaining, remaining_location)
                } else {
                    ParseResult::none(location)
                }
            })
    }
}

/// The result of the [`collect`](trait.ListParserExt.html#method.collect) function in the
/// [`ListParserExt`](trait.ListParserExt.html) trait
pub struct Collect<P, T, I, E>(P, PhantomData<(T, I, E)>);

impl<T, I: FromIterator<T>, E, P: Parser<List<T>, E>> Parser<I, E> for Collect<P, T, I, E> {
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, I, E> {
        let result = self.0.parse(source, location);

        if result.single_parse() || result.is_none() {
            result.map(&|parsed| parsed.drain().collect())
        } else {
            panic!(
                "Cannot collect on parse with more than one result: {}",
                location
            );
        }
    }
}

/// The result of the [`pair`](trait.ParserExt.html#method.pair) function in the
/// [`ParserExt`](trait.ParserExt.html) trait
pub struct Pair<A, B, E>(A, B, PhantomData<E>);

impl<A, B, PA, PB, E> Parser<(A, B), E> for Pair<PA, PB, E>
where
    A: Clone,
    PA: Parser<A, E>,
    PB: Parser<B, E>,
{
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, (A, B), E> {
        self.0
            .ref_parser()
            .and_then(move |left| self.1.ref_parser().map(move |right| (left.clone(), right)))
            .parse(source, location)
    }
}

/// The result of the [`space_after`](trait.ParserExt.html#method.space_after) function in the
/// [`ParserExt`](trait.ParserExt.html) trait
pub struct SpaceAfter<P, E>(P, PhantomData<E>);

impl<T: Clone, E, P: Parser<T, E>> Parser<T, E> for SpaceAfter<P, E> {
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, T, E> {
        self.0.ref_parser().drop(space).parse(source, location)
    }
}

/// The result of the [`maybe_space_after`](trait.ParserExt.html#method.maybe_space_after) function
/// in the [`ParserExt`](trait.ParserExt.html) trait
pub struct MaybeSpaceAfter<P, E>(P, PhantomData<E>);

impl<T: Clone, E, P: Parser<T, E>> Parser<T, E> for MaybeSpaceAfter<P, E> {
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, T, E> {
        self.0
            .ref_parser()
            .drop(space.maybe())
            .parse(source, location)
    }
}

/// The result of the [`space_before`](trait.ParserExt.html#method.space_before) function in the
/// [`ParserExt`](trait.ParserExt.html) trait
pub struct SpaceBefore<P, E>(P, PhantomData<E>);

impl<T, E, P: Parser<T, E>> Parser<T, E> for SpaceBefore<P, E> {
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, T, E> {
        space.skip(self.0.ref_parser()).parse(source, location)
    }
}

/// The result of the [`maybe_space_before`](trait.ParserExt.html#method.maybe_space_before)
/// function in the [`ParserExt`](trait.ParserExt.html) trait
pub struct MaybeSpaceBefore<P, E>(P, PhantomData<E>);

impl<T: Clone, E, P: Parser<T, E>> Parser<T, E> for MaybeSpaceBefore<P, E> {
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, T, E> {
        space
            .maybe()
            .skip(self.0.ref_parser())
            .parse(source, location)
    }
}

/// The result of the [`space_around`](trait.ParserExt.html#method.space_around) function in the
/// [`ParserExt`](trait.ParserExt.html) trait
pub struct SpaceAround<P, E>(P, PhantomData<E>);

impl<T: Clone, E, P: Parser<T, E>> Parser<T, E> for SpaceAround<P, E> {
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, T, E> {
        self.0
            .ref_parser()
            .space_before()
            .space_after()
            .parse(source, location)
    }
}

/// The result of the [`maybe_space_around`](trait.ParserExt.html#method.maybe_space_around)
/// function in the [`ParserExt`](trait.ParserExt.html) trait
pub struct MaybeSpaceAround<P, E>(P, PhantomData<E>);

impl<T: Clone, E, P: Parser<T, E>> Parser<T, E> for MaybeSpaceAround<P, E> {
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, T, E> {
        self.0
            .ref_parser()
            .maybe_space_before()
            .maybe_space_after()
            .parse(source, location)
    }
}
