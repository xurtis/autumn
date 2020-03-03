//! Traits for combining and modifying parsers
//!
//! Combinators provide ways of modifying the behavior of parsers and combining parsers that is
//! consistent across all parser implementations.
//!
//! Parsing sequences
//! =================
//!
//! Parsing sequences of characters or values from input is done using parsers that produce a
//! [`List`](../struct.List.html) and the [`ListParserExt`](trait.ListParserExt.html) trait that is
//! implemented for all such parsers.
//!
//! The [`ListParserExt`](trait.ListParserExt.html) provides three combinators for manipulating
//! parsers over sequences of values:
//!
//!  * [`maybe`](trait.ListParserExt.html#method.maybe) repeats a parser zero or one times,
//!  * [`multiple`](trait.ListParserExt.html#method.multiple) repeats a parser one or more times,
//!    and
//!  * [`and`](trait.ListParserExt.html#method.and) uses two parsers in sequence, concatenating
//!    their results into a single list.
//!
//! ```rust
//! # use autumn::prelude::*;
//! /// Parses C-like identifiers
//! fn identifier(source: &str, location: Span) -> ParseResult<String> {
//!     alphabetic
//!         .or("_")
//!         .and(alphanumeric.or("_").multiple().maybe())
//!         .map(|s| s.to_string())
//!         .parse(source, location)
//! }
//! ```
//!
//! When using both [`multiple`](trait.ParserExt.html#method.multiple) and
//! [`maybe`](trait.ParserExt.html#method.maybe) to achieve zero or more repetitions,
//! `multiple().maybe()` must be used; `maybe().multiple()` can find an infinite number of ways to
//! apply any parser on even an empty string.
//!
//! The characters or values read from input are pushed into the [`List`](../struct.List.html) in
//! last-in first-out (LIFO) order, so the list must be
//! [reversed](../struct.List.html#method.reverse) in order to iterate over the items in the order
//! they were added. The `ToString` implementation for [`List<char>`](../struct.List.html) does
//! this automatically.
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
//!    [`map`](trait.ListParserExt.html#method.map) operation, and
//!  * changing a successful (or already failing) parse into a potentially failing parse with the
//!    [`and_then`](trait.ListParserExt.html#method.and_then) operation.
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
//!         .map(|s| Some(s.to_string()))
//!         .on_none(
//!             character
//!                 .condition(|c| !c.is_whitespace())
//!                 .to_list()
//!                 .multiple()
//!                 .map(|s| s.to_string())
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
//!     "left".skip("right").map(|s| s.to_string())
//! }
//! let skipped = parse(right(), "leftright");
//! # assert_eq!(skipped.values().next().unwrap(), "right");
//!
//! // Produces "left"
//! fn left() -> impl Parser<String> {
//!     "left".drop("right").map(|s| s.to_string())
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
//!     digit.multiple().map(|s| s.to_string()).parse(source, location)
//! }
//!
//! /// Parse a list of integers and get the source location of each integer
//! fn integer_list(source: &str, location: Span) -> ParseResult<Vec<Meta<String, Span>>> {
//!     "["
//!         .and(space.maybe())
//!         .skip(
//!             integer
//!                 .meta()
//!                 .to_list()
//!                 .and(
//!                     space.maybe()
//!                         .and(",")
//!                         .and(space.maybe())
//!                         .skip(integer.meta().to_list())
//!                         .multiple()
//!                         .maybe()
//!                 )
//!                 .maybe()
//!                 .drop(",".and(space.maybe()).maybe())
//!         )
//!         .drop("]")
//!         .map(|integers| {
//!             integers
//!                 // LIFO -> FIFO
//!                 .reverse()
//!                 // Rc<Meta<String, Span>> -> Meta<String, Span>
//!                 .map(|s| s.as_ref().clone())
//!                 .collect()
//!         })
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
//!         .and_then(|text| {
//!             let text = text.to_string();
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
use crate::parse::{List, ParseResult, Parser};
use crate::parsers::empty;
use std::marker::PhantomData;

/// Combinators that can be used on all parsers
pub trait ParserExt<T, E>: Parser<T, E> + Sized {

    /// Evaluate two alternative parsers producing all successful parses from both
    ///
    /// ```rust
    /// # use autumn::prelude::*;
    /// fn alpha_or_beta() -> impl Parser<String> {
    ///     "alpha".or("beta").map(|s| s.to_string())
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
    ///     digit.multiple().map(|i| FromStr::from_str(&i.to_string()).unwrap())
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
    ///     alphabetic.and(alphanumeric.multiple().maybe()).map(|s| s.to_string())
    /// }
    ///
    /// fn literal() -> impl Parser<i32> {
    ///     "-".maybe()
    ///         .and(digit.multiple())
    ///         .map(|s| s.to_string())
    ///         .map(|i| FromStr::from_str(&i).unwrap())
    /// }
    ///
    /// fn tagged_token() -> impl Parser<Token> {
    ///     "identifier"
    ///         .or("literal")
    ///         .drop(space)
    ///         .map(|s| s.to_string())
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

    /// Apply an alternative parser if the preceding parser produces errors or no successful parses
    ///
    /// ```rust
    /// # use autumn::prelude::*;
    /// fn failing_parser() -> impl Parser<String, &'static str> {
    ///     "hello"
    ///         .and("world")
    ///         .map(|s| s.to_string())
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
    ///         .map(|s| s.to_string())
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
    ///     "hello".drop(space).map(|s| s.to_string())
    /// }
    /// ```
    fn drop<V, P: Parser<V, E>>(self, other: P) -> Drop<Self, P, V, E> {
        Drop(self, other, PhantomData)
    }

    /// Apply an additional parser and take its output instead
    ///
    /// ```rust
    /// # use autumn::prelude::*;
    /// fn drop_leading_whitespace() -> impl Parser<String> {
    ///     space.skip("hello").map(|s| s.to_string())
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
    /// fn foutry_two() -> impl Parser<i32> {
    ///     digit
    ///         .multiple()
    ///         .map(|i| FromStr::from_str(&i.to_string()).unwrap())
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
    ///     character
    ///         .condition(|c| *c != '\n')
    ///         .to_list()
    ///         .drop("\n")
    ///         .map(|s| s.to_string())
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
    ///         .map(|s| s.to_string())
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
    ///         .map(|s| s.to_string())
    ///         .map(|s| FromStr::from_str(&s).unwrap())
    ///         .to_list()
    ///         .multiple()
    /// }
    /// ```
    fn to_list(self) -> ListMap<Self, E> {
        ListMap(self, PhantomData)
    }
}

impl<T, E, P: Parser<T, E>> ParserExt<T, E> for P {}

/// Combinators on parsers that produce a [`List`](../struct.List.html)
///
/// Parsing sequences of characters or values from input is done using parsers that produce a
/// [`List`](../struct.List.html) and the [`ListParserExt`](trait.ListParserExt.html) trait that is
/// implemented for all such parsers.
///
/// The [`ListParserExt`](trait.ListParserExt.html) provides three combinators for manipulating
/// parsers over sequences of values:
///
///  * [`maybe`](trait.ListParserExt.html#method.maybe) repeats a parser zero or one times,
///  * [`multiple`](trait.ListParserExt.html#method.multiple) repeats a parser one or more times,
///    and
///  * [`and`](trait.ListParserExt.html#method.and) uses two parsers in sequence, concatenating
///    their results into a single list.
///
/// ```rust
/// # use autumn::prelude::*;
/// /// Parses C-like identifiers
/// fn identifier(source: &str, location: Span) -> ParseResult<String> {
///     alphabetic
///         .or("_")
///         .and(alphanumeric.or("_").multiple().maybe())
///         .map(|s| s.to_string())
///         .parse(source, location)
/// }
/// ```
///
/// When using both [`multiple`](trait.ParserExt.html#method.multiple) and
/// [`maybe`](trait.ParserExt.html#method.maybe) to achieve zero or more repetitions,
/// `multiple().maybe()` must be used; `maybe().multiple()` can find an infinite number of ways to
/// apply any parser on even an empty string.
///
/// The characters or values read from input are pushed into the [`List`](../struct.List.html) in
/// last-in first-out (LIFO) order, so the list must be
/// [reversed](../struct.List.html#method.reverse) in order to iterate over the items in the order
/// they were added. The `ToString` implementation for [`List<char>`](../struct.List.html) does
/// this automatically.
pub trait ListParserExt<T, E>: Parser<List<T>, E> + Sized {

    /// Repeat a parser one or more times
    ///
    /// ```rust
    /// # use autumn::prelude::*;
    /// fn scream_parser() -> impl Parser<List<char>> {
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
    /// fn nondeterministic_parser() -> impl Parser<List<char>> {
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
    /// fn simple_identifier() -> impl Parser<List<char>> {
    ///     alphabetic.and(digit)
    /// }
    /// ```
    fn and<P: Parser<List<T>, E>>(self, other: P) -> And<Self, P, E> {
        And(self, other, PhantomData)
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
pub trait BoxedParserExt<'p, T, E>: Parser<T, E> + Sized + 'p {

    /// Convert a parser into a dynamically dispatched parser
    ///
    /// ```rust
    /// # use autumn::prelude::*;
    /// fn parser() -> impl Parser<List<char>> {
    ///     "a".and("b".or("c")).boxed()
    /// }
    /// ```
    fn boxed(self) -> Boxed<dyn Parser<T, E> + 'p> {
        Boxed::new(self)
    }
}

impl<'p, T: 'p, E: 'p, P: Parser<T, E> + 'p> BoxedParserExt<'p, T, E> for P {}

/// The result of the [`multiple`](trait.ListParserExt.html#method.multiple) function in the
/// [`ListParserExt`](trait.ListParserExt.html) trait
pub struct Multiple<P, E>(P, PhantomData<E>);

impl<T, E, P: Parser<List<T>, E>> Parser<List<T>, E> for Multiple<P, E> {
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, List<T>, E> {
        self.0
            .parse(source, location)
            .and_then(&|parsed, source, mut location| {
                self.parse(source, location.take())
                    .map(&|mut tail| parsed.concat(&mut tail))
                    .or(ParseResult::success(parsed, source, location))
            })
    }
}

/// The result of the [`maybe`](trait.ListParserExt.html#method.maybe) function in the
/// [`ListParserExt`](trait.ListParserExt.html) trait
pub struct Maybe<P, E>(P, PhantomData<E>);

impl<T, E, P: Parser<List<T>, E>> Parser<List<T>, E> for Maybe<P, E> {
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, List<T>, E> {
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

/// The result of the [`and`](trait.ListParserExt.html#method.and) function in the
/// [`ListParserExt`](trait.ListParserExt.html) trait
pub struct And<A, B, E>(A, B, PhantomData<E>);

impl<A, B, T, E> Parser<List<T>, E> for And<A, B, E>
where
    A: Parser<List<T>, E>,
    B: Parser<List<T>, E>,
{
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, List<T>, E> {
        self.0
            .parse(source, location)
            .and_then(&|prefix, source, location| {
                self.1
                    .parse(source, location)
                    .map(&|suffix| prefix.clone().concat(&suffix))
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
