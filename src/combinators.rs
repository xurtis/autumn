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
//! fn identifier<L: Span>(source: &str, location: L) -> ParseResult<String, L> {
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
//! fn parse<L: Span>(
//!     parser: impl Parser<List<char>, L>,
//!     source: &str,
//!     location: L,
//! ) -> ParseResult<String, L> {
//!     parser.map(|s| s.to_string()).end().parse(source, location)
//! }
//!
//! // Produces "right"
//! let skipped = parse("left".skip("right"), "leftright", new_location());
//! # assert_eq!(skipped.values().next().unwrap().inner_ref(), "right");
//!
//! // Produces "left"
//! let dropped = parse("left".drop("right"), "leftright", new_location());
//! # assert_eq!(dropped.values().next().unwrap().inner_ref(), "left");
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
//! fn integer<L: Span>(source: &str, location: L) -> ParseResult<String, L> {
//!     digit.multiple().map(|s| s.to_string()).parse(source, location)
//! }
//!
//! /// Parse a list of integers and get the source location of each integer
//! fn integer_list<L: Span>(source: &str, location: L) -> ParseResult<Vec<Meta<String, L>>, L> {
//!     "["
//!         .and(space.maybe())
//!         .skip(
//!             integer
//!                 .meta()
//!                 .map(List::single)
//!                 .and(
//!                     space.maybe()
//!                         .and(",")
//!                         .and(space.maybe())
//!                         .skip(integer.meta().map(List::single))
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
//!                 // Rc<Meta<String, L>> -> Meta<String, L>
//!                 .map(|s| s.as_ref().clone())
//!                 .collect()
//!         })
//!         .parse(source, location)
//! }
//! # assert!(integer_list("[]", new_location()).is_success());
//! # assert!(integer_list("[1, 2]", new_location()).is_success());
//! # assert!(integer_list("[1, 2, 3]", new_location()).is_success());
//! # assert!(integer_list("[1, 2, 3, 4, ]", new_location()).is_success());
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
//! fn alphabet<L: Span>(source: &str, location: L) -> ParseResult<String, L, &'static str> {
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
pub trait ParserExt<T, L, E>: Parser<T, L, E> + Sized {
    fn or<P: Parser<T, L, E>>(self, other: P) -> Or<Self, P, L, E> {
        Or(self, other, PhantomData)
    }

    fn map<V, F: Fn(T) -> V>(self, map: F) -> Map<Self, F, T, L, E> {
        Map(self, map, PhantomData)
    }

    fn and_then<V, Q: Parser<V, L, E>, F: Fn(T) -> Q>(self, map: F) -> AndThen<Self, F, T, L, E> {
        AndThen(self, map, PhantomData)
    }

    fn on_failure<P: Parser<T, L, E>>(self, other: P) -> OnFailure<Self, P, L, E> {
        OnFailure(self, other, PhantomData)
    }

    fn on_none<P: Parser<T, L, E>>(self, other: P) -> OnNone<Self, P, L, E> {
        OnNone(self, other, PhantomData)
    }

    fn drop<V, P: Parser<V, L, E>>(self, other: P) -> Drop<Self, P, V, L, E> {
        Drop(self, other, PhantomData)
    }

    fn skip<V, P: Parser<V, L, E>>(self, keep: P) -> Skip<Self, P, T, L, E> {
        Skip(self, keep, PhantomData)
    }

    fn matching<V>(self, compare: V) -> Matching<Self, V, L, E>
    where
        T: PartialEq<V>,
    {
        Matching(self, compare, PhantomData)
    }

    fn condition<F: Fn(&T) -> bool>(self, condition: F) -> Condition<Self, F, L, E> {
        Condition(self, condition, PhantomData)
    }

    fn end(self) -> End<Self, L, E> {
        End(self, PhantomData)
    }

    fn catch(self) -> Catch<Self, L, E> {
        Catch(self, PhantomData)
    }

    fn meta(self) -> MetaMap<Self, L, E> {
        MetaMap(self, PhantomData)
    }
}

impl<T, L, E, P: Parser<T, L, E>> ParserExt<T, L, E> for P {}

/// Combinators on parsers that produce a [`List`](../struct.List.html)
pub trait ListParserExt<T, L, E>: Parser<List<T>, L, E> + Sized {
    fn multiple(self) -> Multiple<Self, L, E> {
        Multiple(self, PhantomData)
    }

    fn maybe(self) -> Maybe<Self, L, E> {
        Maybe(self, PhantomData)
    }

    fn and<P: Parser<List<T>, L, E>>(self, other: P) -> And<Self, P, L, E> {
        And(self, other, PhantomData)
    }
}

impl<T, L, E, P: Parser<List<T>, L, E>> ListParserExt<T, L, E> for P {}

/// Boxing parsers for [dynamic
/// dispatch](https://doc.rust-lang.org/book/ch17-02-trait-objects.html#trait-objects-perform-dynamic-dispatch)
pub trait BoxedParserExt<'p, T, L, E>: Parser<T, L, E> + Sized + 'p {
    fn boxed(self) -> Boxed<dyn Parser<T, L, E> + 'p> {
        Boxed::new(self)
    }
}

impl<'p, T: 'p, L: 'p, E: 'p, P: Parser<T, L, E> + 'p> BoxedParserExt<'p, T, L, E> for P {}

/// The result of the [`multiple`](trait.ListParserExt.html#method.multiple) function in the
/// [`ListParserExt`](trait.ListParserExt.html) trait
pub struct Multiple<P, L, E>(P, PhantomData<(L, E)>);

impl<T, L: Span, E, P: Parser<List<T>, L, E>> Parser<List<T>, L, E> for Multiple<P, L, E> {
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, List<T>, L, E> {
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
pub struct Maybe<P, L, E>(P, PhantomData<(L, E)>);

impl<T, L: Span, E, P: Parser<List<T>, L, E>> Parser<List<T>, L, E> for Maybe<P, L, E> {
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, List<T>, L, E> {
        self.0
            .parse(source, location.clone())
            .or(empty(source, location))
    }
}

/// The result of the [`condition`](trait.ParserExt.html#method.condition) function in the
/// [`ParserExt`](trait.ParserExt.html) trait
pub struct Condition<P, F, L, E>(P, F, PhantomData<(L, E)>);

impl<T, L: Span, E, P: Parser<T, L, E>, F: Fn(&T) -> bool> Parser<T, L, E>
    for Condition<P, F, L, E>
{
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, T, L, E> {
        self.0
            .parse(source, location)
            .and_then(&|parsed, source, location: L| {
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
pub struct Matching<P, F, L, E>(P, F, PhantomData<(L, E)>);

impl<T: PartialEq<V>, V, E, L: Span, P: Parser<T, L, E>> Parser<T, L, E> for Matching<P, V, L, E> {
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, T, L, E> {
        self.0
            .parse(source, location)
            .and_then(&|parsed, source, location: L| {
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
pub struct Or<A, B, L, E>(A, B, PhantomData<(L, E)>);

impl<A, B, T: Clone, L: Span, E> Parser<T, L, E> for Or<A, B, L, E>
where
    A: Parser<T, L, E>,
    B: Parser<T, L, E>,
{
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, T, L, E> {
        self.0
            .parse(source, location.clone())
            .or(self.1.parse(source, location))
    }
}

/// The result of the [`and`](trait.ListParserExt.html#method.and) function in the
/// [`ListParserExt`](trait.ListParserExt.html) trait
pub struct And<A, B, L, E>(A, B, PhantomData<(L, E)>);

impl<A, B, T, L: Span, E> Parser<List<T>, L, E> for And<A, B, L, E>
where
    A: Parser<List<T>, L, E>,
    B: Parser<List<T>, L, E>,
{
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, List<T>, L, E> {
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
pub struct Map<P, F, T, L, E>(P, F, PhantomData<(T, L, E)>);

impl<P, F, T, L: Span, E, V> Parser<V, L, E> for Map<P, F, T, L, E>
where
    P: Parser<T, L, E>,
    F: Fn(T) -> V,
{
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, V, L, E> {
        self.0.parse(source, location).map(&self.1)
    }
}

/// The result of the [`and_then`](trait.ParserExt.html#method.and_then) function in the
/// [`ParserExt`](trait.ParserExt.html) trait
pub struct AndThen<P, F, T, L, E>(P, F, PhantomData<(T, L, E)>);

impl<P, F, T, L: Span, E, Q, V> Parser<V, L, E> for AndThen<P, F, T, L, E>
where
    P: Parser<T, L, E>,
    Q: Parser<V, L, E>,
    F: Fn(T) -> Q,
{
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, V, L, E> {
        self.0
            .parse(source, location)
            .and_then(&|value, source, location| (self.1)(value).parse(source, location))
    }
}

/// The result of the [`on_failure`](trait.ParserExt.html#method.on_failure) function in the
/// [`ParserExt`](trait.ParserExt.html) trait
pub struct OnFailure<A, B, L, E>(A, B, PhantomData<(L, E)>);

impl<A, B, T: Clone, L: Span, E> Parser<T, L, E> for OnFailure<A, B, L, E>
where
    A: Parser<T, L, E>,
    B: Parser<T, L, E>,
{
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, T, L, E> {
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
pub struct OnNone<A, B, L, E>(A, B, PhantomData<(L, E)>);

impl<A, B, T: Clone, L: Span, E> Parser<T, L, E> for OnNone<A, B, L, E>
where
    A: Parser<T, L, E>,
    B: Parser<T, L, E>,
{
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, T, L, E> {
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
pub struct Drop<A, B, V, L, E>(A, B, PhantomData<(V, L, E)>);

impl<A, B, T: Clone, V, L: Span, E> Parser<T, L, E> for Drop<A, B, V, L, E>
where
    A: Parser<T, L, E>,
    B: Parser<V, L, E>,
{
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, T, L, E> {
        self.0
            .parse(source, location)
            .and_then(&|keep, source, location| {
                self.1.parse(source, location).map(&|_| keep.clone())
            })
    }
}

/// The result of the [`skip`](trait.ParserExt.html#method.skip) function in the
/// [`ParserExt`](trait.ParserExt.html) trait
pub struct Skip<P, Q, T, L, E>(P, Q, PhantomData<(T, L, E)>);

impl<P, Q, T, V, L: Span, E> Parser<V, L, E> for Skip<P, Q, T, L, E>
where
    P: Parser<T, L, E>,
    Q: Parser<V, L, E>,
{
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, V, L, E> {
        self.0
            .parse(source, location)
            .and_then(&|_, source, location| self.1.parse(source, location))
    }
}

/// The result of the [`boxed`](trait.BoxedParserExt.html#method.boxed) function in the
/// [`BoxedParserExt`](trait.BoxedParserExt.html) trait
pub struct Boxed<P: ?Sized>(Box<P>);

impl<'p, T, L, E> Boxed<dyn Parser<T, L, E> + 'p> {
    pub(crate) fn new<P: Parser<T, L, E> + 'p>(parser: P) -> Self {
        let boxed: Box<dyn Parser<T, L, E> + 'p> = Box::new(parser);
        Boxed(boxed)
    }
}

impl<'p, T, L, E> Parser<T, L, E> for Boxed<dyn Parser<T, L, E> + 'p> {
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, T, L, E> {
        self.0.parse(source, location)
    }
}

/// The result of the [`end`](trait.ParserExt.html#method.end) function in the
/// [`ParserExt`](trait.ParserExt.html) trait
pub struct End<P, L, E>(P, PhantomData<(L, E)>);

impl<T, L: Span, E, P: Parser<T, L, E>> Parser<T, L, E> for End<P, L, E> {
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, T, L, E> {
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
pub struct Catch<P, L, E>(P, PhantomData<(L, E)>);

impl<T, L, E, P: Parser<T, L, E>> Parser<T, L, E> for Catch<P, L, E> {
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, T, L, E> {
        self.0.parse(source, location).catch()
    }
}

/// The result of the [`meta`](trait.ParserExt.html#method.meta) function in the
/// [`ParserExt`](trait.ParserExt.html) trait
pub struct MetaMap<P, L, E>(P, PhantomData<(L, E)>);

impl<T, L: Span, E, P: Parser<T, L, E>> Parser<Meta<T, L>, L, E> for MetaMap<P, L, E> {
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, Meta<T, L>, L, E> {
        self.0.parse(source, location).meta()
    }
}
