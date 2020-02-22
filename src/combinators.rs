//! Cominators for combining parsers

use crate::location::{Meta, Span};
use crate::parse::{List, ParseResult, Parser};
use crate::parsers::empty;
use std::marker::PhantomData;

/// Combinator extensions to parsers
pub trait ParserExt<T, L, E>: Parser<T, L, E> + Sized {
    fn multiple(self) -> Multiple<Self, L, E> {
        Multiple(self, PhantomData)
    }

    fn maybe(self) -> Maybe<Self, L, E> {
        Maybe(self, PhantomData)
    }

    fn or<P: Parser<T, L, E>>(self, other: P) -> Or<Self, P, L, E> {
        Or(self, other, PhantomData)
    }

    fn and<P: Parser<T, L, E>>(self, other: P) -> And<Self, P, L, E> {
        And(self, other, PhantomData)
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

pub trait BoxedParserExt<'p, T, L, E>: Parser<T, L, E> + Sized + 'p {
    fn boxed(self) -> Boxed<dyn Parser<T, L, E> + 'p> {
        Boxed::new(self)
    }
}

impl<'p, T: 'p, L: 'p, E: 'p, P: Parser<T, L, E> + 'p> BoxedParserExt<'p, T, L, E> for P {}

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

pub struct Maybe<P, L, E>(P, PhantomData<(L, E)>);

impl<T, L: Span, E, P: Parser<List<T>, L, E>> Parser<List<T>, L, E> for Maybe<P, L, E> {
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, List<T>, L, E> {
        self.0
            .parse(source, location.clone())
            .or(empty(source, location))
    }
}

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

pub struct Catch<P, L, E>(P, PhantomData<(L, E)>);

impl<T, L, E, P: Parser<T, L, E>> Parser<T, L, E> for Catch<P, L, E> {
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, T, L, E> {
        self.0.parse(source, location).catch()
    }
}

pub struct MetaMap<P, L, E>(P, PhantomData<(L, E)>);

impl<T, L: Span, E, P: Parser<T, L, E>> Parser<Meta<T, L>, L, E> for MetaMap<P, L, E> {
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, Meta<T, L>, L, E> {
        self.0.parse(source, location).meta()
    }
}
