//! Cominators for combining parsers

use crate::location::{Meta, Span};
use crate::parse::{List, ParseResult, Parser};
use crate::parsers::empty;
use std::marker::PhantomData;

/// Combinator extensions to parsers
pub trait ParserExt<T, L, E>: Parser<T, L, E> + Sized {
    fn multiple(self) -> Multiple<Self> {
        Multiple(self)
    }

    fn maybe(self) -> Maybe<Self> {
        Maybe(self)
    }

    fn or<P: Parser<T, L, E>>(self, other: P) -> Or<Self, P> {
        Or(self, other)
    }

    fn and<P: Parser<T, L, E>>(self, other: P) -> And<Self, P> {
        And(self, other)
    }

    fn map<V, F: Fn(T) -> V>(self, map: F) -> Map<Self, F, T> {
        Map(self, map, PhantomData)
    }

    fn and_then<V, Q: Parser<V, L, E>, F: Fn(T) -> Q>(self, map: F) -> AndThen<Self, F, T> {
        AndThen(self, map, PhantomData)
    }

    fn on_failure<P: Parser<T, L, E>>(self, other: P) -> OnFailure<Self, P> {
        OnFailure(self, other)
    }

    fn on_none<P: Parser<T, L, E>>(self, other: P) -> OnNone<Self, P> {
        OnNone(self, other)
    }

    fn drop<V, P: Parser<V, L, E>>(self, other: P) -> Drop<Self, P, V> {
        Drop(self, other, PhantomData)
    }

    fn skip<V, P: Parser<V, L, E>>(self, keep: P) -> Skip<Self, P, T> {
        Skip(self, keep, PhantomData)
    }

    fn matching<V>(self, compare: V) -> Matching<Self, V>
    where
        T: PartialEq<V>,
    {
        Matching(self, compare)
    }

    fn condition<F: Fn(&T) -> bool>(self, condition: F) -> Condition<Self, F> {
        Condition(self, condition)
    }

    fn end(self) -> End<Self> {
        End(self)
    }

    fn meta(self) -> MetaMap<Self> {
        MetaMap(self)
    }
}

impl<T, L, E, P: Parser<T, L, E>> ParserExt<T, L, E> for P {}

pub trait BoxedParserExt<'p, T, L, E>: Parser<T, L, E> + Sized + 'p {
    fn boxed(self) -> Boxed<dyn Parser<T, L, E> + 'p> {
        Boxed::new(self)
    }
}

impl<'p, T: 'p, L: 'p, E: 'p, P: Parser<T, L, E> + 'p> BoxedParserExt<'p, T, L, E> for P {}

pub struct Multiple<P>(P);

impl<T: List + Clone, L: Span, E: Clone, P: Parser<T, L, E>> Parser<T, L, E> for Multiple<P> {
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, T, L, E> {
        self.0
            .parse(source, location)
            .and_then(&|parsed, source, mut location| {
                self.parse(source, location.take())
                    .map(&|mut tail| {
                        let mut parsed = parsed.clone();
                        parsed.concat(&mut tail);
                        parsed
                    })
                    .or(ParseResult::success(parsed, source, location))
            })
    }
}

pub struct Maybe<P>(P);

impl<T: List + Clone, L: Span, E, P: Parser<T, L, E>> Parser<T, L, E> for Maybe<P> {
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, T, L, E> {
        self.0
            .parse(source, location.clone())
            .or(empty(source, location))
    }
}

pub struct Condition<P, F>(P, F);

impl<T, L: Span, E: Clone, P: Parser<T, L, E>, F: Fn(&T) -> bool> Parser<T, L, E>
    for Condition<P, F>
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

pub struct Matching<P, F>(P, F);

impl<T: PartialEq<V>, V, E: Clone, L: Span, P: Parser<T, L, E>> Parser<T, L, E> for Matching<P, V> {
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

pub struct Or<A, B>(A, B);

impl<A, B, T: Clone, L: Span, E> Parser<T, L, E> for Or<A, B>
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

pub struct And<A, B>(A, B);

impl<A, B, T: List + Clone, L: Span, E: Clone> Parser<T, L, E> for And<A, B>
where
    A: Parser<T, L, E>,
    B: Parser<T, L, E>,
{
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, T, L, E> {
        self.0
            .parse(source, location)
            .and_then(&|prefix, source, location| {
                self.1.parse(source, location).map(&|mut suffix| {
                    let mut token = prefix.clone();
                    token.concat(&mut suffix);
                    token
                })
            })
    }
}

pub struct Map<P, F, T>(P, F, PhantomData<T>);

impl<P, F, T, L: Span, E, V> Parser<V, L, E> for Map<P, F, T>
where
    P: Parser<T, L, E>,
    F: Fn(T) -> V,
{
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, V, L, E> {
        self.0.parse(source, location).map(&self.1)
    }
}

pub struct AndThen<P, F, T>(P, F, PhantomData<T>);

impl<P, F, T, L: Span, E: Clone, Q, V> Parser<V, L, E> for AndThen<P, F, T>
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

pub struct OnFailure<A, B>(A, B);

impl<A, B, T: Clone, L: Span, E> Parser<T, L, E> for OnFailure<A, B>
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

pub struct OnNone<A, B>(A, B);

impl<A, B, T: Clone, L: Span, E> Parser<T, L, E> for OnNone<A, B>
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

pub struct Drop<A, B, V>(A, B, PhantomData<V>);

impl<A, B, T: Clone, V, L: Span, E: Clone> Parser<T, L, E> for Drop<A, B, V>
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

pub struct Skip<P, Q, T>(P, Q, PhantomData<T>);

impl<P, Q, T, V, L: Span, E: Clone> Parser<V, L, E> for Skip<P, Q, T>
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

pub struct End<P>(P);

impl<T, L: Span, E: Clone, P: Parser<T, L, E>> Parser<T, L, E> for End<P> {
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

pub struct MetaMap<P>(P);

impl<T, L: Span, E, P: Parser<T, L, E>> Parser<Meta<T, L>, L, E> for MetaMap<P> {
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, Meta<T, L>, L, E> {
        self.0.parse(source, location).meta()
    }
}
