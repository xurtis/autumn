//! Cominators for combining parsers

use crate::location::Span;
use crate::{parsers::empty, List, Parse, ParseResult, Parser};
use std::marker::PhantomData;

/// Combinator extensions to parsers
pub trait ParserExt<T, L>: Parser<T, L> + Sized {
    fn multiple(self) -> Multiple<Self> {
        Multiple(self)
    }

    fn maybe(self) -> Maybe<Self> {
        Maybe(self)
    }

    fn or<P: Parser<T, L>>(self, other: P) -> Or<Self, P> {
        Or(self, other)
    }

    fn and<P: Parser<T, L>>(self, other: P) -> And<Self, P> {
        And(self, other)
    }

    fn map<V, F: Fn(T) -> V>(self, map: F) -> Map<Self, F, T> {
        Map(self, map, PhantomData)
    }

    fn and_then<V, Q: Parser<V, L>, F: Fn(T) -> Q>(self, map: F) -> AndThen<Self, F, T> {
        AndThen(self, map, PhantomData)
    }

    fn drop<V, P: Parser<V, L>>(self, other: P) -> Drop<Self, P, V> {
        Drop(self, other, PhantomData)
    }

    fn skip<V>(self) -> Skip<Self, T> {
        Skip(self, PhantomData)
    }

    fn matching(self, compare: T) -> Matching<Self, T> {
        Matching(self, compare)
    }

    fn condition<F: Fn(&T) -> bool>(self, condition: F) -> Condition<Self, F> {
        Condition(self, condition)
    }
}

impl<T, L, P: Parser<T, L>> ParserExt<T, L> for P {}

pub trait BoxedParserExt<'p, T, L>: Parser<T, L> + Sized + 'p {
    fn boxed(self) -> Boxed<dyn Parser<T, L> + 'p> {
        Boxed::new(self)
    }
}

impl<'p, T: 'p, L: 'p, P: Parser<T, L> + 'p> BoxedParserExt<'p, T, L> for P {}

pub struct Multiple<P>(P);

impl<T: List + Clone, L: Span, P: Parser<T, L>> Parser<T, L> for Multiple<P> {
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, T, L> {
        self.0.parse(source, location).and_either(
            &|parsed, source, location| {
                self.parse(source, location).map(&|mut result| {
                    let mut parsed = parsed.clone();
                    parsed.concat(&mut result);
                    parsed
                })
            },
            &|parsed, source, mut location| {
                let parse = Parse::new(parsed, location.take());
                ParseResult::parsed(parse, source, location)
            },
        )
    }
}

pub struct Maybe<P>(P);

impl<T: List + Clone, L: Span, P: Parser<T, L>> Parser<T, L> for Maybe<P> {
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, T, L> {
        empty::<T, _>(source, location).then_either(&self.0, &empty)
    }
}

pub struct Condition<P, F>(P, F);

impl<T, L: Span, P: Parser<T, L>, F: Fn(&T) -> bool> Parser<T, L> for Condition<P, F> {
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, T, L> {
        self.0
            .parse(source, location)
            .and_then(&|parsed, source, mut location: L| {
                if self.1(&parsed) {
                    ParseResult::parsed(Parse::new(parsed, location.take()), source, location)
                } else {
                    ParseResult::none()
                }
            })
    }
}

pub struct Matching<P, F>(P, F);

impl<T: Eq, L: Span, P: Parser<T, L>> Parser<T, L> for Matching<P, T> {
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, T, L> {
        self.0
            .parse(source, location)
            .and_then(&|parsed, source, mut location: L| {
                if parsed == self.1 {
                    ParseResult::parsed(Parse::new(parsed, location.take()), source, location)
                } else {
                    ParseResult::none()
                }
            })
    }
}

pub struct Or<A, B>(A, B);

impl<A, B, T: Clone, L: Span> Parser<T, L> for Or<A, B>
where
    A: Parser<T, L>,
    B: Parser<T, L>,
{
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, T, L> {
        self.0
            .parse(source, location.clone())
            .or(self.1.parse(source, location))
    }
}

pub struct And<A, B>(A, B);

impl<A, B, T: List + Clone, L: Span> Parser<T, L> for And<A, B>
where
    A: Parser<T, L>,
    B: Parser<T, L>,
{
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, T, L> {
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

impl<P, F, T, L: Span, V> Parser<V, L> for Map<P, F, T>
where
    P: Parser<T, L>,
    F: Fn(T) -> V,
{
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, V, L> {
        self.0.parse(source, location).map(&self.1)
    }
}

pub struct AndThen<P, F, T>(P, F, PhantomData<T>);

impl<P, F, T, L: Span, Q, V> Parser<V, L> for AndThen<P, F, T>
where
    P: Parser<T, L>,
    Q: Parser<V, L>,
    F: Fn(T) -> Q,
{
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, V, L> {
        self.0
            .parse(source, location)
            .and_then(&|value, source, location| (self.1)(value).parse(source, location))
    }
}

pub struct Drop<A, B, V>(A, B, PhantomData<V>);

impl<A, B, T: Clone, V, L: Span> Parser<T, L> for Drop<A, B, V>
where
    A: Parser<T, L>,
    B: Parser<V, L>,
{
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, T, L> {
        self.0
            .parse(source, location)
            .and_then(&|keep, source, location| {
                self.1.parse(source, location).map(&|_| keep.clone())
            })
    }
}

pub struct Skip<P, V>(P, PhantomData<V>);

impl<P, T: List + Clone, V, L: Span> Parser<T, L> for Skip<P, V>
where
    P: Parser<V, L>,
{
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, T, L> {
        self.0
            .parse(source, location)
            .and_then(&|_, source, mut location| {
                ParseResult::parsed(Parse::new(T::new(), location.take()), source, location)
            })
    }
}

pub struct Boxed<P: ?Sized>(Box<P>);

impl<'p, T, L> Boxed<dyn Parser<T, L> + 'p> {
    pub(crate) fn new<P: Parser<T, L> + 'p>(parser: P) -> Self {
        let boxed: Box<dyn Parser<T, L> + 'p> = Box::new(parser);
        Boxed(boxed)
    }
}

impl<'p, T, L> Parser<T, L> for Boxed<dyn Parser<T, L> + 'p> {
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, T, L> {
        self.0.parse(source, location)
    }
}
