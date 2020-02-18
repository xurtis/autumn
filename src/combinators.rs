//! Cominators for combining parsers

use crate::location::{Meta, Span};
use crate::parse::{List, ParseResult, Parser};
use crate::parsers::empty;
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

    fn skip<V, P: Parser<V, L>>(self, keep: P) -> Skip<Self, P, T> {
        Skip(self, keep, PhantomData)
    }

    fn matching(self, compare: T) -> Matching<Self, T> {
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
        self.0
            .parse(source, location)
            .and_then(&|parsed, source, location| {
                self.parse(source, location.clone())
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

impl<T: List + Clone, L: Span, P: Parser<T, L>> Parser<T, L> for Maybe<P> {
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, T, L> {
        self.0
            .parse(source, location.clone())
            .or(empty(source, location))
    }
}

pub struct Condition<P, F>(P, F);

impl<T, L: Span, P: Parser<T, L>, F: Fn(&T) -> bool> Parser<T, L> for Condition<P, F> {
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, T, L> {
        self.0
            .parse(source, location)
            .and_then(&|parsed, source, location: L| {
                if self.1(&parsed) {
                    ParseResult::success(parsed, source, location)
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
            .and_then(&|parsed, source, location: L| {
                if parsed == self.1 {
                    ParseResult::success(parsed, source, location)
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

pub struct Skip<P, Q, T>(P, Q, PhantomData<T>);

impl<P, Q, T, V, L: Span> Parser<V, L> for Skip<P, Q, T>
where
    P: Parser<T, L>,
    Q: Parser<V, L>,
{
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, V, L> {
        self.0
            .parse(source, location)
            .and_then(&|_, source, location| self.1.parse(source, location))
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

pub struct End<P>(P);

impl<T, L: Span, P: Parser<T, L>> Parser<T, L> for End<P> {
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, T, L> {
        self.0
            .parse(source, location)
            .and_then(&|value, source, location| {
                if source.len() == 0 {
                    ParseResult::success(value, source, location)
                } else {
                    ParseResult::none()
                }
            })
    }
}

pub struct MetaMap<P>(P);

impl<T, L: Span, P: Parser<T, L>> Parser<Meta<T, L>, L> for MetaMap<P> {
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, Meta<T, L>, L> {
        self.0.parse(source, location).meta()
    }
}
