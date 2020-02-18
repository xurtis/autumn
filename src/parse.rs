use crate::location::{Meta, Span};
use std::ops::Deref;

pub fn parse<'s, T, L, P>(parser: &P, source: &'s str, location: L) -> ParseResult<'s, T, L>
where
    P: Parser<T, L>,
{
    parser.parse(source, location)
}

/// A parser takes an input source and produces an array of potential tagged values and an array of
/// errors.
pub trait Parser<T, L> {
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, T, L>;
}

impl<T, L, F> Parser<T, L> for F
where
    F: for<'s> Fn(&'s str, L) -> ParseResult<'s, T, L>,
{
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, T, L> {
        self(source, location)
    }
}

#[derive(Debug)]
pub struct ParseResult<'s, T, L> {
    parsed: Vec<(Parse<T, L>, &'s str, L)>,
    errors: Vec<ParseError>,
}

impl<'s, T, L> ParseResult<'s, T, L> {
    pub fn none() -> Self {
        ParseResult {
            parsed: vec![],
            errors: vec![],
        }
    }

    pub fn parsed(value: Parse<T, L>, source: &'s str, location: L) -> Self {
        ParseResult {
            parsed: vec![(value, source, location)],
            errors: vec![],
        }
    }

    pub fn error(error: ParseError) -> Self {
        ParseResult {
            parsed: vec![],
            errors: vec![error],
        }
    }

    pub fn values(&'s mut self) -> impl Iterator<Item = Parse<T, L>> + 's {
        self.parsed.drain(..).map(|(v, _, _)| v)
    }

    pub fn errors(&'s mut self) -> impl Iterator<Item = ParseError> + 's {
        self.errors.drain(..)
    }

    pub fn is_success(&self) -> bool {
        self.errors.len() == 0 && self.parsed.len() > 0
    }

    pub fn single_parse(&self) -> bool {
        self.parsed.len() == 1
    }

    pub fn or(self, other: Self) -> Self {
        let ParseResult {
            mut parsed,
            mut errors,
        } = self;
        let ParseResult {
            parsed: mut other_parsed,
            errors: mut other_errors,
        } = other;
        parsed.append(&mut other_parsed);
        errors.append(&mut other_errors);
        ParseResult { parsed, errors }
    }

    pub fn map<A, F>(self, map: &F) -> ParseResult<'s, A, L>
    where
        F: Fn(T) -> A,
    {
        let ParseResult { parsed, errors } = self;
        let parsed = parsed
            .into_iter()
            .map(|(v, r, l)| (v.map(map), r, l))
            .collect();
        ParseResult { parsed, errors }
    }

    pub fn then<V, F>(self, next: &F) -> ParseResult<'s, V, L>
    where
        F: Parser<V, L>,
    {
        let ParseResult { parsed, mut errors } = self;
        let mut new_parsed = vec![];
        let iter = parsed.into_iter().map(|(_, r, l)| next.parse(r, l));
        for ParseResult {
            mut parsed,
            errors: mut new_errors,
        } in iter
        {
            new_parsed.append(&mut parsed);
            errors.append(&mut new_errors);
        }
        ParseResult {
            parsed: new_parsed,
            errors,
        }
    }
}

impl<'s, T, L: Clone> ParseResult<'s, T, L> {
    pub fn success(value: T, source: &'s str, location: L) -> Self {
        Self::parsed(Parse::new(value, location.clone()), source, location)
    }

    pub fn meta_map<A, F>(self, map: &F) -> ParseResult<'s, A, L>
    where
        F: Fn(Meta<T, L>) -> A,
    {
        let ParseResult { parsed, errors } = self;
        let parsed = parsed
            .into_iter()
            .map(|(v, r, l)| (v.meta_map(map), r, l))
            .collect();
        ParseResult { parsed, errors }
    }
}

impl<'s, T: Clone, L: Clone> ParseResult<'s, T, L> {
    pub fn then_either<A, B, V>(self, first: &A, second: &B) -> ParseResult<'s, V, L>
    where
        A: Parser<V, L>,
        B: Parser<V, L>,
    {
        let ParseResult { parsed, mut errors } = self;
        let first = parsed
            .iter()
            .map(|(_, r, l)| (r.clone(), l.clone()))
            .map(|(r, l)| first.parse(r, l))
            .collect::<Vec<_>>()
            .into_iter();
        let second = parsed.into_iter().map(|(_, r, l)| second.parse(r, l));
        let mut new_parsed = vec![];
        for ParseResult {
            mut parsed,
            errors: mut new_errors,
        } in first.chain(second)
        {
            new_parsed.append(&mut parsed);
            errors.append(&mut new_errors);
        }
        ParseResult {
            parsed: new_parsed,
            errors,
        }
    }
}

impl<'s, T, L: Span> ParseResult<'s, T, L> {
    pub fn and_then<A, F>(self, next: &F) -> ParseResult<'s, A, L>
    where
        F: Fn(T, &'s str, L) -> ParseResult<'s, A, L>,
    {
        let ParseResult { parsed, mut errors } = self;
        let mut new_parsed = vec![];
        let iter = parsed
            .into_iter()
            .map(|(v, r, l)| v.map(|v| next(v, r, l.clone())))
            .map(|v| v.unfold());
        for ParseResult {
            mut parsed,
            errors: mut new_errors,
        } in iter
        {
            new_parsed.append(&mut parsed);
            errors.append(&mut new_errors);
        }
        ParseResult {
            parsed: new_parsed,
            errors,
        }
    }
}

impl<'s, T: Clone, L: Span> ParseResult<'s, T, L> {
    pub fn and_either<A, B, V>(self, first: &A, second: &B) -> ParseResult<'s, V, L>
    where
        A: Fn(T, &'s str, L) -> ParseResult<'s, V, L>,
        B: Fn(T, &'s str, L) -> ParseResult<'s, V, L>,
    {
        let ParseResult { parsed, mut errors } = self;
        let first = parsed
            .iter()
            .map(|(v, r, l)| (v.clone(), r.clone(), l.clone()))
            .map(|(v, r, l)| v.map(|v| first(v, r, l.clone())))
            .map(|v| v.unfold())
            .collect::<Vec<_>>()
            .into_iter();
        let second = parsed
            .into_iter()
            .map(|(v, r, l)| v.map(|v| second(v, r, l.clone())))
            .map(|v| v.unfold());

        let mut new_parsed = vec![];
        for ParseResult {
            mut parsed,
            errors: mut new_errors,
        } in first.chain(second)
        {
            new_parsed.append(&mut parsed);
            errors.append(&mut new_errors);
        }
        ParseResult {
            parsed: new_parsed,
            errors,
        }
    }
}

/// A single successful parse
#[derive(Debug, Clone)]
pub struct Parse<T, L>(Meta<T, L>);

impl<T, L> Parse<T, L> {
    pub fn new(value: T, location: L) -> Self {
        Parse(Meta::new(value, location))
    }

    pub fn map<A>(self, map: impl FnMut(T) -> A) -> Parse<A, L> {
        Parse(self.0.map(map))
    }

    pub fn into_inner(self) -> Meta<T, L> {
        self.0
    }
}

impl<T, L: Clone> Parse<T, L> {
    pub fn meta_map<A>(self, map: &impl Fn(Meta<T, L>) -> A) -> Parse<A, L> {
        let (value, location) = self.into_inner().split();
        let meta = Meta::new(value, location.clone());
        Parse::new(map(meta), location)
    }
}

impl<'s, T, L: Span> Parse<ParseResult<'s, T, L>, L> {
    pub fn unfold(self) -> ParseResult<'s, T, L> {
        let (ParseResult { parsed, errors }, start) = self.into_inner().split();

        let parsed = parsed
            .into_iter()
            .map(|(p, r, l)| (p.into_inner().split(), r, l))
            .map(|((value, mut location), r, l)| {
                location.move_start(start.start().clone());
                (Parse::new(value, location), r, l)
            })
            .collect();

        ParseResult { parsed, errors }
    }
}

impl<T, L> Deref for Parse<T, L> {
    type Target = Meta<T, L>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// Error produced during parsing
#[derive(Debug)]
pub enum ParseError {
    Generic,
}

pub trait List: Sized {
    type Item;

    fn new() -> Self;

    fn push(&mut self, item: Self::Item);

    fn concat(&mut self, other: &mut Self);

    fn single(item: Self::Item) -> Self {
        let mut list = Self::new();
        list.push(item);
        list
    }
}

impl List for String {
    type Item = char;

    fn new() -> Self {
        String::new()
    }

    fn push(&mut self, item: char) {
        String::push(self, item)
    }

    fn concat(&mut self, other: &mut Self) {
        self.push_str(other)
    }
}

impl<T> List for Vec<T> {
    type Item = T;

    fn new() -> Self {
        Vec::new()
    }

    fn push(&mut self, item: T) {
        Vec::push(self, item)
    }

    fn concat(&mut self, other: &mut Self) {
        self.append(other)
    }
}
