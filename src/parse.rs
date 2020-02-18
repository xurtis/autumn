use crate::location::{Meta, Span};

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
    parsed: Vec<(Meta<T, L>, &'s str, L)>,
    errors: Vec<ParseError>,
}

impl<'s, T, L> ParseResult<'s, T, L> {
    pub fn none() -> Self {
        ParseResult {
            parsed: vec![],
            errors: vec![],
        }
    }

    pub fn error(error: ParseError) -> Self {
        ParseResult {
            parsed: vec![],
            errors: vec![error],
        }
    }

    pub fn values<'r>(&'r mut self) -> impl Iterator<Item = Meta<T, L>> + 'r {
        self.parsed.drain(..).map(|(v, _, _)| v)
    }

    pub fn errors<'r>(&'r mut self) -> impl Iterator<Item = ParseError> + 'r {
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
}

impl<'s, T, L: Clone> ParseResult<'s, T, L> {
    pub fn meta(self) -> ParseResult<'s, Meta<T, L>, L> {
        let ParseResult { parsed, errors } = self;
        let parsed = parsed
            .into_iter()
            .map(|(v, r, l)| (v.split(), r, l))
            .map(|((v, s), r, l)| (Meta::new(Meta::new(v, s.clone()), s), r, l))
            .collect();
        ParseResult { parsed, errors }
    }
}

impl<'s, T, L: Span> ParseResult<'s, T, L> {
    pub fn success(value: T, source: &'s str, mut location: L) -> Self {
        ParseResult {
            parsed: vec![(Meta::new(value, location.take()), source, location)],
            errors: vec![],
        }
    }

    pub fn and_then<A, F>(self, next: &F) -> ParseResult<'s, A, L>
    where
        F: Fn(T, &'s str, L) -> ParseResult<'s, A, L>,
    {
        let ParseResult { parsed, mut errors } = self;
        let mut new_parsed = vec![];
        let iter = parsed
            .into_iter()
            .map(|(v, r, l)| v.map(|v| next(v, r, l.clone())))
            .map(unfold);
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

fn unfold<'s, T, L: Span>(meta: Meta<ParseResult<'s, T, L>, L>) -> ParseResult<'s, T, L> {
    let (ParseResult { parsed, errors }, start) = meta.split();

    let parsed = parsed
        .into_iter()
        .map(|(p, r, l)| (p.split(), r, l))
        .map(|((value, mut location), r, l)| {
            location.move_start(start.start().clone());
            (Meta::new(value, location), r, l)
        })
        .collect();

    ParseResult { parsed, errors }
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
