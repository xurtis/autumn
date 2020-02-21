use crate::location::{Meta, Span};

pub fn parse<'s, T, L, E, P>(parser: &P, source: &'s str, location: L) -> ParseResult<'s, T, L, E>
where
    P: Parser<T, L, E>,
{
    parser.parse(source, location)
}

/// A parser takes an input source and produces an array of potential tagged values and an array of
/// errors.
pub trait Parser<T, L, E = ()> {
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, T, L, E>;
}

impl<T, L, E, F> Parser<T, L, E> for F
where
    F: for<'s> Fn(&'s str, L) -> ParseResult<'s, T, L, E>,
{
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, T, L, E> {
        self(source, location)
    }
}

#[derive(Debug)]
pub struct ParseResult<'s, T, L, E = ()> {
    start: L,
    success: bool,
    failure: bool,
    results: Vec<(Meta<T, L>, Vec<Meta<E, L>>, &'s str, L)>,
}

impl<'s, T, L, E> ParseResult<'s, T, L, E> {
    pub fn none(location: L) -> Self {
        ParseResult {
            start: location,
            success: false,
            failure: false,
            results: vec![],
        }
    }

    pub fn values<'r>(&'r mut self) -> impl Iterator<Item = Meta<T, L>> + 'r {
        self.results.drain(..).map(|(value, _, _, _)| value)
    }

    pub fn errors<'r>(&'r mut self) -> impl Iterator<Item = Meta<E, L>> + 'r {
        self.results.drain(..).flat_map(|(_, errors, _, _)| errors)
    }

    pub fn is_success(&self) -> bool {
        self.success && !self.failure
    }

    pub fn is_none(&self) -> bool {
        !self.failure && !self.success
    }

    pub fn single_parse(&self) -> bool {
        self.results.len() == 1
    }

    pub fn or(self, other: Self) -> Self {
        let ParseResult {
            start,
            mut success,
            mut failure,
            mut results,
        } = self;
        let ParseResult {
            success: other_success,
            failure: other_failure,
            results: mut other_results,
            ..
        } = other;
        success |= other_success;
        failure |= other_failure;
        results.append(&mut other_results);
        ParseResult {
            start,
            success,
            failure,
            results,
        }
    }

    pub fn map<A, F>(self, map: &F) -> ParseResult<'s, A, L, E>
    where
        F: Fn(T) -> A,
    {
        let ParseResult {
            start,
            success,
            failure,
            results,
        } = self;
        let results = results
            .into_iter()
            .map(|(v, e, s, l)| (v.map(map), e, s, l))
            .collect();
        ParseResult {
            start,
            success,
            failure,
            results,
        }
    }
}

impl<'s, T, L: Clone, E> ParseResult<'s, T, L, E> {
    pub fn meta(self) -> ParseResult<'s, Meta<T, L>, L, E> {
        let ParseResult {
            start,
            success,
            failure,
            results,
        } = self;
        let results = results
            .into_iter()
            .map(|(v, e, i, l)| (v.split(), e, i, l))
            .map(|((v, s), e, i, l)| (Meta::new(Meta::new(v, s.clone()), s), e, i, l))
            .collect();
        ParseResult {
            start,
            success,
            failure,
            results,
        }
    }
}

impl<'s, T, L: Span, E> ParseResult<'s, T, L, E> {
    pub fn error(value: T, error: E, source: &'s str, mut location: L) -> Self {
        let parse_location = location.take();
        let value = Meta::new(value, parse_location.clone());
        let error = Meta::new(error, parse_location);
        ParseResult {
            start: location.clone(),
            success: false,
            failure: true,
            results: vec![(value, vec![error], source, location)],
        }
    }

    pub fn success(value: T, source: &'s str, mut location: L) -> Self {
        ParseResult {
            start: location.clone(),
            success: true,
            failure: false,
            results: vec![(Meta::new(value, location.take()), vec![], source, location)],
        }
    }
}

impl<'s, T, L: Span, E: Clone> ParseResult<'s, T, L, E> {
    pub fn and_then<A, F>(self, next: &F) -> ParseResult<'s, A, L, E>
    where
        F: Fn(T, &'s str, L) -> ParseResult<'s, A, L, E>,
    {
        let ParseResult { start, results, .. } = self;
        let mut new_results = vec![];
        let mut success = false;
        let mut failure = false;
        let iter = results
            .into_iter()
            .map(|(v, e, s, l)| (v.map(|v| next(v, s, l.clone())), e))
            .map(unfold);
        for ParseResult {
            mut results,
            success: new_success,
            failure: new_failure,
            ..
        } in iter
        {
            success |= new_success;
            failure |= new_failure;
            new_results.append(&mut results);
        }
        ParseResult {
            start,
            success,
            failure,
            results: new_results,
        }
    }
}

fn unfold<'s, T, L: Span, E: Clone>(
    (meta, errors): (Meta<ParseResult<'s, T, L, E>, L>, Vec<Meta<E, L>>),
) -> ParseResult<'s, T, L, E> {
    let (
        ParseResult {
            success,
            mut failure,
            results,
            ..
        },
        start,
    ) = meta.split();

    let results: Vec<_> = results
        .into_iter()
        .map(|(v, e, s, l)| (v.split(), e, s, l))
        .map(|((value, mut location), inner_errors, r, l)| {
            location.move_start(start.start().clone());
            let errors = errors.iter().fold(inner_errors, |mut errors, error| {
                errors.push(error.clone());
                errors
            });
            (Meta::new(value, location), errors, r, l)
        })
        .collect();

    failure |= results.iter().any(|(_, errors, _, _)| errors.len() > 0);

    ParseResult {
        start,
        success,
        failure,
        results,
    }
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
