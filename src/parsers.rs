//! Basic general-purpose parsers

use crate::combinators::ParserExt;
use crate::location::Span;
use crate::{List, ParseResult, Parser};

pub fn none<T, L, E>(_: &str, location: L) -> ParseResult<T, L, E> {
    ParseResult::none(location)
}

pub fn value<T: Clone, L: Span, E>(value: T) -> impl Parser<T, L, E> {
    closure(move |source, location| success(value.clone(), source, location))
}

pub fn error<T: Clone, L: Span, E: Clone>(value: T, error: E) -> impl Parser<T, L, E> {
    closure(move |source, location| failure(value.clone(), error.clone(), source, location))
}

pub fn throw<T: Clone, L: Span, E: Clone>(value: T, error: E) -> impl Parser<T, L, E> {
    closure(move |source, location| exception(value.clone(), error.clone(), source, location))
}

fn success<T, L: Span, E>(value: T, source: &str, location: L) -> ParseResult<T, L, E> {
    ParseResult::success(value, source, location)
}

fn failure<'s, T, L: Span, E>(
    value: T,
    error: E,
    source: &'s str,
    location: L,
) -> ParseResult<'s, T, L, E> {
    ParseResult::error(value, error, source, location)
}

fn exception<'s, T, L: Span, E>(
    value: T,
    error: E,
    source: &'s str,
    location: L,
) -> ParseResult<'s, T, L, E> {
    ParseResult::exception(value, error, source, location)
}

pub fn empty<T, L: Span, E>(source: &str, location: L) -> ParseResult<List<T>, L, E> {
    ParseResult::success(List::new(), source, location)
}

pub fn closure<F, T, L, E>(function: F) -> impl Parser<T, L, E>
where
    F: for<'s> Fn(&'s str, L) -> ParseResult<'s, T, L, E>,
{
    function
}

pub fn any_character<L: Span, E>(source: &str, mut location: L) -> ParseResult<List<char>, L, E> {
    if let Some((_, next)) = source.char_indices().next() {
        location.after(next);
        ParseResult::success(List::new().push(next), &source[next.len_utf8()..], location)
    } else {
        ParseResult::none(location)
    }
}

pub fn single_character<L: Span, E>(source: &str, mut location: L) -> ParseResult<char, L, E> {
    if let Some((_, next)) = source.char_indices().next() {
        location.after(next);
        ParseResult::success(next, &source[next.len_utf8()..], location)
    } else {
        ParseResult::none(location)
    }
}

fn char_condition<'s, L: Span, E>(
    condition: &impl Fn(char) -> bool,
    source: &'s str,
    location: L,
) -> ParseResult<'s, List<char>, L, E> {
    any_character
        .condition(|c| c.clone().map(|c| *c).all(condition))
        .parse(source, location)
}

impl<L: Span, E> Parser<List<char>, L, E> for char {
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, List<char>, L, E> {
        let character = *self;
        any_character
            .condition(move |c| c.clone().all(move |c| *c == character))
            .parse(source, location)
    }
}

pub fn character<L: Span, E>(character: char) -> impl Parser<List<char>, L, E> {
    any_character.condition(move |c| c.clone().all(move |c| *c == character))
}

pub fn digit<L: Span, E>(source: &str, location: L) -> ParseResult<List<char>, L, E> {
    char_condition(&|c| c.is_ascii_digit(), source, location)
}

pub fn alphabetic<L: Span, E>(source: &str, location: L) -> ParseResult<List<char>, L, E> {
    char_condition(&|c| c.is_ascii_alphabetic(), source, location)
}

pub fn alphanumeric<L: Span, E>(source: &str, location: L) -> ParseResult<List<char>, L, E> {
    char_condition(&|c| c.is_ascii_alphanumeric(), source, location)
}

pub fn whitespace<L: Span, E>(source: &str, location: L) -> ParseResult<List<char>, L, E> {
    char_condition(&|c| c.is_whitespace(), source, location)
}

pub fn space<L: Span, E>(source: &str, location: L) -> ParseResult<List<char>, L, E> {
    whitespace.multiple().parse(source, location)
}

pub fn exact<L: Span, E>(must_match: &'static str) -> impl Parser<List<char>, L, E> {
    closure::<_, _, _, E>(move |source, location| {
        if let Some(next) = must_match.chars().next() {
            let remaining = &must_match[next.len_utf8()..];
            character::<_, E>(next)
                .and(exact::<_, E>(remaining))
                .parse(source, location)
        } else {
            empty.parse(source, location)
        }
    })
}

impl<L: Span, E> Parser<List<char>, L, E> for &'static str {
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, List<char>, L, E> {
        exact(self).parse(source, location)
    }
}
