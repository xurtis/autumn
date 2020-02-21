//! Basic general-purpose parsers

use crate::location::Span;
use crate::ParserExt;
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

pub fn success<T, L: Span, E>(value: T, source: &str, location: L) -> ParseResult<T, L, E> {
    ParseResult::success(value, source, location)
}

pub fn failure<'s, T, L: Span, E>(
    value: T,
    error: E,
    source: &'s str,
    location: L,
) -> ParseResult<'s, T, L, E> {
    ParseResult::error(value, error, source, location)
}

pub fn empty<T: List, L: Span, E>(source: &str, location: L) -> ParseResult<T, L, E> {
    ParseResult::success(T::new(), source, location)
}

pub fn closure<F, T, L, E>(function: F) -> impl Parser<T, L, E>
where
    F: for<'s> Fn(&'s str, L) -> ParseResult<'s, T, L, E>,
{
    function
}

pub fn any_character<T, L, E>(source: &str, mut location: L) -> ParseResult<T, L, E>
where
    T: List<Item = char>,
    L: Span,
{
    if let Some((_, next)) = source.char_indices().next() {
        location.after(next);
        ParseResult::success(List::single(next), &source[next.len_utf8()..], location)
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

pub fn char_condition<'s, L: Span, E: Clone>(
    condition: &impl Fn(char) -> bool,
    source: &'s str,
    location: L,
) -> ParseResult<'s, String, L, E> {
    any_character
        .condition(|c: &String| c.chars().all(condition))
        .parse(source, location)
}

impl<L: Span, E: Clone> Parser<String, L, E> for char {
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, String, L, E> {
        let character = *self;
        any_character
            .condition(move |c: &String| c.chars().all(move |s| s == character))
            .parse(source, location)
    }
}

pub fn character<L: Span, E: Clone>(character: char) -> impl Parser<String, L, E> {
    any_character.condition(move |c: &String| c.chars().all(move |s| s == character))
}

pub fn digit<L: Span, E: Clone>(source: &str, location: L) -> ParseResult<String, L, E> {
    char_condition(&|c| c.is_ascii_digit(), source, location)
}

pub fn alphabetic<L: Span, E: Clone>(source: &str, location: L) -> ParseResult<String, L, E> {
    char_condition(&|c| c.is_ascii_alphabetic(), source, location)
}

pub fn alphanumeric<L: Span, E: Clone>(source: &str, location: L) -> ParseResult<String, L, E> {
    char_condition(&|c| c.is_ascii_alphanumeric(), source, location)
}

pub fn whitespace<L: Span, E: Clone>(source: &str, location: L) -> ParseResult<String, L, E> {
    char_condition(&|c| c.is_whitespace(), source, location)
}

pub fn space<L: Span, E: Clone>(source: &str, location: L) -> ParseResult<String, L, E> {
    whitespace.multiple().parse(source, location)
}

pub fn exact<L: Span, E: Clone>(must_match: &'static str) -> impl Parser<String, L, E> {
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

impl<L: Span, E: Clone> Parser<String, L, E> for &'static str {
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, String, L, E> {
        exact(self).parse(source, location)
    }
}
