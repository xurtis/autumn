//! Basic general-purpose parsers

use crate::location::Span;
use crate::ParserExt;
use crate::{List, Parse, ParseResult, Parser};

pub fn none<T, L>(_: &str, _: L) -> ParseResult<T, L> {
    ParseResult::none()
}

pub fn value<T: Clone, L: Span>(value: T) -> impl Parser<T, L> {
    closure(move |source, location| success(value.clone(), source, location))
}

pub fn success<T, L: Span>(value: T, source: &str, location: L) -> ParseResult<T, L> {
    ParseResult::success(value, source, location)
}

pub fn empty<T, L>(source: &str, location: L) -> ParseResult<T, L>
where
    T: List,
    L: Span,
{
    ParseResult::parsed(Parse::new(T::new(), location.clone()), source, location)
}

pub fn tail<T, L>(source: &str, location: L) -> ParseResult<T, L>
where
    T: List,
    L: Span,
{
    if source.len() == 0 {
        ParseResult::parsed(Parse::new(T::new(), location.clone()), source, location)
    } else {
        ParseResult::none()
    }
}

pub fn closure<F, T, L>(function: F) -> impl Parser<T, L>
where
    F: for<'s> Fn(&'s str, L) -> ParseResult<'s, T, L>,
{
    function
}

pub fn any_character<T, L>(source: &str, mut location: L) -> ParseResult<T, L>
where
    T: List<Item = char>,
    L: Span,
{
    if let Some((_, next)) = source.char_indices().next() {
        location.after(next);
        let char_location = location.take();
        let parse = Parse::new(List::single(next), char_location);
        ParseResult::parsed(parse, &source[next.len_utf8()..], location)
    } else {
        ParseResult::none()
    }
}

pub fn single_character<L: Span>(source: &str, mut location: L) -> ParseResult<char, L> {
    if let Some((_, next)) = source.char_indices().next() {
        location.after(next);
        let char_location = location.take();
        let parse = Parse::new(next, char_location);
        ParseResult::parsed(parse, &source[next.len_utf8()..], location)
    } else {
        ParseResult::none()
    }
}

pub fn char_condition<'s, L: Span>(
    condition: &impl Fn(char) -> bool,
    source: &'s str,
    location: L,
) -> ParseResult<'s, String, L> {
    any_character
        .condition(|c: &String| c.chars().all(condition))
        .parse(source, location)
}

impl<L: Span> Parser<String, L> for char {
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, String, L> {
        let character = *self;
        any_character
            .condition(move |c: &String| c.chars().all(move |s| s == character))
            .parse(source, location)
    }
}

pub fn character<L: Span>(character: char) -> impl Parser<String, L> {
    any_character.condition(move |c: &String| c.chars().all(move |s| s == character))
}

pub fn digit<L: Span>(source: &str, location: L) -> ParseResult<String, L> {
    char_condition(&|c| c.is_ascii_digit(), source, location)
}

pub fn alphabetic<L: Span>(source: &str, location: L) -> ParseResult<String, L> {
    char_condition(&|c| c.is_ascii_alphabetic(), source, location)
}

pub fn alphanumeric<L: Span>(source: &str, location: L) -> ParseResult<String, L> {
    char_condition(&|c| c.is_ascii_alphanumeric(), source, location)
}

pub fn whitespace<L: Span>(source: &str, location: L) -> ParseResult<String, L> {
    char_condition(&|c| c.is_whitespace(), source, location)
}

pub fn space<L: Span>(source: &str, location: L) -> ParseResult<String, L> {
    whitespace.multiple().parse(source, location)
}

pub fn exact<L: Span>(must_match: &'static str) -> impl Parser<String, L> {
    closure(move |source, location| {
        if let Some(next) = must_match.chars().next() {
            let remaining = &must_match[next.len_utf8()..];
            character(next)
                .and(exact(remaining))
                .parse(source, location)
        } else {
            empty.parse(source, location)
        }
    })
}

impl<L: Span> Parser<String, L> for &'static str {
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, String, L> {
        exact(self).parse(source, location)
    }
}
