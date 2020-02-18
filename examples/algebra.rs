//! Simple integer algebraic language

use autumn::parsers::*;
use autumn::*;

use std::io::{stdin, stdout, BufRead, BufReader, Write};

type Error = Box<dyn ::std::error::Error>;
type Result<T> = ::std::result::Result<T, Error>;

fn main() -> Result<()> {
    let mut stdin = BufReader::new(stdin());

    loop {
        print!(">>> ");
        stdout().flush()?;

        let mut expr = String::new();
        stdin.read_line(&mut expr)?;

        if expr == "quit" {
            break;
        }

        let mut result = expression.parse(&expr, new_location());
        for result in result.values() {
            println!(" = {}", result.inner());
        }
        for error in result.errors() {
            println!("Err: {:?}", error);
        }
    }

    Ok(())
}

fn expression<L: Span>(source: &str, location: L) -> ParseResult<i32, L> {
    space
        .maybe()
        .and_then(|_| add.drop(space.maybe()))
        .end()
        .parse(source, location)
}

fn add<L: Span>(source: &str, location: L) -> ParseResult<i32, L> {
    sub.drop(operator("+"))
        .and_then(|left| add.map(move |right| left + right))
        .or(sub)
        .parse(source, location)
}

fn sub<L: Span>(source: &str, location: L) -> ParseResult<i32, L> {
    mul.drop(operator("-"))
        .and_then(|left| sub.map(move |right| left - right))
        .or(mul)
        .parse(source, location)
}

fn mul<L: Span>(source: &str, location: L) -> ParseResult<i32, L> {
    div.drop(operator("*"))
        .and_then(|left| mul.map(move |right| left * right))
        .or(div)
        .parse(source, location)
}

fn div<L: Span>(source: &str, location: L) -> ParseResult<i32, L> {
    rem.drop(operator("/"))
        .and_then(|left| div.map(move |right| left / right))
        .or(rem)
        .parse(source, location)
}

fn rem<L: Span>(source: &str, location: L) -> ParseResult<i32, L> {
    paren
        .drop(operator("%"))
        .and_then(|left| rem.map(move |right| left % right))
        .or(paren)
        .parse(source, location)
}

fn paren<L: Span>(source: &str, location: L) -> ParseResult<i32, L> {
    character('(')
        .and(space.maybe())
        .and_then(|_| add)
        .drop(space.maybe().and(character(')')))
        .or(literal)
        .parse(source, location)
}

fn literal<L: Span>(source: &str, location: L) -> ParseResult<i32, L> {
    character('-')
        .maybe()
        .and(digit.multiple())
        .map(|number| number.parse().unwrap())
        .parse(source, location)
}

fn operator<L: Span>(token: &'static str) -> impl Parser<String, L> {
    space.maybe().and(exact(token)).and(space.maybe())
}
