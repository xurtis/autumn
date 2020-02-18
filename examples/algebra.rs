//! Simple integer algebraic language

use autumn::parsers::*;
use autumn::*;

use std::fmt;
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
            println!("Err: {}", error);
        }
    }

    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum EvalError {
    DivisionByZero(i32, i32),
    ModuloZero(i32, i32),
}
use EvalError::*;

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DivisionByZero(n, d) => write!(f, "Division by zero ({} / {})", n, d),
            ModuloZero(n, d) => write!(f, "Remainder of division by zero ({} % {})", n, d),
        }
    }
}

fn expression<L: Span>(source: &str, location: L) -> ParseResult<i32, L, EvalError> {
    space
        .maybe()
        .skip(add.drop(space.maybe()))
        .end()
        .parse(source, location)
}

fn add<L: Span>(source: &str, location: L) -> ParseResult<i32, L, EvalError> {
    sub.and_then(|left| {
        operator("+")
            .skip(add.map(move |right| left + right))
            .or(value(left))
    })
    .parse(source, location)
}

fn sub<L: Span>(source: &str, location: L) -> ParseResult<i32, L, EvalError> {
    mul.and_then(|left| {
        operator("-")
            .skip(sub.map(move |right| left - right))
            .or(value(left))
    })
    .parse(source, location)
}

fn mul<L: Span>(source: &str, location: L) -> ParseResult<i32, L, EvalError> {
    div.and_then(|left| {
        operator("*")
            .skip(mul.map(move |right| left * right))
            .or(value(left))
    })
    .parse(source, location)
}

fn div<L: Span>(source: &str, location: L) -> ParseResult<i32, L, EvalError> {
    rem.parse(source, location)
        .and_then(&|left, source, location| {
            operator("/")
                .skip(div.map(move |right| (left, right)))
                .parse(source, location.clone())
                .and_then(&|(left, right), source, location| {
                    if right != 0 {
                        success(left / right, source, location)
                    } else {
                        failure(DivisionByZero(left, right), location)
                    }
                })
                .or(success(left, source, location))
        })
        .expand_error()
}

fn rem<L: Span>(source: &str, location: L) -> ParseResult<i32, L, EvalError> {
    paren
        .parse(source, location)
        .and_then(&|left, source, location| {
            operator("%")
                .skip(rem.map(move |right| (left, right)))
                .parse(source, location.clone())
                .and_then(&|(left, right), source, location| {
                    if right != 0 {
                        success(left % right, source, location)
                    } else {
                        failure(ModuloZero(left, right), location)
                    }
                })
                .or(success(left, source, location))
        })
        .expand_error()
}

fn paren<L: Span>(source: &str, location: L) -> ParseResult<i32, L, EvalError> {
    character('(')
        .and(space.maybe())
        .and_then(|_| add)
        .drop(space.maybe().and(character(')')))
        .or(literal)
        .parse(source, location)
}

fn literal<L: Span>(source: &str, location: L) -> ParseResult<i32, L, EvalError> {
    character('-')
        .maybe()
        .and(digit.multiple())
        .map(|number| number.parse().unwrap())
        .parse(source, location)
}

fn operator<L: Span>(token: &'static str) -> impl Parser<String, L, EvalError> {
    space.maybe().and(exact(token)).and(space.maybe())
}
