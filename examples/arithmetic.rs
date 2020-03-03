//! Simple integer arithmetic language

use autumn::prelude::*;

use std::fmt;
use std::io::{stdin, stdout, BufRead, BufReader, Write};
use std::str::FromStr;

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

        let result = parse(expression, &expr);
        if result.is_success() {
            for result in result.values() {
                println!(" = {}", result);
            }
        } else {
            for error in result.cloned_errors() {
                println!("Err: {}", error);
            }
        }
    }

    Ok(())
}

#[derive(Debug, Clone, PartialEq)]
enum EvalError {
    DivisionByZero(i32, i32),
    ModuloZero(i32, i32),
    InvalidExpression(String),
    InvalidLiteral(String),
}
use EvalError::*;

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DivisionByZero(n, d) => write!(f, "Division by zero ({} / {})", n, d),
            ModuloZero(n, d) => write!(f, "Remainder of division by zero ({} % {})", n, d),
            InvalidExpression(expr) => write!(f, "Invalid expression: {}", expr),
            InvalidLiteral(expr) => write!(f, "Invalid literal: {}", expr),
        }
    }
}

fn expression(source: &str, location: Span) -> ParseResult<i32, EvalError> {
    space
        .maybe()
        .skip(add.drop(space.maybe()))
        .end()
        .on_none(
            any_character
                .multiple()
                .drop(space)
                .end()
                .map(|s| s.to_string())
                .and_then(|text| throw(1, InvalidExpression(text))),
        )
        .catch()
        .parse(source, location)
}

fn add(source: &str, location: Span) -> ParseResult<i32, EvalError> {
    sub.and_then(|left| {
        operator("+")
            .skip(add.map(move |right| left + right))
            .or(value(left))
    })
    .parse(source, location)
}

fn sub(source: &str, location: Span) -> ParseResult<i32, EvalError> {
    mul.and_then(|left| {
        operator("-")
            .skip(sub.map(move |right| left - right))
            .or(value(left))
    })
    .parse(source, location)
}

fn mul(source: &str, location: Span) -> ParseResult<i32, EvalError> {
    div.and_then(|left| {
        operator("*")
            .skip(mul.map(move |right| left * right))
            .or(value(left))
    })
    .parse(source, location)
}

fn div(source: &str, location: Span) -> ParseResult<i32, EvalError> {
    rem.and_then(&|left| {
        operator("/")
            .skip(div.map(move |right| (left, right)))
            .and_then(&|(left, right)| {
                if right != 0 {
                    value(left / right)
                } else {
                    throw(1, DivisionByZero(left, right))
                }
            })
            .or(value(left))
    })
    .catch()
    .parse(source, location)
}

fn rem(source: &str, location: Span) -> ParseResult<i32, EvalError> {
    paren
        .and_then(&|left| {
            operator("%")
                .skip(rem.map(move |right| (left, right)))
                .and_then(&|(left, right)| {
                    if right != 0 {
                        value(left % right)
                    } else {
                        throw(1, ModuloZero(left, right))
                    }
                })
                .or(value(left))
        })
        .catch()
        .parse(source, location)
}

fn paren(source: &str, location: Span) -> ParseResult<i32, EvalError> {
    "(".and(space.maybe())
        .and_then(|_| add)
        .drop(space.maybe().and(")"))
        .or(literal)
        .on_none(
            character
                .condition(|c| !c.is_whitespace())
                .map(List::single)
                .multiple()
                .map(|s| s.to_string())
                .and_then(|text| throw(1, InvalidLiteral(text))),
        )
        .catch()
        .parse(source, location)
}

fn literal(source: &str, location: Span) -> ParseResult<i32, EvalError> {
    "-".maybe()
        .and(digit.multiple())
        .map(|s| s.to_string())
        .map(|number| FromStr::from_str(&number).unwrap())
        .parse(source, location)
}

fn operator(token: &'static str) -> impl Parser<List<char>, EvalError> {
    space.maybe().and(token).and(space.maybe())
}
