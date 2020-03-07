//! Simple S-expression parser example

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

        let result = sexpression.drop('\n').end().parse(&expr, new_location());
        if result.is_success() {
            for result in result.values() {
                println!("{}", result);
            }
        } else {
            println!("Err: Invalid S-express: {}", expr);
        }
    }

    Ok(())
}

#[derive(Debug, Clone)]
enum SExpression {
    Atom(String),
    Integer(i32),
    Float(f32),
    String(String),
    List(Vec<Meta<SExpression, Span>>),
}

impl fmt::Display for SExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use SExpression::*;
        match self {
            Atom(atom) => write!(f, "{}", atom),
            Integer(integer) => write!(f, "{}", integer),
            Float(float) => write!(f, "{:.1}", float),
            String(string) => write!(f, "{:?}", string),
            List(list) => {
                write!(f, "(")?;
                let mut first = true;
                for item in list.clone() {
                    if !first {
                        write!(f, " ")?;
                    }
                    first = false;
                    write!(f, "[{}]", item)?;
                }
                write!(f, ")")
            }
        }
    }
}

fn expr_space(source: &str, location: Span) -> ParseResult<Span> {
    whitespace.multiple().parse(source, location)
}

fn atom_prefix() -> impl Parser<Span> {
    alphabetic.or("_".or("-"))
}

fn atom_char() -> impl Parser<Span> {
    atom_prefix().or(digit)
}

fn atom(source: &str, location: Span) -> ParseResult<SExpression> {
    atom_prefix()
        .and(atom_char().multiple().maybe())
        .copy_string()
        .map(SExpression::Atom)
        .parse(source, location)
}

fn integer(source: &str, location: Span) -> ParseResult<SExpression> {
    digit
        .multiple()
        .copy_string()
        .map(|i| FromStr::from_str(&i).unwrap())
        .map(SExpression::Integer)
        .parse(source, location)
}

fn float(source: &str, location: Span) -> ParseResult<SExpression> {
    digit
        .multiple()
        .and(".".and(digit.multiple().maybe()))
        .copy_string()
        .map(|i| FromStr::from_str(&i).unwrap())
        .map(SExpression::Float)
        .parse(source, location)
}

fn string(source: &str, location: Span) -> ParseResult<SExpression> {
    "\"".skip(
        any_character
            .str_condition(|c| c.chars().all(|c| c != '\"'))
            .or("\\".and("\""))
            .multiple()
            .maybe()
            .drop("\""),
    )
    .copy_string()
    .map(SExpression::String)
    .parse(source, location)
}

fn list(source: &str, location: Span) -> ParseResult<SExpression> {
    fn expression_list(source: &str, location: Span) -> ParseResult<List<Meta<SExpression, Span>>> {
        sexpression.meta().to_list().parse(source, location)
    }

    "(".and(expr_space.maybe())
        .skip(
            expression_list
                .and(expr_space.skip(expression_list).multiple().maybe())
                .drop(expr_space.maybe())
                .maybe()
                .drop(expr_space.maybe().and(")")),
        )
        .collect()
        .map(SExpression::List)
        .parse(source, location)
}

fn sexpression(source: &str, location: Span) -> ParseResult<SExpression> {
    list.or(atom)
        .or(string)
        .or(float)
        .or(integer)
        .parse(source, location)
}

#[cfg(test)]
mod test {
    use super::*;

    const S_EXPRESSIONS: &'static [&'static str] = &[
        "one",
        "1",
        "1.0",
        "( one)",
        "( one two three)",
        "( one 2 3.0 \"four\")",
        "(one (one two) (two three) \"four five\")",
    ];

    #[test]
    fn s_expressions() {
        println!();
        for expression in S_EXPRESSIONS {
            println!("\n{:#?}", expression);
            let result = parse(sexpression, expression);
            assert!(result.is_success());
            for value in result.values() {
                println!("{}", *value);
            }
        }
    }
}
