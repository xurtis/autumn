//! Simple S-expression parser example

use autumn::prelude::*;

use std::io::{stdin, stdout, BufRead, BufReader, Write};
use std::fmt;

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
                println!("{}", result.inner_ref());
            }
        } else {
            println!("Err: Invalid S-express: {}", expr);
        }
    }

    Ok(())
}

#[derive(Debug, Clone)]
enum SExpression<L> {
    Atom(String),
    Integer(i32),
    Float(f32),
    String(String),
    List(List<Meta<SExpression<L>, L>>),
}

impl<L: fmt::Display> fmt::Display for SExpression<L> {
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


fn expr_space<L: Span>(source: &str, location: L) -> ParseResult<List<char>, L> {
    whitespace.multiple().parse(source, location)
}

fn atom_prefix<L: Span>() -> impl Parser<List<char>, L> {
    alphabetic.or(character('_').or(character('-')))
}

fn atom_char<L: Span>() -> impl Parser<List<char>, L> {
    atom_prefix().or(digit)
}

fn atom<L: Span>(source: &str, location: L) -> ParseResult<SExpression<L>, L> {
    atom_prefix()
        .and(atom_char().multiple().maybe())
        .map(|s| s.to_string())
        .map(SExpression::Atom)
        .parse(source, location)
}

fn integer<L: Span>(source: &str, location: L) -> ParseResult<SExpression<L>, L> {
    digit
        .multiple()
        .map(|i| i.to_string().parse().unwrap())
        .map(SExpression::Integer)
        .parse(source, location)
}

fn float<L: Span>(source: &str, location: L) -> ParseResult<SExpression<L>, L> {
    digit
        .multiple()
        .and(character('.').and(digit.multiple().maybe()))
        .map(|i| i.to_string().parse().unwrap())
        .map(SExpression::Float)
        .parse(source, location)
}

fn string<L: Span>(source: &str, location: L) -> ParseResult<SExpression<L>, L> {
    character('"')
        .skip(
            any_character
                .condition(|c| c.clone().all(|c| *c != '\"'))
                .or(character('\\').and(character('"')))
                .multiple()
                .maybe()
                .drop(character('"')),
        )
        .map(|s| s.to_string())
        .map(SExpression::String)
        .parse(source, location)
}

fn list<L: Span>(source: &str, location: L) -> ParseResult<SExpression<L>, L> {
    fn expression_list<L: Span>(
        source: &str,
        location: L,
    ) -> ParseResult<List<Meta<SExpression<L>, L>>, L> {
        sexpression.meta().map(List::single).parse(source, location)
    }

    character('(')
        .and(expr_space.maybe())
        .skip(
            expression_list
                .and(expr_space.skip(expression_list).multiple().maybe())
                .drop(expr_space.maybe())
                .maybe()
                .drop(expr_space.maybe().and(character(')'))),
        )
        .map(|l| l.reverse())
        .map(SExpression::List)
        .parse(source, location)
}

fn sexpression<L: Span>(source: &str, location: L) -> ParseResult<SExpression<L>, L> {
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
            let result = sexpression.end().parse(expression, new_location());
            assert!(result.is_success());
            for value in result.values() {
                println!("{}", *value);
            }
        }
    }
}
