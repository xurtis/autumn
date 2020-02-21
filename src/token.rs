use crate::prelude::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Identifier(String),
    NumberLiteral(Number),
    StringLiteral(String),
}
use Token::*;

pub fn token<L: Span, E>(source: &str, location: L) -> ParseResult<Token, L, E> {
    identifier
        .map(Identifier)
        .or(number.map(NumberLiteral))
        .or(string_literal.map(StringLiteral))
        .parse(source, location)
}

pub fn identifier<L: Span, E>(source: &str, location: L) -> ParseResult<String, L, E> {
    fn ident_prefix<L: Span, E>(source: &str, location: L) -> ParseResult<List<char>, L, E> {
        alphabetic.or('_').or('-').parse(source, location)
    }
    fn ident_suffix<L: Span, E>(source: &str, location: L) -> ParseResult<List<char>, L, E> {
        ident_prefix.or(digit).parse(source, location)
    }

    ident_prefix
        .and(ident_suffix.multiple().maybe())
        .map(|s| s.to_string())
        .parse(source, location)
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Number {
    Integer8(i8),
    Integer16(i16),
    Integer32(i32),
    Integer64(i64),
    IntegerSize(i64),
    Unsigned8(u8),
    Unsigned16(u16),
    Unsigned32(u32),
    Unsigned64(u64),
    UnsignedSize(u64),
    Real32(f32),
    Real64(f64),
}

pub fn number<L: Span, E>(source: &str, location: L) -> ParseResult<Number, L, E> {
    sign.and_then(|sign| base.map(move |base| (sign, base)))
        .and_then(number_prefix)
        .and_then(number_suffix)
        .parse(source, location)
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum SignedNumber {
    Signed(i64),
    Unsigned(u64),
    Real(f64, bool),
}

fn number_prefix<L: Span, E>((sign, base): (Sign, Base)) -> impl Parser<SignedNumber, L, E> {
    use Digits::*;
    use SignedNumber::*;
    closure(move |source, location| {
        base.and_then(|number| match (sign, number) {
            (Sign::Unsigned, Integer(number)) => value(Unsigned(number)),
            (Sign::Unsigned, Digits::Real(number, divisor)) => {
                value(SignedNumber::Real(number, divisor < 1f64))
            }
            (Sign::Positive, Integer(number)) => value(Signed(number as i64)),
            (Sign::Positive, Digits::Real(number, divisor)) => {
                value(SignedNumber::Real(number, divisor < 1f64))
            }
            (Sign::Negative, Integer(number)) => value(Signed(-(number as i64))),
            (Sign::Negative, Digits::Real(number, divisor)) => {
                value(SignedNumber::Real(-number, divisor < 1f64))
            }
        })
        .parse(source, location)
    })
}

fn number_suffix<'s, L: Span + 's, E: 's>(number: SignedNumber) -> impl Parser<Number, L, E> + 's {
    use Number::*;
    use SignedNumber::*;

    let signed = exact("i")
        .or("i8")
        .or("i16")
        .or("i32")
        .or("i64")
        .or("isize");
    let unsigned = exact("u")
        .or("u8")
        .or("u16")
        .or("u32")
        .or("u64")
        .or("usize");
    let real = exact("r").or("r32").or("r64");

    signed
        .or(unsigned)
        .or(real)
        .maybe()
        .map(|s| s.to_string())
        .and_then(move |suffix| {
            match (number, suffix.as_str()) {
                // Automatic types
                (Unsigned(number), "") => value(Unsigned64(number)).boxed(),
                (Signed(number), "") => value(Integer64(number)).boxed(),
                (Real(number, true), "") => value(Real64(number)).boxed(),

                // Unsigned
                (Unsigned(number), "u") => value(Unsigned64(number)).boxed(),
                (Unsigned(number), "u8") => value(Unsigned8(number as u8)).boxed(),
                (Unsigned(number), "u16") => value(Unsigned16(number as u16)).boxed(),
                (Unsigned(number), "u32") => value(Unsigned32(number as u32)).boxed(),
                (Unsigned(number), "u64") => value(Unsigned64(number)).boxed(),
                (Unsigned(number), "usize") => value(UnsignedSize(number)).boxed(),

                // Signed
                (Unsigned(number), "i") => value(Integer64(number as i64)).boxed(),
                (Unsigned(number), "i8") => value(Integer8(number as i8)).boxed(),
                (Unsigned(number), "i16") => value(Integer16(number as i16)).boxed(),
                (Unsigned(number), "i32") => value(Integer32(number as i32)).boxed(),
                (Unsigned(number), "i64") => value(Integer64(number as i64)).boxed(),
                (Unsigned(number), "isize") => value(IntegerSize(number as i64)).boxed(),
                (Signed(number), "i") => value(Integer64(number)).boxed(),
                (Signed(number), "i8") => value(Integer8(number as i8)).boxed(),
                (Signed(number), "i16") => value(Integer16(number as i16)).boxed(),
                (Signed(number), "i32") => value(Integer32(number as i32)).boxed(),
                (Signed(number), "i64") => value(Integer64(number)).boxed(),
                (Signed(number), "isize") => value(IntegerSize(number)).boxed(),

                // Floating-point
                (Real(number, _), "r") => value(Real64(number)).boxed(),
                (Real(number, _), "r32") => value(Real32(number as f32)).boxed(),
                (Real(number, _), "r64") => value(Real64(number)).boxed(),
                _ => none.boxed(),
            }
        })
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Sign {
    Unsigned,
    Negative,
    Positive,
}

fn sign<L: Span, E>(source: &str, location: L) -> ParseResult<Sign, L, E> {
    use Sign::*;
    character('-')
        .or('+')
        .maybe()
        .map(|s| match s.to_string().as_str() {
            "-" => Negative,
            "+" => Positive,
            _ => Unsigned,
        })
        .parse(source, location)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Base {
    Binary,
    Octal,
    Decimal,
    Hexadecimal,
}

fn base<L: Span, E>(source: &str, location: L) -> ParseResult<Base, L, E> {
    use Base::*;
    character('0')
        .and(
            character('b')
                .or('B')
                .or('o')
                .or('O')
                .or('d')
                .or('D')
                .or('x')
                .or('X'),
        )
        .maybe()
        .map(|s| match s.to_string().as_str() {
            "0d" | "0D" => Decimal,
            "0b" | "0B" => Binary,
            "0o" | "0O" => Octal,
            "0x" | "0X" => Hexadecimal,
            _ => Decimal,
        })
        .parse(source, location)
}

impl<L: Span, E> Parser<Digits, L, E> for Base {
    fn parse<'s>(&self, source: &'s str, location: L) -> ParseResult<'s, Digits, L, E> {
        fn digit<'l, L: Span + 'l, E: 'l>(
            (base, digits): (Base, Digits),
        ) -> impl Parser<(Base, Digits), L, E> + 'l {
            use Digits::*;
            base.digit()
                .map(move |digit| {
                    let digits = match digits {
                        Integer(value) => Integer((value.saturating_mul(base.value())) + digit),
                        // Integer component of float
                        Real(value, divisor) if divisor > 1f64 => {
                            let digit = digit as f64;
                            let base = base.value() as f64;
                            Real((value * base) + digit, divisor)
                        }
                        // Fractional component of float
                        Real(value, divisor) => {
                            let digit = digit as f64;
                            let base = base.value() as f64;
                            Real(value + (digit * divisor), divisor / base)
                        }
                    };
                    (base, digits)
                })
                .or(character('.').and_then(move |_| {
                    let digits = match digits {
                        Integer(_) => none.boxed(),
                        Real(digits, _) => {
                            let base = base.value() as f64;
                            value(Real(digits, 1f64 / base)).boxed()
                        }
                    };
                    digits.map(move |digits| (base, digits))
                }))
        };

        fn rec<L: Span, E>(
            (base, digits): (Base, Digits),
            source: &str,
            location: L,
        ) -> ParseResult<(Base, Digits), L, E> {
            let continued = location.clone();
            let complete = ParseResult::success((base, digits), source, location);
            digit((base, digits))
                .or(character('_').map(|_| (base, digits)))
                .parse(source, continued)
                .and_then(&rec)
                .or(complete)
        };

        Digits::either
            .parse(source, location)
            .and_then(&|digits, source, location| digit((*self, digits)).parse(source, location))
            .and_then(&rec)
            .map(&|(_, digits)| digits)
    }
}

impl Base {
    fn value(self) -> u64 {
        use Base::*;
        match self {
            Binary => 2,
            Octal => 8,
            Decimal => 10,
            Hexadecimal => 16,
        }
    }

    fn digit<L: Span, E>(self) -> impl Parser<u64, L, E> {
        use Base::*;
        match self {
            Binary => Self::binary_digit,
            Octal => Self::octal_digit,
            Decimal => Self::decimal_digit,
            Hexadecimal => Self::hexadecimal_digit,
        }
    }

    fn binary_digit<L: Span, E>(source: &str, location: L) -> ParseResult<u64, L, E> {
        single_character
            .condition(|c| '0' <= *c && *c <= '1')
            .map(|c| (c as u8) - b'0')
            .map(|c| c as u64)
            .parse(source, location)
    }

    fn octal_digit<L: Span, E>(source: &str, location: L) -> ParseResult<u64, L, E> {
        single_character
            .condition(|c| '0' <= *c && *c <= '7')
            .map(|c| (c as u8) - b'0')
            .map(|c| c as u64)
            .parse(source, location)
    }

    fn decimal_digit<L: Span, E>(source: &str, location: L) -> ParseResult<u64, L, E> {
        single_character
            .condition(|c| '0' <= *c && *c <= '9')
            .map(|c| (c as u8) - b'0')
            .map(|c| c as u64)
            .parse(source, location)
    }

    fn hexadecimal_digit<L: Span, E>(source: &str, location: L) -> ParseResult<u64, L, E> {
        single_character
            .condition(|c| Self::is_hexadecimal(*c))
            .map(Self::as_hexadecimal)
            .map(|c| c as u64)
            .parse(source, location)
    }

    fn is_hexadecimal(c: char) -> bool {
        ('0' <= c && c <= '9') || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F')
    }

    fn as_hexadecimal(c: char) -> u8 {
        if 'a' <= c && c <= 'f' {
            (c as u8) - b'a'
        } else if 'A' <= c && c <= 'F' {
            (c as u8) - b'A'
        } else {
            (c as u8) - b'0'
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Digits {
    Integer(u64),
    Real(f64, f64),
}

impl Digits {
    fn either<L: Span, E>(source: &str, location: L) -> ParseResult<Self, L, E> {
        ParseResult::success(Digits::Integer(0), source, location.clone()).or(ParseResult::success(
            Digits::Real(0f64, 2f64),
            source,
            location,
        ))
    }
}

pub fn string_literal<L: Span, E>(source: &str, location: L) -> ParseResult<String, L, E> {
    character('"')
        .skip(
            any_character
                .condition(|c| !c.clone().any(|c| *c == '"'))
                .or(character('\\').and('"'))
                .multiple()
                .maybe(),
        )
        .drop('"')
        .map(|s| s.to_string())
        .parse(source, location)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sequence<L: Span, E>(source: &str, location: L) -> ParseResult<List<Token>, L, E> {
        fn token_list<L: Span, E>(source: &str, location: L) -> ParseResult<List<Token>, L, E> {
            token.drop(space).parse(source, location).map(&List::single)
        }

        space
            .maybe()
            .and_then(|_| token_list.multiple().maybe().and(token.map(&List::single)))
            .drop(space.maybe())
            .end()
            .parse(source, location)
    }

    const IDENTIFIERS: &'static [&'static str] = &["One", "Two", "_Three", "four-five"];

    #[test]
    fn identifiers() {
        for identifier_token in IDENTIFIERS {
            println!("\n{:?}", identifier_token);
            let result: ParseResult<_, _> =
                identifier.end().parse(identifier_token, new_location());
            assert!(result.is_success());
            assert!(result.single_parse());
            for value in result.values() {
                println!("{:?}", value.inner_ref());
            }
        }
    }

    const SEQUENCES: &'static [&'static str] = &[
        "One Two _Three four-five",
        " One Two _Three four-five ",
        "1234",
        "1234i32",
        "123i8",
        "1234i16",
        "1234i64",
        "1234isize",
        "1234u 1234u32 123u8 1234u16 1234u64 1234usize",
        "1234. 1234.0 123r32 1234.r32 1234.0r64 1234.r64 1234r",
        "\"Hello my old friend \\\"James\\\", if you may allow me to address you so\"",
        "0b1010_1010.0101_0101 0b10_1010.0001r",
        "0o70_70.07_07",
        "0xf0_._0f",
        "0.125",
        "\"This number will be very large:\" 4200000000000000000000000r",
    ];

    #[test]
    fn sequences() {
        for token_sequence in SEQUENCES {
            println!("\n{:?}", token_sequence);
            let result: ParseResult<_, _> = sequence(token_sequence, new_location());
            assert!(result.is_success());
            assert!(result.single_parse());
            for value in result.values() {
                println!("{:?}", value.inner_ref());
            }
        }
    }
}
