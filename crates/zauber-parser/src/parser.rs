// Spec of the syntax is here? https://pubs.opengroup.org/onlinepubs/9699919799/utilities/file.html
//
// FIXME: make error messages include context: https://github.com/fflorent/nom_locate/issues/35#issuecomment-912223059
//

#![allow(unused_imports)]  // for development

use nom::{
    branch::alt,
    bytes::complete::{escaped_transform, is_not, tag, take_till},
    character::complete::{
        alpha1, char, digit1, hex_digit1, line_ending, multispace0, multispace1, not_line_ending,
        oct_digit1, one_of, space0, space1,
    },
    combinator::{all_consuming, cut, fail, map, map_res, not, opt, rest, value},
    error::context,
    multi::{many0, many0_count, many1, many1_count},
    sequence::{delimited, preceded, terminated, tuple},
    Finish, Parser,
};
use std::path::Path;
use std::str::FromStr;

use zauber_ast as ast;

use crate::err;

pub type IResult<'a, O> = nom::IResult<&'a str, O, nom::error::Error<&'a str>>;

pub fn parse_magic_file<P>(fname: P) -> Result<Vec<ast::MagicPattern>, err::Error>
where
    P: AsRef<Path>,
{
    parse_magic_file_path(fname.as_ref())
}

fn parse_magic_file_path(fname: &Path) -> Result<Vec<ast::MagicPattern>, err::Error> {
    let contents = std::fs::read_to_string(fname)?;
    parse_magic_file_contents(&contents, Some(fname))
}

fn run_nom_parser<'a, P, O>(
    mut p: P,
    fname: Option<&Path>,
    line_num: usize,
    line: &'a str,
) -> Result<O, err::Error>
where
    P: nom::Parser<&'a str, O, nom::error::Error<&'a str>>,
{
    let make_location = || err::Location {
        fname: fname.map(|p| p.to_owned()),
        line_num,
    };

    match p.parse(line) {
        Ok(("", o)) => Ok(o),
        Ok((_i, _o)) => Err(err::Error::TrailingInput {
            location: make_location(),
        }),
        Err(e) => {
            let location = make_location();
            let line = line.to_owned();
            let message = format!("{e}");
            return Err(err::Error::ParseError {
                location,
                message,
                line,
            });
        }
    }
}

pub fn parse_magic_file_contents(contents: &str, fname: Option<&Path>) -> Result<Vec<ast::MagicPattern>, err::Error> {

    let mut patterns: Vec<ast::MagicPattern> = Vec::with_capacity(128);

    for (line_num, line) in (1usize..).zip(contents.lines()) {
        let line = line.trim();

        if line.is_empty() {
            // skip blank lines
        } else if line.starts_with('#') {
            // skip comment lines
        } else if line.starts_with("!:") {
            // directive
            patterns.push(run_nom_parser(directive, fname, line_num, line)?);
        } else {
            // magic pattern
            patterns.push(run_nom_parser(pattern, fname, line_num, line)?);
        }
    }

    Ok(patterns)
}

fn tabs1(i: &str) -> IResult<()> {
    let (i, _) = many1_count(char('\t'))(i)?;
    Ok((i, ()))
}

fn pattern(i: &str) -> IResult<ast::MagicPattern> {
    let (i, level) = context("level", level)(i)?;
    let (i, offset) = context("offset", offset)(i)?;
    let (i, _) = cut(tabs1)(i)?;
    let (i, test) = context("test", cut(test))(i)?;
    let (i, _) = cut(tabs1)(i)?;
    let (i, message) = context("message", cut(message))(i)?;

    let pat = ast::MagicPattern::Pattern(ast::Pattern {
        level,
        offset,
        test,
        message,
    });
    Ok((i, pat))
}

fn level(i: &str) -> IResult<u64> {
    let (i, c) = many0_count(char('>'))(i)?;
    Ok((i, c as u64))
}

fn offset(i: &str) -> IResult<ast::Offset> {
    alt((direct, relative, indirect))(i)
}

fn direct(i: &str) -> IResult<ast::Offset> {
    map(integral::<i64>(), ast::Offset::Direct)(i)
}

fn relative(i: &str) -> IResult<ast::Offset> {
    let (i, o) = preceded(char('&'), cut(offset))(i)?;
    let o = ast::Offset::Relative(Box::new(o));
    Ok((i, o))
}

fn indirect(i: &str) -> IResult<ast::Offset> {
    let (_i, _) = char('(')(i)?;
    panic!("FIXME: unimplemented");
    // let (i, _) = char(')')(i)?;

    // let o = ast::Offset::Indirect {
    //     base,
    //     signed,
    //     specifier,
    //     rhs
    // };
    // Ok((i, o))
}

fn test(i: &str) -> IResult<ast::Test> {
    alt((
        integral_test::<u8>("byte").map(ast::Test::Byte),
        integral_test::<u16>("short").map(ast::Test::Short),
        integral_test::<u32>("long").map(ast::Test::Long),
        search_test,
        string_test,
        name_test,
        use_test,
    ))(i)
}

fn integral_test<'a, I: Integral>(name: &'a str) -> impl FnMut(&'a str) -> IResult<ast::Expr<I>> {
    move |i: &'a str| {
        use ast::Expr;

        let (i, _) = tag(name)(i)?;
        let (i, _) = cut(tabs1)(i)?;
        let (i, op) = opt(one_of("=<>&^~!"))(i)?;
        let (i, n) = integral::<I>()(i)?;
        let expr = match op {
            Some('=') => Expr::Eq(n),
            Some('<') => Expr::Lt(n),
            Some('>') => Expr::Gt(n),
            Some('&') => Expr::BitAnd(n),
            Some('^') => Expr::BitXor(n),
            Some('~') => Expr::BitNot(n),
            Some('!') => Expr::Neq(n),
            _ => Expr::Eq(n), // equality test is the default if not explicitly given
        };
        Ok((i, expr))
    }
}

fn name_test(i: &str) -> IResult<ast::Test> {
    let (i, _) = tag("name")(i)?;
    let (i, _) = cut(tabs1)(i)?;
    let (i, s) = not_line_ending(i)?;
    Ok((i, ast::Test::Name(s.to_string())))
}

fn use_test(i: &str) -> IResult<ast::Test> {
    let (i, _) = tag("use")(i)?;
    let (i, _) = cut(tabs1)(i)?;
    let (i, s) = not_line_ending(i)?;
    Ok((i, ast::Test::Use(s.to_string())))
}

fn search_test(i: &str) -> IResult<ast::Test> {
    let (i, _) = tag("search")(i)?;
    let (i, range) = cut(opt(preceded(char('/'), integral::<usize>())))(i)?;
    let (i, flags) = opt(preceded(char('/'), search_flags))(i)?;
    let (i, _) = cut(tabs1)(i)?;
    let (i, s) = string(i)?;
    Ok((
        i,
        ast::Test::Search {
            string: s,
            range,
            flags: flags.unwrap_or(ast::SearchFlags::default()),
        },
    ))
}

fn search_flags(i: &str) -> IResult<ast::SearchFlags> {
    let (i, flags) = many0(alt((
        char('b').map(|_| ast::SearchFlags::ForceBinary),
        char('C').map(|_| ast::SearchFlags::UpperCaseInsensitive),
        char('c').map(|_| ast::SearchFlags::LowerCaseInsensitive),
        char('f').map(|_| ast::SearchFlags::MatchFullWord),
        char('T').map(|_| ast::SearchFlags::Trim),
        char('t').map(|_| ast::SearchFlags::ForceText),
        char('W').map(|_| ast::SearchFlags::CompactWhitespace),
        char('w').map(|_| ast::SearchFlags::OptionalBlanks),
    )))(i)?;
    let flags = ast::SearchFlags::from_iter(flags);
    Ok((i, flags))
}

fn string_test(i: &str) -> IResult<ast::Test> {
    let (i, _) = tag("string")(i)?;
    let (i, width) = cut(opt(preceded(char('/'), integral::<usize>())))(i)?;
    let (i, flags) = opt(preceded(char('/'), search_flags))(i)?;
    let (i, _) = tabs1(i)?;
    let (i, s) = string(i)?;
    Ok((
        i,
        ast::Test::String {
            string: s,
            width,
            flags: flags.unwrap_or(ast::SearchFlags::default()),
        },
    ))
}

// FIXME: support C style escapes: https://en.wikipedia.org/wiki/Escape_sequences_in_C
fn string(i: &str) -> IResult<String> {
    let (i, s) = escaped_transform(
        is_not("\\ \t"),
        '\\',
        alt((
            value(" ", tag(" ")),
            value("\\", tag("\\")),
            value("\t", tag("t")),
            value("\n", tag("n")),
            value("\r", tag("r")),
            map(digit1, |ds: &str| ds), // FIXME: convert from octal
        )),
    )(i)?;
    Ok((i, s.to_string()))
}

fn message(i: &str) -> IResult<String> {
    map(rest, |r: &str| r.to_string())(i)
}

fn directive(i: &str) -> IResult<ast::MagicPattern> {
    map(
        alt((mime, strength, ext, apple)),
        ast::MagicPattern::Directive,
    )(i)
}

fn mime(i: &str) -> IResult<ast::Directive> {
    let (i, _) = directive_keyword("!:mime")(i)?;
    let (i, m) = map_res(cut(rest), |m: &str| ast::Mime::from_str(m))(i)?;
    Ok((i, ast::Directive::Mime(m)))
}

fn strength(i: &str) -> IResult<ast::Directive> {
    let (i, _) = directive_keyword("!:strength")(i)?;
    let (i, _) = cut(tabs1)(i)?;
    let (i, operator) = strength_op(i)?;
    let (i, _) = space0(i)?;
    let (i, value) = integral::<u8>()(i)?;
    Ok((i, ast::Directive::Strength { operator, value }))
}

fn strength_op(i: &str) -> IResult<ast::StrengthOp> {
    alt((
        char('+').map(|_| ast::StrengthOp::Add),
        char('-').map(|_| ast::StrengthOp::Sub),
        char('*').map(|_| ast::StrengthOp::Mul),
        char('/').map(|_| ast::StrengthOp::Div),
    ))(i)
}

fn ext(i: &str) -> IResult<ast::Directive> {
    let (i, _) = directive_keyword("!:ext")(i)?;
    let (i, e) = cut(rest)(i)?;
    Ok((i, ast::Directive::Ext(e.to_string())))
}

fn apple(i: &str) -> IResult<ast::Directive> {
    let (i, _) = directive_keyword("!:apple")(i)?;
    let (i, e) = cut(rest)(i)?;
    Ok((i, ast::Directive::Apple(e.to_string())))
}

fn directive_keyword<'a>(keyword: &'a str) -> impl FnMut(&'a str) -> IResult<()> {
    move |i: &str| {
        let (i, _) = tag(keyword)(i)?;
        let (i, _) = cut(tabs1)(i)?;
        Ok((i, ()))
    }
}

trait Integral
where
    Self: Sized,
{
    type Err;

    fn from_decimal(input: &str) -> Result<Self, Self::Err>;
    fn from_hexadecimal(input: &str) -> Result<Self, Self::Err>;
    fn from_octal(input: &str) -> Result<Self, Self::Err>;
}

macro_rules! make_integral_impl {
    ($ty:ty) => {
        impl Integral for $ty {
            type Err = std::num::ParseIntError;

            fn from_decimal(input: &str) -> Result<Self, Self::Err> {
                Self::from_str_radix(input, 10)
            }

            fn from_hexadecimal(input: &str) -> Result<Self, Self::Err> {
                Self::from_str_radix(input, 16)
            }

            fn from_octal(input: &str) -> Result<Self, Self::Err> {
                Self::from_str_radix(input, 8)
            }
        }
    };
}

make_integral_impl!(u8);
make_integral_impl!(u16);
make_integral_impl!(u32);
make_integral_impl!(u64);

make_integral_impl!(i8);
make_integral_impl!(i16);
make_integral_impl!(i32);
make_integral_impl!(i64);

make_integral_impl!(usize);

fn integral<'a, I: Integral>() -> impl FnMut(&'a str) -> IResult<'a, I> {
    move |i| {
        alt((
            map_res(preceded(tag("0x"), cut(hex_digit1)), |ds: &str| {
                I::from_hexadecimal(ds)
            }),
            map_res(digit1, |ds: &str| {
                if ds.len() > 1 && ds.starts_with("0") {
                    I::from_octal(ds)
                } else {
                    I::from_decimal(ds)
                }
            }),
        ))(i)
    }
}
