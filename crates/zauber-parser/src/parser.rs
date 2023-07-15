// Spec of the syntax is here? https://pubs.opengroup.org/onlinepubs/9699919799/utilities/file.html
//
// FIXME: make error messages include context: https://github.com/fflorent/nom_locate/issues/35#issuecomment-912223059
//

use mime::Mime;
use nom::{
    branch::alt,
    bytes::complete::{escaped_transform, is_not, tag, take_till},
    character::complete::{
        alpha1, char, digit1, hex_digit1, line_ending, multispace0, multispace1, not_line_ending,
        oct_digit1, one_of, space0, space1,
    },
    combinator::{all_consuming, cut, fail, map, map_res, not, opt, value},
    error::context,
    multi::{many0, many0_count, many1},
    sequence::{delimited, preceded, terminated, tuple},
    Finish, IResult, Parser,
};
use nom_locate::LocatedSpan;
use std::path::Path;
use std::str::FromStr;

use crate::{ast, err};

type Span<'a> = LocatedSpan<&'a str>;
type PResult<'a, T> = IResult<Span<'a>, T>;

pub fn parse_magic_file<P>(fname: P) -> Result<Vec<ast::MagicPattern>, err::Error>
where
    P: AsRef<Path>,
{
    parse_magic_file_path(fname.as_ref())
}

fn parse_magic_file_path(fname: &Path) -> Result<Vec<ast::MagicPattern>, err::Error> {
    let contents = std::fs::read_to_string(fname)?;
    let span = Span::new(&contents);
    match magic_file.parse(span).finish() {
        Ok((input, pats)) => match *input.fragment() {
            "" => Ok(pats),
            _ => Err(err::Error::TrailingInput {
                location: err::Location {
                    fname: fname.to_owned(),
                    line_num: input.location_line() as usize,
                    column_num: input.get_utf8_column(),
                },
            }),
        },
        Err(err) => {
            panic!(
                "*** error at {:?}:{}:{}\n{}",
                fname,
                err.input.location_line(),
                err.input.get_utf8_column(),
                err
            );
        }
    }
}

fn magic_file(i: Span) -> PResult<Vec<ast::MagicPattern>> {
    let (i, ps) = many0(context("magic pattern", magic_pattern))(i)?;
    let (i, _) = comments_or_whitespace(i)?;
    Ok((i, ps))
}

fn comments_or_whitespace(i: Span) -> PResult<()> {
    let (i, _c) = many0_count(alt((terminated(whitespace0, line_ending), comment)))(i)?;
    Ok((i, ()))
}

fn whitespace0(i: Span) -> PResult<()> {
    let (i, _) = space0(i)?;
    Ok((i, ()))
}

fn whitespace1(i: Span) -> PResult<()> {
    let (i, _) = space1(i)?;
    Ok((i, ()))
}

fn comment(i: Span) -> PResult<()> {
    let (i, _) = char('#')(i)?;
    let (i, _) = cut(take_through_eol)(i)?;
    Ok((i, ()))
}

fn magic_pattern(i: Span) -> PResult<ast::MagicPattern> {
    let (i, ()) = comments_or_whitespace(i)?;
    alt((
        context("pattern", pattern).map(ast::MagicPattern::Pattern),
        context("directive", directive).map(ast::MagicPattern::Directive),
    ))(i)
}

fn pattern(i: Span) -> PResult<ast::Pattern> {
    let (i, level) = context("level", level)(i)?;
    let (i, offset) = context("offset", offset)(i)?;
    let (i, _) = cut(whitespace1)(i)?;
    let (i, test) = context("test", cut(test))(i)?;
    let (i, _) = cut(whitespace1)(i)?;
    let (i, message) = context("message", cut(message))(i)?;

    let pat = ast::Pattern {
        level,
        offset,
        test,
        message,
    };
    Ok((i, pat))
}

fn level(i: Span) -> PResult<u64> {
    let (i, c) = many0_count(char('>'))(i)?;
    Ok((i, c as u64))
}

fn offset(i: Span) -> PResult<ast::Offset> {
    alt((direct, relative, indirect))(i)
}

fn direct(i: Span) -> PResult<ast::Offset> {
    map(integral::<i64>(), ast::Offset::Direct)(i)
}

fn relative(i: Span) -> PResult<ast::Offset> {
    let (i, o) = preceded(char('&'), cut(offset))(i)?;
    let o = ast::Offset::Relative(Box::new(o));
    Ok((i, o))
}

fn indirect(i: Span) -> PResult<ast::Offset> {
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

fn test(i: Span) -> PResult<ast::Test> {
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

fn integral_test<'a, I: Integral>(name: &'a str) -> impl FnMut(Span) -> PResult<ast::Expr<I>> + 'a {
    move |i: Span| {
        use ast::Expr;

        let (i, _) = tag(name)(i)?;
        let (i, _) = cut(whitespace1)(i)?;
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

fn name_test(i: Span) -> PResult<ast::Test> {
    let (i, _) = tag("name")(i)?;
    let (i, _) = cut(whitespace1)(i)?;
    let (i, s) = not_line_ending(i)?;
    Ok((i, ast::Test::Name(s.to_string())))
}

fn use_test(i: Span) -> PResult<ast::Test> {
    let (i, _) = tag("use")(i)?;
    let (i, _) = cut(whitespace1)(i)?;
    let (i, s) = not_line_ending(i)?;
    Ok((i, ast::Test::Use(s.to_string())))
}

fn search_test(i: Span) -> PResult<ast::Test> {
    let (i, _) = tag("search")(i)?;
    let (i, range) = cut(opt(preceded(char('/'), integral::<usize>())))(i)?;
    let (i, flags) = opt(preceded(char('/'), search_flags))(i)?;
    let (i, _) = cut(whitespace1)(i)?;
    let (i, s) = string(i)?;
    println!("*** s = {s:?}");
    Ok((
        i,
        ast::Test::Search {
            string: s,
            range,
            flags: flags.unwrap_or(ast::SearchFlags::default()),
        },
    ))
}

fn search_flags(i: Span) -> PResult<ast::SearchFlags> {
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

fn string_test(i: Span) -> PResult<ast::Test> {
    let (i, _) = tag("string")(i)?;
    let (i, width) = cut(opt(preceded(char('/'), integral::<usize>())))(i)?;
    let (i, flags) = opt(preceded(char('/'), search_flags))(i)?;
    let (i, _) = whitespace1(i)?;
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
fn string(i: Span) -> PResult<String> {
    // let (i, s) = escaped(one_of("#!abcdefghijklmnopqrstuvwxyz/1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ"), '\\', one_of(" \t\\0x"))(i)?;
    let (i, s) = escaped_transform(is_not("\\ \t"), '\\', alt((
        value(" ", tag(" ")),
        value("\\", tag("\\")),
        value("\t", tag("t")),
        value("\n", tag("n")),
        value("\r", tag("r")),
        map(digit1, |ds: Span| *ds.fragment()),  // FIXME: convert from octal
    )))(i)?;
    // one_of(" \t\n\rtnr\\0x"))(i)?;
    Ok((i, s.to_string()))
}

fn message(i: Span) -> PResult<String> {
    map(take_through_eol, |r| r.fragment().to_string())(i)
}

fn directive(i: Span) -> PResult<ast::Directive> {
    alt((mime, strength, ext, apple))(i)
}

fn mime(i: Span) -> PResult<ast::Directive> {
    let (i, _) = directive_keyword("!:mime")(i)?;
    let (i, m) = map_res(cut(take_through_eol), |m: Span| {
        Mime::from_str(m.fragment())
    })(i)?;
    Ok((i, ast::Directive::Mime(m)))
}

fn strength(i: Span) -> PResult<ast::Directive> {
    let (i, _) = directive_keyword("!:strength")(i)?;
    let (i, _) = cut(whitespace1)(i)?;
    let (i, operator) = strength_op(i)?;
    let (i, _) = whitespace0(i)?;
    let (i, value) = integral::<u8>()(i)?;
    Ok((i, ast::Directive::Strength { operator, value }))
}

fn strength_op(i: Span) -> PResult<ast::StrengthOp> {
    alt((
        char('+').map(|_| ast::StrengthOp::Add),
        char('-').map(|_| ast::StrengthOp::Sub),
        char('*').map(|_| ast::StrengthOp::Mul),
        char('/').map(|_| ast::StrengthOp::Div),
    ))(i)
}

fn ext(i: Span) -> PResult<ast::Directive> {
    let (i, _) = directive_keyword("!:ext")(i)?;
    let (i, e) = cut(take_through_eol)(i)?;
    Ok((i, ast::Directive::Ext(e.fragment().to_string())))
}

fn apple(i: Span) -> PResult<ast::Directive> {
    let (i, _) = directive_keyword("!:apple")(i)?;
    let (i, e) = cut(take_through_eol)(i)?;
    Ok((i, ast::Directive::Apple(e.fragment().to_string())))
}

fn directive_keyword<'a>(keyword: &'a str) -> impl FnMut(Span) -> PResult<()> + 'a {
    move |i: Span| {
        let (i, _) = tag(keyword)(i)?;
        let (i, _) = cut(whitespace1)(i)?;
        Ok((i, ()))
    }
}

/// Consume characters until a `\n` or `\r\n` sequence is seen.
/// The end of line delimiter is consumed but not included in the result.
fn take_through_eol(i: Span) -> PResult<Span> {
    terminated(not_line_ending, line_ending)(i)
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

fn integral<I: Integral>() -> impl FnMut(Span) -> PResult<I> {
    move |i| {
        alt((
            map_res(preceded(tag("0x"), cut(hex_digit1)), |ds: Span| {
                I::from_hexadecimal(ds.fragment())
            }),
            map_res(digit1, |ds: Span| {
                let s = ds.fragment();
                if s.len() > 1 && s.starts_with("0") {
                    I::from_octal(s)
                } else {
                    I::from_decimal(s)
                }
            }),
        ))(i)
    }
}
