use bitflags::bitflags;
use mime::Mime;


#[derive(Debug)]
pub enum MagicPattern {
    Pattern(Pattern),
    Directive(Directive),
}

#[derive(Debug)]
pub struct Pattern {
    pub level: u64,
    pub offset: Offset,
    pub test: Test,
    pub message: String,
}

#[derive(Debug)]
pub enum Directive {
    Apple(String),
    Ext(String),
    Mime(Mime),
    Strength {
        operator: StrengthOp,
        value: u8,
    }
}

#[derive(Debug, Copy, Clone)]
pub enum StrengthOp {
    Add,
    Sub,
    Mul,
    Div,
}

/*
     offset       A number specifying the offset (in bytes) into the file of the data which is to be tested.  This
                  offset can be a negative number if it is:
                  •   The first direct offset of the magic entry (at continuation level 0), in which case it is
                      interpreted an offset from end end of the file going backwards.  This works only when a file
                      descriptor to the file is available and it is a regular file.
                  •   A continuation offset relative to the end of the last up-level field (&).


     Offsets do not need to be constant, but can also be read from the file being examined.  If the
     first character following the last > is a ( then the string after the parenthesis is
     interpreted as an indirect offset.  That means that the number after the parenthesis is used
     as an offset in the file.  The value at that offset is read, and is used again as an offset in
     the file.  Indirect offsets are of the form: (( x [[.,][bBcCeEfFgGhHiIlmsSqQ]][+-][ y ]).  The
     value of x is used as an offset in the file.  A byte, id3 length, short or long is read at
     that offset depending on the [bBcCeEfFgGhHiIlmsSqQ] type specifier.  The value is treated as
     signed if “”, is specified or unsigned if “”.  is specified.  The capitalized types interpret
     the number as a big endian value, whereas the small letter versions interpret the number as a
     little endian value; the m type interprets the number as a middle endian (PDP-11) value.  To
     that number the value of y is added and the result is used as an offset in the file.  The
     default type if one is not specified is long.
*/

/// The type to be read by an indirect offset:
///
///     Type    Sy Mnemonic   Sy Endian Sy Size
///     bcBc    Byte/Char     N/A       1
///     efg     Double        Little    8
///     EFG     Double        Big       8
///     hs      Half/Short    Little    2
///     HS      Half/Short    Big       2
///     i       ID3           Little    4
///     I       ID3           Big       4
///     m       Middle        Middle    4
///     o       Octal         Textual   Variable
///     q       Quad          Little    8
///     Q       Quad          Big       8
#[derive(Debug, Copy, Clone)]
pub enum OffsetType {
    /// `bcB`
    Byte,
    /// `efg`
    LEDouble,
    /// `EFG`
    BEDouble,
    /// `hs`
    LEShort,
    /// `HS`
    BEShort,
    /// `i`
    LEID3,
    /// `I`
    BEID3,
    /// `m`
    Middle,
    /// `o`
    Octal,
    /// `q`
    LEQuad,
    /// `Q`
    BEQuad,
}

#[derive(Debug)]
pub enum Offset {
    Direct(i64),
    Relative(Box<Offset>),
    Indirect {
        base: Box<Offset>,
        signed: bool,
        specifier: OffsetType,
        rhs: Option<(OffsetOp, Box<Offset>)>,
    },
}

#[derive(Debug, Copy, Clone)]
pub enum OffsetOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
}

#[derive(Debug)]
pub enum Expr<T> {
    /// `=`, the value from the file must equal the specified value
    Eq(T),

    /// `<`, the value from the file must be less than the specified value
    Lt(T),

    /// `>`, the value from the file must be greater than the specified value
    Gt(T),

    /// &, the value from the file must have set all of the bits that are set in the specified value
    BitAnd(T),

    /// ^, the value from the file must have clear any of the bits that are set in the specified value
    BitXor(T),

    /// ~, the value specified after is negated before tested
    BitNot(T),

    /// `x`, any value will match.
    Any,

    /// `!` the value from the file must not equal the specified value
    Neq(T),
}

bitflags! {
    #[derive(Default, Clone, Copy, Debug, PartialEq, Eq, Hash)]
    pub struct SearchFlags: u32 {
        /// `b`  Force binary file test.
        const ForceBinary = 0x01;

        /// `C`  Use upper case insensitive matching: upper case characters in the magic match both
        /// lower and upper case characters in the target, whereas lower case characters in the magic
        /// only match upper case characters in the target.
        const UpperCaseInsensitive = 0x02;

        /// `c`  Use lower case insensitive matching: lower case characters in the magic match both
        /// lower and upper case characters in the target, whereas upper case characters in the magic
        /// only match upper case characters in the target.  To do a complete case insensitive match,
        /// specify both “c” and “C”.
        const LowerCaseInsensitive = 0x04;

        /// `f`  Require that the matched string is a full word, not a partial word match.
        const MatchFullWord = 0x08;

        /// `T`  Trim the string, i.e. leading and trailing whitespace
        const Trim = 0x10;

        /// `t`  Force text file test.
        const ForceText = 0x20;

        /// `W`  Compact whitespace in the target, which must contain at least one whitespace character.
        /// If the magic has n consecutive blanks, the target needs at least n consecutive blanks to
        /// match.
        const CompactWhitespace = 0x40;

        /// `w`  Treat every blank in the magic as an optional blank.  is deleted before the string is
        /// printed.
        const OptionalBlanks = 0x80;
    }
}

/*
     The numeric types may optionally be followed by & and a numeric value, to specify that the
     value is to be AND'ed with the numeric value before any comparisons are done.  Prepending a u
     to the type indicates that ordered comparisons should be unsigned.
 */
#[derive(Debug)]
pub enum Test {
    /// `byte`, a one-byte value
    Byte(Expr<u8>),

    /// `short`, a two-byte value in this machine's native byte order
    Short(Expr<u16>),

    /// `long`, a four-byte value in this machine's native byte order
    Long(Expr<u32>),

    // /// `quad`, an eight-byte value in this machine's native byte order
    // Quad(Expr<u64>),

    /// `float`, a 32-bit single precision IEEE floating point number in this machine's native byte order
    /// Float(f32),

    /// `double`, a 64-bit double precision IEEE floating point number in this machine's native byte order
    /// Double(f64),

    /// `search`, a literal string search starting at the given offset.  The same modifier flags
    /// can be used as for string patterns.  The search expression must contain the range in the
    /// form `/number`, that is the number of positions at which the match will be attempted,
    /// starting from the start offset.  This is suitable for searching larger binary expressions
    /// with variable offsets, using `\` escapes for special characters.  The order of modifier and
    /// number is not relevant.
    Search {
        string: String,
        range: Option<usize>,
        flags: SearchFlags,
    },

    /// `string`  A string of bytes.  The string type specification can be optionally followed by a
    /// `/<width>` option and optionally followed by a set of flags `/[bCcftTtWw]*`.  The width
    /// limits the number of characters to be copied.  Zero means all characters.
    String {
        string: String,
        width: Option<usize>,
        flags: SearchFlags,
    },

    /// `name`, define a “named” magic instance that can be called from another use magic entry,
    /// like a subroutine call.  Named instance direct magic offsets are relative to the offset of
    /// the previous matched entry, but indirect offsets are relative to the beginning of the file
    /// as usual.  Named magic entries always match.
    Name(String),

    /// `use`, recursively call the named magic starting from the current offset.  If the name of
    /// the referenced begins with a ^ then the endianness of the magic is switched; if the magic
    /// mentioned leshort for example, it is treated as beshort and vice versa.  This is useful to
    /// avoid duplicating the rules for different endianness.
    Use(String),

    /*
    indirect        Starting at the given offset, consult the magic database again.  The offset of the
                    indirect magic is by default absolute in the file, but one can specify /r to indicate
                    that the offset is relative from the beginning of the entry.
    */

    /*
    pstring         A Pascal-style string where the first byte/short/int is interpreted as the unsigned
                    length.  The length defaults to byte and can be specified as a modifier.  The
                    following modifiers are supported:
                        B  A byte length (default).
                        H  A 2 byte big endian length.
                        h  A 2 byte little endian length.
                        L  A 4 byte big endian length.
                        l  A 4 byte little endian length.
                        J  The length includes itself in its count.
                    The string is not NUL terminated.  “J” is used rather than the more valuable “I”
                    because this type of length is a feature of the JPEG format.

    date            A four-byte value interpreted as a UNIX date.

    qdate           An eight-byte value interpreted as a UNIX date.

    ldate           A four-byte value interpreted as a UNIX-style date, but interpreted as local time
                    rather than UTC.

    qldate          An eight-byte value interpreted as a UNIX-style date, but interpreted as local time
                    rather than UTC.

    qwdate          An eight-byte value interpreted as a Windows-style date.

    beid3           A 32-bit ID3 length in big-endian byte order.

    beshort         A two-byte value in big-endian byte order.

    belong          A four-byte value in big-endian byte order.

    bequad          An eight-byte value in big-endian byte order.

    befloat         A 32-bit single precision IEEE floating point number in big-endian byte order.

    bedouble        A 64-bit double precision IEEE floating point number in big-endian byte order.

    bedate          A four-byte value in big-endian byte order, interpreted as a Unix date.

    beqdate         An eight-byte value in big-endian byte order, interpreted as a Unix date.

    beldate         A four-byte value in big-endian byte order, interpreted as a UNIX-style date, but
                    interpreted as local time rather than UTC.

    beqldate        An eight-byte value in big-endian byte order, interpreted as a UNIX-style date, but
                    interpreted as local time rather than UTC.

    beqwdate        An eight-byte value in big-endian byte order, interpreted as a Windows-style date.

    bestring16      A two-byte unicode (UCS16) string in big-endian byte order.

    leid3           A 32-bit ID3 length in little-endian byte order.

    leshort         A two-byte value in little-endian byte order.

    lelong          A four-byte value in little-endian byte order.

    lequad          An eight-byte value in little-endian byte order.

    lefloat         A 32-bit single precision IEEE floating point number in little-endian byte order.

    ledouble        A 64-bit double precision IEEE floating point number in little-endian byte order.

    ledate          A four-byte value in little-endian byte order, interpreted as a UNIX date.

    leqdate         An eight-byte value in little-endian byte order, interpreted as a UNIX date.

    leldate         A four-byte value in little-endian byte order, interpreted as a UNIX-style date, but
                    interpreted as local time rather than UTC.

    leqldate        An eight-byte value in little-endian byte order, interpreted as a UNIX-style date, but
                    interpreted as local time rather than UTC.

    leqwdate        An eight-byte value in little-endian byte order, interpreted as a Windows-style date.

    lestring16      A two-byte unicode (UCS16) string in little-endian byte order.

    melong          A four-byte value in middle-endian (PDP-11) byte order.

    medate          A four-byte value in middle-endian (PDP-11) byte order, interpreted as a UNIX date.

    meldate         A four-byte value in middle-endian (PDP-11) byte order, interpreted as a UNIX-style
                    date, but interpreted as local time rather than UTC.

    regex           A regular expression match in extended POSIX regular expression syntax (like egrep).
                    Regular expressions can take exponential time to process, and their performance is
                    hard to predict, so their use is discouraged.  When used in production environments,
                    their performance should be carefully checked.  The size of the string to search
                    should also be limited by specifying /<length>, to avoid performance issues scanning
                    long files.  The type specification can also be optionally followed by /[c][s][l].
                    The “c” flag makes the match case insensitive, while the “s” flag update the offset to
                    the start offset of the match, rather than the end.  The “l” modifier, changes the
                    limit of length to mean number of lines instead of a byte count.  Lines are delimited
                    by the platforms native line delimiter.  When a line count is specified, an implicit
                    byte count also computed assuming each line is 80 characters long.  If neither a byte
                    or line count is specified, the search is limited automatically to 8KiB.  ^ and $
                    match the beginning and end of individual lines, respectively, not beginning and end
                    of file.

    der             Parse the file as a DER Certificate file.  The test field is used as a der type that
                    needs to be matched.  The DER types are: eoc, bool, int, bit_str, octet_str, null,
                    obj_id, obj_desc, ext, real, enum, embed, utf8_str, rel_oid, time, res2, seq, set,
                    num_str, prt_str, t61_str, vid_str, ia5_str, utc_time, gen_time, gr_str, vis_str,
                    gen_str, univ_str, char_str, bmp_str, date, tod, datetime, duration, oid-iri,
                    rel-oid-iri.  These types can be followed by an optional numeric size, which indicates
                    the field width in bytes.

    guid            A Globally Unique Identifier, parsed and printed as XXXXXXXX-XXXX-XXXX-XXXX-
                    XXXXXXXXXXXX.  It's format is a string.


    offset          This is a quad value indicating the current offset of the file.  It can be used to
                    determine the size of the file or the magic buffer.  For example the magic entries:

                          -0      offset  x       this file is %lld bytes
                          -0      offset  <=100   must be more than 100 \
                              bytes and is only %lld

    octal           A string representing an octal number.

    default         This is intended to be used with the test x (which is always true) and it has no type.
                    It matches when no other test at that continuation level has matched before.  Clearing
                    that matched tests for a continuation level, can be done using the clear test.

    clear           This test is always true and clears the match flag for that continuation level.  It is
                    intended to be used with the default test.
    */
}
