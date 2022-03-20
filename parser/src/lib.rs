extern crate core;

pub mod shared;
pub mod turtle;

pub mod prelude {
    pub use nom::{
        branch::alt,
        bytes::complete::{
            tag, tag_no_case, take, take_till, take_till1, take_until, take_until1, take_while,
            take_while1,
        },
        character::{
            complete::{char, i64 as I64, line_ending, multispace0, multispace1, space0},
            is_alphanumeric, is_space,
        },
        combinator::{all_consuming, cut, eof, map, opt, peek, recognize},
        error::{make_error, Error, ErrorKind},
        multi::{many0, separated_list0, separated_list1},
        number::complete::{double, float, recognize_float},
        sequence::{delimited, pair, preceded, separated_pair, terminated},
        AsChar, IResult, InputIter, ParseTo, Parser,
    };
}

pub mod grammar {

    pub const PN_LOCAL_ESC: [char; 16] = [
        '_', '~', '-', '!', '$', '&', '\'', '(', ')', '*', '+', '=', '/', '?', '#', '%',
    ];
    pub const PERCENT: &str = "%";
    pub const STRING_LITERAL_QUOTE: &str = r#"""#;
    pub const STRING_LITERAL_SINGLE_QUOTE: &str = "'";
    pub const STRING_LITERAL_LONG_SINGLE_QUOTE: &str = "'''";
    pub const STRING_LITERAL_LONG_QUOTE: &str = r#"""""#;
    pub const LANGTAG: &str = "@";
    pub const BLANK_NODE_LABEL: &str = "_:";
}
