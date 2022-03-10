use nom::{
    bytes::complete::take_while,
    character::complete::*,
    combinator::map,
    sequence::{delimited, preceded},
    IResult,
};

pub const SIMPLE_LITERAL: &str = "http://www.w3.org/2001/XMLSchema#string";
pub const LANG_LITERAL: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString";
pub const NS_TYPE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";

