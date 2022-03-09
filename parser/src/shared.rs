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

#[derive(Debug)]
pub struct Statement<'a> {
    pub subject: Node<'a>,
    pub predicate: Node<'a>,
    pub object: Node<'a>,
}

#[derive(Debug, PartialEq)]
pub enum Node<'a> {
    Iri(&'a str),
    BlankNode(&'a str),
    Literal {
        datatype: Box<Node<'a>>,
        value: &'a str,
        lang: Option<&'a str>,
    },
}

pub fn extract_iri(s: &str) -> IResult<&str, Node<'_>> {
    preceded(
        multispace0,
        map(
            delimited(char('<'), take_while(|s: char| s != '>'), char('>')),
            Node::Iri,
        ),
    )(s)
}
