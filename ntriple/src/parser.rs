#![allow(dead_code, unused_imports)]

use nom::branch::alt;
use nom::bytes::complete::{tag, take_until};
use nom::combinator::flat_map;
use nom::complete::take;
use nom::error::Error;
use nom::multi::many0;
use nom::sequence::{delimited, tuple};
use nom::AsChar;
use nom::{
    bytes::complete::{escaped, take_while},
    character::{complete::*, is_alphanumeric},
    combinator::{cut, map},
    error::{context, ContextError, ParseError},
    multi::separated_list0,
    sequence::{preceded, terminated},
    IResult,
};

const SIMPLE_LITERAL: &str = "http://www.w3.org/2001/XMLSchema#string";
const LANG_LITERAL: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString";

#[derive(Debug)]
struct Triple<'a> {
    subject: Node<'a>,
    predicate: Node<'a>,
    object: Node<'a>,
}
#[derive(Debug)]
enum Node<'a> {
    Iri(&'a str),
    Literal {
        datatype: Box<Node<'a>>,
        value: &'a str,
        lang: Option<&'a str>,
    },
}

fn extract_lang<'a>(s: &'a str) -> IResult<&'a str, &'a str> {
    preceded(char('@'), take_while(|a: char| a.is_alpha() || a == '-'))(s)
}

fn extract_url<'a>(s: &'a str) -> IResult<&'a str, Node<'a>> {
    preceded(
        multispace0,
        map(
            delimited(char('<'), take_while(|s: char| s != '>'), char('>')),
            |uri| Node::Iri(uri),
        ),
    )(s)
}

fn extract_literal<'a>(s: &'a str) -> IResult<&'a str, Node> {
    let mut extract_value = delimited(char('"'), take_while(|s: char| s != '"'), char('"'));
    let mut extract_literal = preceded(tag("^^"), extract_url);

    let (no_white_space, _) = multispace0(s)?;
    let (remaining, value) = extract_value(no_white_space)?;

    if let Ok((remaining, datatype)) = extract_literal(remaining) {
        return Ok((
            remaining,
            Node::Literal {
                datatype: Box::new(datatype),
                value: value,
                lang: None,
            },
        ));
    } else if let Ok((remaining, lang)) = extract_lang(remaining) {
        return Ok((
            remaining,
            Node::Literal {
                datatype: Box::new(Node::Iri(LANG_LITERAL)),
                value: value,
                lang: Some(lang),
            },
        ));
    } else {
        return Ok((
            remaining,
            Node::Literal {
                datatype: Box::new(Node::Iri(SIMPLE_LITERAL)),
                value: value,
                lang: None,
            },
        ));
    }
}

fn parse_one_triple<'a>(s: &'a str) -> IResult<&'a str, Triple<'a>> {
    map(
        tuple((
            extract_url,
            extract_url,
            cut(alt((extract_url, extract_literal))),
        )),
        |(sub, pred, obj)| Triple {
            subject: sub,
            predicate: pred,
            object: obj,
        },
    )(s)
}

fn parse_list_triples<'a>(s: &'a str) -> IResult<&'a str, Vec<Triple<'a>>> {
    many0(terminated(
        parse_one_triple,
        preceded(multispace0, char('.')),
    ))(s)
}

#[cfg(test)]
mod tests {
    use nom::error::ErrorKind;

    use super::*;

    #[test]
    fn parse_list_triples_test() {
        let triple = r#"  
         <http://bittich.be/some/url/123>    <http://example.org/firstName><http://n.com/nordine>.
             <http://bittich.be/some/url/123><http://example.org/firstName><http://n.com/nordine>.
             <http://example.org/show/218> <http://www.w3.org/2000/01/rdf-schema#label> "That Seventies Show".
             <http://example.org/show/218> <http://example.org/show/localName> "That Seventies Show"@en .
         <http://bittich.be/some/url/123>    <http://example.org/firstName><http://n.com/nordine>  .
         <http://bittich.be/some/url/123>    <http://example.org/firstName><http://n.com/nordine>  .
         <http://bittich.be/some/url/123>    <http://example.org/firstName><http://n.com/nordine>  .

         <http://bittich.be/some/url/123>    <http://example.org/firstName><http://n.com/nordine>  .
         <http://example.org/show/218> <http://example.org/show/localName> "Cette Série des Années Septante"@fr-be .

         
         "#;

        let (_remaining, triples) = parse_list_triples(triple).unwrap();
        assert!(triples.len() == 9);
        println!("{:?}", triples);
    }
}
