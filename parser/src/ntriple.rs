#![allow(dead_code, unused_imports)]
use nom::branch::alt;
use nom::bytes::complete::{tag, take_till, take_until};
use nom::combinator::{flat_map, opt, value};
use nom::complete::take;
use nom::error::{make_error, Error, ErrorKind};
use nom::multi::many0;
use nom::sequence::{delimited, pair, tuple};
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

/// https://www.w3.org/TR/n-triples/

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
    BlankNode(&'a str),
    Literal {
        datatype: Box<Node<'a>>,
        value: &'a str,
        lang: Option<&'a str>,
    },
}

fn extract_lang<'a>(s: &'a str) -> IResult<&'a str, &'a str> {
    preceded(char('@'), take_while(|a: char| a.is_alpha() || a == '-'))(s)
}

fn extract_iri<'a>(s: &'a str) -> IResult<&'a str, Node<'a>> {
    preceded(
        multispace0,
        map(
            delimited(char('<'), take_while(|s: char| s != '>'), char('>')),
            |uri| Node::Iri(uri),
        ),
    )(s)
}

fn extract_bnode<'a>(s: &'a str) -> IResult<&'a str, Node<'a>> {
    let (remaining, _) = multispace0(s)?;
    let (remaining, label) =
        delimited(tag("_:"), take_while(|s: char| s != ' '), char(' '))(remaining)?;
    if label.starts_with('.') || label.ends_with('.') || label.starts_with('-') {
        let err: Error<&'a str> = make_error(label, ErrorKind::IsNot);
        return Err(nom::Err::Error(err));
    }
    Ok((remaining, Node::BlankNode(label)))
}

fn extract_literal<'a>(s: &'a str) -> IResult<&'a str, Node> {
    let mut extract_value = delimited(char('"'), take_while(|s: char| s != '"'), char('"'));
    let mut extract_literal = preceded(tag("^^"), extract_iri);

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
    let (remaining, _) = multispace0(s)?;
    let (remaining, _) = skip_comments(remaining)?;
    map(
        tuple((
            alt((extract_iri, extract_bnode)),
            extract_iri,
            cut(alt((extract_iri, extract_bnode, extract_literal))),
        )),
        |(sub, pred, obj)| Triple {
            subject: sub,
            predicate: pred,
            object: obj,
        },
    )(remaining)
}

fn parse_list_triples<'a>(s: &'a str) -> IResult<&'a str, Vec<Triple<'a>>> {
    many0(terminated(
        parse_one_triple,
        preceded(multispace0, cut(tag("."))),
    ))(s)
}

fn skip_comments<'a>(s: &'a str) -> IResult<&'a str, Vec<&'a str>> {
    many0(terminated(
        preceded(char('#'), take_until("\n")),
        multispace0,
    ))(s)
}

#[cfg(test)]
mod tests {
    use nom::{combinator::value, error::ErrorKind, sequence::pair};

    use super::*;

    #[test]
    fn parse_list_triples_test() {
        let triple = r#" 
        # this is a comment
         <http://bittich.be/some/url/123>    <http://example.org/firstName><http://n.com/nordine>. # this is a comment at EOF
             <http://bittich.be/some/url/123><http://example.org/firstName><http://n.com/nordine>.
             <http://example.org/show/218> <http://www.w3.org/2000/01/rdf-schema#label> "That Seventies Show".
             <http://example.org/show/218> <http://example.org/show/localName> "That Seventies Show"@en .
         <http://bittich.be/some/url/123>    <http://example.org/firstName><http://n.com/nordine>  .
         <http://bittich.be/some/url/123>    <http://example.org/firstName><http://n.com/nordine>  .
         <http://bittich.be/some/url/123>    <http://example.org/firstName><http://n.com/nordine>  .
         #  the entire line is commented <http://bittich.be/some/url/123>    <http://example.org/firstName><http://n.com/nordine>  .
         #  the entire line is commented <http://bittich.be/some/url/123>    <http://example.org/firstName><http://n.com/nordine>  .
         #  the entire line is commented <http://bittich.be/some/url/123>    <http://example.org/firstName><http://n.com/nordine>  .



            #  the entire line is commented <http://bittich.be/some/url/123>    <http://example.org/firstName><http://n.com/nordine>  .
            _:alice <http://xmlns.com/foaf/0.1/knows> _:bob .
            _:bob <http://xmlns.com/foaf/0.1/knows> _:alice .
         <http://bittich.be/some/url/123>    <http://example.org/firstName><http://n.com/nordine>  .
         <http://example.org/show/218> <http://example.org/show/localName> "Cette Série des Années Septante"@fr-be .

         <http://en.wikipedia.org/wiki/Helium> <http://example.org/elements/specificGravity> "1.663E-4"^^<http://www.w3.org/2001/XMLSchema#double> .     # xsd:double
         "#;

        let (remaining, triples) = parse_list_triples(triple).unwrap();
        println!("{:?}", remaining);
        println!("{:?}", triples);
        assert_eq!(triples.len(), 12);
    }
    #[test]
    fn test_multi_comments() {
        let triples = r#" 
            #  the entire line is commented <http://bittich.be/some/url/123>    <http://example.org/firstName><http://n.com/nordine>  .
            #  the entire line is commented <http://bittich.be/some/url/123>    <http://example.org/firstName><http://n.com/nordine>  .
            #  the entire line is commented <http://bittich.be/some/url/123>    <http://example.org/firstName><http://n.com/nordine>  .
            _:alice <http://xmlns.com/foaf/0.1/knows> _:bob .
            _:bob <http://xmlns.com/foaf/0.1/knows> _:alice .
         <http://bittich.be/some/url/123>    <http://example.org/firstName><http://n.com/nordine>  .
         <http://example.org/show/218> <http://example.org/show/localName> "Cette Série des Années Septante"@fr-be .

         <http://en.wikipedia.org/wiki/Helium> <http://example.org/elements/specificGravity> "1.663E-4"^^<http://www.w3.org/2001/XMLSchema#double> .     # xsd:double
         <http://en.wikipedia.org/wiki/Helium> <http://example.org/elements/specificGravity> "1.663E-4"^^<http://www.w3.org/2001/XMLSchema#double> .     # xsd:double
         "#;
        let (_, triples) = parse_list_triples(triples).unwrap();
        assert_eq!(6, triples.len());
    }
}
