/// https://www.w3.org/TR/n-triples/
use nom::branch::alt;
use nom::bytes::complete::{tag, take_until};

use nom::error::{make_error, Error, ErrorKind};
use nom::multi::many0;
use nom::sequence::{delimited, tuple};
use nom::AsChar;
use nom::{
    bytes::complete::take_while,
    character::complete::*,
    combinator::{cut, map},
    sequence::{preceded, terminated},
    IResult,
};

use crate::shared::{LANG_LITERAL, XSD_STRING};

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
fn extract_lang(s: &str) -> IResult<&str, &str> {
    preceded(char('@'), take_while(|a: char| a.is_alpha() || a == '-'))(s)
}

fn extract_bnode(s: &str) -> IResult<&str, Node<'_>> {
    let (remaining, _) = multispace0(s)?;
    let (remaining, label) =
        delimited(tag("_:"), take_while(|s: char| !s.is_whitespace()), space1)(remaining)?;
    if label.starts_with('.') || label.ends_with('.') || label.starts_with('-') {
        let err: Error<&str> = make_error(label, ErrorKind::IsNot);
        return Err(nom::Err::Error(err));
    }
    Ok((remaining, Node::BlankNode(label)))
}

fn extract_literal(s: &str) -> IResult<&str, Node<'_>> {
    let mut extract_value = delimited(char('"'), take_while(|s: char| s != '"'), char('"'));
    let mut extract_lit = preceded(tag("^^"), extract_iri);

    let (no_white_space, _) = multispace0(s)?;
    let (remaining, value) = extract_value(no_white_space)?;

    if let Ok((remaining, datatype)) = extract_lit(remaining) {
        Ok((
            remaining,
            Node::Literal {
                datatype: Box::new(datatype),
                value,
                lang: None,
            },
        ))
    } else if let Ok((remaining, lang)) = extract_lang(remaining) {
        Ok((
            remaining,
            Node::Literal {
                datatype: Box::new(Node::Iri(LANG_LITERAL)),
                value,
                lang: Some(lang),
            },
        ))
    } else {
        Ok((
            remaining,
            Node::Literal {
                datatype: Box::new(Node::Iri(XSD_STRING)),
                value,
                lang: None,
            },
        ))
    }
}

fn parse_one_triple(s: &str) -> IResult<&str, Statement<'_>> {
    let (remaining, _) = preceded(multispace0, comments)(s)?;
    map(
        tuple((
            alt((extract_iri, extract_bnode)),
            cut(extract_iri),
            cut(alt((extract_iri, extract_bnode, extract_literal))),
        )),
        |(sub, pred, obj)| Statement {
            subject: sub,
            predicate: pred,
            object: obj,
        },
    )(remaining)
}

pub fn parse(s: &str) -> IResult<&str, Vec<Statement<'_>>> {
    many0(terminated(
        parse_one_triple,
        preceded(multispace0, cut(tag("."))),
    ))(s)
}

fn comments(s: &str) -> IResult<&str, Vec<&str>> {
    many0(terminated(
        preceded(char('#'), take_until("\n")),
        multispace0,
    ))(s)
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn parse_test() {
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

        let (remaining, triples) = parse(triple).unwrap();
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
        let (_, triples) = parse(triples).unwrap();
        assert_eq!(6, triples.len());
    }
}
