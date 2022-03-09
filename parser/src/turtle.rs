#![allow(dead_code, unused_variables, unused_mut, unused_imports)]

use std::collections::HashMap;

use nom::{
    bytes::complete::{tag, tag_no_case, take_till, take_until, take_while},
    character::complete::*,
    combinator::opt,
    error::{make_error, Error, ErrorKind},
    sequence::{pair, preceded, terminated},
    IResult, Parser,
};

use crate::shared::{extract_iri, Node, Statement};

const BASE_TURTLE: &str = "@base";
const BASE_SPARQL: &str = "BASE";
const PREFIX_TURTLE: &str = "@prefix";
const PREFIX_SPARQL: &str = "PREFIX";

/// https://www.w3.org/TR/turtle

struct Context<'a> {
    current_base: Option<Node<'a>>,
    prefixes: HashMap<&'a str, Node<'a>>,
}

enum TurtleValue<'a> {
    Base(Node<'a>),
    Prefix((&'a str, Node<'a>)),
}

fn extract_base(s: &str) -> IResult<&str, Node<'_>> {
    let (remaining, base) = preceded(
        multispace0,
        tag_no_case(BASE_SPARQL).or(tag_no_case(BASE_TURTLE)),
    )(s)?;
    match base {
        BASE_SPARQL => extract_iri(remaining),
        BASE_TURTLE => terminated(extract_iri, preceded(multispace0, char('.')))(remaining),
        _ => {
            let err: Error<&str> = make_error(base, ErrorKind::IsNot);
            Err(nom::Err::Error(err))
        }
    }
}

fn extract_prefix(s: &str) -> IResult<&str, (&str, Node<'_>)> {
    let (remaining, prefix) = preceded(
        multispace0,
        tag_no_case(PREFIX_SPARQL).or(tag_no_case(PREFIX_TURTLE)),
    )(s)?;
    let mut get_prefix = preceded(
        multispace0,
        pair(
            take_while(|s: char| s != '<' && !s.is_whitespace()),
            extract_iri,
        ),
    );
    match prefix {
        PREFIX_SPARQL => get_prefix(remaining),
        PREFIX_TURTLE => terminated(get_prefix, preceded(multispace0, char('.')))(remaining),
        _ => {
            let err: Error<&str> = make_error(prefix, ErrorKind::IsNot);
            Err(nom::Err::Error(err))
        }
    }
}

fn extract_triples(s: &str) -> IResult<&str, Vec<Statement<'_>>> {
    // let (remaining, triples) = take_till(cond)
    todo!()
}

#[cfg(test)]
mod test {
    use crate::shared::Node;
    use crate::turtle::{extract_base, extract_prefix, Context};
    use std::collections::HashMap;
    use std::rc::Rc;

    #[test]
    fn base_test() {
        let base_sparql = r#"
              BASE   <http://one.example/sparql>

        "#;
        let base_turtle = r#"

             @base    <http://one.example/turtle> .
        "#;

        let (remaining, base_turtle) = extract_base(base_turtle).unwrap();
        assert_eq!(Node::Iri("http://one.example/turtle"), base_turtle);

        let (remaining, base_sparql) = extract_base(base_sparql).unwrap();
        assert_eq!(Node::Iri("http://one.example/sparql"), base_sparql);
    }

    #[test]
    fn prefix_test() {
        let prefix_sparql = r#"
        PREFIX p: <http://two.example/sparql>

        "#;
        let prefix_turtle = r#"

             @prefix    p:    <http://two.example/turtle> .
        "#;
        let prefix_empty_turtle = r#"

             @prefix    :    <http://two.example/empty> .
        "#;

        let (remaining, prefix_turtle) = extract_prefix(prefix_turtle).unwrap();
        assert_eq!(
            ("p:", Node::Iri("http://two.example/turtle")),
            prefix_turtle
        );
        let (remaining, prefix_empty_turtle) = extract_prefix(prefix_empty_turtle).unwrap();
        assert_eq!(
            (":", Node::Iri("http://two.example/empty")),
            prefix_empty_turtle
        );

        let (remaining, prefix_sparql) = extract_prefix(prefix_sparql).unwrap();
        assert_eq!(
            ("p:", Node::Iri("http://two.example/sparql")),
            prefix_sparql
        );
    }

    #[test]
    fn extract_triples() {
        let s = r#"
            <http://en.wikipedia.org/wiki/Helium>                                                                                  
            <http://example.org/elements/atomicNumber>  "2" ;                                                                              
            <http://example.org/elements/atomicMass> "4.002602" ;                                                                          
            <http://example.org/elements/specificGravity> "1.663E-4" .     
        "#;
    }
}
