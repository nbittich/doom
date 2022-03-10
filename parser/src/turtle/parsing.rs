#![allow(
    dead_code,
    unused_variables,
    unused_mut,
    unused_imports,
    unused_must_use
)]

use std::collections::HashMap;

use nom::{
    branch::alt,
    bytes::complete::{
        tag, tag_no_case, take, take_till, take_till1, take_until, take_until1, take_while,
        take_while1,
    },
    character::{complete::*, is_space},
    combinator::{map, opt, peek},
    error::{make_error, Error, ErrorKind},
    multi::{many0, separated_list0},
    sequence::{delimited, pair, preceded, separated_pair, terminated},
    AsChar, IResult, InputIter, Parser,
};

use crate::shared::{LANG_LITERAL, SIMPLE_LITERAL};

use super::model::{Iri, TurtleValue, BASE_SPARQL, BASE_TURTLE, PREFIX_SPARQL, PREFIX_TURTLE};

fn extract_prefixed_iri(s: &str) -> IResult<&str, Iri> {
    let mut extract_prefixed = map(
        separated_pair(take_until1(":"), tag(":"), take_until1(" ")),
        |(prefix, local_name)| Iri::Prefixed { prefix, local_name },
    );
    preceded(multispace0, extract_prefixed)(s)
}

fn extract_enclosed_iri(s: &str) -> IResult<&str, Iri> {
    let mut extract_enclosed = map(
        delimited(char('<'), take_while(|s: char| s != '>'), char('>')),
        Iri::Enclosed,
    );

    preceded(multispace0, extract_enclosed)(s)
}
fn extract_iri(s: &str) -> IResult<&str, Iri> {
    alt((extract_enclosed_iri, extract_prefixed_iri))(s)
}

fn extract_base(s: &str) -> IResult<&str, TurtleValue<'_>> {
    let (remaining, base) = preceded(
        multispace0,
        tag_no_case(BASE_SPARQL).or(tag_no_case(BASE_TURTLE)),
    )(s)?;
    match base {
        BASE_SPARQL => map(extract_enclosed_iri, |iri| TurtleValue::Base(iri))(remaining),
        BASE_TURTLE => map(
            terminated(extract_enclosed_iri, preceded(multispace0, char('.'))),
            |iri| TurtleValue::Base(iri),
        )(remaining),
        _ => {
            let err: Error<&str> = make_error(base, ErrorKind::IsNot);
            Err(nom::Err::Error(err))
        }
    }
}
fn extract_prefix(s: &str) -> IResult<&str, TurtleValue<'_>> {
    let (remaining, prefix) = preceded(
        multispace0,
        tag_no_case(PREFIX_SPARQL).or(tag_no_case(PREFIX_TURTLE)),
    )(s)?;
    let mut get_prefix = preceded(
        multispace0,
        map(
            separated_pair(take_until(":"), tag(":"), extract_enclosed_iri),
            |(prefix, iri)| TurtleValue::Prefix((prefix, iri)),
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

// TODO
fn extract_turtle_b_node(s: &str) -> IResult<&str, Iri> {
    todo!()
}
fn extract_literal(s: &str) -> IResult<&str, Iri> {
    todo!()
}

fn extract_object_lists(s: &str) -> IResult<&str, Vec<Iri>> {
    separated_list0(
        char(','),
        alt((extract_enclosed_iri, extract_turtle_b_node, extract_literal)),
    )(s)
}

fn predicate_lists(s: &str) -> IResult<&str, (Iri, Vec<(Iri, Vec<Iri>)>)> {
    let (remaining, subject) = extract_enclosed_iri(s)?; // TODO handle other cases
    let (remaining, list) = preceded(
        multispace0,
        separated_list0(
            delimited(multispace0, tag(";"), multispace0),
            pair(extract_enclosed_iri, extract_object_lists),
        ),
    )(remaining)?;

    let (remaining, terminated_dot) = preceded(multispace0, char('.'))(remaining)?;

    Ok((remaining, (subject, list)))
}

#[cfg(test)]
mod test {
    use crate::turtle::model::Iri;

    use super::{extract_base, extract_prefix, TurtleValue};
    use std::collections::HashMap;
    use std::rc::Rc;

    use super::predicate_lists;

    #[test]
    fn base_test() {
        let base_sparql = r#"
              BASE   <http://one.example/sparql>

        "#;
        let base_turtle = r#"

             @base    <http://one.example/turtle> .
        "#;

        let (remaining, base_turtle) = extract_base(base_turtle).unwrap();
        assert_eq!(
            TurtleValue::Base(Iri::Enclosed("http://one.example/turtle")),
            base_turtle
        );

        let (remaining, base_sparql) = extract_base(base_sparql).unwrap();
        assert_eq!(
            TurtleValue::Base(Iri::Enclosed("http://one.example/sparql")),
            base_sparql
        );
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
            TurtleValue::Prefix(("p", Iri::Enclosed("http://two.example/turtle"))),
            prefix_turtle
        );
        let (remaining, prefix_empty_turtle) = extract_prefix(prefix_empty_turtle).unwrap();
        assert_eq!(
            TurtleValue::Prefix(("", Iri::Enclosed("http://two.example/empty"))),
            prefix_empty_turtle
        );

        let (remaining, prefix_sparql) = extract_prefix(prefix_sparql).unwrap();
        assert_eq!(
            TurtleValue::Prefix(("p", Iri::Enclosed("http://two.example/sparql"))),
            prefix_sparql
        );
    }

    #[test]
    fn predicate_lists_test() {
        let s = r#"
            <http://en.wikipedia.org/wiki/Helium>                                                                                  
            <http://example.org/elements/atomicNumber>  "2" ;                                                                              
            <http://example.org/elements/atomicMass> "4.002602" ;                                                                          
            <http://example.org/elements/specificGravity> "1.663E-4" .     
        "#;
        let (remaining, res) = predicate_lists(s).unwrap();
        assert_eq!(3, res.1.len());

        let s = r#"
            <http://en.wikipedia.org/wiki/Helium>  <http://example.org/elements/atomicNumber>  "2".                                                                           
        "#;
        let (remaining, res) = predicate_lists(s).unwrap();
        assert_eq!(1, res.1.len());

        let s = r#"
            <http://en.wikipedia.org/wiki/Helium>  <http://example.org/elements/atomicNumber>  "2".                                                                           
            <http://en.wikipedia.org/wiki/Helium>  <http://example.org/elements/atomicNumber>  "2".                                                                           
        "#;
        let (remaining, res) = predicate_lists(s).unwrap();
        assert_eq!(1, res.1.len());
        let s = r#"
            <http://example.org/#spiderman> <http://xmlns.com/foaf/0.1/name> "Spiderman", "Человек-паук"@ru .                                                                         
        "#;
        let (remaining, res) = predicate_lists(s).unwrap();
        assert_eq!(1, res.1.len());
        assert_eq!(2, res.1[0].1.len());
        dbg!(res);
    }
}
