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
    character::{complete::*, is_alphanumeric, is_space},
    combinator::{all_consuming, eof, map, opt, peek, recognize, cut},
    error::{make_error, Error, ErrorKind},
    multi::{many0, separated_list0},
    number::complete::{double, float, recognize_float},
    sequence::{delimited, pair, preceded, separated_pair, terminated},
    AsChar, IResult, InputIter, ParseTo, Parser,
};
use rand::{distributions::Alphanumeric, prelude::ThreadRng, Rng};

use crate::{
    shared::{LANG_LITERAL, XSD_STRING},
    turtle::model::Literal,
};

use super::model::{
    BlankNode, Iri, TurtleValue, BASE_SPARQL, BASE_TURTLE, PREFIX_SPARQL, PREFIX_TURTLE,
};

// BASE & PREFIX
// -----------------------------------------------------------------------------------------------------------------------------------------
fn extract_base(s: &str) -> IResult<&str, TurtleValue<'_>> {
    let (remaining, base) = preceded(
        multispace0,
        tag_no_case(BASE_SPARQL).or(tag_no_case(BASE_TURTLE)),
    )(s)?;
    match base {
        BASE_SPARQL => map(extract_enclosed_iri, |iri| TurtleValue::Base(Box::new(iri)))(remaining),
        BASE_TURTLE => map(
            terminated(extract_enclosed_iri, preceded(multispace0, char('.'))),
            |iri| TurtleValue::Base(Box::new(iri)),
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
            |(prefix, iri)| TurtleValue::Prefix((prefix, Box::new(iri))),
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
// -----------------------------------------------------------------------------------------------------------------------------------------

// EXTRACT IRI
// -----------------------------------------------------------------------------------------------------------------------------------------
fn extract_prefixed_iri(s: &str) -> IResult<&str, TurtleValue> {
    let mut extract_prefixed = map(
        separated_pair(
            take_while(|s: char| s.is_alphanumeric()),
            tag(":"),
            take_while(|s: char| !s.is_whitespace()),
        ),
        |(prefix, local_name)| Iri::Prefixed { prefix, local_name },
    );
    map(preceded(multispace0, extract_prefixed), TurtleValue::Iri)(s)
}
fn extract_labeled_bnode(s: &str) -> IResult<&str, TurtleValue<'_>> {
    let mut parse_labeled_bnode =
        delimited(tag("_:"), take_while(|s: char| !s.is_whitespace()), space0);
    let (remaining, _) = multispace0(s)?;
    let (remaining, label) = parse_labeled_bnode(remaining)?;
    if label.starts_with('.') || label.ends_with('.') || label.starts_with('-') {
        let err: Error<&str> = make_error(label, ErrorKind::IsNot);
        return Err(nom::Err::Error(err));
    }
    Ok((remaining, TurtleValue::BNode(BlankNode::Labeled(label))))
}
fn extract_enclosed_iri(s: &str) -> IResult<&str, TurtleValue> {
    let mut extract_enclosed = map(
        delimited(char('<'), take_while(|s: char| s != '>'), char('>')),
        Iri::Enclosed,
    );

    map(preceded(multispace0, extract_enclosed), TurtleValue::Iri)(s)
}
fn extract_iri(s: &str) -> IResult<&str, TurtleValue> {
    alt((
        extract_enclosed_iri,
        extract_prefixed_iri,
        extract_labeled_bnode,
        extract_unlabeled_bnode
    ))(s)
}
// -----------------------------------------------------------------------------------------------------------------------------------------

// EXTRACT LITERALS
// -----------------------------------------------------------------------------------------------------------------------------------------
fn parse_boolean(s: &str) -> IResult<&str, bool> {
    let (remaining, val) = terminated(
        map(alt((tag("true"), tag("false"))), |b: &str| {
            b.parse::<bool>().map_err(|err| {
                let err: Error<&str> = make_error(b, ErrorKind::IsNot);
                nom::Err::Error(err)
            })
        }),
        multispace0,
    )(s)?;
    let boolean_value = val?;
    Ok((remaining, boolean_value))
}

fn parse_number(s: &str) -> IResult<&str, Literal> {
    fn try_parse_int(s: &str) -> IResult<&str, i64> {
        all_consuming(i64)(s)
    }
    fn try_parse_decimal(s: &str) -> IResult<&str, f32> {
        all_consuming(float)(s)
    }
    fn try_parse_double(s: &str) -> IResult<&str, f64> {
        all_consuming(double)(s)
    }
    let (remaining, num) = recognize_float(s)?;
    if let Ok((_, n)) = try_parse_int(num) {
        Ok((remaining, Literal::Integer(n)))
    } else if let Ok((_, n)) = try_parse_decimal(num) {
        Ok((remaining, Literal::Decimal(n)))
    } else if let Ok((_, n)) = try_parse_double(num) {
        Ok((remaining, Literal::Double(n)))
    } else {
        let err: Error<&str> = make_error(s, ErrorKind::IsNot);
        return Err(nom::Err::Error(err));
    }
}

fn extract_primitive_literal(s: &str) -> IResult<&str, TurtleValue> {
    let (no_white_space, _) = multispace0(s)?;

    if let Ok((remaining, boolean_value)) = parse_boolean(no_white_space) {
        return Ok((
            remaining,
            TurtleValue::Literal(Literal::Boolean(boolean_value)),
        ));
    } else if let Ok((remaining, number_value)) = parse_number(no_white_space) {
        return Ok((remaining, TurtleValue::Literal(number_value)));
    } else {
        let err: Error<&str> = make_error(no_white_space, ErrorKind::IsNot);
        return Err(nom::Err::Error(err));
    }
}

// TODO handle primitive literal datatype when sharing prefix
fn extract_string_literal(s: &str) -> IResult<&str, TurtleValue> {
    let mut single_quote_literal = delimited(tag("'"), take_until1("'"), tag("'"));
    let mut double_quote_literal = delimited(char('"'), take_until1(r#"""#), char('"'));
    let mut multiline_quote_literal =
        delimited(tag(r#"'''"#), take_until1(r#"'''"#), tag(r#"'''"#));
    let mut extract_datatype = preceded(tag("^^"), extract_iri);

    fn extract_lang(s: &str) -> IResult<&str, &str> {
        preceded(char('@'), take_while(|a: char| a.is_alpha() || a == '-'))(s)
    }

    let (no_white_space, _) = multispace0(s)?;
    let (remaining, string_literal) = alt((
        single_quote_literal,
        double_quote_literal,
        multiline_quote_literal,
    ))(no_white_space)?;

    if let Ok((remaining, datatype)) = extract_datatype(remaining) {
        Ok((
            remaining,
            TurtleValue::Literal(Literal::Quoted {
                datatype: Some(Box::new(datatype)),
                value: string_literal,
                lang: None,
            }),
        ))
    } else if let Ok((remaining, lang)) = extract_lang(remaining) {
        Ok((
            remaining,
            TurtleValue::Literal(Literal::Quoted {
                datatype: Some(Box::new(TurtleValue::Iri(Iri::Enclosed(LANG_LITERAL)))),
                value: string_literal,
                lang: Some(lang),
            }),
        ))
    } else {
        Ok((
            remaining,
            TurtleValue::Literal(Literal::Quoted {
                datatype: Some(Box::new(TurtleValue::Iri(Iri::Enclosed(XSD_STRING)))),
                value: string_literal,
                lang: None,
            }),
        ))
    }
}

fn extract_literal(s: &str) -> IResult<&str, TurtleValue> {
    alt((extract_string_literal, extract_primitive_literal))(s)
}
// -----------------------------------------------------------------------------------------------------------------------------------------

// TODO nested + unlabeled bnode
fn extract_unlabeled_bnode(s: &str) -> IResult<&str, TurtleValue> {
    let unlabled_bnode_fn = |s| Ok((s, TurtleValue::BNode(BlankNode::Unlabeled)));
    let mut extract = preceded(char('['), terminated(alt((
        predicate_lists(unlabled_bnode_fn),
        unlabled_bnode_fn,
    )
    ), preceded(multispace0, cut(char(']')))));
    preceded(multispace0,extract)(s)
} 

fn extract_object_lists(s: &str) -> IResult<&str, TurtleValue> {
    map(
        separated_list0(char(','), alt((extract_iri, extract_literal))),
        TurtleValue::ObjectList,
    )(s)
}

fn predicate_lists<'a, F>(
    mut subject_extractor: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, TurtleValue<'a>>
where
    F: FnMut(&'a str) -> IResult<&'a str, TurtleValue<'a>>,
{
    move |s| {
        let (remaining, subject) = subject_extractor(s)?; // TODO handle other cases

        let (remaining, list) = preceded(
            multispace0,
            separated_list0(
                delimited(multispace0, tag(";"), multispace0),
                map(
                    pair(extract_iri, alt((extract_object_lists, extract_iri))),
                    |(predicate, objects)| TurtleValue::PredicateObjectList {
                        predicate: Box::new(predicate),
                        object_list: Box::new(objects),
                    },
                ),
            ),
        )(remaining)?;
        // let (remaining, _) = preceded(multispace0, alt((tag("."), eof)))(remaining)?;

        Ok((
            remaining,
            TurtleValue::Statement {
                subject: Box::new(subject),
                predicates: list,
            },
        ))
    }
}

#[cfg(test)]
mod test {
    use crate::turtle::model::{BlankNode, Iri};
    use crate::turtle::parsing::{extract_labeled_bnode, extract_iri};

    use super::{extract_base, extract_prefix, extract_prefixed_iri, TurtleValue, extract_unlabeled_bnode};
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
            TurtleValue::Base(Box::new(TurtleValue::Iri(Iri::Enclosed(
                "http://one.example/turtle"
            )))),
            base_turtle
        );

        let (remaining, base_sparql) = extract_base(base_sparql).unwrap();
        assert_eq!(
            TurtleValue::Base(Box::new(TurtleValue::Iri(Iri::Enclosed(
                "http://one.example/sparql"
            )))),
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
            TurtleValue::Prefix((
                "p",
                Box::new(TurtleValue::Iri(Iri::Enclosed("http://two.example/turtle")))
            )),
            prefix_turtle
        );
        let (remaining, prefix_empty_turtle) = extract_prefix(prefix_empty_turtle).unwrap();
        assert_eq!(
            TurtleValue::Prefix((
                "",
                Box::new(TurtleValue::Iri(Iri::Enclosed("http://two.example/empty")))
            )),
            prefix_empty_turtle
        );

        let (remaining, prefix_sparql) = extract_prefix(prefix_sparql).unwrap();
        assert_eq!(
            TurtleValue::Prefix((
                "p",
                Box::new(TurtleValue::Iri(Iri::Enclosed("http://two.example/sparql")))
            )),
            prefix_sparql
        );
    }

    #[test]
    fn extract_prefixed_iri_test() {
        let s = "foaf:firstName";
        let res = extract_prefixed_iri(s);
        assert_eq!(
            Ok((
                "",
                TurtleValue::Iri(Iri::Prefixed {
                    prefix: "foaf",
                    local_name: "firstName",
                }),
            ),),
            res
        );
    }
    #[test]
    fn extract_labeled_bnode_test() {
        let s = "_:alice";
        let res = extract_labeled_bnode(s);
        assert_eq!(
            Ok(("", TurtleValue::BNode(BlankNode::Labeled("alice")))),
            res
        );
    }
   
    #[test]
    fn predicate_lists_test() {
        let s = r#"
            <http://en.wikipedia.org/wiki/Helium>
            <http://example.org/elements/atomicNumber>  2 ;
            <http://example.org/elements/atomicMass> 4.002602 ;
            <http://example.org/elements/isOk> true ;
            <http://example.org/elements/isNotOk> false ;
            <http://example.org/elements/specificGravity> 1.663E-4 .
        "#;
        let (remaining, res) = predicate_lists(extract_iri)(s).unwrap();
        dbg!(&res);
        let s = r#"
            _:helium <http://example.org/elements/atomicNumber>  "2".
        "#;
        let (remaining, res) = predicate_lists(extract_iri)(s).unwrap();
        dbg!(&res);

        let s = r#"
            <http://en.wikipedia.org/wiki/Helium>  <http://example.org/elements/atomicNumber>  "2".
            <http://en.wikipedia.org/wiki/Helium>  <http://example.org/elements/atomicNumber>  "2".
        "#;
        let (remaining, res) = predicate_lists(extract_iri)(s).unwrap();
        let s = r#"
            <http://example.org/#spiderman> <http://xmlns.com/foaf/0.1/name> "Spiderman", "Человек-паук"@ru .
        "#;
        let (remaining, res) = predicate_lists(extract_iri)(s).unwrap();
        dbg!(res);
    }

    #[test]
    fn unlabeled_nested_bnode(){
        let s = r#"
        [ foaf:name "Alice" ] foaf:knows [
    foaf:name "Bob" ;
    foaf:knows [
        foaf:name "Eve" ] ;
    foaf:mbox <bob@example.com> ] .
				
        "#;
       let res = predicate_lists(extract_iri)(s);
       dbg!(res);
    }
}
