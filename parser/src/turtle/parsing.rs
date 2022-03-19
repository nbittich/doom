use std::collections::HashMap;
use std::ops::RangeBounds;

pub use crate::grammar::*;
pub use crate::prelude::*;
use crate::shared::NS_TYPE;

use crate::{
    shared::{LANG_LITERAL, RDF_NIL, XSD_STRING},
    turtle::model::Literal,
};

use super::model::{
    BlankNode, Iri, TurtleValue, BASE_SPARQL, BASE_TURTLE, PREFIX_SPARQL, PREFIX_TURTLE,
};
use super::turtle_doc::TurtleDoc;

fn comments(s: &str) -> IResult<&str, Vec<&str>> {
    many0(delimited(
        multispace0,
        preceded(char('#'), take_until("\n")),
        line_ending,
    ))(s)
}

// BASE & PREFIX
// -----------------------------------------------------------------------------------------------------------------------------------------
fn base(s: &str) -> IResult<&str, TurtleValue<'_>> {
    let (remaining, base) = preceded(
        multispace0,
        tag_no_case(BASE_SPARQL).or(tag_no_case(BASE_TURTLE)),
    )(s)?;
    match base {
        BASE_SPARQL => map(enclosed_iri, |iri| TurtleValue::Base(Box::new(iri)))(remaining),
        BASE_TURTLE => map(
            terminated(enclosed_iri, preceded(multispace0, char('.'))),
            |iri| TurtleValue::Base(Box::new(iri)),
        )(remaining),
        _ => {
            let err: Error<&str> = make_error(base, ErrorKind::IsNot);
            Err(nom::Err::Error(err))
        }
    }
}
fn prefix(s: &str) -> IResult<&str, TurtleValue<'_>> {
    let (remaining, prefix) = preceded(
        multispace0,
        tag_no_case(PREFIX_SPARQL).or(tag_no_case(PREFIX_TURTLE)),
    )(s)?;
    let mut get_prefix = preceded(
        multispace0,
        map(
            separated_pair(take_until(":"), tag(":"), enclosed_iri),
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

fn object_lists(s: &str) -> IResult<&str, TurtleValue> {
    let (remaining, mut list) = separated_list1(char(','), object)(s)?;
    if list.len() > 1 {
        return Ok((remaining, TurtleValue::ObjectList(list)));
    } else {
        if let Some(single_value) = list.pop() {
            return Ok((remaining, single_value));
        } else {
            let err: Error<&str> = make_error(s, ErrorKind::LengthValue);
            return Err(nom::Err::Error(err));
        }
    }
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
            separated_list1(
                delimited(multispace0, tag(";"), comments),
                map(pair(predicate, object_lists), |(predicate, objects)| {
                    TurtleValue::PredicateObject {
                        predicate: Box::new(predicate),
                        object: Box::new(objects),
                    }
                }),
            ),
        )(remaining)?;
        Ok((
            remaining,
            TurtleValue::Statement {
                subject: Box::new(subject),
                predicate_objects: list,
            },
        ))
    }
}
// new version

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
        all_consuming(I64)(s)
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

fn primitive_literal(s: &str) -> IResult<&str, TurtleValue> {
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
fn string_literal(s: &str) -> IResult<&str, TurtleValue> {
    let mut single_quote_literal = delimited(
        tag(STRING_LITERAL_SINGLE_QUOTE),
        take_until1(STRING_LITERAL_SINGLE_QUOTE),
        tag(STRING_LITERAL_SINGLE_QUOTE),
    );
    let mut double_quote_literal = delimited(
        tag(STRING_LITERAL_QUOTE),
        take_until1(STRING_LITERAL_QUOTE),
        tag(STRING_LITERAL_QUOTE),
    );
    let mut long_single_quote_literal = delimited(
        tag(STRING_LITERAL_LONG_SINGLE_QUOTE),
        take_until1(STRING_LITERAL_LONG_SINGLE_QUOTE),
        tag(STRING_LITERAL_LONG_SINGLE_QUOTE),
    );
    let mut long_quote_literal = delimited(
        tag(STRING_LITERAL_LONG_QUOTE),
        take_until1(STRING_LITERAL_LONG_QUOTE),
        tag(STRING_LITERAL_LONG_QUOTE),
    );
    let mut datatype = preceded(tag("^^"), iri);

    fn lang(s: &str) -> IResult<&str, &str> {
        preceded(tag(LANGTAG), take_while(|a: char| a.is_alpha() || a == '-'))(s)
    }

    let (no_white_space, _) = multispace0(s)?;
    let (remaining, string_literal) = alt((
        single_quote_literal,
        double_quote_literal,
        long_quote_literal,
        long_single_quote_literal,
    ))(no_white_space)?;

    if let Ok((remaining, datatype)) = datatype(remaining) {
        Ok((
            remaining,
            TurtleValue::Literal(Literal::Quoted {
                datatype: Some(Box::new(datatype)),
                value: string_literal,
                lang: None,
            }),
        ))
    } else if let Ok((remaining, lang)) = lang(remaining) {
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

fn literal(s: &str) -> IResult<&str, TurtleValue> {
    alt((string_literal, primitive_literal))(s)
}
// -----------------------------------------------------------------------------------------------------------------------------------------
fn prefixed_iri(s: &str) -> IResult<&str, TurtleValue> {
    let mut prefixed = map(
        separated_pair(
            take_while(|s: char| s.is_alphanumeric()),
            tag(":"),
            take_while(|s: char| s.is_alphanumeric() || PN_LOCAL_ESC.contains(&s)),
        ),
        |(prefix, local_name)| Iri::Prefixed { prefix, local_name },
    );
    map(preceded(multispace0, prefixed), TurtleValue::Iri)(s)
}

fn collection(s: &str) -> IResult<&str, TurtleValue> {
    let (remaining, _) = multispace0(s)?;
    let (remaining, res) = preceded(
        char('('),
        terminated(
            separated_list0(multispace1, object),
            preceded(multispace0, cut(char(')'))),
        ),
    )(remaining)?;
    if res.is_empty() {
        Ok((remaining, TurtleValue::Iri(Iri::Enclosed(RDF_NIL))))
    } else {
        Ok((remaining, TurtleValue::Collection(res)))
    }
}

fn anon_bnode(s: &str) -> IResult<&str, TurtleValue> {
    let unlabeled_subject = |s| Ok((s, TurtleValue::BNode(BlankNode::Unlabeled)));
    let mut extract = preceded(
        char('['),
        terminated(
            alt((predicate_lists(unlabeled_subject), unlabeled_subject)),
            preceded(multispace0, cut(char(']'))),
        ),
    );
    preceded(multispace0, extract)(s) // todo probably multispace here useless
}
fn labeled_bnode(s: &str) -> IResult<&str, TurtleValue> {
    let mut parse_labeled_bnode = delimited(
        tag(BLANK_NODE_LABEL),
        take_while(|s: char| !s.is_whitespace()),
        space0,
    );
    let (remaining, _) = multispace0(s)?; // todo maybe remove this
    let (remaining, label) = parse_labeled_bnode(remaining)?;
    if label.starts_with('.') || label.ends_with('.') || label.starts_with('-') {
        let err: Error<&str> = make_error(label, ErrorKind::IsNot);
        return Err(nom::Err::Error(err));
    }
    Ok((remaining, TurtleValue::BNode(BlankNode::Labeled(label))))
}

fn blank_node(s: &str) -> IResult<&str, TurtleValue> {
    alt((labeled_bnode, anon_bnode))(s)
}

fn enclosed_iri(s: &str) -> IResult<&str, TurtleValue> {
    let mut enclosed = map(
        delimited(char('<'), take_while(|s: char| s != '>'), char('>')),
        Iri::Enclosed,
    );

    map(preceded(multispace0, enclosed), TurtleValue::Iri)(s)
}

fn iri(s: &str) -> IResult<&str, TurtleValue> {
    alt((prefixed_iri, enclosed_iri))(s)
}

fn subject(s: &str) -> IResult<&str, TurtleValue> {
    alt((blank_node, iri, collection))(s)
}
fn predicate(s: &str) -> IResult<&str, TurtleValue> {
    alt((
        map(terminated(char('a'), multispace1), |s| {
            TurtleValue::Iri(Iri::Enclosed(NS_TYPE))
        }),
        iri,
    ))(s)
}
fn object(s: &str) -> IResult<&str, TurtleValue> {
    alt((iri, blank_node, collection, literal))(s) // NORDIne
}

fn triples(s: &str) -> IResult<&str, TurtleValue> {
    terminated(predicate_lists(subject), preceded(multispace0, tag(".")))(s)
}

fn directive(s: &str) -> IResult<&str, TurtleValue> {
    alt((base, prefix))(s)
}

fn statement(s: &str) -> IResult<&str, TurtleValue> {
    preceded(comments, alt((directive, triples)))(s)
}

fn turtle_doc(s: &str) -> IResult<&str, TurtleDoc> {
    map(many0(statement), TurtleDoc)(s)
}

#[cfg(test)]
mod test {
    use crate::turtle::model::{BlankNode, Iri};
    use crate::turtle::parsing::{iri, labeled_bnode, subject, triples};

    use super::{anon_bnode, base, collection, prefix, prefixed_iri, turtle_doc, TurtleValue};
    use std::collections::HashMap;
    use std::rc::Rc;
    use crate::turtle::turtle_doc::Model;

    use super::predicate_lists;

    #[test]
    fn base_test() {
        let base_sparql = r#"
              BASE   <http://one.example/sparql>

        "#;
        let base_turtle = r#"

             @base    <http://one.example/turtle> .
        "#;

        let (remaining, base_turtle) = base(base_turtle).unwrap();
        assert_eq!(
            TurtleValue::Base(Box::new(TurtleValue::Iri(Iri::Enclosed(
                "http://one.example/turtle"
            )))),
            base_turtle
        );

        let (remaining, base_sparql) = base(base_sparql).unwrap();
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

        let (remaining, prefix_turtle) = prefix(prefix_turtle).unwrap();
        assert_eq!(
            TurtleValue::Prefix((
                "p",
                Box::new(TurtleValue::Iri(Iri::Enclosed("http://two.example/turtle")))
            )),
            prefix_turtle
        );
        let (remaining, prefix_empty_turtle) = prefix(prefix_empty_turtle).unwrap();
        assert_eq!(
            TurtleValue::Prefix((
                "",
                Box::new(TurtleValue::Iri(Iri::Enclosed("http://two.example/empty")))
            )),
            prefix_empty_turtle
        );

        let (remaining, prefix_sparql) = prefix(prefix_sparql).unwrap();
        assert_eq!(
            TurtleValue::Prefix((
                "p",
                Box::new(TurtleValue::Iri(Iri::Enclosed("http://two.example/sparql")))
            )),
            prefix_sparql
        );
    }

    #[test]
    fn prefixed_iri_test() {
        let s = "foaf:firstName";
        let res = prefixed_iri(s);
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
    fn labeled_bnode_test() {
        let s = "_:alice";
        let res = labeled_bnode(s);
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
        let (remaining, res) = triples(s).unwrap();
        dbg!(&res);
        let s = r#"
            _:helium <http://example.org/elements/atomicNumber>  "2".
        "#;
        let (remaining, res) = triples(s).unwrap();
        dbg!(&res);

        let s = r#"
            <http://en.wikipedia.org/wiki/Helium>  <http://example.org/elements/atomicNumber>  "2".
            <http://en.wikipedia.org/wiki/Helium>  <http://example.org/elements/atomicNumber>  "2".
        "#;
        let (remaining, res) = triples(s).unwrap();
        let s = r#"
            <http://example.org/#spiderman> <http://xmlns.com/foaf/0.1/name> "Spiderman", "Человек-паук"@ru .
        "#;
        let (remaining, res) = triples(s).unwrap();
        dbg!(res);
        let s = r#"
            <http://example.org/#spiderman> a person:Person,skos:Concept.
        "#;
        let res = triples(s);
        dbg!(res);
    }

    #[test]
    fn unlabeled_nested_bnode() {
        let s = r#"
        [ foaf:name "Alice" ] foaf:knows [
    foaf:name "Bob" ;
    foaf:lastName "George", "Joshua" ;
    foaf:knows [
        foaf:name "Eve" ] ;
    foaf:mbox <bob@example.com>] .
				
        "#;
        let res = triples(s);
        dbg!(res);

        let s = r#"[] foaf:knows [foaf:name "Bob"] ."#;
        let res = triples(s);
        dbg!(res);
    }

    #[test]
    fn collection_test() {
        let s = r#":a :b ( "apple" "banana" ) ."#;
        let res = triples(s);
        dbg!(res);
        let s = r#"(1 2.0 3E1) :p "w" ."#;
        let res = triples(s);
        dbg!(res);
        let s = r#"(1 [:p :q] ( 2 ) ) :p2 :q2 ."#;
        let res = triples(s);
        dbg!(res);
        let s = r#":subject :predicate2 () ."#;
        let res = triples(s);
        dbg!(res);
    }

    #[test]
    fn turtle_doc_test() {
        let doc = include_str!("./example/turtle_doc.ttl");
        let (remaining, turtle) = turtle_doc(doc).unwrap();
        let model = Model::new(turtle.0);
        dbg!(model);
    }
    #[test]
    fn turtle_doc_bnode_test() {
        let doc =  r#"
        @prefix foaf: <http://foaf.com/>.
        [ foaf:name "Alice" ] foaf:knows [
    foaf:name "Bob" ;
    foaf:lastName "George", "Joshua" ;
    foaf:knows [
        foaf:name "Eve" ] ;
    foaf:mbox <bob@example.com>] .

        "#;
        let (remaining, turtle) = turtle_doc(doc).unwrap();
        let model = Model::new(turtle.0);
        dbg!(model);
    }
}
