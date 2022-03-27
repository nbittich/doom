use crate::prelude::*;
use crate::shared::RDF_NIL;
use crate::sparql::common::var;
use crate::sparql::expression::Expr;
use crate::sparql::path::{group, iri, negate, path, Path};
use crate::triple_common_parser::literal::literal;
use crate::triple_common_parser::prologue::{base_sparql, prefix_sparql};
use crate::triple_common_parser::triple::{
    anon_bnode, collection, labeled_bnode, ns_type, object_list, predicate_list,
};
use crate::triple_common_parser::{comments, BlankNode, Iri, Literal};
use std::collections::VecDeque;

#[derive(Debug, PartialEq)]
pub enum SparqlValue<'a> {
    Variable(&'a str),
    BNode(BlankNode<'a>),
    Collection(VecDeque<SparqlValue<'a>>),
    Base(Iri<'a>),
    Filter(Expr<'a>),
    Literal(Literal<'a>),
    Prefix((&'a str, Iri<'a>)),
    Path(Path<'a>),
    ObjectList(Vec<SparqlValue<'a>>),
    PredicateObject {
        predicate: Box<SparqlValue<'a>>,
        object: Box<SparqlValue<'a>>,
    },
    TriplePattern {
        subject: Box<SparqlValue<'a>>,
        predicate_objects: Vec<SparqlValue<'a>>,
    },
    GraphPattern {
        graph: Box<SparqlValue<'a>>,
        block: Box<SparqlValue<'a>>,
    },
    Block(Vec<SparqlValue<'a>>),
    Prologue(Vec<SparqlValue<'a>>),
}

fn variable(s: &str) -> ParserResult<SparqlValue> {
    map(var, SparqlValue::Variable)(s)
}
fn directive(s: &str) -> ParserResult<SparqlValue> {
    alt((
        map(base_sparql, SparqlValue::Base),
        map(prefix_sparql, SparqlValue::Prefix),
    ))(s)
}

fn graph_pattern(s: &str) -> ParserResult<SparqlValue> {
    let extract_graph_word = preceded(terminated(comments, multispace0), tag_no_case("graph"));
    preceded(
        extract_graph_word,
        map(
            pair(alt((variable, map(iri, SparqlValue::Path))), block),
            |(g, b)| SparqlValue::GraphPattern {
                graph: Box::new(g),
                block: Box::new(b),
            },
        ),
    )(s)
}
fn prologue(s: &str) -> ParserResult<SparqlValue> {
    map(
        many0(preceded(multispace0, directive)),
        SparqlValue::Prologue,
    )(s)
}

fn object(s: &str) -> ParserResult<SparqlValue> {
    alt((
        map(iri, SparqlValue::Path),
        variable,
        map(literal, SparqlValue::Literal),
        blank_node,
        collection_sparql,
    ))(s)
}
fn subject(s: &str) -> ParserResult<SparqlValue> {
    alt((
        variable,
        blank_node,
        map(iri, SparqlValue::Path),
        collection_sparql,
    ))(s)
}

fn triple_pattern(s: &str) -> ParserResult<SparqlValue> {
    terminated(
        preceded(
            comments,
            terminated(
                predicate_lists(subject),
                preceded(multispace0, opt(tag("."))),
            ),
        ),
        comments,
    )(s)
}

fn block(s: &str) -> ParserResult<SparqlValue> {
    delimited(
        preceded(multispace0, tag("{")),
        map(many0(alt((triple_pattern, block))), SparqlValue::Block),
        preceded(multispace0, tag("}")),
    )(s)
}

fn collection_sparql(s: &str) -> ParserResult<SparqlValue> {
    map(collection(object), |res: VecDeque<SparqlValue>| {
        if res.is_empty() {
            SparqlValue::Path(Path::Iri(Iri::Enclosed(RDF_NIL)))
        } else {
            SparqlValue::Collection(res)
        }
    })(s)
}

fn blank_node(s: &str) -> ParserResult<SparqlValue> {
    alt((map(labeled_bnode, SparqlValue::BNode), anon_bnode_sparql))(s)
}

fn anon_bnode_sparql(s: &str) -> ParserResult<SparqlValue> {
    let anon_parser = |s| {
        let unlabeled_subject = |s| Ok((s, SparqlValue::BNode(BlankNode::Unlabeled)));
        alt((predicate_lists(unlabeled_subject), unlabeled_subject))(s)
    };
    anon_bnode(anon_parser)(s)
}

fn object_lists(s: &str) -> ParserResult<SparqlValue> {
    object_list(object, SparqlValue::ObjectList)(s)
}
fn predicate(s: &str) -> ParserResult<SparqlValue> {
    alt((
        map(ns_type, |iri| SparqlValue::Path(Path::Iri(iri))),
        map(alt((negate, group, path)), SparqlValue::Path),
        variable,
    ))(s)
}

fn predicate_lists<'a, F>(subject_extractor: F) -> impl FnMut(&'a str) -> ParserResult<SparqlValue>
where
    F: Fn(&'a str) -> ParserResult<SparqlValue>,
{
    let map_predicate_object = |(predicate, objects)| SparqlValue::PredicateObject {
        predicate: Box::new(predicate),
        object: Box::new(objects),
    };
    predicate_list(
        subject_extractor,
        predicate,
        object_lists,
        map_predicate_object,
        |subject, list| SparqlValue::TriplePattern {
            subject: Box::new(subject),
            predicate_objects: list,
        },
    )
}

#[cfg(test)]
mod test {

    use crate::shared::NS_TYPE;
    use crate::sparql::path::Path;
    use crate::sparql::sparql_parser::BlankNode;

    use crate::sparql::sparql_parser::SparqlValue::{
        Block, GraphPattern, PredicateObject, TriplePattern, Variable,
    };
    use crate::sparql::sparql_parser::{
        block, directive, graph_pattern, prologue, variable, SparqlValue,
    };
    use crate::triple_common_parser::Iri::Enclosed;
    macro_rules! a_box {
        ($a:expr) => {
            Box::new($a)
        };
    }
    #[test]
    fn test_variable() {
        let s = "?pxx ";
        assert_eq!(Variable("pxx"), variable(s).unwrap().1);
        let s = "$x ";
        assert_eq!(Variable("x"), variable(s).unwrap().1);
    }
    #[test]
    fn test_directive() {
        let s = "BASE <http://xxx.com>";
        assert_eq!(
            SparqlValue::Base(Enclosed("http://xxx.com")),
            directive(s).unwrap().1
        );
        let s = "PREFIX p: <http://xxx.com>";
        assert_eq!(
            SparqlValue::Prefix(("p", Enclosed("http://xxx.com"))),
            directive(s).unwrap().1
        );
    }
    #[test]
    fn test_prologue() {
        let s = r#"
        BASE <http://xxx.com>
        PREFIX s: <http://sss.com>
        PREFIX p: <http://ppp.com>
        PREFIX q: <http://qqq.com>
        PREFIX r: <http://rrr.com>
        "#;
        let res = prologue(s).unwrap();
        if let SparqlValue::Prologue(vec) = res.1 {
            assert_eq!(5, vec.len());
            assert!(vec
                .iter()
                .find(|p| *p == &SparqlValue::Base(Enclosed("http://xxx.com")))
                .is_some());
            assert!(vec
                .iter()
                .find(|p| *p == &SparqlValue::Prefix(("s", Enclosed("http://sss.com"))))
                .is_some());
            assert!(vec
                .iter()
                .find(|p| *p == &SparqlValue::Prefix(("p", Enclosed("http://ppp.com"))))
                .is_some());
            assert!(vec
                .iter()
                .find(|p| *p == &SparqlValue::Prefix(("q", Enclosed("http://qqq.com"))))
                .is_some());
            assert!(vec
                .iter()
                .find(|p| *p == &SparqlValue::Prefix(("r", Enclosed("http://rrr.com"))))
                .is_some());
        } else {
            panic!("not prologue");
        }
    }

    #[test]
    fn test_block() {
        let s = r#"
            {
                #comment
                ?s ?p ?o; #comment
                ?y [?x ?z] .#comment
                #comment
                {
                #comment
                    ?v ?w ?z #comment
                }
            }
        "#;
        let (_, block) = block(s).unwrap();
        assert_eq!(
            SparqlValue::Block(vec![
                SparqlValue::TriplePattern {
                    subject: a_box!(Variable("s")),
                    predicate_objects: vec![
                        PredicateObject {
                            predicate: a_box!(Variable("p")),
                            object: a_box!(Variable("o"))
                        },
                        PredicateObject {
                            predicate: a_box!(Variable("y")),
                            object: a_box!(TriplePattern {
                                subject: a_box!(SparqlValue::BNode(BlankNode::Unlabeled)),
                                predicate_objects: vec![PredicateObject {
                                    predicate: a_box!(Variable("x")),
                                    object: a_box!(Variable("z"))
                                },]
                            })
                        },
                    ]
                },
                Block(vec![TriplePattern {
                    subject: a_box!(SparqlValue::Variable("v")),
                    predicate_objects: vec![PredicateObject {
                        predicate: a_box!(Variable("w")),
                        object: a_box!(Variable("z"))
                    },]
                }])
            ]),
            block
        );
    }
    #[test]
    fn test_graph_pattern() {
        let s = r#"
        # a comment
            Graph ?g { ?s a <http://whatsup.com/X>} # a comment
        "#;
        let (_, gp) = graph_pattern(s).unwrap();
        assert_eq!(
            GraphPattern {
                graph: a_box!(Variable("g")),
                block: a_box!(Block(vec![TriplePattern {
                    subject: a_box!(Variable("s")),
                    predicate_objects: vec![PredicateObject {
                        predicate: a_box!(SparqlValue::Path(Path::Iri(Enclosed(NS_TYPE)))),
                        object: a_box!(SparqlValue::Path(Path::Iri(Enclosed(
                            "http://whatsup.com/X"
                        ))))
                    }]
                }]))
            },
            gp
        );

        let s = r#"
        # a comment
            GRAPH <http://ggg.com>{
            ?s ?p ?o.
            } # a comment
        "#;
        let (_, gp) = graph_pattern(s).unwrap();
        assert_eq!(
            GraphPattern {
                graph: a_box!(SparqlValue::Path(Path::Iri(Enclosed("http://ggg.com")))),
                block: a_box!(Block(vec![TriplePattern {
                    subject: a_box!(Variable("s")),
                    predicate_objects: vec![PredicateObject {
                        predicate: a_box!(Variable("p")),
                        object: a_box!(Variable("o"))
                    }]
                }]))
            },
            gp
        );
    }
}
