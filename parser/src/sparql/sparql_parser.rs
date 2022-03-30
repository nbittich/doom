use crate::prelude::*;
use crate::shared::RDF_NIL;
use crate::sparql::built_in::built_in_call;
use crate::sparql::common::{tag_no_case_no_space, tag_no_space, var};
use crate::sparql::expression::{as_expr, bracketed, expr, Expr};
use crate::sparql::path::{group, iri, negate, path, Path};
use crate::triple_common_parser::iri::enclosed_iri;
use crate::triple_common_parser::literal::literal_sparql as literal;
use crate::triple_common_parser::prologue::{base_sparql, prefix_sparql};
use crate::triple_common_parser::triple::{
    anon_bnode, collection, labeled_bnode, ns_type, object_list, predicate_list,
};
use crate::triple_common_parser::{comments, BlankNode, Iri, Literal};
use nom::multi::many1;
use std::collections::VecDeque;

#[derive(Debug, PartialEq)]
pub enum SparqlValue<'a> {
    Variable(&'a str),
    LimitOffsetClause {
        limit: Option<u32>,
        offset: Option<u32>,
    },
    SolutionModifier {
        group_clause: Option<Vec<SparqlValue<'a>>>,
        having_clause: Option<Vec<SparqlValue<'a>>>,
        order_clause: Option<Vec<SparqlValue<'a>>>,
        limit_clause: Option<Box<SparqlValue<'a>>>,
    },
    BuiltInCall(Expr<'a>),
    Constraint(Expr<'a>),
    Expr(Expr<'a>),
    Variables(Vec<SparqlValue<'a>>),
    All,
    BNode(BlankNode<'a>),
    Collection(VecDeque<SparqlValue<'a>>),
    Base(Iri<'a>),
    As(Expr<'a>),
    Bind(Box<SparqlValue<'a>>),
    SelectClause {
        distinct: bool,
        reduced: bool,
        variables: Box<SparqlValue<'a>>,
    },
    Filter(Expr<'a>),
    From(Iri<'a>),
    FromNamed(Iri<'a>),
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
fn filter(s: &str) -> ParserResult<SparqlValue> {
    map(
        remove_comments(preceded(
            tag_no_case_no_space("FILTER"),
            terminated(expr, comments),
        )),
        SparqlValue::Filter,
    )(s)
}

fn directive(s: &str) -> ParserResult<SparqlValue> {
    alt((
        map(base_sparql, SparqlValue::Base),
        map(prefix_sparql, SparqlValue::Prefix),
    ))(s)
}
fn remove_comments<'a, F, E>(f: F) -> impl FnMut(&'a str) -> ParserResult<E>
where
    F: FnMut(&'a str) -> ParserResult<E>,
{
    preceded(terminated(comments, multispace0), f)
}

fn graph_pattern(s: &str) -> ParserResult<SparqlValue> {
    preceded(
        remove_comments(tag_no_case_no_space("graph")),
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
    remove_comments(terminated(predicate_lists(subject), opt(tag_no_space("."))))(s)
}

fn block(s: &str) -> ParserResult<SparqlValue> {
    delimited(
        remove_comments(tag_no_space("{")),
        map(
            many0(alt((triple_pattern, filter, bind, block))),
            SparqlValue::Block,
        ),
        remove_comments(tag_no_space("}")),
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
fn from(s: &str) -> ParserResult<SparqlValue> {
    preceded(
        tag_no_case_no_space("FROM"),
        map(
            pair(opt(tag_no_case_no_space("NAMED")), enclosed_iri),
            |(named, iri)| {
                if named.is_some() {
                    SparqlValue::FromNamed(iri)
                } else {
                    SparqlValue::From(iri)
                }
            },
        ),
    )(s)
}
fn group_by(s: &str) -> ParserResult<Vec<SparqlValue>> {
    preceded(
        tag_no_case_no_space("GROUP BY"),
        many1(alt((
            map(built_in_call, SparqlValue::BuiltInCall),
            as_exp,
            variable,
        ))),
    )(s)
}
fn constraint(s: &str) -> ParserResult<SparqlValue> {
    map(alt((bracketed, built_in_call)), SparqlValue::Constraint)(s) // todo should be just bracketed + built_in_call + function_call
}
fn having(s: &str) -> ParserResult<Vec<SparqlValue>> {
    preceded(
        tag_no_case_no_space("HAVING"),
        many1(alt((constraint, as_exp, variable))),
    )(s)
}
fn order_by(s: &str) -> ParserResult<Vec<SparqlValue>> {
    preceded(
        tag_no_case_no_space("ORDER BY"),
        many1(alt((
            alt((
                map(
                    preceded(tag_no_case_no_space("ASC"), bracketed),
                    SparqlValue::Expr,
                ),
                map(
                    preceded(tag_no_case_no_space("DESC"), bracketed),
                    SparqlValue::Expr,
                ),
            )),
            constraint,
            variable,
        ))),
    )(s)
}
fn limit_clause(s: &str) -> ParserResult<SparqlValue> {
    let limit_clause = || preceded(tag_no_case_no_space("LIMIT"), U32);
    let offset_clause = || preceded(tag_no_case_no_space("OFFSET"), U32);

    alt((
        map(
            pair(limit_clause(), opt(offset_clause())),
            |(limit, offset)| SparqlValue::LimitOffsetClause {
                limit: Some(limit),
                offset,
            },
        ),
        map(
            pair(offset_clause(), opt(limit_clause())),
            |(offset, limit)| SparqlValue::LimitOffsetClause {
                limit,
                offset: Some(offset),
            },
        ),
    ))(s)
}

fn solution_modifier(s: &str) -> ParserResult<SparqlValue> {
    map(
        tuple((opt(group_by), opt(having), opt(order_by), opt(limit_clause))),
        |(gc, h, order, limit)| SparqlValue::SolutionModifier {
            group_clause: gc,
            having_clause: h,
            limit_clause: limit.map(Box::new),
            order_clause: order,
        },
    )(s)
}
fn select_clause(s: &str) -> ParserResult<SparqlValue> {
    preceded(
        tag_no_case_no_space("SELECT"),
        map_res(
            tuple((
                opt(tag_no_case_no_space("DISTINCT")),
                opt(tag_no_case_no_space("REDUCED")),
                variables_or_all,
            )),
            |(distinct, reduced, variables)| {
                if distinct.is_some() && reduced.is_some() {
                    let err: Error<&str> = make_error(s, ErrorKind::Fail);
                    Err(nom::Err::Error(err))
                } else {
                    Ok(SparqlValue::SelectClause {
                        distinct: distinct.is_some(),
                        reduced: reduced.is_some(),
                        variables: Box::new(variables),
                    })
                }
            },
        ),
    )(s)
}
fn where_clause(s: &str) -> ParserResult<SparqlValue> {
    todo!()
}

fn all(s: &str) -> ParserResult<SparqlValue> {
    map(tag_no_case_no_space("*"), |_| SparqlValue::All)(s)
}

fn variables_or_all(s: &str) -> ParserResult<SparqlValue> {
    alt((
        all,
        map(many1(alt((variable, as_exp))), SparqlValue::Variables),
    ))(s)
}

fn as_exp(s: &str) -> ParserResult<SparqlValue> {
    map(as_expr, |expression| SparqlValue::As(expression))(s)
}
fn bind(s: &str) -> ParserResult<SparqlValue> {
    preceded(
        tag_no_case_no_space("BIND"),
        map(as_exp, |expression| SparqlValue::Bind(Box::new(expression))),
    )(s)
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
    use crate::sparql::expression::{ArithmeticOperator, BuiltInCall, Expr, RelationalOperator};
    use crate::sparql::path::Path;
    use crate::sparql::sparql_parser::{bind, from, select_clause, solution_modifier, BlankNode};

    use crate::sparql::sparql_parser::SparqlValue::{
        Block, GraphPattern, PredicateObject, TriplePattern, Variable,
    };
    use crate::sparql::sparql_parser::{
        block, directive, graph_pattern, prologue, variable, SparqlValue,
    };
    use crate::triple_common_parser::Iri::Enclosed;
    use crate::triple_common_parser::Iri::Prefixed;
    use crate::triple_common_parser::Literal;
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
    #[test]
    fn test_graph_filter() {
        let s = r#"
        # a comment
            GRAPH <http://ggg.com>{
            ?s ?p ?o.
            # a comment FILTER (?mbox1 = ?mbox2 && ?name1 != ?name2)# a comment
            FILTER (?mbox1 = ?mbox2 && ?name1 != ?name2)# a comment
            } # a comment
        "#;
        let (_, gp) = graph_pattern(s).unwrap();
        assert_eq!(
            gp,
            GraphPattern {
                graph: a_box!(SparqlValue::Path(Path::Iri(Enclosed("http://ggg.com",),),)),
                block: a_box!(Block(vec![
                    TriplePattern {
                        subject: a_box!(Variable("s",)),
                        predicate_objects: vec![PredicateObject {
                            predicate: a_box!(Variable("p",)),
                            object: a_box!(Variable("o",)),
                        },],
                    },
                    SparqlValue::Filter(Expr::Bracketed(a_box!(Expr::ConditionalAnd {
                        left: a_box!(Expr::Relational {
                            left: a_box!(Expr::Variable("mbox1",)),
                            operator: RelationalOperator::Equals,
                            right: a_box!(Expr::Variable("mbox2",)),
                        }),
                        right: a_box!(Expr::Relational {
                            left: a_box!(Expr::Variable("name1",)),
                            operator: RelationalOperator::Diff,
                            right: a_box!(Expr::Variable("name2",)),
                        }),
                    })),),
                ],)),
            }
        );
        let s = r#"
        #comment
        graph ?g { ?x foaf:name  ?name ;
        foaf:mbox  ?mbox .
        # acomment
         FILTER isLiteral (?mbox)#comment
          }
        "#;
        let (_, gp) = graph_pattern(s).unwrap();
        assert_eq!(
            GraphPattern {
                graph: a_box!(Variable("g",)),
                block: a_box!(Block(vec![
                    TriplePattern {
                        subject: a_box!(Variable("x",)),
                        predicate_objects: vec![
                            PredicateObject {
                                predicate: a_box!(SparqlValue::Path(Path::Iri(Prefixed {
                                    prefix: "foaf",
                                    local_name: "name",
                                },),)),
                                object: a_box!(Variable("name",)),
                            },
                            PredicateObject {
                                predicate: a_box!(SparqlValue::Path(Path::Iri(Prefixed {
                                    prefix: "foaf",
                                    local_name: "mbox",
                                },),)),
                                object: a_box!(Variable("mbox",)),
                            },
                        ],
                    },
                    SparqlValue::Filter(Expr::BuiltInCall(a_box!(BuiltInCall::IsLiteral(
                        Expr::Variable("mbox",),
                    )),),),
                ],)),
            },
            gp
        );
    }

    #[test]
    fn test_bind() {
        let s = " BIND (?p*(1-?discount) AS ?price)";
        let (_, res) = bind(s).unwrap();
        assert_eq!(
            res,
            SparqlValue::Bind(a_box!(SparqlValue::As(Expr::AsExpr {
                expression: a_box!(Expr::Arithmetic {
                    left: a_box!(Expr::Variable("p",)),
                    operator: ArithmeticOperator::Multiply,
                    right: a_box!(Expr::Bracketed(a_box!(Expr::Arithmetic {
                        left: a_box!(Expr::Literal(Literal::Integer(1,),)),
                        operator: ArithmeticOperator::Subtract,
                        right: a_box!(Expr::Variable("discount",)),
                    }),)),
                }),
                variable: a_box!(Expr::Variable("price",)),
            })))
        )
    }

    #[test]
    fn test_graph_bind() {
        let s = r#"
        # a comment
            GRAPH <http://ggg.com>{
            ?s ?p ?o
            BIND (?p*(1-?discount) AS ?price)
            # a comment FILTER (?mbox1 = ?mbox2 && ?name1 != ?name2)# a comment
            FILTER (?mbox1 = ?mbox2 && ?name1 != ?name2)# a comment
            } # a comment
        "#;
        let (_, gp) = graph_pattern(s).unwrap();
        assert_eq!(
            gp,
            GraphPattern {
                graph: a_box!(SparqlValue::Path(Path::Iri(Enclosed("http://ggg.com",),),)),
                block: a_box!(Block(vec![
                    TriplePattern {
                        subject: a_box!(Variable("s",)),
                        predicate_objects: vec![PredicateObject {
                            predicate: a_box!(Variable("p",)),
                            object: a_box!(Variable("o",)),
                        },],
                    },
                    SparqlValue::Bind(a_box!(SparqlValue::As(Expr::AsExpr {
                        expression: a_box!(Expr::Arithmetic {
                            left: a_box!(Expr::Variable("p",)),
                            operator: ArithmeticOperator::Multiply,
                            right: a_box!(Expr::Bracketed(a_box!(Expr::Arithmetic {
                                left: a_box!(Expr::Literal(Literal::Integer(1,),)),
                                operator: ArithmeticOperator::Subtract,
                                right: a_box!(Expr::Variable("discount",)),
                            }),)),
                        }),
                        variable: a_box!(Expr::Variable("price",)),
                    }))),
                    SparqlValue::Filter(Expr::Bracketed(a_box!(Expr::ConditionalAnd {
                        left: a_box!(Expr::Relational {
                            left: a_box!(Expr::Variable("mbox1",)),
                            operator: RelationalOperator::Equals,
                            right: a_box!(Expr::Variable("mbox2",)),
                        }),
                        right: a_box!(Expr::Relational {
                            left: a_box!(Expr::Variable("name1",)),
                            operator: RelationalOperator::Diff,
                            right: a_box!(Expr::Variable("name2",)),
                        }),
                    })),),
                ],)),
            }
        );
    }

    #[test]
    fn test_from() {
        let s = "FROM NAMED <http://example.org/foaf/aliceFoaf>";
        let (_, named) = from(s).unwrap();
        assert_eq!(
            named,
            SparqlValue::FromNamed(Enclosed("http://example.org/foaf/aliceFoaf"))
        );
        let s = "FROM <http://example.org/foaf/aliceFoaf>";
        let (_, named) = from(s).unwrap();
        assert_eq!(
            named,
            SparqlValue::From(Enclosed("http://example.org/foaf/aliceFoaf"))
        );
    }
    #[test]
    fn test_select_clause() {
        let s = "SELECT DISTINCT ?name (<http://xx.com> as ?s) ?p ?o";
        let (_, clause) = select_clause(s).unwrap();
        assert_eq!(
            clause,
            SparqlValue::SelectClause {
                distinct: true,
                reduced: false,
                variables: a_box!(SparqlValue::Variables(vec![
                    Variable("name",),
                    SparqlValue::As(Expr::AsExpr {
                        expression: a_box!(Expr::Path(Path::Iri(Enclosed("http://xx.com",),),)),
                        variable: a_box!(Expr::Variable("s",)),
                    },),
                    Variable("p",),
                    Variable("o",),
                ],)),
            },
        );
        let s = "SELECT REDUCED ?name (<http://xx.com> as ?s) ?p ?o";
        let (_, clause) = select_clause(s).unwrap();
        assert_eq!(
            clause,
            SparqlValue::SelectClause {
                distinct: false,
                reduced: true,
                variables: a_box!(SparqlValue::Variables(vec![
                    Variable("name",),
                    SparqlValue::As(Expr::AsExpr {
                        expression: a_box!(Expr::Path(Path::Iri(Enclosed("http://xx.com",),),)),
                        variable: a_box!(Expr::Variable("s",)),
                    },),
                    Variable("p",),
                    Variable("o",),
                ],)),
            },
        );
        let s = "SELECT REDUCED DISTINCT ?name (<http://xx.com> as ?s) ?p ?o";
        let res = select_clause(s);
        assert!(res.is_err());
    }

    #[test]
    fn test_solution_modifier() {
        let s = r#"
            GROUP BY ?x ?y ?z
            HAVING(?size > 10)
            ORDER BY ?x ?y
            LIMIT 10 OFFSET 5
        "#;
        let (_, solution) = solution_modifier(s).unwrap();
        assert_eq!(
            solution,
            SparqlValue::SolutionModifier {
                group_clause: Some(vec![Variable("x"), Variable("y"), Variable("z"),],),
                having_clause: Some(vec![SparqlValue::Constraint(Expr::Bracketed(a_box!(
                    Expr::Relational {
                        left: a_box!(Expr::Variable("size",)),
                        operator: RelationalOperator::Greater,
                        right: a_box!(Expr::Literal(Literal::Integer(10,),)),
                    }
                ),),)]),
                order_clause: Some(vec![Variable("x"), Variable("y"),]),
                limit_clause: Some(a_box!(SparqlValue::LimitOffsetClause {
                    limit: Some(10,),
                    offset: Some(5,),
                }),),
            }
        )
    }
}
