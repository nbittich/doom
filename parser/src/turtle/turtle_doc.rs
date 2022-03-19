use crate::shared::{DEFAULT_WELL_KNOWN_PREFIX, XSD_BOOLEAN, XSD_DECIMAL, XSD_DOUBLE, XSD_INTEGER};
use crate::turtle::ast_struct::{BlankNode, Iri, TurtleValue};
use std::borrow::{Borrow, Cow};
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Write};
use std::rc::Rc;
use uuid::adapter::Urn;
use uuid::Uuid;

struct Context<'a> {
    base: Option<&'a str>,
    prefixes: HashMap<&'a str, &'a str>,
}

#[derive(PartialEq, Debug)]
enum Literal<'a> {
    Quoted {
        datatype: Option<Box<Node<'a>>>,
        value: &'a str,
        lang: Option<&'a str>,
    },
    Double(f64),
    Decimal(f32),
    Integer(i64),
    Boolean(bool),
}
#[derive(PartialEq, Debug)]
enum Node<'a> {
    Iri(Cow<'a, str>),
    Literal(Literal<'a>),
    Ref(Rc<Node<'a>>),
    List(Vec<Node<'a>>),
}
#[derive(PartialEq, Debug)]
struct Statement<'a> {
    subject: Node<'a>,
    predicate: Node<'a>,
    object: Node<'a>,
}
#[derive(PartialEq, Debug)]
pub struct TurtleDoc<'a> {
    statements: Vec<Statement<'a>>,
}
impl<'a> TurtleDoc<'a> {
    pub fn new(turtle_values: Vec<TurtleValue<'a>>) -> Self {
        let mut context = Context {
            base: None,
            prefixes: HashMap::new(),
        };
        let mut turtle_doc = TurtleDoc { statements: vec![] };

        for turtle_value in turtle_values {
            match turtle_value {
                TurtleValue::Base(base) => {
                    context.base = Some(TurtleDoc::extract_iri(base));
                }
                TurtleValue::Prefix((prefix, iri)) => {
                    let iri = TurtleDoc::extract_iri(iri);
                    context.prefixes.insert(prefix, iri);
                }
                statement @ TurtleValue::Statement {
                    subject: _,
                    predicate_objects: _,
                } => {
                    Self::add_statement(statement, &context, &mut turtle_doc);
                }
                _ => panic!("only directive & statement allowed!"), // todo should be an error result
            }
        }
        turtle_doc
    }

    fn extract_iri(value: Box<TurtleValue>) -> &str {
        if let TurtleValue::Iri(Iri::Enclosed(iri)) = *value {
            iri
        } else {
            panic!("base not good"); // todo return error
        }
    }

    fn add_statement<'x>(
        stmt: TurtleValue<'a>,
        ctx: &'x Context,
        turtle_doc: &'x mut TurtleDoc<'a>,
    ) -> Node<'a> {
        if let TurtleValue::Statement {
            subject,
            predicate_objects,
        } = stmt
        {
            let subject = {
                let subject = Self::get_node(*subject, ctx, turtle_doc);
                if let Node::Ref(s) = subject {
                    Rc::clone(&s)
                } else {
                    Rc::new(subject)
                }
            };
            for predicate_object in predicate_objects {
                if let TurtleValue::PredicateObject { predicate, object } = predicate_object {
                    let predicate = Self::get_node(*predicate, ctx, turtle_doc);
                    let object = Self::get_node(*object, ctx, turtle_doc);
                    match object {
                        Node::List(nodes) => {
                            let predicate = if let Node::Ref(p) = predicate {
                                Rc::clone(&p)
                            } else {
                                Rc::new(predicate)
                            };
                            for node in nodes {
                                turtle_doc.statements.push(Statement {
                                    subject: Node::Ref(Rc::clone(&subject)),
                                    predicate: Node::Ref(Rc::clone(&predicate)),
                                    object: node,
                                });
                            }
                        }
                        node @ _ => turtle_doc.statements.push(Statement {
                            subject: Node::Ref(Rc::clone(&subject)),
                            predicate,
                            object: node,
                        }),
                    }
                } else {
                    panic!("at this point it should be a predicate_object") // todo error handling
                }
            }
            Node::Ref(subject)
        } else {
            panic!("not a statement, weird"); // todo it should be a result
        }
    }
    fn get_node<'x>(
        value: TurtleValue<'a>,
        ctx: &'x Context,
        turtle_doc: &'x mut TurtleDoc<'a>,
    ) -> Node<'a> {
        match value {
            TurtleValue::Iri(Iri::Prefixed { prefix, local_name }) => {
                let prefix = *ctx.prefixes.get(prefix).expect("prefix not found");
                let full_iri = prefix.to_owned() + local_name;
                return Node::Iri(Cow::Owned(full_iri));
            }
            TurtleValue::Iri(Iri::Enclosed(iri)) => {
                if !iri.starts_with("http://") && !iri.starts_with("https://") {
                    if let Some(base) = ctx.base {
                        return Node::Iri(Cow::Owned(base.to_owned() + iri));
                    }
                }
                return Node::Iri(Cow::Borrowed(iri));
            }
            TurtleValue::Literal(literal) => {
                return match literal {
                    super::ast_struct::Literal::Boolean(b) => Node::Literal(Literal::Boolean(b)),
                    super::ast_struct::Literal::Double(b) => Node::Literal(Literal::Double(b)),
                    super::ast_struct::Literal::Decimal(b) => Node::Literal(Literal::Decimal(b)),
                    super::ast_struct::Literal::Integer(b) => Node::Literal(Literal::Integer(b)),
                    super::ast_struct::Literal::Quoted {
                        datatype,
                        lang,
                        value,
                    } => {
                        let datatype: Option<Box<Node<'a>>> = if let Some(datatype) = datatype {
                            Some(Box::new(Self::get_node(*datatype, ctx, turtle_doc)))
                        } else {
                            None
                        };
                        Node::Literal(Literal::Quoted {
                            datatype,
                            lang,
                            value,
                        })
                    }
                }
            }
            TurtleValue::BNode(BlankNode::Labeled(label)) => {
                return Node::Iri(Cow::Owned(DEFAULT_WELL_KNOWN_PREFIX.to_owned() + label));
            }
            TurtleValue::BNode(BlankNode::Unlabeled) => {
                let uuid = Uuid::new_v4().to_string();
                return Node::Iri(Cow::Owned(format!("{DEFAULT_WELL_KNOWN_PREFIX}{uuid}")));
            }
            statement @ TurtleValue::Statement {
                subject: _,
                predicate_objects: _,
            } => {
                return Self::add_statement(statement, ctx, turtle_doc);
            }
            TurtleValue::Collection(nodes) => {
                let subject = TurtleValue::BNode(BlankNode::Unlabeled);
                return Self::get_node(
                    TurtleValue::Statement {
                        subject: Box::new(subject),
                        predicate_objects: nodes,
                    },
                    ctx,
                    turtle_doc,
                );
            }
            TurtleValue::PredicateObject { predicate, object } => {
                panic!("why this happens?");
            }
            TurtleValue::ObjectList(values) => {
                let nodes: Vec<Node<'a>> = values
                    .into_iter()
                    .map(|v| Self::get_node(v, ctx, turtle_doc))
                    .collect();
                return Node::List(nodes);
            }
            _ => panic!("should never happen"), // todo should be an error - result
        }
    }
}

impl Display for Node<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            &Node::Iri(iri) => f.write_str(&format!("<{}>", iri)),
            &Node::Ref(iri) => f.write_str(&format!("{}", iri)),
            &Node::Literal(Literal::Quoted {
                datatype,
                lang,
                value,
            }) => {
                let mut s = format!(r#""{value}""#);
                if let Some(datatype) = datatype {
                    s.push_str(&format!(r#"^^{datatype}"#));
                } else if let Some(lang) = lang {
                    s.push_str(&format!(r#"@{lang}"#));
                }
                f.write_str(&s)
            }
            &Node::Literal(Literal::Integer(i)) => {
                f.write_str(&format!(r#""{i}"^^<{}>"#, XSD_INTEGER))
            }
            &Node::Literal(Literal::Decimal(d)) => {
                f.write_str(&format!(r#""{d}"^^<{}>"#, XSD_DECIMAL))
            }
            &Node::Literal(Literal::Double(d)) => {
                f.write_str(&format!(r#""{d}"^^<{}>"#, XSD_DOUBLE))
            }
            &Node::Literal(Literal::Boolean(d)) => {
                f.write_str(&format!(r#""{d}"^^<{}>"#, XSD_BOOLEAN))
            }
            _ => panic!("shouldn't happen"),
        }
    }
}

impl Display for Statement<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Statement {
            subject,
            predicate,
            object,
        } = self;
        f.write_str(&format!(r#"{subject} {predicate} {object}."#))
    }
}

impl Display for TurtleDoc<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(
            &self
                .statements
                .iter()
                .map(Statement::to_string)
                .collect::<Vec<String>>()
                .join("\n"),
        )
    }
}
