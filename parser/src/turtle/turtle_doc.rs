use crate::shared::DEFAULT_WELL_KNOWN_PREFIX;
use crate::turtle::model::{BlankNode, Iri, TurtleValue};
use std::borrow::Cow;
use std::collections::HashMap;
use std::rc::Rc;
use uuid::adapter::Urn;
use uuid::Uuid;

#[derive(PartialEq, Debug)]
pub struct TurtleDoc<'a>(pub Vec<TurtleValue<'a>>);

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
}
#[derive(PartialEq, Debug)]
struct Statement<'a> {
    subject: Node<'a>,
    predicate: Node<'a>,
    object: Node<'a>,
}
struct Model<'a> {
    statements: Vec<Statement<'a>>,
}
impl<'a> Model<'a> {
    fn new(turtle_values: Vec<TurtleValue<'a>>) -> Self {
        let mut context = Context {
            base: None,
            prefixes: HashMap::new(),
        };
        let mut model = Model { statements: vec![] };

        for turtle_value in turtle_values {
            match turtle_value {
                TurtleValue::Base(base) => {
                    context.base = Some(Model::extract_iri(base));
                }
                TurtleValue::Prefix((prefix, iri)) => {
                    let iri = Model::extract_iri(iri);
                    context.prefixes.insert(prefix, iri);
                }
                statement @ TurtleValue::Statement {
                    subject: _,
                    predicate_objects: _,
                } => {
                    Self::add_statement(statement, &context, &mut model);
                }
                _ => panic!("only directive & statement allowed!"), // todo should be an error result
            }
        }
        model
    }

    fn extract_iri(value: Box<TurtleValue>) -> &str {
        if let TurtleValue::Iri(Iri::Enclosed(iri)) = *value {
            iri
        } else {
            panic!("base not good"); // todo return error
        }
    }

    fn add_statement<'x>(stmt: TurtleValue<'a>, ctx: &'x Context, model: &'x mut Model) {
        if let TurtleValue::Statement {
            subject,
            predicate_objects,
        } = stmt
        {
            let subject = Self::get_node(*subject, ctx, model);
        }
    }
    fn get_node<'x>(value: TurtleValue<'a>, ctx: &'x Context, model: &'x mut Model) -> Node<'a> {
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
                    super::model::Literal::Boolean(b) => Node::Literal(Literal::Boolean(b)),
                    super::model::Literal::Double(b) => Node::Literal(Literal::Double(b)),
                    super::model::Literal::Decimal(b) => Node::Literal(Literal::Decimal(b)),
                    super::model::Literal::Integer(b) => Node::Literal(Literal::Integer(b)),
                    super::model::Literal::Quoted {
                        datatype,
                        lang,
                        value,
                    } => {
                        let datatype: Option<Box<Node<'a>>> = if let Some(datatype) = datatype {
                            Some(Box::new(Self::get_node(*datatype, ctx, model)))
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
            TurtleValue::Collection(nodes) => {}
            TurtleValue::Statement {
                predicate_objects,
                subject,
            } => {}
            TurtleValue::PredicateObject { predicate, object } => {}
            TurtleValue::ObjectList(nodes) => {}
            _ => panic!("should never happen"), // todo should be an error - result
        }
        todo!()
    }
}
