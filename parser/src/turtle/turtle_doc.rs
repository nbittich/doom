use crate::shared::{
    DEFAULT_WELL_KNOWN_PREFIX, RDF_FIRST, RDF_NIL, RDF_REST, XSD_BOOLEAN, XSD_DECIMAL, XSD_DOUBLE,
    XSD_INTEGER,
};
use crate::triple_common_parser::Literal as ASTLiteral;
use crate::triple_common_parser::{BlankNode, Iri};
use crate::turtle::turtle_parser::{statements, TurtleValue};
use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::fs::File;
use std::io::Read;
use std::num::{ParseFloatError, ParseIntError};
use std::ops::Add;
use std::path::Path;
use std::rc::Rc;
use std::str::ParseBoolError;
use uuid::Uuid;

struct Context<'a> {
    base: Option<&'a str>,
    prefixes: HashMap<&'a str, &'a str>,
}

#[derive(PartialEq, Debug)]
pub enum Literal<'a> {
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
pub enum Node<'a> {
    Iri(Cow<'a, str>),
    Literal(Literal<'a>),
    Ref(Rc<Node<'a>>),
    List(Vec<Node<'a>>),
}
#[derive(PartialEq, Debug)]
pub struct Statement<'a> {
    pub subject: Node<'a>,
    pub predicate: Node<'a>,
    pub object: Node<'a>,
}
#[derive(PartialEq, Debug, Default)]
pub struct TurtleDoc<'a> {
    prefixes: HashMap<&'a str, &'a str>,
    statements: Vec<Statement<'a>>,
}

impl<'a> TurtleDoc<'a> {
    pub fn from_string(s: &'a str) -> Result<Self, TurtleDocError> {
        let (_, statements) = statements(s).map_err(|err| TurtleDocError {
            message: format!("parsing error: {err}"),
        })?;
        Self::new(statements)
    }
    pub fn from_file(path: &'a Path, buf: &'a mut String) -> Result<Self, TurtleDocError> {
        let mut file = File::open(path).map_err(|err| TurtleDocError {
            message: format!("cannot open file: {err}"),
        })?;
        file.read_to_string(buf).map_err(|err| TurtleDocError {
            message: format!("cannot read file: {err}"),
        })?;
        Self::from_string(buf)
    }

    pub fn add_statement(&mut self, subject: Node<'a>, predicate: Node<'a>, object: Node<'a>) {
        self.statements.push(Statement {
            subject,
            predicate,
            object,
        });
    }
    pub fn len(&self) -> usize {
        self.statements.len()
    }
    pub fn is_empty(&self) -> bool {
        self.statements.is_empty()
    }

    // TODO this is temporary. it won't work if node is a ref because comparing a ref & iri is not the same thing
    pub fn list_statements(
        &self,
        subject: Option<&Node>,
        predicate: Option<&Node>,
        object: Option<&Node>,
    ) -> Vec<&Statement> {
        let mut statements: Vec<&Statement> = self.statements.iter().collect();
        if let Some(subject) = subject {
            statements = statements
                .into_iter()
                .filter(|s| &s.subject == subject)
                .collect();
        }
        if let Some(predicate) = predicate {
            statements = statements
                .into_iter()
                .filter(|s| &s.predicate == predicate)
                .collect();
        }
        if let Some(object) = object {
            statements = statements
                .into_iter()
                .filter(|s| &s.object == object)
                .collect();
        }
        statements
    }

    fn new(turtle_values: Vec<TurtleValue<'a>>) -> Result<Self, TurtleDocError> {
        let mut context = Context {
            base: None,
            prefixes: HashMap::new(),
        };
        let mut statements: Vec<Statement> = vec![];

        for turtle_value in turtle_values {
            match turtle_value {
                TurtleValue::Base(base) => {
                    context.base = Some(Self::extract_iri(base)?);
                }
                TurtleValue::Prefix((prefix, iri)) => {
                    let iri = TurtleDoc::extract_iri(iri)?;
                    context.prefixes.insert(prefix, iri);
                }
                statement @ TurtleValue::Statement {
                    subject: _,
                    predicate_objects: _,
                } => {
                    Self::get_node(statement, &context, &mut statements)?;
                }
                _ => {
                    return Err(TurtleDocError {
                        message: "incorrect turtle value".into(),
                    });
                }
            }
        }
        Ok(TurtleDoc {
            statements,
            prefixes: context.prefixes,
        })
    }

    fn extract_iri(value: Iri) -> Result<&str, TurtleDocError> {
        if let Iri::Enclosed(iri) = value {
            Ok(iri)
        } else {
            Err(TurtleDocError {
                message: "Not enclosed iri".into(),
            })
        }
    }

    fn get_node<'x>(
        value: TurtleValue<'a>,
        ctx: &'x Context,
        statements: &'x mut Vec<Statement<'a>>,
    ) -> Result<Node<'a>, TurtleDocError> {
        match value {
            TurtleValue::Iri(Iri::Prefixed { prefix, local_name }) => {
                let prefix = *ctx.prefixes.get(prefix).ok_or(TurtleDocError {
                    message: "prefix not found".into(),
                })?;
                let full_iri = prefix.to_owned() + local_name;
                Ok(Node::Iri(Cow::Owned(full_iri)))
            }
            TurtleValue::Iri(Iri::Enclosed(iri)) => {
                if !iri.starts_with("http://") && !iri.starts_with("https://") {
                    if let Some(base) = ctx.base {
                        return Ok(Node::Iri(Cow::Owned(base.to_owned() + iri)));
                    }
                }
                Ok(Node::Iri(Cow::Borrowed(iri)))
            }
            TurtleValue::Literal(literal) => {
                let literal = match literal {
                    ASTLiteral::Boolean(b) => Node::Literal(Literal::Boolean(b)),
                    ASTLiteral::Double(b) => Node::Literal(Literal::Double(b)),
                    ASTLiteral::Decimal(b) => Node::Literal(Literal::Decimal(b)),
                    ASTLiteral::Integer(b) => Node::Literal(Literal::Integer(b)),
                    ASTLiteral::Quoted {
                        datatype,
                        lang,
                        value,
                    } => {
                        let datatype: Option<Node<'a>> = if let Some(datatype) = datatype {
                            Some(Self::get_node(TurtleValue::Iri(datatype), ctx, statements)?)
                        } else {
                            None
                        };
                        match datatype {
                            Some(Node::Iri(iri)) if iri == XSD_BOOLEAN => {
                                Node::Literal(Literal::Boolean(value.parse().map_err(
                                    |e: ParseBoolError| TurtleDocError {
                                        message: e.to_string(),
                                    },
                                )?))
                            }
                            Some(Node::Iri(iri)) if iri == XSD_INTEGER => {
                                Node::Literal(Literal::Integer(value.parse().map_err(
                                    |e: ParseIntError| TurtleDocError {
                                        message: e.to_string(),
                                    },
                                )?))
                            }
                            Some(Node::Iri(iri)) if iri == XSD_DECIMAL => {
                                Node::Literal(Literal::Decimal(value.parse().map_err(
                                    |e: ParseFloatError| TurtleDocError {
                                        message: e.to_string(),
                                    },
                                )?))
                            }
                            Some(Node::Iri(iri)) if iri == XSD_DOUBLE => {
                                Node::Literal(Literal::Double(value.parse().map_err(
                                    |e: ParseFloatError| TurtleDocError {
                                        message: e.to_string(),
                                    },
                                )?))
                            }
                            dt => Node::Literal(Literal::Quoted {
                                datatype: dt.map(Box::new),
                                lang,
                                value,
                            }),
                        }
                    }
                };
                Ok(literal)
            }
            TurtleValue::BNode(BlankNode::Labeled(label)) => Ok(Node::Iri(Cow::Owned(
                DEFAULT_WELL_KNOWN_PREFIX.to_owned() + label,
            ))),
            TurtleValue::BNode(BlankNode::Unlabeled) => {
                let uuid = Uuid::new_v4().to_string();
                Ok(Node::Iri(Cow::Owned(format!(
                    "{DEFAULT_WELL_KNOWN_PREFIX}{uuid}"
                ))))
            }
            TurtleValue::Statement {
                subject,
                predicate_objects,
            } => {
                let subject = {
                    let subject = Self::get_node(*subject, ctx, statements)?;
                    if let Node::Ref(s) = subject {
                        s
                    } else {
                        Rc::new(subject)
                    }
                };
                for predicate_object in predicate_objects {
                    if let TurtleValue::PredicateObject { predicate, object } = predicate_object {
                        let predicate = Self::get_node(*predicate, ctx, statements)?;
                        let object = Self::get_node(*object, ctx, statements)?;
                        match object {
                            Node::List(nodes) => {
                                let predicate = if let Node::Ref(p) = predicate {
                                    p
                                } else {
                                    Rc::new(predicate)
                                };
                                for node in nodes {
                                    statements.push(Statement {
                                        subject: Node::Ref(Rc::clone(&subject)),
                                        predicate: Node::Ref(Rc::clone(&predicate)),
                                        object: node,
                                    });
                                }
                            }
                            node => statements.push(Statement {
                                subject: Node::Ref(Rc::clone(&subject)),
                                predicate,
                                object: node,
                            }),
                        }
                    } else {
                        return Err(TurtleDocError {
                            message: "at this point it should be a predicate_object".into(),
                        });
                    }
                }
                Ok(Node::Ref(subject))
            }
            TurtleValue::Collection(mut nodes) => {
                let subject = TurtleValue::BNode(BlankNode::Unlabeled);
                let first = nodes.pop_front().ok_or(TurtleDocError {
                    message: "collection should have at least one element. something went wrong"
                        .into(),
                })?;

                let rest = if nodes.is_empty() {
                    TurtleValue::Iri(Iri::Enclosed(RDF_NIL))
                } else {
                    TurtleValue::Collection(nodes)
                };
                return Self::get_node(
                    TurtleValue::Statement {
                        subject: Box::new(subject),
                        predicate_objects: vec![
                            TurtleValue::PredicateObject {
                                predicate: Box::new(TurtleValue::Iri(Iri::Enclosed(RDF_FIRST))),
                                object: Box::new(first),
                            },
                            TurtleValue::PredicateObject {
                                predicate: Box::new(TurtleValue::Iri(Iri::Enclosed(RDF_REST))),
                                object: Box::new(rest),
                            },
                        ],
                    },
                    ctx,
                    statements,
                );
            }
            TurtleValue::PredicateObject {
                predicate: _,
                object: _,
            } => Err(TurtleDocError {
                message: "PredicateObject: should never happen".into(),
            }),
            TurtleValue::ObjectList(values) => {
                let nodes: Vec<Node<'a>> = values
                    .into_iter()
                    .map(|v| Self::get_node(v, ctx, statements))
                    .collect::<Result<Vec<Node<'a>>, TurtleDocError>>()?;
                Ok(Node::List(nodes))
            }
            _ => Err(TurtleDocError {
                message: "should never happen".into(),
            }),
        }
    }
}
impl<'a> IntoIterator for TurtleDoc<'a> {
    type Item = Statement<'a>;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.statements.into_iter()
    }
}
impl Add for TurtleDoc<'_> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        let mut statements: Vec<Statement> = self
            .statements
            .into_iter()
            .chain(rhs.statements.into_iter())
            .collect();
        let prefixes: HashMap<&str, &str> = self
            .prefixes
            .into_iter()
            .chain(rhs.prefixes.into_iter())
            .collect();
        statements.dedup();
        TurtleDoc {
            statements,
            prefixes,
        }
    }
}
impl Display for Node<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Iri(iri) => f.write_str(&format!("<{}>", iri)),
            Node::Ref(iri) => f.write_str(&format!("{}", iri)),
            Node::Literal(Literal::Quoted {
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
            Node::Literal(Literal::Integer(i)) => {
                f.write_str(&format!(r#""{i}"^^<{}>"#, XSD_INTEGER))
            }
            Node::Literal(Literal::Decimal(d)) => {
                f.write_str(&format!(r#""{d}"^^<{}>"#, XSD_DECIMAL))
            }
            Node::Literal(Literal::Double(d)) => {
                f.write_str(&format!(r#""{d}"^^<{}>"#, XSD_DOUBLE))
            }
            Node::Literal(Literal::Boolean(d)) => {
                f.write_str(&format!(r#""{d}"^^<{}>"#, XSD_BOOLEAN))
            }
            _ => Err(std::fmt::Error),
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

#[derive(PartialEq, Debug)]
pub struct TurtleDocError {
    pub message: String,
}
impl Display for TurtleDocError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("error: {}", self.message))
    }
}

#[cfg(test)]
mod test {
    use crate::turtle::turtle_doc::{Literal, Node, TurtleDoc};
    use std::borrow::Cow;
    use Cow::Borrowed;
    use Node::Iri;

    #[test]
    fn turtle_doc_test() {
        let doc = include_str!("example/input.ttl");
        let expected = include_str!("example/output.ttl");
        let turtle = TurtleDoc::from_string(doc).unwrap();
        let expected_turtle = TurtleDoc::from_string(expected).unwrap();
        let expected_statements = expected_turtle.list_statements(None, None, None);
        let statements = turtle.list_statements(None, None, None);
        assert_eq!(&expected_statements.len(), &statements.len());

        assert_eq!(expected_turtle.to_string(), turtle.to_string());
    }
    #[test]
    fn turtle_doc_bnode_test() {
        let doc = r#"
        @prefix foaf: <http://foaf.com/>.
        [ foaf:name "Alice" ] foaf:knows [
    foaf:name "Bob" ;
    foaf:lastName "George", "Joshua" ;
    foaf:knows [
        foaf:name "Eve" ] ;
    foaf:mbox <bob@example.com>] .

        "#;
        let turtle = TurtleDoc::from_string(doc).unwrap();
        assert_eq!(8, turtle.statements.len());
    }

    #[test]
    fn turtle_doc_collection_test() {
        let s = r#"
        @prefix : <http://example.com/>.
        :a :b ( "apple" "banana" ) .
        "#;
        let turtle = TurtleDoc::from_string(s).unwrap();
        assert_eq!(5, turtle.statements.len());
    }
    #[test]
    fn turtle_doc_add_test() {
        let doc1 = r#"
        @prefix : <http://example.com/>.
        :a :b ( "apple" "banana" ) .
        "#;
        let doc2 = r#"
        @prefix foaf: <http://foaf.com/>.
        [ foaf:name "Alice" ] foaf:knows [
    foaf:name "Bob" ;
    foaf:lastName "George", "Joshua" ;
    foaf:knows [
        foaf:name "Eve" ] ;
    foaf:mbox <bob@example.com>] .

        "#;
        let turtle1 = TurtleDoc::from_string(doc1).unwrap();
        assert_eq!(5, (&turtle1.statements).len());

        let turtle2 = TurtleDoc::from_string(doc2).unwrap();
        assert_eq!(8, (&turtle2.statements).len());

        let turtle3 = turtle1 + turtle2;
        assert_eq!(13, (&turtle3.statements).len());
        let mut turtle = TurtleDoc::default();
        turtle.add_statement(
            Iri(Borrowed("http://xxx.com/123")),
            Iri(Borrowed("http://bar.com/345")),
            Node::Literal(Literal::Decimal(123f32)),
        );
        let turtle4 = turtle + turtle3;
        assert_eq!(14, turtle4.statements.len());
    }
    #[test]
    fn turtle_doc_list_statements_test() {
        let doc = r#"
        @prefix foaf: <http://foaf.com/>.
        [ foaf:name "Alice" ] foaf:knows [
    foaf:name "Bob" ;
    foaf:lastName "George", "Joshua" ;
    foaf:knows [
        foaf:name "Eve" ] ;
    foaf:mbox <bob@example.com>] .

        "#;
        let turtle = TurtleDoc::from_string(doc).unwrap();
        let statements =
            turtle.list_statements(None, None, Some(&Iri(Borrowed("bob@example.com"))));
        assert_eq!(1, statements.len());
        println!("{statements:?}");
        let statement = statements[0];
        let statements = turtle.list_statements(Some(&statement.subject), None, None);
        assert_eq!(5, statements.len());
    }

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
         <http://example.org/show/218> <http://example.org/show/localName> "Cette S??rie des Ann??es Septante"@fr-be .

         <http://en.wikipedia.org/wiki/Helium> <http://example.org/elements/specificGravity> "1.663E-4"^^<http://www.w3.org/2001/XMLSchema#double> .     # xsd:double
         "#;

        let triples = TurtleDoc::from_string(triple).unwrap();
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
         <http://example.org/show/218> <http://example.org/show/localName> "Cette S??rie des Ann??es Septante"@fr-be .

         <http://en.wikipedia.org/wiki/Helium> <http://example.org/elements/specificGravity> "1.663E-4"^^<http://www.w3.org/2001/XMLSchema#double> .     # xsd:double
         <http://en.wikipedia.org/wiki/Helium> <http://example.org/elements/specificGravity> "1.663E-4"^^<http://www.w3.org/2001/XMLSchema#double> .     # xsd:double
         <http://en.wikipedia.org/wiki/Helium> <http://example.org/elements/specificGravity> "1.663E-4"^^<http://www.w3.org/2001/XMLSchema#decimal> .     # xsd:double
         <http://en.wikipedia.org/wiki/Helium> <http://example.org/elements/specificGravity> "1123"^^<http://www.w3.org/2001/XMLSchema#integer> .     # xsd:double
         <http://en.wikipedia.org/wiki/Helium> <http://example.org/elements/nice> "true"^^<http://www.w3.org/2001/XMLSchema#boolean> .
         "#;
        let triples = TurtleDoc::from_string(triples).unwrap();
        assert_eq!(9, triples.len());
    }
}
