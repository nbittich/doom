pub const BASE_TURTLE: &str = "@base";
pub const BASE_SPARQL: &str = "BASE";
pub const PREFIX_TURTLE: &str = "@prefix";
pub const PREFIX_SPARQL: &str = "PREFIX";

#[derive(PartialEq, Debug)]
pub enum TurtleValue<'a> {
    Base(Box<TurtleValue<'a>>),
    Prefix((&'a str, Box<TurtleValue<'a>>)),
    Iri(Iri<'a>),
    Literal(Literal<'a>),
    BNode(BlankNode<'a>),
    ObjectList(Vec<TurtleValue<'a>>),
    Collection(Vec<TurtleValue<'a>>),
    PredicateObject {
        predicate: Box<TurtleValue<'a>>,
        object: Box<TurtleValue<'a>>,
    },
    Statement {
        subject: Box<TurtleValue<'a>>,
        predicate_objects: Vec<TurtleValue<'a>>,
    },
}
#[derive(PartialEq, Debug)]
pub enum Iri<'a> {
    Enclosed(&'a str),
    Prefixed {
        prefix: &'a str,
        local_name: &'a str,
    },
}
#[derive(PartialEq, Debug)]
pub enum Literal<'a> {
    Quoted {
        datatype: Option<Box<TurtleValue<'a>>>,
        value: &'a str,
        lang: Option<&'a str>,
    },
    Double(f64),
    Decimal(f32),
    Integer(i64),
    Boolean(bool),
}
#[derive(PartialEq, Debug)]
pub enum BlankNode<'a> {
    Labeled(&'a str),
    Unlabeled,
}
