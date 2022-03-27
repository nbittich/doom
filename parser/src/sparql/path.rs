
use crate::prelude::*;
use crate::triple_common_parser::iri::iri as common_iri;
use crate::triple_common_parser::Iri;

#[derive(Debug, PartialEq)]
pub enum Path<'a> {
    Iri(Iri<'a>),
    Inverse(Iri<'a>),
    OneOrMore(Box<Path<'a>>),
    Group(Box<Path<'a>>),
    Negate(Box<Path<'a>>),
    ZeroOrMore(Box<Path<'a>>),
    Sequence(Vec<Path<'a>>),
    Alternative {
        elt1: Box<Path<'a>>,
        elt2: Box<Path<'a>>,
    },
}

pub(super) fn inverse_iri(s: &str) -> ParserResult<Path> {
    map(preceded(char('^'), common_iri), Path::Inverse)(s)
}
pub(super) fn iri(s: &str) -> ParserResult<Path> {
    alt((map(common_iri, Path::Iri), inverse_iri))(s)
}
pub(super) fn path(s: &str) -> ParserResult<Path> {
    alt((arbitrary_length, iri))(s)
}

pub(super) fn group(s: &str) -> ParserResult<Path> {
    fn group_fn(s: &str) -> ParserResult<Path> {
        delimited(char('('), alt((alternative, sequence, path)), char(')'))(s)
    }
    map(
        alt((
            map(terminated(group_fn, char('+')), |p| {
                Path::OneOrMore(Box::new(p))
            }),
            map(terminated(group_fn, char('*')), |p| {
                Path::ZeroOrMore(Box::new(p))
            }),
            group_fn,
        )),
        |p| Path::Group(Box::new(p)),
    )(s)
}

pub(super) fn negate(s: &str) -> ParserResult<Path> {
    map(preceded(char('!'), alt((group, path))), |p| {
        Path::Negate(Box::new(p))
    })(s)
}
pub(super) fn arbitrary_length(s: &str) -> ParserResult<Path> {
    alt((
        map(terminated(iri, char('+')), |p| Path::OneOrMore(Box::new(p))),
        map(terminated(iri, char('*')), |p| {
            Path::ZeroOrMore(Box::new(p))
        }),
    ))(s)
}

pub(super) fn sequence(s: &str) -> ParserResult<Path> {
    map(separated_list1(tag("/"), path), |mut seq| {
        if seq.len() == 1 {
            seq.pop().unwrap()
        } else {
            Path::Sequence(seq)
        }
    })(s)
}
pub(super) fn alternative(s: &str) -> ParserResult<Path> {
    map(
        separated_pair(path, delimited(multispace0, tag("|"), multispace0), path),
        |(elt1, elt2)| Path::Alternative {
            elt1: Box::new(elt1),
            elt2: Box::new(elt2),
        },
    )(s)
}

#[cfg(test)]
mod test {
    use crate::sparql::path;
    use crate::sparql::path::Path;
    use crate::sparql::path::Path::{
        Alternative, Group, Inverse, Iri, Negate, OneOrMore, Sequence,
    };
    use crate::triple_common_parser::Iri::{Enclosed, Prefixed};
    macro_rules! a_box {
        ($a:expr) => {
            Box::new($a)
        };
    }
    #[test]
    fn test_inverse_iri() {
        let s = "^foaf:knows";
        let (_, path) = path::inverse_iri(s).unwrap();
        assert_eq!(
            Path::Inverse(Prefixed {
                prefix: "foaf",
                local_name: "knows"
            }),
            path
        );
        let s = "^<http://foaf.com/knows>";
        let (_, path) = path::inverse_iri(s).unwrap();
        assert_eq!(Path::Inverse(Enclosed("http://foaf.com/knows")), path);
    }

    #[test]
    fn test_arbitrary_length_path() {
        let s = "rdfs:subClassOf*";
        let (_, path) = path::arbitrary_length(s).unwrap();
        assert_eq!(
            Path::ZeroOrMore(a_box!(Iri(Prefixed {
                prefix: "rdfs",
                local_name: "subClassOf"
            }))),
            path
        );
        let s = "rdfs:subClassOf+";
        let (_, path) = path::arbitrary_length(s).unwrap();
        assert_eq!(
            Path::OneOrMore(a_box!(Iri(Prefixed {
                prefix: "rdfs",
                local_name: "subClassOf"
            }))),
            path
        );
    }
    #[test]
    fn test_negate() {
        let s = "!rdfs:subClassOf*";
        let (_, path) = path::negate(s).unwrap();
        assert_eq!(
            Path::Negate(a_box!(Path::ZeroOrMore(a_box!(Iri(Prefixed {
                prefix: "rdfs",
                local_name: "subClassOf"
            }))))),
            path
        );
        let s = "!(rdfs:subClassOf+)";
        let (_, path) = path::negate(s).unwrap();
        assert_eq!(
            Path::Negate(a_box!(Path::Group(a_box!(Path::OneOrMore(a_box!(Iri(
                Prefixed {
                    prefix: "rdfs",
                    local_name: "subClassOf"
                }
            ))))))),
            path
        );
    }
    #[test]
    fn test_group() {
        let s = "!(rdf:type|^rdf:type)+";
        let (_, path) = path::negate(s).unwrap();
        assert_eq!(
            Negate(a_box!(Group(a_box!(OneOrMore(a_box!(Alternative {
                elt1: a_box!(Iri(Prefixed {
                    prefix: "rdf",
                    local_name: "type",
                },)),
                elt2: a_box!(Inverse(Prefixed {
                    prefix: "rdf",
                    local_name: "type",
                },))
            }),))))),
            path
        );
    }
    #[test]
    fn test_alternative_path() {
        let s = "dc:title|rdfs:label";
        let (_, path) = path::alternative(s).unwrap();
        assert_eq!(
            Alternative {
                elt1: a_box!(Iri(Prefixed {
                    prefix: "dc",
                    local_name: "title",
                })),
                elt2: a_box!(Iri(Prefixed {
                    prefix: "rdfs",
                    local_name: "label",
                }))
            },
            path
        );

        let s = "dc:title | ^<http://rdfs.com/label>";
        let (_, path) = path::alternative(s).unwrap();
        assert_eq!(
            Alternative {
                elt1: a_box!(Iri(Prefixed {
                    prefix: "dc",
                    local_name: "title",
                })),
                elt2: a_box!(Path::Inverse(Enclosed("http://rdfs.com/label")))
            },
            path
        );
    }

    #[test]
    fn test_sequence_path() {
        let s = "foaf:knows+/^foaf:knows/foaf:name";
        let (_, path) = path::sequence(s).unwrap();
        assert_eq!(
            Sequence(vec![
                OneOrMore(a_box!(Iri(Prefixed {
                    prefix: "foaf",
                    local_name: "knows",
                }))),
                Path::Inverse(Prefixed {
                    prefix: "foaf",
                    local_name: "knows",
                },),
                Iri(Prefixed {
                    prefix: "foaf",
                    local_name: "name",
                },),
            ],),
            path
        )
    }
}
