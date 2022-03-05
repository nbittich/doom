#![allow(dead_code, unused_imports)]

use nom::bytes::complete::{tag, take_until};
use nom::combinator::flat_map;
use nom::complete::take;
use nom::multi::many0;
use nom::sequence::{delimited, tuple};
use nom::AsChar;
use nom::{
    bytes::complete::{escaped, take_while},
    character::{complete::*, is_alphanumeric},
    combinator::{cut, map},
    error::{context, ContextError, ParseError},
    multi::separated_list0,
    sequence::{preceded, terminated},
    IResult,
};
#[derive(Debug)]
struct Triple<'a> {
    subject: &'a str,
    predicate: &'a str,
    object: &'a str,
}

fn extract_url<'a>(s: &'a str) -> IResult<&'a str, &'a str> {
    preceded(
        multispace0,
        delimited(
            char('<'),
            take_while(|s: char| s.is_alphanum() || s.eq(&'/') || s.eq(&':') || s.eq(&'.')),
            char('>'),
        ),
    )(s)
}

fn parse_one_triple<'a>(s: &'a str) -> IResult<&'a str, Triple<'a>> {
    map(
        tuple((extract_url, extract_url, extract_url)),
        |(sub, pred, obj)| Triple {
            subject: sub,
            predicate: pred,
            object: obj,
        },
    )(s)
}

fn parse_list_triples<'a>(s: &'a str) -> IResult<&'a str, Vec<Triple<'a>>> {
    many0(terminated(
        parse_one_triple,
        preceded(multispace0, char('.')),
    ))(s)
}

#[cfg(test)]
mod tests {
    use nom::error::ErrorKind;

    use super::*;

    #[test]
    fn parse_list_triples_test() {
        let triple = r#"  
         <http://bittich.be/some/url/123>    <http://example.org/firstName><http://n.com/nordine>.
         <http://bittich.be/some/url/123><http://example.org/firstName><http://n.com/nordine>.


         <http://bittich.be/some/url/123>    <http://example.org/firstName><http://n.com/nordine>  .
         <http://bittich.be/some/url/123>    <http://example.org/firstName><http://n.com/nordine>  .
         <http://bittich.be/some/url/123>    <http://example.org/firstName><http://n.com/nordine>  .

         <http://bittich.be/some/url/123>    <http://example.org/firstName><http://n.com/nordine>  .
         
         "#;

        println!("{:?}", parse_list_triples(triple));
    }
}
