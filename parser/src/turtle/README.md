# turtle parser
<i>transforms a turtle string to a TurtleDoc</i>

## Work in progress

- Covers roughly the turtle spec https://www.w3.org/TR/turtle/
- Collections, Blank nodes, predicateObjectlist,etc
- Comments are filtered

## Todo
- error handling
- iri as described in the spec
- cleanup
- Jena like api

### Example

#### Input
```turtle
   @prefix foaf: <http://foaf.com/>.
    [ foaf:name "Alice" ] foaf:knows [
    foaf:name "Bob" ;
    foaf:lastName "George", "Joshua" ;
    foaf:knows [
        foaf:name "Eve" ] ;
    foaf:mbox <bob@example.com>] .

```
#### Output
```turtle
<http://example.org/.well-known/genid#e162c9a7-52cf-4240-9359-b1b1f977f642> <http://foaf.com/name> "Alice"^^<http://www.w3.org/2001/XMLSchema#string>.
<http://example.org/.well-known/genid#6955800d-16db-49f8-a614-b0cbea3d7fb2> <http://foaf.com/name> "Bob"^^<http://www.w3.org/2001/XMLSchema#string>.
<http://example.org/.well-known/genid#6955800d-16db-49f8-a614-b0cbea3d7fb2> <http://foaf.com/lastName> "George"^^<http://www.w3.org/2001/XMLSchema#string>.
<http://example.org/.well-known/genid#6955800d-16db-49f8-a614-b0cbea3d7fb2> <http://foaf.com/lastName> "Joshua"^^<http://www.w3.org/2001/XMLSchema#string>.
<http://example.org/.well-known/genid#7a9cfb63-1bfa-44ee-bfb9-c3d3db7da920> <http://foaf.com/name> "Eve"^^<http://www.w3.org/2001/XMLSchema#string>.
<http://example.org/.well-known/genid#6955800d-16db-49f8-a614-b0cbea3d7fb2> <http://foaf.com/knows> <http://example.org/.well-known/genid#7a9cfb63-1bfa-44ee-bfb9-c3d3db7da920>.
<http://example.org/.well-known/genid#6955800d-16db-49f8-a614-b0cbea3d7fb2> <http://foaf.com/mbox> <bob@example.com>.
<http://example.org/.well-known/genid#e162c9a7-52cf-4240-9359-b1b1f977f642> <http://foaf.com/knows> <http://example.org/.well-known/genid#6955800d-16db-49f8-a614-b0cbea3d7fb2>
```
