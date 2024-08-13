# URI Parsing

URI Parsing is a library for building composable URI parsers using
[ppx_typed_fields](https://github.com/janestreet/ppx_typed_fields).

See the Bonsai Guide [chapter on
urls](https://github.com/janestreet/bonsai/blob/master/docs/how_to/uri_parsing.md).
This library contains the "typed" parsing/unparsing module for URL Var. The
reason that this library exists as a separate library is that URL Var has a
JavaScript browser dependency which makes it unable to run outside of a JavaScript
browser context.

Splitting off the parsing/unparsing logic allows it to be used on native OCaml.
