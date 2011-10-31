=== Library: Peg-parser ===

This is a memoizing (or, more correctly, caching) recursive descent parser
that handles attributed parsing expression grammars (PEGs), as described at
http://en.wikipedia.org/wiki/Parsing_expression_grammar.

It isn't too tricky to use. Basically, a PEG can act as both a lexical and
phrase grammar, and acts similarly to a forward-looking greedy regular
expression processor, except that it acts on tokens instead of characters.

This library allows for simple rules-based declarations and also custom parser
functions (you'll need some of these to get characters and literals from the
stream).

The parser can simplify and consolidate syntax elements in an upwards
direction along the syntax tree via token class slots, and downward or across
the syntax tree via attributes. You can also define a context class to keep
global information for later semantic processing.

-- Dustin Voss
