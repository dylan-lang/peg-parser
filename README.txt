=== Library: Peg-parser ===

This is a memoizing (or, more correctly, caching) recursive descent parser
that handles attributed parsing expression grammars (PEGs), as described at
'http://en.wikipedia.org/wiki/Parsing_expression_grammar'.

It isn't too tricky to use. Basically, a PEG can act as both a lexical and
phrase grammar, and acts similarly to a forward-looking greedy regular
expression processor, except that it acts on tokens instead of characters.

This library allows for simple rules-based declarations and also custom parser
functions (you'll need some of these to get characters and literals from the
stream).

The parser can simplify and consolidate syntax elements in an upwards direction
along the syntax tree via token class slots, and downward or across the syntax
tree via dynamically-bound attributes. You can also define a context class to
keep global information for later semantic processing.

-- Dustin Voss


--- Compilation ---

Open Dylan currently has problems properly inferring types when compiling this
library into another project. It reports numerous spurious errors. To avoid
seeing these, pipe the compilation of libraries that 'use' [em] this library
through "egrep" [i] as follows:

| BASH: dylan-compiler ... 2>&1 | egrep -v '^$|^ |^[[:digit:]]|<parse-failure>|<parse-extent>'
| CSH:  dylan-compiler ... |& egrep -v '^$|^ |^[[:digit:]]|<parse-failure>|<parse-extent>'
