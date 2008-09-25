This is a memoizing recursive descent parser that handles parsing expression
grammars (PEGs), as described at
http://en.wikipedia.org/wiki/Parsing_expression_grammar.

It isn't too tricky to use. Basically, a PEG can act as both a lexical and
phrase grammar, and acts similarly to a forward-looking greedy regular
expression processor, except that it acts on tokens instead of characters.

This library allows for simple rules-based declarations and also custom parser
functions (you'll need at least one of these to get characters and literals
from the stream).

The parser can simplify and consolidate syntax elements in an upwards
direction along the syntax tree, and you can define a context class to do
something similar to a true attributed grammar.

-- Dustin Voss


-----------------
THINGS TO IMPROVE
-----------------

* Give it a more rigorous attributed grammar.

* Soft error recovery. Right now, the parser just gives up when it can't
  match. I'd like the parser to be capable of skipping to a checkpoint.
