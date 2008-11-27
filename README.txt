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


-----------------
THINGS TO IMPROVE
-----------------

* Soft error recovery. Right now, the parser just gives up when it can't
  figure out a valid parse. I'd like the parser to be capable of skipping to a
  checkpoint.

I think I have a good way to do this. Make an operator function called
resync-parse that takes two arguments, each the name of a parser function. The
first parser function employs some heuristic to skip ahead in the stream to a
point where the second parser function should be able to pick up parsing.

If the first parser function succeeds, the resync-parse operation succeeds and
returns that semantic value. After that, all other parser functions fail
until, in the normal course of backtracking and trying alternate parses, the
second parser function is executed. That one executes normally and recovery is
completed. If the first parser function fails, the resync-parse operation
fails and the parser retries or gives up as it would in the absence of the
resync-parse operation.

Regardless, the accumulated error returned by the resync-parse operation is
the error that caused it to be attempted.
