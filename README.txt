This is a recursive descent parser that handles parsing expression grammars
(PEGs), as described at
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

* Implement caching. The cache could consist of a data structure similar to:
    
    Character
    position   Cached parse results

    130        for "verb" + context A: no match
               for "verb" + context B: no match
               for "article" + context C: matched, result "the"
               for "article" + context B: no match
               for "sentence" + context C: matched, result "the dog bit me"
               
    131        ...

  Since the parser never looks backward, as the parser consumes tokens, old
  data could be discarded. A look-ahead counter would have to be maintained,
  as look-ahead *pretends* to consume tokens and old data shouldn't be
  discarded while looking ahead. But what the look-ahead sees can stay in the
  cache after the look-ahead finishes.

  I expect a <deque> plus *cache-start-position* plus *lookahead-depth* would
  do the trick. Each <deque> element would be a <table> where the keys are a
  combination of a token name and a context and the values would be the
  product of that token's parser (if it matches). The *lookahead-depth* would
  be incremented then decremented by the req-next and not-next parsers.
  Cache-hit testing would happen in the parse-xxx functions, and perhaps the
  updating of *cache-start-position* and discarding of old data would too.

* Soft error recovery. Right now, the parser just gives up at the first thing
  it can't match. I'm not sure what it should do, but it doesn't do it.
