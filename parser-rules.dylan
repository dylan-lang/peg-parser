module: peg-parser
synopsis: Discussion and implementation of PEG parser rules, as described at
          http://en.wikipedia.org/wiki/Parsing_expression_grammar


/**          
FUNCTION: rule parser
---------------------
SYNOPSIS: A function that partially parses a stream according to a rule.

Rule parsers are created by the 'seq', 'choice', 'many', 'opt', 'opt-seq',
'opt-choice', opt-many', 'req-next', 'not-next', and 'nil' functions used in
'parser-definer' macros. They may also be created manually for efficiency
or to support special behaviors via the 'parser-method-definer' macro.

Rule parsers must be named "parse-something" to work with the 'parser-definer'
macro. If the parser fails to match or reaches end-of-stream, it must return
#f for the 'product' and 'success?' values and roll back the position of the
stream so that another parser may be tried.

If the parser fails to match, it must return an instance of <parse-failure>
describing the expected or unexpected element that caused the parser to fail.

If the parser succeeds, it may return an instance of <parse-success> or
<parse-failure>. It should return an instance of <parse-failure> to describe
errors that arose while parsing, but from which the parser backtracked and
recovered. If there were no parse errors, recovered or otherwise, then the
parser should return <parse-success>. Parse errors should be combined with
'combine-extents'.

Conceptually, <parse-failure> instances describe why a parser did not parse
further than it did. If all parsers ultimately fail to match, one of these
errors indicated a possible root cause. <Parse-success> instances describe the
furthest the parser has parsed and are only relevant for 'not-next' parsers.

All these behaviors are implemented automatically when using
'parser-method-definer' or 'parser-definer' macros or the 'seq' etc. functions.

ARGUMENTS:
   stream   - An instance of <positionable-stream>.
   context  - A context object.

VALUES:
   product     - An instance of <sequence>, #f, or some other value
                 (usually an instance of <token>), depending on the
                 parser's rule(s). #f indicates no stream elements were
                 consumed or the token was not present.
   success?    - An instance of <boolean>, indicating whether the parser
                 succeeded. Parsers may succeed even if no product results.
   extent      - An instance of <parse-extent>.
**/


/**
SYNOPSIS: Builds a rule parser matching a sequence of elements.
          Equivalent to PEG "p1 p2" operation.
ARGUMENTS:
   #rest sub-rules - A series of rule parsers, all of which must succeed
                     for the returned parser to succeed.
VALUES:
   rule-parser - A rule parser returning a <sequence>. The sequence will
                 contain the sub-rules' products.
**/
define function seq (#rest sub-rules) => (rule-parser :: <function>)
   local method parse-seq
      (stream :: <positionable-stream>, context :: <parse-context>)
   => (product :: false-or(<sequence>), success? :: <boolean>, extent :: <parse-extent>)
      let pos = stream.stream-position;
      let product = make(<vector>, size: sub-rules.size);
      let found? = #t;
      let combined-extent = make(<parse-success>, position: pos);
      for (rule in sub-rules, i from 0, while: found?)
         let (prod, succ?, ext) = rule(stream, context);
         combined-extent := combine-extents(combined-extent, ext);
         if (succ?)
            product[i] := prod;
         else
            stream.stream-position := pos;
            found? := #f;
         end if;
      end for;
      if (~found? & instance?(combined-extent, <parse-success>))
         combined-extent := make(<parse-failure>, position: pos)
      end if;
      values(found? & product, found?, combined-extent)
   end method;
   parse-seq
end function;


/**
SYNOPSIS: Builds a rule parser matching one of several elements.
          Equivalent to PEG "p1 / p2" operation.
ARGUMENTS:
   #rest sub-rules - A series of rule parsers, the first of which to
                     succeed supplies the parser's product.
VALUES:
   rule-parser - A rule parser returning one of the sub-rules' products.
**/
define function choice (#rest sub-rules) => (rule-parser :: <function>)
   local method parse-choice
      (stream :: <positionable-stream>, context :: <parse-context>)
   => (product :: false-or(<object>), success? :: <boolean>, extent :: <parse-extent>)
      let pos = stream.stream-position;
      let product = #f;
      let found? = #f;
      let combined-extent = make(<parse-success>, position: pos);
      for (rule in sub-rules, until: found?)
         let (prod, succ?, ext) = rule(stream, context);
         combined-extent := combine-extents(combined-extent, ext);
         if (succ?)
            product := prod;
            found? := #t;
         else
            stream.stream-position := pos;
         end if;
      end for;
      if (~found? & instance?(combined-extent, <parse-success>))
         combined-extent := make(<parse-failure>, position: pos)
      end if;
      values(product, found?, combined-extent)
   end method;
   parse-choice
end function;


/**
SYNOPSIS: Builds a rule parser matching one or more elements.
          Equivalent to PEG "p1+" operation.
ARGUMENTS:
   sub-rule - A rule parser.
VALUES:
   rule-parser - A rule parser returning a <sequence> containing the
                 sub-rule's products. 
**/
define function many (sub-rule :: <function>) => (rule-parser :: <function>)
   local method parse-many
      (stream :: <positionable-stream>, context :: <parse-context>)
   => (product :: false-or(<sequence>), success? :: <boolean>, extent :: <parse-extent>)
      let pos = stream.stream-position;
      let product = make(<deque>);
      let found? = #t;
      let combined-extent = make(<parse-success>, position: pos);
      while (found?)
         let (prod, succ?, ext) = sub-rule(stream, context);
         combined-extent := combine-extents(combined-extent, ext);
         if (succ?)
            push-last(product, prod);
         else
            found? := #f;
         end if;
      end while;
      if (product.empty?)
         stream.stream-position := pos;
         product := #f;
         combined-extent := make(<parse-failure>, position: pos);
      end if;
      values(product, product ~= #f, combined-extent)
   end method;
   parse-many
end function;


/**
SYNOPSIS: Builds a rule parser matching zero or one element.
          Equivalent to PEG "p1?" operation.
ARGUMENTS:
   sub-rule - A rule parser.
VALUES:
   rule-parser - A rule parser returning the sub-rule's product, or #f if
                 the element is not present.
**/
define function opt (sub-rule :: <function>) => (rule-parser :: <function>)
   local method parse-opt
      (stream :: <positionable-stream>, context :: <parse-context>)
   => (product :: false-or(<object>), success? :: singleton(#t), extent :: <parse-extent>)
      let (prod, succ?, ext) = sub-rule(stream, context);
      values(prod, #t, ext);
   end method;
   parse-opt
end function;


/**
SYNOPSIS: Builds a rule parser matching zero or more elements.
          Equivalent to PEG "p1*" operation.
ARGUMENTS:
   sub-rule - A rule parser.
VALUES:
   rule-parser - A rule parser returning a <sequence> containing the
                 sub-rule's products, or #f if the elements are not present. 
**/
define function opt-many (sub-rule :: <function>) => (rule-parser :: <function>)
   opt(many(sub-rule))
end function;


/**
SYNOPSIS: Builds a rule parser matching all elements or none of them.
          Equivalent to PEG "(p1 p2)?" operation.
ARGUMENTS:
   #rest sub-rules - A series of rule parsers, all of which must match
                     for this parser to match.
VALUES:
   rule-parser - A rule parser returning #f or a <sequence> containing
                 all sub-rules' products.
**/
define function opt-seq (#rest sub-rules) => (rule-parser :: <function>)
   opt(apply(seq, sub-rules));
end function;


/**
SYNOPSIS: Builds a rule parser matching one of the specified elements or
none of them. Equivalent to PEG "(p1 / p2)?" operation.
ARGUMENTS:
   #rest sub-rules - A series of rule parsers.
VALUES:
   rule-parser - A rule parser returning #f or the product of the
                 matching rule.
**/
define function opt-choice (#rest sub-rules) => (rule-parser :: <function>)
   opt(apply(choice, sub-rules));
end function;


/**
SYNOPSIS: Builds a rule parser that looks ahead to match the sub-rule
without consuming any elements. Equivalent to PEG "&p1" operation.
ARGUMENTS:
   sub-rule - A rule parser.
VALUES:
   rule-parser - A rule parser returning #f.
**/
define function req-next (sub-rule :: <function>) => (rule-parser :: <function>)
   local method parse-req-next
      (stream :: <positionable-stream>, context :: <parse-context>)
   => (product :: singleton(#f), success? :: <boolean>, extent :: <parse-extent>)
      context.lookahead-depth := context.lookahead-depth + 1;
      let pos = stream.stream-position;
      let (prod, succ?, ext) = sub-rule(stream, context);
      if (~succ?)
         ext := combine-extents(ext, make(<parse-failure>, position: pos))
      else
         ext := combine-extents(ext, make(<parse-success>, position: pos))
      end if;
      stream.stream-position := pos;
      context.lookahead-depth := context.lookahead-depth - 1;
      values(#f, succ?, ext)
   end method;
   parse-req-next
end function;


/**
SYNOPSIS: Builds a rule parser that looks ahead to ensure the sub-rule
does not match, but does not consume any elements in doing so.
Equivalent to PEG "!p1" operation.
ARGUMENTS:
   sub-rule - A rule parser.
VALUES:
   rule-parser - A rule parser returning #f.
**/
define function not-next (sub-rule :: <function>) => (rule-parser :: <function>)
   local method parse-not-next
      (stream :: <positionable-stream>, context :: <parse-context>)
   => (product :: singleton(#f), success? :: <boolean>, extent :: <parse-extent>)
      context.lookahead-depth := context.lookahead-depth + 1;
      let pos = stream.stream-position;
      let (prod, succ?, ext) = sub-rule(stream, context);
      if (succ?)
         if (instance?(ext, <parse-success>))
            ext := make(<parse-failure>, position: ext.parse-position,
                        expected-other-than-list: ext.parse-success-list)
         else
            ext := make(<parse-failure>, position: ext.parse-position,
                        expected-list: ext.parse-expected-other-than-list,
                        expected-other-than-list: ext.parse-expected-list)
         end if;
         ext := combine-extents(ext, make(<parse-failure>, position: pos))
      else
         ext := combine-extents(ext, make(<parse-success>, position: pos))
      end if;
      stream.stream-position := pos;
      context.lookahead-depth := context.lookahead-depth - 1;
      values(#f, ~succ?, ext)
   end method;
   parse-not-next
end function;


/**
SYNOPSIS: Builds a rule parser that does not consume any input and always
succeeds, returning a constant semantic value. Useful for distinguishing
and aligning parallel token sequences.
ARGUMENTS:
   product - An instance of <object>.
VALUES:
   rule-parser - A rule parser returning 'product'.
**/
define function nil (product :: <object>) => (rule-parser :: <function>)
   local method parse-nil
      (stream, context :: <parse-context>)
   => (product :: <object>, success? :: singleton(#t), extent :: <parse-success>)
      values(product, #t, make(<parse-success>, position: stream.stream-position))
   end method;
   parse-nil
end function;


/**
SYNOPSIS: Builds a rule parser that executes the sub-rule but attempts to
prevent 'combine-extents' from affecting its caller's existing <parse-extent>.
This rule is intended for use with a fallback element in a 'choice' rule that
ignores a parse failure and continues.
ARGUMENTS:
  rule - A rule parser.
VALUES:
  rule-parser - As 'rule', but does not affect caller's extent if successful.
*/
define function skip (rule :: <function>)
=> (rule-parser :: <function>)
   local method parse-skip
      (stream, context :: <parse-context>)
   => (product :: <object>, success? :: <boolean>, extent :: <parse-extent>)
      let (prod, succ?, ext) = rule(stream, context);
      if (succ?)
         values(prod, succ?, make(<parse-success>, position: stream.stream-position))
      else
         values(prod, succ?, ext)
      end if;
   end method;
   parse-skip
end function;
