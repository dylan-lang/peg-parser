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
If the parser succeeds, it may return #f or an instance of <parse-failure>. In
this case, the instance of <parse-failure> describes errors that arose while
parsing, but from which the parser backtracked and recovered. If there were no
parse errors, recovered or otherwise, then the parser should return #f. Parse
errors should be combined with 'combine-errors'.

Conceptually, <parse-failure> instances describe why a parser did not parse
further than it did. If all parsers ultimately fail to match, one of these
errors indicated a possible root cause.

These behaviors are done automatically when using 'parser-method-definer' or
'parser-definer' macros or the 'seq' etc. functions.

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
   error       - An instance of <parse-failure> or #f. A <parse-failure> may be
                 returned even if the parser succeeds.
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
   => (product :: false-or(<sequence>),
       success? :: <boolean>, error :: false-or(<parse-failure>))
      let pos = stream.stream-position;
      let product = make(<vector>, size: sub-rules.size);
      let found? = #t;
      let combined-error = #f;
      for (rule in sub-rules, i from 0, while: found?)
         let (prod, succ?, err) = rule(stream, context);
         combined-error := combine-errors(combined-error, err);
         if (succ?)
            product[i] := prod;
         else
            stream.stream-position := pos;
            found? := #f;
         end if;
      end for;
      if (~found? & ~combined-error)
         combined-error := make(<parse-failure>, position: pos);
      end if;
      values(found? & product, found?, combined-error)
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
   => (product :: false-or(<object>),
       success? :: <boolean>, error :: false-or(<parse-failure>))
      let pos = stream.stream-position;
      let product = #f;
      let found? = #f;
      let combined-error = #f;
      for (rule in sub-rules, until: found?)
         let (prod, succ?, err) = rule(stream, context);
         combined-error := combine-errors(combined-error, err);
         if (succ?)
            product := prod;
            found? := #t;
         else
            stream.stream-position := pos;
         end if;
      end for;
      if (~found? & ~combined-error)
         combined-error := make(<parse-failure>, position: pos);
      end if;
      values(product, found?, combined-error)
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
   => (product :: false-or(<sequence>),
       success? :: <boolean>, error :: false-or(<parse-failure>))
      let pos = stream.stream-position;
      let product = make(<deque>);
      let found? = #t;
      let combined-error = #f;
      while (found?)
         let (prod, succ?, err) = sub-rule(stream, context);
         combined-error := combine-errors(combined-error, err);
         if (succ?)
            push-last(product, prod);
         else
            found? := #f;
         end if;
      end while;
      if (product.empty?)
         stream.stream-position := pos;
         product := #f;
         combined-error := combined-error | make(<parse-failure>, position: pos);
      end if;
      values(product, product ~= #f, combined-error)
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
   => (product :: false-or(<object>),
       success? :: singleton(#t), error :: false-or(<parse-failure>))
      let (prod, succ?, err) = sub-rule(stream, context);
      values(prod, #t, err);
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
   => (product :: singleton(#f),
       success? :: <boolean>, error :: false-or(<parse-failure>))
      context.lookahead-depth := context.lookahead-depth + 1;
      let pos = stream.stream-position;
      let (prod, succ?, err) = sub-rule(stream, context);
      if (~succ?)
         err := combine-errors(err, make(<parse-failure>, position: pos));
      end if;
      stream.stream-position := pos;
      context.lookahead-depth := context.lookahead-depth - 1;
      values(#f, succ?, err)
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
   => (product :: singleton(#f),
       success? :: <boolean>, error :: false-or(<parse-failure>))
      context.lookahead-depth := context.lookahead-depth + 1;
      let pos = stream.stream-position;
      let (prod, succ?, err) = sub-rule(stream, context);
      if (succ?)
         let comb = combine-errors(err, make(<parse-failure>, position: pos));
         err := make(<parse-failure>, position: comb.failure-position,
                     expected-list: comb.parse-expected-other-than-list,
                     expected-other-than-list: comb.parse-expected-list);
      end if;
      stream.stream-position := pos;
      context.lookahead-depth := context.lookahead-depth - 1;
      values(#f, ~succ?, err)
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
   => (product :: <object>,
       success? :: singleton(#t), error :: singleton(#f))
      values(product, #t, #f)
   end method;
   parse-nil
end function;


/**
SYNOPSIS: Builds a rule parser that executes the sub-rule but attempts to
preserve <parse-failure>. This rule is intended for use with a fallback element
in a 'choice' rule that ignores a parse failure and continues.
ARGUMENTS:
  rule - A rule parser.
VALUES:
  rule-parser - As 'rule', but does not affect <parse-failure> if successful.
*/
define function skip (rule :: <function>)
=> (rule-parser :: <function>)
   local method parse-skip
      (stream, context :: <parse-context>)
   => (product :: <object>,
       success? :: <boolean>, error :: false-or(<parse-failure>))
      let (prod, succ?, err) = rule(stream, context);
      values(prod, succ?, ~succ? & err)
   end method;
   parse-skip
end function;
