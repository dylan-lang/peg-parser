module: peg-parser
synopsis: Discussion and implementation of PEG parser rules, as described at
          http://en.wikipedia.org/wiki/Parsing_expression_grammar

          
/// FUNCTION: rule parser
/// ---------------------
/// SYNOPSIS: A function that partially parses a stream according to a rule.
///
/// Rule parsers are created by the 'seq', 'choice', 'many', 'opt', 'opt-seq',
/// 'opt-choice', opt-many', 'req-next', and 'not-next' functions used in
/// 'parser-definer' macros. They may also be created manually for efficiency
/// or to support special behaviors via the 'parser-method-definer' macro.
///
/// Rule parsers must be named "parse-something" to work with the 'parser-definer'
/// macro. If the parser fails to match, it must signal a <parse-failure> error
/// after rolling back the position of the stream (so that another parser may 
/// be tried).
///
/// Rollback and naming are done automatically when using 'parser-method-definer'
/// or 'parser-definer' macros or the 'seq' etc. functions, but with the
/// 'parser-method-definer' macro, you have to signal <parse-failure> yourself.
///
/// ARGUMENTS:
///   stream   - An instance of <positionable-stream>.
///   context  - A context object.
/// VALUES:
///   product  - An instance of <sequence>, #f, or some other value (usually
///              an instance of <token>), depending on the parser's rule(s).
/// CONDITIONS:
///   If the parser fails to match, it signals <parse-failure>.


/// SYNOPSIS: Builds a rule parser matching a sequence of elements.
///           Equivalent to PEG "p1 p2" operation.
/// ARGUMENTS:
///   "#rest sub-rules" - A series of rule parsers, all of which must succeed
///                       for the returned parser to succeed.
/// VALUES:
///   rule-parser - A rule parser returning a <sequence>. The sequence will
///                 contain the sub-rules' products.
define function seq (#rest sub-rules) => (rule-parser :: <function>)
   local method seq-parser (stream :: <positionable-stream>, context)
   => (product :: <sequence>)
      let pos = stream.stream-position;
      let product = make(<vector>, size: sub-rules.size);
      block()
         for (rule in sub-rules, i from 0)
            product[i] := rule(stream, context);
         end for;
      exception (err :: <parse-failure>)
         stream.stream-position := pos;
         error(err);
      end block;
      product
   end method;
   
   let format-string = apply(concatenate, "all of", map(always(" %s"), sub-rules));
   *rule-name-parts*[seq-parser] := add(as(<list>, sub-rules), format-string);
   seq-parser
end function;


/// SYNOPSIS: Builds a rule parser matching one of several elements.
///           Equivalent to PEG "p1 / p2" operation.
/// ARGUMENTS:
///   "#rest sub-rules" - A series of rule parsers, the first of which to
///                       succeed supplies the parser's product.
/// VALUES:
///   rule-parser - A rule parser returning one of the sub-rules' products.
define function choice (#rest sub-rules) => (rule-parser :: <function>)
   local method choice-parser (stream :: <positionable-stream>, context)
   => (product)
      let pos = stream.stream-position;
      let product = #f;
      let found = #f;
      for (rule in sub-rules, until: product)
         block()
            product := rule(stream, context);
            found := #t;
         exception (err :: <parse-failure>)
            stream.stream-position := pos;
         end block;
      end for;
      if (~found)
         error(make(<parse-failure>, position: pos, expected: rule-name(choice-parser)));
      end if;
      product
   end method;
   
   let format-string = apply(concatenate, "one of", map(always(" %s"), sub-rules));
   *rule-name-parts*[choice-parser] := add(as(<list>, sub-rules), format-string);
   choice-parser
end function;


/// SYNOPSIS: Builds a rule parser matching one or more elements.
///           Equivalent to PEG "p1+" operation.
/// ARGUMENTS:
///   sub-rule - A rule parser.
/// VALUES:
///   rule-parser - A rule parser returning a <sequence> containing the
///                 sub-rule's products. 
define function many (sub-rule :: <function>) => (rule-parser :: <function>)
   local method many-parser (stream :: <positionable-stream>, context)
   => (product :: <sequence>)
      let pos = stream.stream-position;
      let product = make(<deque>);
      block()
         while (#t)
            push-last(product, sub-rule(stream, context));
         end while;
      exception (err :: <parse-failure>)
         // Only consider it a failure if product is empty.
         if (product.empty?)
            stream.stream-position := pos;
            error(err)
         end
      end block;
      product
   end method;
   
   *rule-name-parts*[many-parser] := list("one or more of %s", sub-rule);
   many-parser
end function;


/// SYNOPSIS: Builds a rule parser matching zero or one element.
///           Equivalent to PEG "p1?" operation.
/// ARGUMENTS:
///   sub-rule - A rule parser.
/// VALUES:
///   rule-parser - A rule parser returning the sub-rule's product, or #f if
///                 the element is not present.
define function opt (sub-rule :: <function>) => (rule-parser :: <function>)
   local method opt-parser (stream :: <positionable-stream>, context)
   => (product :: false-or(<object>))
      block()
         sub-rule(stream, context)
      exception (err :: <parse-failure>)
         #f    // Do nothing; indicates item not present.
      end block
   end method;
   
   *rule-name-parts*[opt-parser] := list("optional %s", sub-rule);
   opt-parser
end function;


/// SYNOPSIS: Builds a rule parser matching zero or more elements.
///           Equivalent to PEG "p1*" operation.
/// ARGUMENTS:
///   sub-rule - A rule parser.
/// VALUES:
///   rule-parser - A rule parser returning a <sequence> containing the
///                 sub-rule's products, or #f if the elements are not present. 
define function opt-many (sub-rule :: <function>) => (rule-parser :: <function>)
   local method opt-many-parser (stream :: <positionable-stream>, context)
   => (product :: false-or(<sequence>))
      let product = make(<deque>);
      block()
         while (#t)
            push-last(product, sub-rule(stream, context));
         end while;
      exception (err :: <parse-failure>)
         // Do nothing; indicates series has ended.
      end block;
      if (product.empty?) #f else product end if
   end method;
   
   *rule-name-parts*[opt-many-parser] := list("zero or more of %s", sub-rule);
   opt-many-parser
end function;


/// SYNOPSIS: Builds a rule parser matching all elements or none of them.
///           Equivalent to PEG "(p1 p2)?" operation.
/// ARGUMENTS:
///   "#rest sub-rules" - A series of rule parsers, all of which must match
///                       for this parser to match.
/// VALUES:
///   rule-parser - A rule parser returning #f or a <sequence> containing
///                 all sub-rules' products.
define function opt-seq (#rest sub-rules) => (rule-parser :: <function>)
   let parser = opt(apply(seq, sub-rules));
   let format-string = apply(concatenate, "optionally all of",
                             map(always(" %s"), sub-rules));
   *rule-name-parts*[parser] := add(as(<list>, sub-rules), format-string);
   parser
end function;


/// SYNOPSIS: Builds a rule parser matching one of the specified elements or
/// none of them. Equivalent to PEG "(p1 / p2)?" operation.
/// ARGUMENTS:
///   "#rest sub-rules" - A series of rule parsers.
/// VALUES:
///   rule-parser - A rule parser returning #f or the product of the
///                 matching rule.
define function opt-choice (#rest sub-rules) => (rule-parser :: <function>)
   let parser = opt(apply(choice, sub-rules));
   let format-string = apply(concatenate, "optionally one of",
                             map(always(" %s"), sub-rules));
   *rule-name-parts*[parser] := add(as(<list>, sub-rules), format-string);
   parser
end function;


/// SYNOPSIS: Builds a rule parser that looks ahead to match the sub-rule
/// without consuming any elements. Equivalent to PEG "&p1" operation.
/// ARGUMENTS:
///   sub-rule - A rule parser.
/// VALUES:
///   rule-parser - A rule parser returning #f.
define function req-next (sub-rule :: <function>) => (rule-parser :: <function>)
   local method req-next-parser (stream :: <positionable-stream>, context)
   => (product :: <boolean>)
      let pos = stream.stream-position;
      block()
         // Don't change context; don't keep results.
         sub-rule(stream, context);
         // If fails, condition continues on.
      cleanup
         stream.stream-position := pos;
      end block;
      #f // Doesn't consume anything.
   end method;
   
   *rule-name-parts*[req-next-parser] := list("expect %s", sub-rule);
   req-next-parser
end function;


/// SYNOPSIS: Builds a rule parser that looks ahead to ensure the sub-rule
/// does not match, but does not consume any elements in doing so.
/// Equivalent to PEG "!p1" operation.
/// ARGUMENTS:
///   sub-rule - A rule parser.
/// VALUES:
///   rule-parser - A rule parser returning #f.
define function not-next (sub-rule :: <function>) => (rule-parser :: <function>)
   local method not-next-parser (stream :: <positionable-stream>, context)
   => (product :: <boolean>)
      let pos = stream.stream-position;
      let failure =
            block()
               sub-rule(stream, context);
               make(<parse-failure>, position: pos, expected: rule-name(not-next-parser));
            cleanup
               stream.stream-position := pos;
            exception (err :: <parse-failure>)
               #f // Indicates success.
            end block;
      if (failure) error(failure) end;
      #f // Doesn't consume anything.
   end method;
   
   *rule-name-parts*[not-next-parser] := list("not %s", sub-rule);
   not-next-parser
end function;
