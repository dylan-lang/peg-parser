module: peg-parser
synopsis: PEG parser cached and uncached parser functions.

// See parser-rules.dylan for a full explanation of rule parsers. Basically,
// rule parsers parse a stream in a given context and return a value or sequence
// of values called the "product."

// This auxiliary macro generates the cached or uncached parser function.

define macro parser-function
{  parser-function cached,
      #key ?token-name:name, ?token-type:expression, ?label:expression = #f,
      ??parser-attr:*, #all-keys;
   end
} => {
   define function "parse-" ## ?token-name
      (stream :: <positionable-stream>, context :: <parse-context>)
   => (value :: false-or(?token-type), success? :: <boolean>,
       failure :: false-or(<parse-failure>))
      let start-pos = stream.stream-position;
      let start-pos-index = as(<integer>, start-pos);

      indent-trace();
      let pos-cache = element(context.cache, start-pos-index, default: #f);
      let cached-result = pos-cache & element(pos-cache, ?#"token-name", default: #f);
      let (value, success?, failure) =
            if (cached-result)
               
               // Result cached. Return appropriate values.
               
               // Note a hit.
               if (*parser-cache-hits*)
                  context.parser-cache-hits[?#"token-name"] :=
                        element(context.parser-cache-hits, ?#"token-name", default: 0) + 1;
               end if;
               
               if (cached-result.success-pos)
                  stream.stream-position := cached-result.success-pos;
                  let end-pos-index = as(<integer>, cached-result.success-pos);
                  format-trace("%s (cached) matched stream pos %x-%x",
                               ?"token-name", start-pos-index, end-pos-index);
                  values(cached-result.semantic-value, #t,
                         cached-result.parse-failure);
               else
                  let err = cached-result.parse-failure;
                  let fail-pos-index = as(<integer>, err.failure-position);
                  format-trace("%s (cached) no match, exp. %s at stream pos %x",
                               ?"token-name", err.parse-expected, fail-pos-index);
                  values(cached-result.semantic-value, #f, err);
               end if;
            else
               
               // Result not cached. Call parser function.
               
               let parser-label = ?label;
               format-trace("%s...", ?"token-name");
               
               // Set up grammar attributes.
               with-attributes (??parser-attr, ...)

                  // Call parser rule to get product.
                  let (prod, succ? :: <boolean>, err :: false-or(<parse-failure>)) =
                        ?token-name ## "-parser-rule" (stream, context);

                  // Compute proposed semantic value.
                  let prop-value :: false-or(?token-type) =
                        succ? & ?token-name ## "-parser-value" (prod, start-pos,
                                                                stream.stream-position);

                  // Call user-defined afterwards clause, which may cause failure.
                  let after-error :: false-or(<parse-failure>) =
                        if (succ?)
                           let end-pos-index = as(<integer>, stream.stream-position);
                           "after-" ## ?token-name
                                 (context, prod, prop-value, start-pos-index, end-pos-index)
                        end if;

                  if (after-error)
                     after-error.failure-position := after-error.failure-position | start-pos;
                     if (after-error.empty-description?)
                        after-error.parse-expected-list :=
                              list(format-to-string("valid %s", parser-label | "input"));
                     end if;
                     succ? := #f;
                     err := after-error;
                  end if;
      
                  // Consolidate lower level error descriptions into a better
                  // description for this parser. Best description is anything
                  // from a later position, otherwise the parser label or
                  // after-error. If no better description, leave err alone.
                  if (~succ? & err.failure-position = start-pos)
                     if (parser-label & ~after-error)
                        err.parse-expected-list := list(parser-label);
                        err.parse-expected-other-than-list := #();
                     end if;
                  end if;
      
                  // Log results of parsing.
                  if (succ?)
                     let end-pos-index = as(<integer>, stream.stream-position);
                     format-trace("%s matched stream pos %x-%x",
                                  ?"token-name", start-pos-index, end-pos-index);
                  else
                     let fail-pos-index = as(<integer>, err.failure-position);
                     format-trace("%s no match, exp. %s at stream pos %x",
                                  ?"token-name", err.parse-expected, fail-pos-index);
                  end if;

                  // Compute actual semantic value.
                  let value :: false-or(?token-type) = succ? & prop-value;
                  
                  // Call user-defined cleanup clause.
                  "cleanup-" ## ?token-name (context, value, succ?, err);

                  // Store in cache.
                  unless (context.cache.size = 0)
                     let pos-cache = pos-cache | make(<table>);
                     context.cache[start-pos-index] := pos-cache;
                     pos-cache[?#"token-name"] :=
                           make(<parse-result>, value: value, failure: err,
                                success-pos: succ? & stream.stream-position);
                  end unless;

                  // Return values.
                  values(value, succ?, err);
               end with-attributes;
            end if;
      outdent-trace();
      values(value, success?, failure);
   end function
}

{  parser-function uncached,
      #key ?token-name:name, ?token-type:expression, ?label:expression = #f,
      ??parser-attr:*, #all-keys;
   end
} => {
   define function "parse-" ## ?token-name
      (stream :: <positionable-stream>, context :: <parse-context>)
   => (value :: false-or(?token-type), success? :: <boolean>,
       failure :: false-or(<parse-failure>))
      let start-pos = stream.stream-position;
      let start-pos-index = as(<integer>, start-pos);

      indent-trace();

      let parser-label = ?label;
      format-trace("%s...", ?"token-name");
      
      // Set up grammar attributes.
      with-attributes (??parser-attr, ...)

         // Call parser rule to get product.
         let (prod, succ? :: <boolean>, err :: false-or(<parse-failure>)) =
               ?token-name ## "-parser-rule" (stream, context);

         // Compute proposed semantic value.
         let prop-value :: false-or(?token-type) =
               succ? & ?token-name ## "-parser-value" (prod, start-pos,
                                                       stream.stream-position);

         // Call user-defined afterwards clause, which may cause failure.
         let after-error :: false-or(<parse-failure>) =
               if (succ?)
                  let end-pos-index = as(<integer>, stream.stream-position);
                  "after-" ## ?token-name
                        (context, prod, prop-value, start-pos-index, end-pos-index)
               end if;

         if (after-error)
            after-error.failure-position := after-error.failure-position | start-pos;
            if (after-error.empty-description?)
               after-error.parse-expected-list :=
                     list(format-to-string("valid %s", parser-label | "input"));
            end if;
            succ? := #f;
            err := after-error;
         end if;
      
         // Consolidate lower level error descriptions into a better
         // description for this parser. Best description is anything
         // from a later position, otherwise the parser label or
         // after-error. If no better description, leave err alone.
         if (~succ? & err.failure-position = start-pos)
            if (parser-label & ~after-error)
               err.parse-expected-list := list(parser-label);
               err.parse-expected-other-than-list := #();
            end if;
         end if;
      
         // Log results of parsing.
         if (succ?)
            let end-pos-index = as(<integer>, stream.stream-position);
            format-trace("%s matched stream pos %x-%x",
                         ?"token-name", start-pos-index, end-pos-index);
         else
            let fail-pos-index = as(<integer>, err.failure-position);
            format-trace("%s no match, exp. %s at stream pos %x",
                         ?"token-name", err.parse-expected, fail-pos-index);
         end if;

         // Compute actual semantic value.
         let value :: false-or(?token-type) = succ? & prop-value;

         // Call user-defined cleanup clause.
         "cleanup-" ## ?token-name (context, value, succ?, err);
      
         // Return values.
         outdent-trace();
         values(value, succ?, err);
      end with-attributes;
   end function
}
end macro;


// This auxiliary macro turns slot clauses into a class declaration.

define macro class-specifier
   {  class-specifier
         #key ?token-type:name, ??super:name = <token>, ??slot, #all-keys;
      end
   } => {
      define class ?token-type (??super, ...)
         ??slot; ...
      end class
   }

slot:
   { ?:variable } => { slot ?variable }
   { ?:variable = ?:expression } => { slot ?variable }
   { inherited ?:name = ?:expression } => { inherited slot ?name }
end macro;


// This auxiliary macro turns slot clauses into an initialize function. The
// initialization expressions will be in terms of ?product-name.

define macro initialize-specifier
   {  initialize-specifier
         #key ?token-type:name, ?product-name:name, ?product-type:expression,
         ??slot, #all-keys;
      end
   } => {
      define method initialize
            (token :: ?token-type, #next next-method,
             #key ?product-name :: type-union(?product-type, singleton(unsupplied()))
                  = unsupplied())
         next-method(token);
         if (supplied?(?product-name))
            ??slot; ...
         end if;
      end method;
   }

slot:
   { ?slot-name:name :: ?slot-type:expression = ?:expression }
      => { token.?slot-name := ?expression }
   { inherited ?slot-name:name = ?:expression }
      => { token.?slot-name := ?expression }
   { ?slot-name:name :: ?slot-type:expression }
      => { #f /* because Gwydion Dylan prefers ";#f;" to ";;" */ }
end macro;


// This auxiliary macro generates the after- and cleanup- functions.

define macro user-functions
   {  user-functions
         #key ?token-name:name, ?after-ctxt:variable, ?after-prod:variable,
         ?after-value:variable, ?after-start:variable, ?after-end:variable,
         ?after-body:expression, ?after-fail:name, ?cleanup-ctxt:variable,
         ?cleanup-value:variable, ?cleanup-succ:variable, ?cleanup-err:variable,
         ?cleanup-body:expression,
         #all-keys;
      end
   } => {
      define inline function "after-" ## ?token-name
         (?after-ctxt, ?after-prod, ?after-value, ?after-start, ?after-end)
      => (new-err :: false-or(<parse-failure>))
         block (?after-fail)
            ?after-body; #f
         end block
      end function;
      
      define inline function "cleanup-" ## ?token-name
         (?cleanup-ctxt, ?cleanup-value, ?cleanup-succ, ?cleanup-err)
      => ()
         ?cleanup-body
      end function
   }
end macro;

