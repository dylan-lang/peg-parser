module: peg-parser
synopsis: PEG parser macro definitions.


/// SYNOPSIS: Defines an arbitrary 'rule parser'.
/// DISCUSSION: This macro defines a rule parser that includes support for
/// debugging and other features described for rule parsers.
define macro parser-method-definer
   {
      define parser-method ?token:name
      (?stream:name, ?context:name :: ?type:name)
      => (?results:*)
         ?:body
      end
   } => {
      define function "parse-" ## ?token
      (?stream :: <positionable-stream>, ?context :: ?type)
      => (?results)
         indent-trace();
         format-trace(?"token" ## "...");
         let pos = ?stream.stream-position;
         block()
            ?body
         afterwards
            format-trace(?"token" ## " matched chars %x-%x",
                   pos, ?stream.stream-position);
         cleanup
            outdent-trace();
         exception (err :: <parse-failure>)
            ?stream.stream-position := pos;
            indent-trace();
            format-trace(?"token" ## " no match, exp. %s at char %x",
                         err.parse-expected, err.failure-position);
            outdent-trace();
            error(err)
         end;
      end function;

      *rule-names*["parse-" ## ?token] := ?"token";
      *rule-name-parts*["parse-" ## ?token] := #( ?"token" );
   }
end macro;


/// SYNOPSIS: Defines a 'rule parser' and perhaps a token class for a given
/// token.
///
/// The macro takes three forms. A form like
/// [code]
/// define parser t (<c>)
///   rule many(t2) => tokens;
///   slot content = tokens[1];
/// end parser;
/// [end code]
/// defines a rule parser named `parse-t` and a token class named `<t-token>`
/// which inherits from `<c>` (optional) and `<token>` [code qv]. `<t-token>` will
/// have a slot named `content` that gets initialized to the expression `tokens[1]`,
/// where `tokens` is the product of the rule `many(t2)`.
/// 
/// A form like 
/// [code]
/// define parser t
///   rule many(t2) => tokens;
///   yield tokens[1];
/// end parser;
/// [end code]
/// defines a rule parser that returns `tokens[1]` directly, without defining
/// a `<t-token>` class.
///
/// A form like
/// [code]
/// define parser t
///   rule many(t2)
/// end parser;
/// [end code]
/// defines a rule parser that return `#"t"`.
define macro parser-definer

   //
   // This form creates a parser that return an initialized <token> class.
   {
      define parser ?token-name:name (?supers)
         rule ?rule => ?product-name:name;
         ?class-slots
      end
   } => {
      // Define the class.
      class-specifier(?token-name; ?supers; ?class-slots);
      
      // Initialize the class's slots based on results of rule.
      define method initialize (?token-name :: "<" ## ?token-name ## "-token>",
            #next next-method, #key ?product-name = unsupplied())
         next-method();
         if (supplied?(?product-name))
            slot-initializers(?token-name; ?class-slots)
         end if;
      end method;
      
      // Define the parser rule as the result of all the 'seq' etc. functions.
      define constant ?token-name ## "-parser-rule" = ?rule;
      
      // Define the parser function including tracing and rollback. Result is
      // <token> subclass, slots initialized by 'initialize' function above.
      define function "parse-" ## ?token-name
         (stream :: <positionable-stream>, context)
      => (token :: "<" ## ?token-name ## "-token>")
         indent-trace();
         format-trace(?"token-name" ## "...");
         let pos = stream.stream-position;
         let production =
               block()
                  ?token-name ## "-parser-rule" (stream, context)
               afterwards
                  format-trace(?"token-name" ## " matched chars %x-%x",
                               pos, stream.stream-position);
               cleanup
                  outdent-trace();
               exception (err :: <parse-failure>)
                  err.parse-expected :=
                        concatenate!(err.parse-expected, " in " ## ?"token-name");
                  indent-trace();
                  format-trace(?"token-name" ## " no match, exp. %s at char %x",
                               err.parse-expected, err.failure-position);
                  outdent-trace();
                  error(err)
               end;
         make("<" ## ?token-name ## "-token>", 
              start: pos, end: stream.stream-position, ?product-name: production)
      end function;
      
      // Names for the parser.
      *rule-names*["parse-" ## ?token-name] := "?token-name";
      *rule-name-parts*["parse-" ## ?token-name] := #( ?"token-name" );
   }

   //
   // This form creates a parser that returns the result of an expression.
   {
      define parser ?token-name:name
         rule ?rule => ?product-name:name;
         yield ?:expression;
      end
   } => {
      // Define the parser rule as the result of all the 'seq' etc. functions.
      define constant ?token-name ## "-parser-rule" = ?rule;
      
      // Define the parser function including tracing and rollback. Result is
      // yield expression.
      define function "parse-" ## ?token-name
            (stream :: <positionable-stream>, context) => (token)
         indent-trace();
         format-trace(?"token-name" ## "...");
         let pos = stream.stream-position;
         let ?product-name =
               block()
                  ?token-name ## "-parser-rule" (stream, context)
               afterwards
                  format-trace(?"token-name" ## " matched chars %x-%x",
                               pos, stream.stream-position);
               cleanup
                  outdent-trace();
               exception (err :: <parse-failure>)
                  err.parse-expected :=
                        concatenate!(err.parse-expected, " in " ## ?"token-name");
                  indent-trace();
                  format-trace(?"token-name" ## " no match, exp. %s at char %x",
                         err.parse-expected, err.failure-position);
                  outdent-trace();
                  error(err)
               end;
         ?expression
      end function;
      
      // Names for parser.
      *rule-names*["parse-" ## ?token-name] := "?token-name";
      *rule-name-parts*["parse-" ## ?token-name] := #( ?"token-name" );
   }

   //
   // This form creates a parser that returns a symbol.
   {
      define parser ?token-name:name
         rule ?rule;
      end
   } => {
      // Define the parser rule as the result of all the 'seq' etc. functions.
      define constant ?token-name ## "-parser-rule" = ?rule;
      
      // Define the parser function including tracing and rollback. Result is
      // a symbol, same as token name.
      define function "parse-" ## ?token-name
            (stream :: <positionable-stream>, context) => (token :: <symbol>)
         indent-trace();
         format-trace(?"token-name" ## "...");
         let pos = stream.stream-position;
         block()
            ?token-name ## "-parser-rule" (stream, context)
         afterwards
            format-trace(?"token-name" ## " matched chars %x-%x",
                         pos, stream.stream-position);
         cleanup
            outdent-trace();
         exception (err :: <parse-failure>)
            err.parse-expected :=
                  concatenate!(err.parse-expected, " in " ## ?"token-name");
            format-trace("  " ## ?"token-name" ## " no match, exp. %s at %x",
                         err.parse-expected, err.failure-position);
            error(err)
         end;
         ?#"token-name"
      end function;
      
      // Names for parser.
      *rule-names*["parse-" ## ?token-name] := ?"token-name";
      *rule-name-parts*["parse-" ## ?token-name] := #( ?"token-name" );
   }
   
// Only allow names, the 'seq' etc. functions, or (as a fallback) any token.
// Names are assumed to be tokens, and changed to the tokens' parser function.
// 'seq' etc. takes those parser functions and generates a new function from
// them.
rule:
   { seq(?nested-rule) ... } => { seq(?nested-rule) ... }
   { choice(?nested-rule) ... } => { choice(?nested-rule) ... }
   { many(?nested-rule) ... } => { many(?nested-rule) ... }
   { opt(?nested-rule) ... } => { opt(?nested-rule) ... }
   { opt-seq(?nested-rule) ... } => { opt-seq(?nested-rule) ... }
   { opt-choice(?nested-rule) ... } => { opt-choice(?nested-rule) ... }
   { opt-many(?nested-rule) ... } => { opt-many(?nested-rule) ... }
   { req-next(?nested-rule) ... } => { req-next(?nested-rule) ... }
   { not-next(?nested-rule) ... } => { not-next(?nested-rule) ... }
   { ?:name ... } => { "parse-" ## ?name ... }
   { ?:token ... } => { ?token ... }
   { } => { }
nested-rule:
   { ?rule } => { ?rule }

// Make sure token classes at least inherit from <token>.
supers:
   { ?:name, ... } => { ?name, ... }
   { } => { <token> }

// No transformation; simply describe/enforce syntax.
class-slots:
   { slot ?:variable = ?:expression; ... } => { slot ?variable = ?expression; ... }
   { slot ?:variable; ... } => { slot ?variable; ... }
   { } => { }
end macro;


// This auxiliary macro turns slot clauses into a class declaration. It can't
// just do the slot part because `define class` can't have a macro call inside.
define macro class-specifier
   {
      class-specifier (?:name; ?supers:*; ?class-slots)
   } => {
      define class "<" ## ?name ## "-token>" (?supers)
         ?class-slots;
      end class;
   }
class-slots:
   { slot ?:variable = ?:expression; ... } => { slot ?variable; ... }
   { slot ?:variable; ... } => { slot ?variable; ... }
   { } => { }
end macro;


// This auxiliary macro turns slot clauses into field initializations. This is
// done in the 'initialize' function for the token class so that we have access
// to the parse product as declared.
define macro slot-initializers
   { slot-initializers (?token:name;
                        slot ?slot-name:name :: ?slot-type:expression = ?:expression;
                        ?more:*) }
      => { ?token.?slot-name := ?expression; slot-initializers(?token; ?more) }
   { slot-initializers (?token:name;
                        slot ?slot-name:name :: ?slot-type:expression;
                        ?more:*) }
      => { slot-initializers(?token; ?more) }
   { slot-initializers (?token:name) }
      => { }
end macro;
