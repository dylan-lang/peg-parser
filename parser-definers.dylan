module: peg-parser
synopsis: PEG parser macro definitions.

// See parser-rules.dylan for a full explanation of rule parsers. Basically,
// rule parsers parse a stream in a given context and return a value or sequence
// of values called the "product."

/// SYNOPSIS: Defines an arbitrary 'rule parser'.
/// DISCUSSION: This macro defines a rule parser that includes support for
/// debugging and other features described for rule parsers. The main part of
/// the parser is Dylan code supplied by you.
///
/// [code]
/// define parser-method char (stream, context)
/// => (char :: false-or(<character>))
///   read-element(stream, on-end-of-stream: #f)
/// end
/// [end code]
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
            format-trace(?"token" ## " matched stream pos %x-%x",
                   pos, ?stream.stream-position);
         cleanup
            outdent-trace();
         exception (err :: <parse-failure>)
            ?stream.stream-position := pos;
            indent-trace();
            format-trace(?"token" ## " no match, exp. %s at stream pos %x",
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
/// The macro takes three forms: class, yield, and basic.
/// 
/// === Class form ===
///
/// This form creates a token class.
///
/// [code]
/// define parser t (<c>)
///   rule many(t2) => tokens;
///   inherited slot content = tokens[1];
///   slot more-content :: <string> = tokens[2];
/// end parser;
/// [end code]
///
/// defines a rule parser named `parse-t` and a token class named `<t-token>`
/// which inherits from `<c>` (optional) and `<token>`. `<t-token>` will have
/// a slot named `content` (inherited from <c>) and a slot named `more-content`.
/// When <t-token> is initialized, `tokens` gets set to the product of the rule
/// `many(t2)`, `content` gets set to the expression `tokens[1]`, and
/// `more-content` gets set to the expression `tokens[2]` (which must be a
/// <string>).
/// 
/// === Yield form ===
///
/// Yield form returns a value.
///
/// [code]
/// define parser t
///   rule many(t2) => tokens;
///   yield tokens[1];
/// end parser;
/// [end code]
///
/// defines a rule parser that returns `tokens[1]` directly, without defining
/// a `<t-token>` class.
///
/// === Basic form ===
///
/// This form returns a token symbol.
///
/// [code]
/// define parser t
///   rule many(t2)
/// end parser;
/// [end code]
///
/// defines a rule parser that return `#"t"`.
///
/// === Affecting context ===
///
/// All three forms allow two additional clauses, "afterwards" and "cleanup,"
/// that perform actions after the rule parser matches or fails to match.
///
/// [code]
/// define parser t ()
///   rule many(t2) => tokens;
///   slot t2 = tokens.count
///   afterwards (context, tokens)
///     // Executes if match is successful. 'tokens' is local to this clause.
///     context.total-t2-count := context.total-t2-count + tokens.size
///   cleanup (context)
///     // Executes if match is successful or not after 'afterwards' clause.
///     // 'tokens' is not accessible.
///     context.tried-t? := #t
/// end parser;
/// [end code]
///
/// [code]
/// define parser t
///   rule many(t2);
///   afterwards (context, tokens)
///     // The product of this parser is #"t" because this is a token symbol
///     // parser, but the local variable 'tokens' will be the product of
///     // many(t2).
///     ...
/// end parser;
/// [end code]
///
define macro parser-definer

   //
   // This form creates a parser that return an initialized <token> class.
   {
      define parser ?token-name:name (?supers)
         rule ?rule => ?product-name:name;
         ?class-slots-and-clauses
      end
   } => {
      // Define the class.
      class-specifier(?token-name; ?supers; ?class-slots-and-clauses);
      
      // Initialize the class's slots based on results of rule.
      define method initialize (?token-name :: "<" ## ?token-name ## "-token>",
            #next next-method, #key ?product-name = unsupplied())
         next-method();
         if (supplied?(?product-name))
            slot-initializers(?token-name; ?class-slots-and-clauses)
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
                  // User-defined match action on rule product
                  "match-" ## ?token-name
                        (context, ?token-name ## "-parser-rule" (stream, context))
               afterwards
                  format-trace(?"token-name" ## " matched stream pos %x-%x",
                               pos, stream.stream-position);
               cleanup
                  outdent-trace();
                  // User-defined cleanup actions
                  "cleanup-" ## ?token-name (context);
               exception (err :: <parse-failure>)
                  unless (err.failure-position) err.failure-position := pos end;
                  err.parse-expected :=
                        concatenate!(err.parse-expected, " in " ## ?"token-name");
                  indent-trace();
                  format-trace(?"token-name" ## " no match, exp. %s at stream pos %x",
                               err.parse-expected, err.failure-position);
                  outdent-trace();
                  error(err)
               end;
         make("<" ## ?token-name ## "-token>", 
              start: pos, end: stream.stream-position, ?product-name: production)
      end function;
      
      // User defined action functions
      user-functions(?token-name; ?class-slots-and-clauses);
      
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
         ?body-clauses
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
                  // User-defined match action on rule product
                  "match-" ## ?token-name
                        (context, ?token-name ## "-parser-rule" (stream, context))
               afterwards
                  format-trace(?"token-name" ## " matched stream pos %x-%x",
                               pos, stream.stream-position);
               cleanup
                  outdent-trace();
                  // User-defined cleanup actions
                  "cleanup-" ## ?token-name (context);
               exception (err :: <parse-failure>)
                  unless (err.failure-position) err.failure-position := pos end;
                  err.parse-expected :=
                        concatenate!(err.parse-expected, " in " ## ?"token-name");
                  indent-trace();
                  format-trace(?"token-name" ## " no match, exp. %s at stream pos %x",
                         err.parse-expected, err.failure-position);
                  outdent-trace();
                  error(err)
               end;
         ?expression
      end function;
      
      // User defined action functions
      user-functions(?token-name; ?body-clauses);
      
      // Names for parser.
      *rule-names*["parse-" ## ?token-name] := "?token-name";
      *rule-name-parts*["parse-" ## ?token-name] := #( ?"token-name" );
   }

   //
   // This form creates a parser that returns a symbol.
   {
      define parser ?token-name:name
         rule ?rule;
         ?body-clauses
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
            // User-defined match action on rule product
            "match-" ## ?token-name
                  (context, ?token-name ## "-parser-rule" (stream, context))
         afterwards
            format-trace(?"token-name" ## " matched stream pos %x-%x",
                         pos, stream.stream-position);
         cleanup
            outdent-trace();
            // User-defined cleanup actions
            "cleanup-" ## ?token-name (context);
         exception (err :: <parse-failure>)
            unless (err.failure-position) err.failure-position := pos end;
            err.parse-expected :=
                  concatenate!(err.parse-expected, " in " ## ?"token-name");
            format-trace("  " ## ?"token-name" ## " no match, exp. %s at stream pos %x",
                         err.parse-expected, err.failure-position);
            error(err)
         end;
         ?#"token-name"
      end function;
      
      // User defined action functions
      user-functions(?token-name; ?body-clauses);
      
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
class-slots-and-clauses:
   { slot ?:variable = ?:expression; ... }
      => { slot ?variable = ?expression; ... }
   { inherited slot ?:name = ?:expression; ... }
      => { inherited slot ?name = ?expression; ... }
   { ?body-clauses } => { ?body-clauses }
   
// Optional. Note that the body is turned into an expression, which is why the
// 'user-functions' auxiliary macro takes an expression instead of a body.
body-clauses:
   { afterwards (?context:name, ?product:name) ?:body; ... }
      => { afterwards ?context, ?product, ?body; ... }
   { cleanup (?context:name) ?:body }
      => { cleanup ?context, ?body }
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
   { slot ?:variable = ?:expression; ... }
      => { slot ?variable; ... }
   { inherited slot ?:name = ?:expression; ... }
      => { inherited slot ?name; ... }
      
   // These are extra baggage to be ignored.
   { afterwards ?dummy:*; ... } => { ... }
   { cleanup ?dummy:*; ... } => { ... }
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
                        inherited slot ?slot-name:name = ?:expression;
                        ?more:*) }
      => { ?token.?slot-name := ?expression; slot-initializers(?token; ?more) }
      
   // These are extra baggage to be ignored.
   { slot-initializers (?token:name; afterwards ?dummy:*; ?more:*) }
      => { slot-initializers(?token; ?more) }
   { slot-initializers (?token:name; cleanup ?dummy:*; ?more:*) }
      => { slot-initializers(?token; ?more) }
   { slot-initializers (?token:name) }
      => { }
end macro;


// This auxiliary macro generates the match- and cleanup- functions.
define macro user-functions

   // Slot declarations are extra baggage to be ignored.
   { user-functions(?token:name; slot ?dummy:*; ?more:*) }
      => { user-functions(?token; ?more) }
   { user-functions(?token:name; inherited slot ?dummy:*; ?more:*) }
      => { user-functions(?token; ?more) }

   {
      user-functions(?token:name;
            afterwards ?after-ctxt:name, ?after-prod:name, ?after-expr:expression;
            cleanup ?clean-ctxt:name, ?clean-expr:expression)
   } => {
      define inline function "match-" ## ?token (?after-ctxt, ?after-prod)
      => (p)
         ?after-expr; ?after-prod;
      end function;
      define inline function "cleanup-" ## ?token (?clean-ctxt) => ()
         ?clean-expr
      end function;
   }
   
   {
      user-functions(?token:name;
            cleanup ?clean-ctxt:name, ?clean-expr:expression)
   } => {
      define inline function "match-" ## ?token (c, p) => (p)
         p
      end function;
      define inline function "cleanup-" ## ?token (?clean-ctxt) => ()
         ?clean-expr
      end function;
   }
   
   {
      user-functions(?token:name;
            afterwards ?after-ctxt:name, ?after-prod:name, ?after-expr:expression)
   } => {
      define inline function "match-" ## ?token (?after-ctxt, ?after-prod)
      => (p)
         ?after-expr; ?after-prod
      end function;
      define inline function "cleanup-" ## ?token (c) => ()
      end function;
   }
   
   {
      user-functions(?token:name)
   } => {
      define inline function "match-" ## ?token (c, p) => (p)
         p
      end function;
      define inline function "cleanup-" ## ?token (c) => ()
      end function;
   }
end macro;
