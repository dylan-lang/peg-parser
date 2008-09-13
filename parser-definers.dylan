module: peg-parser
synopsis: PEG parser macro definitions.

// See parser-rules.dylan for a full explanation of rule parsers. Basically,
// rule parsers parse a stream in a given context and return a value or sequence
// of values called the "product."

/// SYNOPSIS: Defines an arbitrary rule parser.
///
/// This macro defines a rule parser that includes support for debugging and other
/// features described for rule parsers. The main part of the parser is Dylan code
/// supplied by you, but the first line must be a label.
///
/// Like all rule parsers, the parser function created by this macro returns three
/// values: the parse product or #f, a success flag, and an error. However, as a
/// convenience, you may write the parser with only one return value.
///
/// VALUES:
///   product  - Required.
///   success? - Optional. If omitted and 'product' is true, defaults to #t.
///   error    - Optional. If omitted or #f and 'success?' is #f, an appropriate
///              error will be created. A missing description or position will be
///              filled in according to the rollback position or label.
///
/// Here are two equivalent rule parsers:
///
/// [code]
/// define parser-method char (stream, context)
/// => char :: false-or(<character>);
///   label "character";
///   read-element(stream, on-end-of-stream: #f)
/// end
/// [end code]
///
/// [code]
/// define parser-method char (stream, context)
/// => (char :: false-or(<character>), success? :: <boolean>,
///     error :: false-or(<parse-failure>))
///   label "character";
///   let ch = read-element(stream, on-end-of-stream: #f);
///   if (ch)
///     values(ch, #t, #f)
///   else
///     values(#f, #f, make(<parse-failure>))
///   end if
/// end
/// [end code]

define macro parser-method-definer

   //
   // These are alternate forms for different value list styles.
   { define parser-method ?:name (?params:*) => (?results:*) ; ?rest:* end }
      => { define parser-method ?name (?params) => (?results) ?rest end }
   { define parser-method ?:name (?params:*) => ?result:variable ; ?rest:* end }
      => { define parser-method ?name (?params) => (?result) ?rest end }
   
   //
   // These are alternate forms for defaulted value lists.
   { define parser-method ?:name (?params:*) => (?r1:variable) ?rest:* end }
      => { define parser-method ?name (?params) =>
           (?r1, r2 :: singleton(#f), r3 :: singleton(#f)) ?rest end }
   { define parser-method ?:name (?params:*) => (?r1:variable, ?r2:variable) ?rest:* end }
      => { define parser-method ?name (?params) => 
           (?r1, ?r2, r3 :: singleton(#f)) ?rest end }
   
   //
   // This is the main form.
   {
      define parser-method ?token-name:name
         (?stream:name, ?context-name:name :: ?context-type:expression)
      => (?res:name :: ?res-type:expression, ?succ:name :: ?succ-type:expression,
          ?err:name :: ?err-type:expression)
         label ?label:expression;
         ?:body
      end
   } => {
      define function "parse-" ## ?token-name
         (?stream :: <positionable-stream>, ?context-name :: ?context-type)
      => (?res :: ?res-type, ?succ :: <boolean>, ?err :: false-or(<parse-failure>))
         indent-trace();
         format-trace(?"token-name" ## "...");
         let pos = ?stream.stream-position;
         let (?res :: ?res-type, ?succ :: <boolean>, ?err :: false-or(<parse-failure>)) = 
               ?body;

         // Default the values returned by ?body.
         if (?res & ?succ = #f)
            ?succ := #t;
         end if;
         if (?succ = #f & ?err = #f)
            ?err := make(<parse-failure>, expected: ?label, position: pos);
         end if;
         if (?err)
            if (?err.empty-description?)
               ?err.parse-expected-list := list(as(<string>, ?label));
            end if;
            if (?err.failure-position = #f)
               ?err.failure-position := pos
            end if;
         end if;

         if (?succ)
            format-trace(?"token-name" ## " matched stream pos %x-%x",
                         pos, ?stream.stream-position);
         else
            ?stream.stream-position := pos;
            format-trace(?"token-name" ## " no match, exp. %s at stream pos %x",
                         ?err.parse-expected, ?err.failure-position);
         end if;
         outdent-trace();
         values(?succ & ?res | #f, ?succ, ?err);
      end function;
   }
end macro;


/// SYNOPSIS: Defines a rule parser and perhaps a token class for a given
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
/// defines a rule parser named parse-t and a token class named <t-token>
/// which inherits from <c> and <token>. The superclass is optional, but the
/// parentheses aren't. <t-token> will have a slot named content (inherited
/// from <c>) and a slot named more-content. When <t-token> is initialized,
/// tokens gets set to the product of the rule `many(t2)`, content gets set to
/// the expression `tokens[1]`, and more-content gets set to the expression
/// `tokens[2]` (which must be a <string>).
/// 
/// === Yield form ===
///
/// Yield form returns a value.
///
/// [code]
/// define parser t :: <token>
///   rule many(t2) => tokens;
///   yield tokens[1];
/// end parser;
/// [end code]
///
/// defines a rule parser that returns `tokens[1]` (which must be a <token>)
/// directly, without defining a <t-token> class. The type specialization is
/// optional.
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
/// defines a rule parser that returns #"t".
///
/// === Error handling ===
/// 
/// All three forms allow a label clause to appear before the rule. The label
/// clause is a string that describes the result of the parser, and may be used
/// to simplify the list of alternative possibilities that would otherwise
/// be returned in the event of a parser failure.
///
/// [code]
/// define parser t
///   label "series of t2";
///   rule many(t2)
/// end parser;
/// [end code]
///
/// === Affecting context ===
///
/// All three forms allow two additional clauses, "afterwards" and "cleanup,"
/// that perform actions after the rule parser matches or fails to match. These
/// actions are inherited by token subclasses.
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

define macro parser-definer

   //
   // These delegate to labeled-parser-definer once a label is created.
   
   {
      define parser ?:name (?supers:*)
         label ?label:expression;
         rule ?rule:*;
         ?rest:*
      end
   } => {
      define constant "$" ## ?name ## "-parser-label" = ?label;
      define labeled-parser ?name (?supers)
         rule ?rule;
         ?rest
      end
   }
   
   {
      define parser ?:name :: ?:expression
         label ?label:expression;
         rule ?rule:*;
         yield ?yield:*;
         ?rest:*
      end
   } => {
      define constant "$" ## ?name ## "-parser-label" = ?label;
      define labeled-parser ?name :: ?expression
         rule ?rule;
         yield ?yield;
         ?rest
      end
   }
   
   {
      define parser ?:name
         label ?label:expression;
         rule ?rule:*;
         ?rest:*
      end
   } => {
      define constant "$" ## ?name ## "-parser-label" = ?label;
      define labeled-parser ?name
         rule ?rule;
         ?rest
      end
   }

   {
      define parser ?:name (?supers:*)
         rule ?rule:*;
         ?rest:*
      end
   } => {
      define constant "$" ## ?name ## "-parser-label" = #f;
      define labeled-parser ?name (?supers)
         rule ?rule;
         ?rest
      end
   }
   
   {
      define parser ?:name :: ?:expression
         rule ?rule:*;
         yield ?yield:*;
         ?rest:*
      end
   } => {
      define constant "$" ## ?name ## "-parser-label" = #f;
      define labeled-parser ?name :: ?expression
         rule ?rule;
         yield ?yield;
         ?rest
      end
   }
   
   {
      define parser ?:name
         rule ?rule:*;
         ?rest:*
      end
   } => {
      define constant "$" ## ?name ## "-parser-label" = #f;
      define labeled-parser ?name
         rule ?rule;
         ?rest
      end
   }
end macro;


define macro labeled-parser-definer

   //
   // This form creates a parser that return an initialized <token> class.
   {
      define labeled-parser ?token-name:name (?supers)
         rule ?rule => ?product-name:name :: ?product-type:expression;
         ?class-slots-and-clauses
      end
   } => {
      // Define the class.
      class-specifier(?token-name; ?supers; ?class-slots-and-clauses);
      
      // Initialize the class's slots based on results of rule.
      define method initialize (?token-name :: "<" ## ?token-name ## "-token>",
            #next next-method,
            #key ?product-name :: type-union(?product-type, singleton(unsupplied()))
                 = unsupplied())
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
      => (token :: false-or("<" ## ?token-name ## "-token>"),
          success? :: <boolean>, error :: false-or(<parse-failure>))
         parser-body(prolog ?token-name);
         let pos = stream.stream-position;
         let (prod :: ?product-type, succ? :: <boolean>, err :: false-or(<parse-failure>)) =
               ?token-name ## "-parser-rule" (stream, context);
         parser-body(main ?token-name, ?product-type, stream, context, pos,
                     prod, succ?, err);
         values(succ? & make("<" ## ?token-name ## "-token>", 
                             start: pos, end: stream.stream-position,
                             ?product-name: prod),
                succ?, err);
      end function;
      
      // User defined action functions
      user-functions(?token-name; ?class-slots-and-clauses);
   }

   //
   // This form creates a parser that returns the result of an expression.
   {
      define labeled-parser ?token-name:name :: ?token-type:expression
         rule ?rule => ?product-name:name :: ?product-type:expression;
         yield ?:expression;
         ?body-clauses
      end
   } => {
      // Define the parser rule as the result of all the 'seq' etc. functions.
      define constant ?token-name ## "-parser-rule" = ?rule;
      
      // Define the yield expression as a separate function to avoid name
      // collision.
      define inline function ?token-name ## "-yield-expr"
            (?product-name :: ?product-type) => (yield :: ?token-type)
         ?expression;
      end function;
      
      // Define the parser function including tracing and rollback. Result is
      // yield expression.
      define function "parse-" ## ?token-name
         (stream :: <positionable-stream>, context)
      => (token :: false-or(?token-type), 
          success? :: <boolean>, error :: false-or(<parse-failure>))
         parser-body(prolog ?token-name);
         let pos = stream.stream-position;
         let (prod :: ?product-type, succ? :: <boolean>, err :: false-or(<parse-failure>)) =
               ?token-name ## "-parser-rule" (stream, context);
         parser-body(main ?token-name, ?product-type, stream, context, pos,
                     prod, succ?, err);
         values(succ? & ?token-name ## "-yield-expr" (prod), succ?, err);
      end function;
      
      // User defined action functions
      user-functions(?token-name; ?body-clauses);
   }

   //
   // This form creates a parser that returns a symbol.
   {
      define labeled-parser ?token-name:name
         rule ?rule;
         ?body-clauses
      end
   } => {
      // Define the parser rule as the result of all the 'seq' etc. functions.
      define constant ?token-name ## "-parser-rule" = ?rule;
      
      // Define the parser function including tracing and rollback. Result is
      // a symbol, same as token name.
      define function "parse-" ## ?token-name
         (stream :: <positionable-stream>, context)
      => (token :: false-or(<symbol>),
          success? :: <boolean>, error :: false-or(<parse-failure>))
         parser-body(prolog ?token-name);
         let pos = stream.stream-position;
         let (prod, succ? :: <boolean>, err :: false-or(<parse-failure>)) =
               ?token-name ## "-parser-rule" (stream, context);
         parser-body(main ?token-name, <object>, stream, context, pos,
                     prod, succ?, err);
         values(succ? & ?#"token-name", succ?, err)
      end function;
      
      // User defined action functions
      user-functions(?token-name; ?body-clauses);
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
   { afterwards (?context:variable, ?product:variable) ?:body ... }
      => { afterwards ?context, ?product, ?body; ... }
   { cleanup (?context:variable) ?:body }
      => { cleanup ?context, ?body }
   { } => { }
end macro;


// This auxiliary macro generates most of the parsers' bodies. It could be
// simpler if calling macro could use prod, etc., declared here.

define macro parser-body
   {
      parser-body(prolog ?token-name:name)
   } => {
      indent-trace();
      format-trace(?"token-name" ## "...");
   }

   {
      parser-body(main ?token-name:name, ?product-type:expression,
                  ?stream:name, ?context:name, ?pos:name,
                  ?prod:name, ?succ:name, ?err:name)
   } => {
      // Replace or fill in missing error description.
      if (?err & "$" ## ?token-name ## "-parser-label")
         if (?err.failure-position = ?pos)
            ?err.parse-expected-list := list("$" ## ?token-name ## "-parser-label");
            ?err.parse-expected-other-than-list := #();
         end if;
         if (?err.empty-description?)
            let msg = format-to-string("error in %s",
                                       "$" ## ?token-name ## "-parser-label");
            ?err.parse-expected-other-than-list := list(msg);
         end if;
      end if;
      if (?succ)
         "match-" ## ?token-name (?context, ?prod);
         format-trace(?"token-name" ## " matched stream pos %x-%x",
                      ?pos, ?stream.stream-position);
      else
         format-trace(?"token-name" ## " no match, exp. %s at stream pos %x",
                      ?err.parse-expected, ?err.failure-position);
      end if;
      outdent-trace();
      "cleanup-" ## ?token-name (?context);
   }
end macro;


// This auxiliary macro turns slot clauses into a class declaration. It can't
// just do the slot part because `define class` can't have a macro call inside.

define macro class-specifier
   {
      class-specifier (?:name; ?supers:*; ?class-slots)
   } => {
      define class "<" ## ?name ## "-token>" (?supers)
         ?class-slots
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
            afterwards ?after-ctxt:name :: ?after-ctxt-type:expression,
                       ?after-prod:name :: ?after-prod-type:expression,
                       ?after-expr:expression;
            cleanup ?clean-ctxt:name :: ?clean-ctxt-type:expression,
                    ?clean-expr:expression)
   } => {
      define inline function "match-" ## ?token
         (?after-ctxt :: ?after-ctxt-type, ?after-prod :: ?after-prod-type)
      => (p :: ?after-prod-type)
         ?after-expr; ?after-prod;
      end function;
      define inline function "cleanup-" ## ?token
            (?clean-ctxt :: ?clean-ctxt-type) => ()
         ?clean-expr
      end function;
   }
   
   {
      user-functions(?token:name;
            cleanup ?clean-ctxt:name :: ?clean-ctxt-type:expression,
                    ?clean-expr:expression)
   } => {
      define inline function "match-" ## ?token (c, p) => (p)
         p
      end function;
      define inline function "cleanup-" ## ?token
            (?clean-ctxt :: ?clean-ctxt-type) => ()
         ?clean-expr
      end function;
   }
   
   {
      user-functions(?token:name;
            afterwards ?after-ctxt:name :: ?after-ctxt-type:expression,
                       ?after-prod:name :: ?after-prod-type:expression,
                       ?after-expr:expression)
   } => {
      define inline function "match-" ## ?token
         (?after-ctxt :: ?after-ctxt-type, ?after-prod :: ?after-prod-type)
      => (p :: ?after-prod-type)
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
