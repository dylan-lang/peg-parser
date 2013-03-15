module: peg-parser
synopsis: PEG parser macro definitions.

// See parser-rules.dylan for a full explanation of rule parsers. Basically,
// rule parsers parse a stream in a given context and return a value or sequence
// of values called the "product."

/**
SYNOPSIS: Defines an arbitrary rule parser.

This macro defines a rule parser that includes support for debugging and other
features described for rule parsers. The main part of the parser is Dylan code
supplied by you, but the first line must be a label.

Like all rule parsers, the parser function created by this macro returns three
values: the parse product or #f, a success flag, and an error. However, as a
convenience, you may write the parser with only one return value.

VALUES:
  product  - Required.
  success? - Optional. If omitted and 'product' is true, defaults to #t.
  extent   - Optional. If omitted or #f, an appropriate '<parse-extent>' will be
             created. A missing description or position will be filled in
             according to the rollback position or label.

Here are two equivalent rule parsers:

[code]
define parser-method char (stream, context)
=> char :: false-or(<character>);
  label "character";
  read-element(stream, on-end-of-stream: #f)
end
[end code]

[code]
define parser-method char (stream, context)
=> (char :: false-or(<character>), success? :: <boolean>,
    extent :: false-or(<parse-extent>))
  label "character";
  let ch = read-element(stream, on-end-of-stream: #f);
  if (ch)
    values(ch, #t, #f)
  else
    values(#f, #f, make(<parse-extent>))
  end if
end
[end code]
**/

define macro parser-method-definer

   //
   // These are alternate forms for different value list styles.
   //

   { define parser-method ?:name (?params:*) => (?results:*) ; ?rest:* end }
      => { define parser-method ?name (?params) => (?results) ?rest end }
   { define parser-method ?:name (?params:*) => ?result:variable ; ?rest:* end }
      => { define parser-method ?name (?params) => (?result) ?rest end }

   //
   // These are alternate forms for defaulted value lists.
   //

   { define parser-method ?:name (?params:*) => (?r1:variable) ?rest:* end }
      => { define parser-method ?name (?params) => (?r1, r2, r3) ?rest end }
   { define parser-method ?:name (?params:*) => (?r1:variable, ?r2:variable) ?rest:* end }
      => { define parser-method ?name (?params) => (?r1, ?r2, r3) ?rest end }

   //
   // This is the main form.
   //

   {  define parser-method ?token-name:name
         (?stream:name, ?context-name:name :: ?context-type:expression)
      => (?res:name :: ?res-type:expression, ?succ:name :: ?succ-type:expression,
          ?ext:name :: ?ext-type:expression)
         label ?label:expression;
         ?:body
      end
   } => {
      define function "parse-" ## ?token-name
         (?stream :: <positionable-stream>, ?context-name :: ?context-type)
      => (?res :: ?res-type, ?succ :: <boolean>, ?ext :: <parse-extent>)
         indent-trace();
         format-trace("%s...", ?"token-name");
         let pos = ?stream.stream-position;
         let (?res :: ?res-type, ?succ :: <boolean>, maybe-extent :: false-or(<parse-extent>)) =
               ?body;

         // Default the values returned by ?body.
         if (?res & ?succ = #f)
            ?succ := #t;
         end if;
         let ?ext :: <parse-extent> =
               maybe-extent |
               if (?succ)
                  make(<parse-success>, success: ?label, position: pos)
               else
                  make(<parse-failure>, expected: ?label, position: pos)
               end if;
         if (instance?(?ext, <parse-success>))
            if (?ext.parse-success-list.empty?)
               ?ext.parse-success-list := list(as(<string>, ?label))
            end if
         else
            if (?ext.empty-failure-lists?)
               ?ext.parse-expected-list := list(as(<string>, ?label))
            end if
         end if;
         if (?ext.parse-position = #f)
            ?ext.parse-position := pos
         end if;

         if (?succ)
            format-trace("%s matched stream pos %x-%x",
                         ?"token-name", pos, ?stream.stream-position);
         else
            ?stream.stream-position := pos;
            format-trace("%s no match, exp. %s at stream pos %x",
                         ?"token-name", ?ext.parse-expected, ?ext.parse-position);
         end if;
         outdent-trace();
         values(?succ & ?res, ?succ, ?ext);
      end function
   }
end macro;


/**
SYNOPSIS: Defines a rule parser and perhaps a token class for a given token.

The macro takes three forms: class, yield, and symbol.

--- Class form ---

This form creates a token class.

[code]
define parser t (<c>, <token>)
  rule many(t2) => tokens;
  parse-context => context;
  inherited slot content = tokens[1];
  slot more-content :: <string> = tokens[2];
end parser;
[end code]

This defines a rule parser named `parse-t` and a token class named `<t-token>`
which inherits from `<c>` and `<token>`. The superclass is optional, but the
parentheses aren't. The `parse-context` clause is optional. `<t-token>` will
have a slot named `content` (inherited from `<c>`) and a slot named
`more-content`. When `<t-token>` is initialized, `tokens` gets set to the
product of the rule `many(t2)`, `context` gets set to the parse context,
`content` gets set to the expression `tokens[1]`, and `more-content` gets set to
the expression `tokens[2]` (which must be a '<string>').

--- Yield form ---

Yield form returns a value.

[code]
define parser t :: <token>
  rule many(t2) => tokens;
  parse-context => context;
  yield tokens[1];
end parser;
[end code]

This defines a rule parser that returns `tokens[1]` (which must be a '<token>')
directly, without defining a `<t-token>` class. The type specialization is
optional, as is the `parse-context` clause. The `yield` expression may refer to
tokens and context.

--- Symbol form ---

This form returns a token symbol.

[code]
define parser t
  rule many(t2)
end parser;
[end code]

This defines a rule parser that returns #"t".

--- Error handling ---

All three forms allow a label clause to appear before the rule. The label clause
is a string that describes the result of the parser, and may be used to simplify
the list of alternative possibilities that would otherwise be returned in the
event of a parser failure.

[code]
define parser t
  label "series of t2";
  rule many(t2)
end parser;
[end code]

--- Parse context and attributes ---

The parse context is the global parsing state. You can subclass it, but if you
want to perform extra checking or something, you are better off using
attributes. All three forms allow an attributes clause.

[code]
define parser t
  rule many(t2);
attributes
  t2-count :: <integer> = 0,
  t2-present? :: <boolean> = #t;
end parser;
[end code]

The `t2-count` and `t2-present?` attributes will be available to all parsers
called directly or indirectly by `parse-t` via 'attr' and 'attr-setter'. These
are renamed versions of the 'dynamic-binding' macros in the '::dynamic-binding'
[qv] library.

[code]
let a = attr(t2-count);
let a = attr(t2-count, default: #f);
attr(t2-count) := 3;
attr-setter(3, t2-count);
[end code]

Attributes are valid in all called parsers, in slot initialization expressions,
and in afterwards and cleanup clauses. Attribute initialization expressions may
refer to attributes defined earlier in the clause.

--- Afterwards and cleanup ---

All three forms allow two additional clauses, "afterwards" and "cleanup," that
perform actions after the rule parser matches or fails to match. The
"afterwards" clause is only executed if the rule parser matches; the "cleanup"
clause is always executed.

The "afterwards" clause has the following arguments:
   'context' [api]   - The context.
   'product' [api]   - The parse product.
   'value' [api]     - The semantic value.
   'start-pos' [api] - Stream position at the start of the parse.
   'end-pos' [api]   - Next stream position.
   'fail:' [api]     - A name. This will be bound to an exit function taking a
                       '<parse-failure>' instance. Calling this causes the parse
                       to fail even if it would have otherwise succeeded.

The "cleanup" clause has the following arguments:
   'context' [api]   - As above.
   'value' [api]     - The semantic value, or #f if the parser did not succeed.
   'success?' [api]  - An instance of '<boolean>', indicating whether the parser
                       succeeded. Parsers may succeed even if no product
                       results.
   'extent' [api]    - An instance of '<parse-extent>'.

[code]
define parser t2 (<token>)
  rule t3 => token;
  slot t3-value = token.value;
  afterwards (context, token, value, start-pos, end-pos, fail-parse)
    // Executes if match is successful. 'token' is different from the one
    // in the rule and slot clauses. Invalidate cache if attribute adjustment
    // will cause something to parse differently.
    let t2-count = attr(t2-count);
    t2-count := t2-count + 1;
    if (t2-count > $too-many)
      fail-parse(make(<parse-failure>, expected-other-than: "this many"))
    end if;
    attr(t2-count) := t2-count;
    invalidate-parser-cache(context, from: end-pos);
end parser;
[end code]

[code]
define parser t
  rule many(t2);
  afterwards (context, tokens)
    // The product of this parser is #"t" because this is a token symbol
    // parser, but the local variable 'tokens' will be the product of
    // many(t2).
    ...
  cleanup (context, value, success?, extent)
    // You can adjust context in either clause. Invalidate entire cache if
    // the adjustment will cause something to parse differently.
    context.tried-t? := #t;
    invalidate-parser-cache(context);
end parser;
[end code]

--- Caching ---

All three forms allow "caching" as a modifier, i.e. `define caching parser`. It
is better, performance-wise, to only cache certain important productions. The
cache is kept in the '<parse-context>' instance and may be preallocated by
supplying the 'cache-stream:' keyword to 'make'. Cache hit statistics are kept
if '*parser-cache-hits*' is #t and can be retrieved by calling
'parser-cache-hits' on the '<parse-context>' instance. This retrieves a table
containing productions and corresponding cache hits.

If the context is altered in such a way to affect parsing, the cache should be
invalidated completely because the context is global. If an attribute is
altered in such a way to affect parsing, the cache should be invalidated from
'end-pos' on (see 'Afterwards and cleanup').

Once a parser's result is cached, that parser's "afterwards" and "cleanup"
clauses are never re-evaluated at the cached location. For this reason, it is
best not to use the 'look-ahead?' method in an "afterwards" or "cleanup" clause
that is directly or indirectly part of a caching parser. The look-ahead state is
"frozen" in the cache; if the parser is called when the look-ahead state is
different, the clauses are not re-evaluated and the result will be whatever was
cached under the earlier look-ahead state.
**/

define macro parser-definer

   // The specific parser builder macros are called with property lists that
   // can include the following symbols and values.
   //
   // All styles
   //    Either         parser-type:   cached
   //       or          parser-type:   uncached
   //    One            token-name:    (name)
   //    One            token-type:    (type)
   //    Zero or one    label:         (expression)
   //    One            rule:          (expression)
   //    Zero or more   parser-attr:   (name) :: (type) = (expression)
   //    Zero or one    after-ctxt:    (name) :: (type)
   //       as above    after-prod:    (name) :: (type)
   //       as above    after-value:   (name) :: (type)
   //       as above    after-start:   (name) :: (type)
   //       as above    after-end:     (name) :: (type)
   //       as above    after-body:    (expression)
   //       as above    after-fail:    (name)
   //          or       after-fail:    omitted
   //    Zero or one    cleanup-ctxt:  (name) :: (type)
   //       as above    cleanup-value: (name) :: (type)
   //       as above    cleanup-succ:  (name) :: (type)
   //       as above    cleanup-ext:   (name) :: (type)
   //       as above    cleanup-body:  (expression)
   //
   // Class and yield style
   //    One            product-name:  (name)
   //    One            product-type:  (type)
   //    Either         context-name:  (name)
   //       or          context-name:  omitted
   //    Either         context-type:  (type)
   //       or          context-type:  <parse-context>
   //
   // Class style
   //    Zero or more   super:         (type)
   //    Zero or more   slot:          (name) :: (type)
   //       or          slot:          (name) :: (type) = (expression)
   //       or          slot:          inherited (name) = (expression)
   //
   // Yield style
   //    One            yield-expr:    (expression)


   //
   // These forms create parsers that return an initialized <token> class.
   //

   {  define ?parser-type parser ?token-name:name (?supers)
         rule ?rule => ?rule-product:variable;
         ?context-and-slots-clauses
      end
   } => {
      class-style-parser
         ?parser-type, token-name: ?token-name, ?supers,
         token-type: "<" ## ?token-name ## "-token>", ?rule, ?rule-product,
         ?context-and-slots-clauses
      end
   }

   {  define ?parser-type parser ?token-name:name (?supers)
         label ?label:expression;
         rule ?rule => ?rule-product:variable;
         ?context-and-slots-clauses
      end
   } => {
      class-style-parser
         ?parser-type, token-name: ?token-name, ?supers, ?label,
         token-type: "<" ## ?token-name ## "-token>", ?rule, ?rule-product,
         ?context-and-slots-clauses
      end
   }

   //
   // These forms create parsers that return the result of an expression.
   //

   {  define ?parser-type parser ?token-name:name :: ?token-type:expression
         rule ?rule => ?rule-product:variable;
         ?context-and-yield-clauses
      end
   } => {
      yield-style-parser
         ?parser-type, token-name: ?token-name, token-type: ?token-type,
         ?rule, ?rule-product, ?context-and-yield-clauses
      end
   }

   {  define ?parser-type parser ?token-name:name :: ?token-type:expression
         label ?label:expression;
         rule ?rule => ?rule-product:variable;
         ?context-and-yield-clauses
      end
   } => {
      yield-style-parser
         ?parser-type, token-name: ?token-name, token-type: ?token-type,
         ?label, ?rule, ?rule-product, ?context-and-yield-clauses
      end
   }

   //
   // These forms create parsers that return a symbol.
   //

   {  define ?parser-type parser ?token-name:name
         rule ?rule;
         ?body-clauses
      end
   } => {
      symbol-style-parser
         ?parser-type, token-name: ?token-name, token-type: <symbol>, ?rule,
         ?body-clauses
      end
   }

   {  define ?parser-type parser ?token-name:name
         label ?label:expression;
         rule ?rule;
         ?body-clauses
      end
   } => {
      symbol-style-parser
         ?parser-type, token-name: ?token-name, token-type: <symbol>, ?label,
         ?rule, ?body-clauses
      end
   }

// Cached or uncached parser.
parser-type:
   { caching } => { parser-type: cached }
   { } => { parser-type: uncached }

// Optional superclasses.
supers:
   { ?:name, ... } => { super: ?name, ... }
   { } => { }

// Optional label.
label:
   { ?:expression } => { label: ?expression }

// Only allow names, the 'seq' etc. functions, or any token (for commas).
// Names are assumed to be tokens, and changed to the tokens' parser function.
// 'seq' etc. takes those parser functions and generates a new function from
// them. The reason for nested-rule: is to allow rule-part to recurse twice --
// once for the parens and again to continue a comma-separated sequence.
rule:
   { ?rule-part } => { rule: ?rule-part }
rule-part:
   { seq(?nested-rules) ... } => { seq(?nested-rules) ... }
   { choice(?nested-rules) ... } => { choice(?nested-rules) ... }
   { many(?nested-rules) ... } => { many(?nested-rules) ... }
   { opt(?nested-rules) ... } => { opt(?nested-rules) ... }
   { opt-seq(?nested-rules) ... } => { opt-seq(?nested-rules) ... }
   { opt-choice(?nested-rules) ... } => { opt-choice(?nested-rules) ... }
   { opt-many(?nested-rules) ... } => { opt-many(?nested-rules) ... }
   { req-next(?nested-rules) ... } => { req-next(?nested-rules) ... }
   { not-next(?nested-rules) ... } => { not-next(?nested-rules) ... }
   { skip(?nested-rules) ... } => { skip(?nested-rules) ... }
   { nil(?:expression) ... } => { nil(?expression) ... }
   { ?:name ... } => { "parse-" ## ?name ... }
   { ?:token ... } => { ?token ... }
   { } => { }
nested-rules:
   { ?rule-part } => { ?rule-part }

// Product name and type.
rule-product:
   { ?:name :: ?:expression } => { product-name: ?name, product-type: ?expression }

// Context name and type.
context-and-slots-clauses:
   { parse-context => ?context-var:variable; ?class-slots-and-clauses }
      => { ?context-var, ?class-slots-and-clauses }
   { ?class-slots-and-clauses }
      => { context-name: omitted, context-type: <parse-context>,
           ?class-slots-and-clauses }

context-and-yield-clauses:
   {  parse-context => ?context-var:variable;
      yield ?yield-expr:expression;
      ?body-clauses
   } => {
      ?context-var, ?yield-expr, ?body-clauses
   }

   {  yield ?yield-expr:expression;
      ?body-clauses
   } => {
      context-name: omitted, context-type: <parse-context>, ?yield-expr,
      ?body-clauses
   }

context-var:
   { ?:name :: ?:expression } => { context-name: ?name, context-type: ?expression }

// Yield expression.
yield-expr:
   { ?:expression } => { yield-expr: ?expression }

// Optional class slots, then the attributes/afterwards/cleanup clauses.
class-slots-and-clauses:
   { slot ?:variable; ... }
      => { slot: ?variable, ... }
   { slot ?:variable = ?:expression; ... }
      => { slot: ?variable = ?expression, ... }
   { inherited slot ?:name = ?:expression; ... }
      => { slot: inherited ?name = ?expression, ... }
   { ?body-clauses } => { ?body-clauses }

body-clauses:
   { ?attributes-clause } => { ?attributes-clause }

// Optional attributes clause, then the afterwards/cleanup clauses.
attributes-clause:
   { attributes ?attributes-list; ?afterwards-clause }
      => { ?attributes-list, ?afterwards-clause }
   { ?afterwards-clause } => { ?afterwards-clause }

attributes-list:
   { ?:variable = ?:expression, ... } => { parser-attr: ?variable = ?expression, ... }
   { } => { }

// Optional afterwards clause, then the cleanup clause. Note that bodies are
// turned into expressions in the expansions, which is why the 'after-body:'
// and 'cleanup-body:' symbols are expressions.
afterwards-clause:
   {  afterwards (?context:variable, ?product:variable, ?value:variable,
                  ?start:variable, ?end:variable, #key ?fail:name = omitted)
         ?:body
      ?cleanup-clause
   } => {
      after-ctxt: ?context, after-prod: ?product, after-value: ?value,
      after-start: ?start, after-end: ?end, after-body: ?body, after-fail: ?fail,
      ?cleanup-clause
   }
   { ?cleanup-clause } => { ?cleanup-clause }

// Optional cleanup clause.
cleanup-clause:
   {  cleanup (?context:variable, ?value:variable, ?succ:variable, ?ext:variable)
         ?:body
   } => {
      cleanup-ctxt: ?context, cleanup-value: ?value, cleanup-succ: ?succ,
      cleanup-ext: ?ext, cleanup-body: ?body
   }
   { } => { }
end macro;


// This auxiliary macro generates class-style parsers.

define macro class-style-parser
   {  class-style-parser
         #rest ?clauses:*, #key ?parser-type:name, ?token-name:token,
         ?token-type:expression, ?rule:expression, ?product-name:name,
         ?product-type:expression, ?context-name:name, ?context-type:expression,
         #all-keys;
      end
   } => {
      // Define the class.
      class-specifier ?clauses end;

      // Initialize the class's slots based on results of rule.
      initialize-specifier ?clauses end;

      // Define the parser rule by evaluating all the 'seq' etc. functions.
      define function ?token-name ## "-parser-rule"
         (stream :: <positionable-stream>, context :: ?context-type)
      => (product, success? :: <boolean>, extent :: <parse-extent>)
         let evaluated-rule = ?rule;
         evaluated-rule(stream, context)
      end function;
      
      // Define the parser value as a <token> subclass, slots initialized by
      // 'initialize' function above.
      define inline function ?token-name ## "-parser-value"
         (?context-name :: ?context-type, ?product-name :: ?product-type,
          start-pos :: type-union(<integer>, <stream-position>),
          end-pos :: type-union(<integer>, <stream-position>))
      => (value :: ?token-type)
         make(?token-type, start: start-pos, end: end-pos,
              ?context-name: ?context-name, ?product-name: ?product-name)
      end function;

      // Define the parser function including tracing and rollback.
      parser-function ?parser-type, ?clauses end;

      // User defined action functions
      after-function ?clauses end;
      cleanup-function ?clauses end;
   }
end macro;


// This auxiliary macro generates yield-style parsers.

define macro yield-style-parser
   {  yield-style-parser
         #rest ?clauses:*, #key ?parser-type:name, ?token-name:token,
         ?token-type:expression, ?rule:expression, ?product-name:name,
         ?product-type:expression, ?context-name:name, ?context-type:expression,
         ?yield-expr:expression, #all-keys;
      end
   } => {
      // Define the parser rule by evaluating all the 'seq' etc. functions.
      define function ?token-name ## "-parser-rule"
         (stream :: <positionable-stream>, context :: ?context-type)
      => (product, success? :: <boolean>, extent :: <parse-extent>)
         let evaluated-rule = ?rule;
         evaluated-rule(stream, context)
      end function;
      
      // Define the parser value as the result of the yield expression.
      define function ?token-name ## "-parser-value"
         (?context-name :: ?context-type, ?product-name :: ?product-type,
          start-pos, end-pos)
      => (value :: ?token-type)
         ?yield-expr;
      end function;

      // Define the parser function including tracing and rollback.
      parser-function ?parser-type, ?clauses end;

      // User defined action functions
      after-function ?clauses end;
      cleanup-function ?clauses end;
   }
end macro;


// This auxiliary macro generates symbol-style parsers.

define macro symbol-style-parser
   {  symbol-style-parser
         #rest ?clauses:*, #key ?parser-type:name, ?token-name:token,
         ?rule:expression, #all-keys;
      end
   } => {
      // Define the parser rule by evaluating all the 'seq' etc. functions.
      define function ?token-name ## "-parser-rule"
         (stream :: <positionable-stream>, context :: <parse-context>)
      => (product, success? :: <boolean>, extent :: <parse-extent>)
         let evaluated-rule = ?rule;
         evaluated-rule(stream, context)
      end function;
      
      // Define the parser value as a symbol, same as token name.
      define inline function ?token-name ## "-parser-value"
         (context, product, start-pos, end-pos)
      => (value :: <symbol>)
         ?#"token-name"
      end function;

      // Define the parser function including tracing and rollback.
      parser-function ?parser-type, ?clauses end;

      // User defined action functions
      after-function ?clauses end;
      cleanup-function ?clauses end;
   }
end macro;
