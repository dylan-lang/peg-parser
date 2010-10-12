module: dylan-user

define library peg-parser
   use common-dylan;
   use dynamic-binding;
   use io;
   export peg-parser, peg-parser-client;
end library;

// Synopsis: Module for parser creation.
define module peg-parser
   // from common-dylan
   use dylan;
   use common-extensions, exclude: { format-to-string };
   // from dynamic-binding
   use dynamic-binding,
      rename: { with-dynamic-bindings => with-attributes,
                dynamic-binding => attr,
                dynamic-binding-setter => attr-setter,
                <binding-not-in-dynamic-scope> => <attribute-not-in-dynamic-scope>,
                binding-name => attribute-name },
      export: all;
   // from io
   use streams;
   use format;
   use standard-io;

   export
      <parse-context>, look-ahead?,
      parser-cache-hits, invalidate-parser-cache, *parser-cache-hits*;
      
   export
      parser-definer, parser-method-definer,
      <parse-extent>, <parse-success>, <parse-failure>, parse-position,
      parse-expected-list, parse-expected-other-than-list, parse-success-list,
      parse-expected, combine-extents,
      <token>, parse-start, parse-end, parse-start-setter, parse-end-setter;
   
   export
      seq, choice, many, opt, req-next, not-next, nil, skip,
      opt-seq, opt-choice, opt-many;

   export
      collect-subelements, *parser-trace*;
end module;   

/// Synopsis: Simplified module for parser usage.
define module peg-parser-client
   use peg-parser, export:
      { <parse-extent>, <parse-success>, <parse-failure>, parse-position,
        parse-expected-list, parse-expected-other-than-list, parse-success-list,
        parse-expected,
        <token>, parse-start, parse-end,
        <parse-context>, parser-cache-hits, *parser-cache-hits*,
        *parser-trace* }
end module;
