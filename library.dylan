module: dylan-user

define library peg-parser
   use common-dylan;
   use io;
   export peg-parser, peg-parser-client;
end library;

/// Synopsis: Parser creation.
define module peg-parser
   // from common-dylan
   use dylan;
   use common-extensions, exclude: {format-to-string};
   // from io
   use streams;
   use format;
   use standard-io;

   export
      <parse-context>, parser-cache-hits, invalidate-parser-cache,
      *parser-cache-hits*;
      
   export
      parser-definer, parser-method-definer,
      <parse-failure>, parse-expected, combine-errors, failure-position,
      parse-expected-list, parse-expected-other-than-list,
      <token>, parse-start, parse-end;
   
   export
      seq, choice, many, opt, req-next, not-next, nil,
      opt-seq, opt-choice, opt-many;

   export
      collect-subelements, *parser-trace*;
   
   // This is only exported because of a bug in Gwydion Dylan (#7392).
   // Do not use.
   export labeled-parser-definer;
end module;

/// Synopsis: Parser usage.
define module peg-parser-client
   use peg-parser, export:
      { <parse-failure>, failure-position,
        parse-expected, parse-expected-list, parse-expected-other-than-list,
        <token>, parse-start, parse-end,
        <parse-context>, parser-cache-hits, invalidate-parser-cache,
        *parser-cache-hits*, *parser-trace* }
end module;
