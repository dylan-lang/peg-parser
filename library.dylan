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
      parser-definer, parser-method-definer,
      <parse-failure>, parse-expected, combine-errors, failure-position,
      parse-expected-list, parse-expected-other-than-list,
      <token>, parse-start, parse-end,
      seq, choice, many, opt, opt-seq, opt-choice, opt-many, req-next, not-next,
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
        <token>, parse-start, parse-end, *parser-trace* }
end module;
