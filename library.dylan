module: dylan-user

define library peg-parser
   use common-dylan;
   use io;
   export peg-parser;
end library;

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
      <parse-failure>, parse-expected, failure-position,
      <token>, parse-start, parse-end,
      seq, choice, many, opt, opt-seq, opt-choice, opt-many, req-next, not-next,
      collect-subelements, *parser-trace*
end module;
