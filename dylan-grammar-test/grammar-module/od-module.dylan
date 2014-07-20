module: dylan-user

define module dylan-grammar
   // from common-dylan
   use common-dylan, exclude: {format-to-string};
   // from io
   use streams;
   use format, import: {format-to-string};
   use format-out;
   // from strings
   use strings;
   // from peg-parser
   use peg-parser-client, export: all;
   use peg-parser;

   export parse-source-file;
end module;
