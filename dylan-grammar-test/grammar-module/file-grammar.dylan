module: dylan-grammar

define method join-header-values (#rest value-lines) => (value :: <string>)
   reduce1(method (a,b) concatenate(a, "\n", b) end, value-lines)
end function;

define lexical-parsers
   hdr-char    in "abcdefghijklmnopqrstuvwxyz0123456789-"
               label "hyphen or alphanumeric character";
   hdr-space   in " \t" label "whitespace";
end;

//
// Dylan Interchange Format
//

define parser source-file ()
   rule seq(many(header), opt-seq(hdr-eol, source-record), not-next(char))
   => tokens;
   slot headers = tokens[0];
   slot source-record = tokens[1] & tokens[1][1];
end parser;

define parser header ()
   label "interchange format header";
   rule seq(hdr-keyword, hdr-value, opt-many(hdr-addl-value))
   => tokens;
   slot keyword :: <string> = tokens[0];
   slot value :: <string>
      = apply(join-header-values, tokens[1], tokens[2] | #[]); 
end parser;

define parser hdr-keyword :: <string>
   rule seq(alphabetic-character, opt-many(hdr-char), colon, opt(hdr-eol))
   => tokens;
   yield apply(concatenate, tokens[0], tokens[1] | #[]);
end parser;

define parser hdr-value :: <string>
   rule seq(opt-many(seq(not-next(hdr-eol), char)), hdr-eol)
   => tokens;
   yield apply(concatenate, collect-subelements(tokens[0], 1));
end parser;

define parser hdr-addl-value :: <string>
   rule seq(hdr-whitespace, many(seq(not-next(hdr-eol), char)), hdr-eol)
   => tokens;
   yield apply(concatenate, collect-subelements(tokens[1], 1));
end parser;

define parser hdr-whitespace
   rule many(hdr-space);
end parser;

define parser hdr-eol
   rule seq(opt(hdr-space), eol);
end parser;
