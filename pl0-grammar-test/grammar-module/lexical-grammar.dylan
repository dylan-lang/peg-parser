module: PL0-grammar


define class <lexeme> (<token>)
   slot text :: <string>
end class;

define macro lexemes-definer
   { define lexemes ?parse-items end } => { ?parse-items }
parse-items:
   { ?:name = ?:expression ?sep:name; ... }
   => {  define parser-method "lit-" ## ?name (stream, context)
         => (string :: false-or(<string>))
            label format-to-string("\"%s\"", ?expression);
            read-matching-string(stream, ?expression);
         end parser-method;
      
         define parser "lex-" ## ?name (<lexeme>)
            rule seq("lit-" ## ?name, ?sep) => tokens;
            inherited slot text = tokens[0];
         afterwards (context, tokens, result, start-pos, end-pos)
            result.parse-end := tokens.last.parse-start;
         end parser;
         
         ... }
   { } => { }
end macro;


define lexemes
   \PERIOD      = "."         nil-sep;
   \EQUAL       = "="         nil-sep;
   \COMMA       = ","         nil-sep;
   \SEMICOLON   = ";"         nil-sep;
   \CONST       = "CONST"     word-sep;
   \VAR         = "VAR"       word-sep;
   \PROCEDURE   = "PROCEDURE" word-sep;
   \COLON-EQUAL = ":="        nil-sep;
   \CALL        = "CALL"      word-sep;
   \BEGIN       = "BEGIN"     word-sep;
   \END         = "END"       word-sep;
   \IF          = "IF"        word-sep;
   \THEN        = "THEN"      word-sep;
   \WHILE       = "WHILE"     word-sep;
   \DO          = "DO"        word-sep;
   \ODD         = "ODD"       word-sep;
   \HASH        = "#"         nil-sep;
   \LT          = "<"         lt-sep;
   \LTE         = "<="        nil-sep;
   \GT          = ">"         gt-sep;
   \GTE         = ">="        nil-sep;
   \PLUS        = "+"         nil-sep;
   \MINUS       = "-"         nil-sep;
   \STAR        = "*"         nil-sep;
   \SLASH       = "/"         nil-sep;
   \LF-PAREN    = "("         nil-sep;
   \RT-PAREN    = ")"         nil-sep;  
end lexemes;

define parser lex-IDENT (<lexeme>)
   rule seq(letter, opt-many(choice(letter, digit)), nil-sep) => tokens;
   inherited slot text = concatenate(as(<string>, tokens[0]), tokens[1] | "");
afterwards (context, tokens, result, start-pos, end-pos)
   result.parse-end := tokens.last.parse-start;
end;

define parser lex-NUMBER (<lexeme>)
   rule seq(many(digit), nil-sep) => tokens;
   inherited slot text = as(<string>, tokens[0]);
afterwards (context, tokens, result, start-pos, end-pos)
   result.parse-end := tokens.last.parse-start;
end; 

define parser nil-sep (<token>)
   rule opt-space => token;
end;

define parser word-sep (<token>)
   rule seq(not-next(letter), opt-space) => token;
end;

define parser lt-sep (<token>)
   rule seq(not-next(lit-equal), opt-space) => token;
end;

define parser gt-sep (<token>)
   rule seq(not-next(lit-equal), opt-space) => token;
end;

define parser opt-space
   rule opt-many(whitespace)
end;

define parser-method letter (stream, context)
=> (result :: false-or(<character>))
   label "letter";
   let char = read-element(stream, on-end-of-stream: #f);
   case
      char & char.alphabetic? => char;
      otherwise => #f;
   end case;
end parser-method;

define parser-method digit (stream, context)
=> (result :: false-or(<character>))
   label "digit";
   let char = read-element(stream, on-end-of-stream: #f);
   case
      char & char.digit? => char;
      otherwise => #f;
   end case;
end parser-method;

define parser-method whitespace (stream, context)
=> (result :: false-or(<character>))
   label "whitespace";
   let char = read-element(stream, on-end-of-stream: #f);
   case
      char & char.whitespace? => char;
      otherwise => #f;
   end case;
end parser-method;

define parser-method char (stream, context)
=> (result :: false-or(<character>))
   label "character";
   read-element(stream, on-end-of-stream: #f);
end parser-method;

define method read-matching-string (stream, string :: <string>)
=> (str :: false-or(<string>))
   let in-str = read(stream, string.size, on-end-of-stream: #f);
   case
      in-str = string => string;
      otherwise => #f;
   end case
end method;
