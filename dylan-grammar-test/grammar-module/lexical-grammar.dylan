module: dylan-grammar

define method concatenated-strings (s :: <string>) => (s :: <string>)
   s
end method;

define method concatenated-strings (s :: <sequence>) => (s :: <string>)
   apply(concatenate, "", s)
end method;

define method translate-escape-char (s :: <string>) => (s :: <string>)
   select (s by \=)
      "a" => "\<07>";
      "b" => "\<08>";
      "e" => "\<1b>";
      "f" => "\<0c>";
      "n" => "\<0a>";
      "r" => "\<0d>";
      "t" => "\<09>";
      "0" => "\<00>";
      otherwise => s;
   end select;
end method;

define method translate-escape-code (s :: <string>) => (s :: <string>)
   let code = string-to-integer(s, base: 16);
   format-to-string("%c", as(<character>, code));
end method;

//
// Lexical Syntax
//

define parser space
   rule many(choice(many(space-char), comment));
end;

define caching parser opt-space
   rule opt(space);
end;

// Token separators keep tokens that begin identically from being confused.
define parser word-sep              rule not-next(any-character) end;
define parser pound-word-sep        rule nil(#f) end;
define parser colon-sep             rule not-next(choice(colon, equal)) end;  
define parser double-colon-sep      rule nil(#f) end;
define parser arrow-sep             rule nil(#f) end;
define parser double-question-sep   rule nil(#f) end;
define parser question-equal-sep    rule nil(#f) end;
define parser question-sep          rule not-next(choice(equal, question)) end;
define parser ellipsis-sep          rule nil(#f) end;
define parser double-pound-sep      rule nil(#f) end;
// unused define parser pound-sep             rule not-next(choice(alphabetic-character, pound, lf-paren, lf-brack)) end;
// unused define parser double-slash-sep      rule nil(#f) end;
// unused define parser lf-comment-sep        rule nil(#f) end;         
// unused define parser rt-comment-sep        rule not-next(word-character-not-alphabetic) end;
define parser empty-list-sep        rule nil(#f) end;
// unused define parser bin-sep               rule nil(#f) end;
// unused define parser oct-sep               rule nil(#f) end;
// unused define parser hex-sep               rule nil(#f) end;
define parser lf-list-sep           rule nil(#f) end;
define parser lf-vector-sep         rule nil(#f) end;
define parser identical-sep         rule nil(#f) end;
define parser not-identical-sep     rule nil(#f) end;
define parser not-equal-sep         rule nil(#f) end;
define parser lt-equal-sep          rule nil(#f) end;      
define parser gt-equal-sep          rule nil(#f) end;
define parser bind-sep              rule nil(#f) end;
define parser plus-sep              rule nil(#f) end;
define parser minus-sep             rule nil(#f) end;
define parser star-sep              rule not-next(choice(word-character-not-alphabetic, slash)) end;
define parser slash-sep             rule not-next(slash) end;
define parser caret-sep             rule not-next(word-character-not-alphabetic) end;
define parser equal-sep             rule not-next(gt) end;
define parser lt-sep                rule not-next(choice(word-character-not-alphabetic, equal)) end;
define parser gt-sep                rule not-next(choice(word-character-not-alphabetic, equal)) end;
define parser amp-sep               rule not-next(word-character-not-alphabetic) end;
define parser vert-bar-sep          rule not-next(word-character-not-alphabetic) end;
define parser not-sep               rule not-next(equal) end;
define parser apos-sep              rule nil(#f) end;
// unused define parser backslash-sep         rule nil(#f) end;
define parser quote-sep             rule nil(#f) end;
define parser period-sep            rule not-next(seq(period, period)) end;
// unused define parser exp-sep               rule not-next(any-character) end;
define parser comma-sep             rule nil(#f) end;
define parser semicolon-sep         rule nil(#f) end;
// unused define parser esc-char-sep          rule not-next(any-character) end;
define parser lf-paren-sep          rule nil(#f) end;
define parser rt-paren-sep          rule nil(#f) end;
define parser lf-brack-sep          rule nil(#f) end;
define parser rt-brack-sep          rule nil(#f) end;
define parser lf-brace-sep          rule nil(#f) end;
define parser rt-brace-sep          rule nil(#f) end;
// unused define parser bin-digit-sep         rule not-next(binary-digit) end;
// unused define parser oct-digit-sep         rule not-next(octal-digit)  end;
// unused define parser dec-digit-sep         rule not-next(dec-digit)    end;
// unused define parser hex-digit-sep         rule not-next(hex-digit)    end;
   
//
// Comments
//

define parser comment
   rule choice(single-line-comment, multiline-comment);
end;

define parser single-line-comment
   rule seq(double-slash, opt-many(seq(not-next(eol), char)), eol);
end;

define parser multiline-comment
   rule seq(lf-comment,
            opt-many(choice(multiline-comment, seq(not-next(rt-comment), char))),
            rt-comment);
end;

//
// Tokens
//

define parser lex-TOKEN
   rule choice(lex-NAME, lex-SYMBOL, lex-NUMBER, lex-CHARACTER-LITERAL,
               lex-STRING, lex-UNARY-OPERATOR, lex-BINARY-OPERATOR, punctuation,
               lex-POUND-WORD);
end;

define parser punctuation
   rule choice(lex-LF-BRACK, lex-RT-BRACK, lex-DOUBLE-COLON, /*unused lex-MINUS,*/
               lex-IDENTICAL, lex-ARROW, lex-LF-LIST, lex-LF-VECTOR,
               lex-DOUBLE-POUND, lex-DOUBLE-QUESTION, lex-QUESTION-EQUAL,
               lex-LF-PAREN, lex-RT-PAREN, lex-COMMA, lex-PERIOD,
               lex-SEMICOLON, lex-LF-BRACE, lex-RT-BRACE, lex-EQUAL,
               lex-QUESTION, lex-ELLIPSIS);
end;

define parser pound-word :: <string>
   rule choice(true, false, next, rest, key, all-keys, include) => token;
   yield token;
end;

define parser lex-POUND-WORD (<token>)
   rule seq(opt-space, pound-word, req-next(pound-word-sep)) => tokens;
   slot value :: <string> = tokens[1];
end;

// Tokens directly used in phrase grammar.

define parser lex-DEFINE (<token>)
   rule seq(opt-space, lit-define, req-next(word-sep)) => tokens;
   slot value :: <string> = tokens[1]
end;

define parser lex-END (<token>)
   rule seq(opt-space, lit-end, req-next(word-sep)) => tokens;
   slot value :: <string> = tokens[1]
end;

define parser lex-HANDLER (<token>)
   rule seq(opt-space, lit-handler, req-next(word-sep)) => tokens;
   slot value :: <string> = tokens[1]
end;

define caching parser lex-LET (<token>)
   rule seq(opt-space, lit-let, req-next(word-sep)) => tokens;
   slot value :: <string> = tokens[1]
end;

define parser lex-LOCAL (<token>)
   rule seq(opt-space, lit-local, req-next(word-sep)) => tokens;
   slot value :: <string> = tokens[1];
end;

define parser lex-MACRO (<token>)
   rule seq(opt-space, lit-macro, req-next(word-sep)) => tokens;
   slot value :: <string> = tokens[1]
end;

define parser lex-METHOD (<token>) 
   rule seq(opt-space, lit-method, req-next(word-sep)) => tokens; 
   slot value :: <string> = tokens[1]
end;

define parser lex-OTHERWISE (<token>) 
   rule seq(opt-space, lit-otherwise, req-next(word-sep)) => tokens; 
   slot value :: <string> = tokens[1]
end;

define parser lex-DOUBLE-COLON (<token>)
   rule seq(opt-space, double-colon, req-next(double-colon-sep)) => tokens;
   slot value :: <string> = tokens[1]
end;

define parser lex-ARROW (<token>) 
   rule seq(opt-space, arrow, req-next(arrow-sep)) => tokens;
   slot value :: <string> = tokens[1]
end;

define parser lex-DOUBLE-QUESTION (<token>)
   rule seq(opt-space, double-question, req-next(double-question-sep)) => tokens;
   slot value :: <string> = tokens[1]
end;

define parser lex-QUESTION-EQUAL (<token>) 
   rule seq(opt-space, question-equal, req-next(question-equal-sep)) => tokens;
   slot value :: <string> = tokens[1]
end;

define parser lex-QUESTION (<token>) 
   rule seq(opt-space, question, req-next(question-sep)) => tokens;
   slot value :: <string> = tokens[1]
end;

define parser lex-ELLIPSIS (<token>) 
   rule seq(opt-space, ellipsis, req-next(ellipsis-sep)) => tokens;
   slot value :: <string> = tokens[1]
end;

define parser lex-DOUBLE-POUND (<token>) 
   rule seq(opt-space, double-pound, req-next(double-pound-sep)) => tokens;
   slot value :: <string> = tokens[1]
end;

define parser lex-EMPTY-LIST (<token>) 
   rule seq(opt-space, empty-list, req-next(empty-list-sep)) => tokens;
   slot value :: <string> = tokens[1]
end;

define parser lex-TRUE (<token>) 
   rule seq(opt-space, true, req-next(pound-word-sep)) => tokens;
   slot value :: <string> = tokens[1]
end;

define parser lex-FALSE (<token>) 
   rule seq(opt-space, false, req-next(pound-word-sep)) => tokens;
   slot value :: <string> = tokens[1]
end;

define parser lex-NEXT (<token>) 
   rule seq(opt-space, next, req-next(pound-word-sep)) => tokens;
   slot value :: <string> = tokens[1]
end;

define parser lex-REST (<token>) 
   rule seq(opt-space, rest, req-next(pound-word-sep)) => tokens;
   slot value :: <string> = tokens[1]
end;

define parser lex-KEY (<token>) 
   rule seq(opt-space, key, req-next(pound-word-sep)) => tokens;
   slot value :: <string> = tokens[1]
end;

define parser lex-ALL-KEYS (<token>) 
   rule seq(opt-space, all-keys, req-next(pound-word-sep)) => tokens;
   slot value :: <string> = tokens[1]
end;

define caching parser lex-LF-LIST (<token>) 
   rule seq(opt-space, lf-list, req-next(lf-list-sep)) => tokens;
   slot value :: <string> = tokens[1]
end;

define parser lex-LF-VECTOR (<token>) 
   rule seq(opt-space, lf-vector, req-next(lf-vector-sep)) => tokens;
   slot value :: <string> = tokens[1]
end;

define parser lex-IDENTICAL (<token>) 
   rule seq(opt-space, identical, req-next(identical-sep)) => tokens;
   slot value :: <string> = tokens[1]
end;

define parser lex-EQUAL (<token>) 
   rule seq(opt-space, equal, req-next(equal-sep)) => tokens;
   slot value :: <string> = tokens[1]
end;

define parser lex-PERIOD (<token>) 
   rule seq(opt-space, period, req-next(period-sep)) => tokens;
   slot value :: <string> = tokens[1]
end;

define parser lex-COMMA (<token>) 
   rule seq(opt-space, comma, req-next(comma-sep)) => tokens;
   slot value :: <string> = tokens[1]
end;

define parser lex-SEMICOLON (<token>) 
   rule seq(opt-space, semicolon, req-next(semicolon-sep)) => tokens;
   slot value :: <string> = tokens[1]
end;

define parser lex-LF-PAREN (<token>) 
   rule seq(opt-space, lf-paren, req-next(lf-paren-sep)) => tokens;
   slot value :: <string> = tokens[1]
end;

define parser lex-RT-PAREN (<token>) 
   rule seq(opt-space, rt-paren, req-next(rt-paren-sep)) => tokens;
   slot value :: <string> = tokens[1]
end;

define parser lex-LF-BRACK (<token>) 
   rule seq(opt-space, lf-brack, req-next(lf-brack-sep)) => tokens;
   slot value :: <string> = tokens[1]
end;

define parser lex-RT-BRACK (<token>) 
   rule seq(opt-space, rt-brack, req-next(rt-brack-sep)) => tokens;
   slot value :: <string> = tokens[1]
end;

define parser lex-LF-BRACE (<token>) 
   rule seq(opt-space, lf-brace, req-next(lf-brace-sep)) => tokens;
   slot value :: <string> = tokens[1]
end;

define parser lex-RT-BRACE (<token>) 
   rule seq(opt-space, rt-brace, req-next(rt-brace-sep)) => tokens;
   slot value :: <string> = tokens[1]
end;


//
// Reserved Words
//

define parser reserved-word :: <string>
   rule choice(core-word, begin-word, function-word,
               define-body-word, define-list-word)
   => token;
   yield token;
end;

define parser core-word :: <string>
   rule seq(choice(lit-define, lit-end, lit-handler, lit-let, lit-local, lit-macro,
                   lit-otherwise),
            req-next(word-sep))
   => tokens;
   yield tokens[0];
end;

define caching parser begin-word :: <string>
   rule seq(choice(lit-begin, lit-block, lit-case, lit-for, lit-if, lit-method,
                   lit-select, lit-unless, lit-until, lit-while),
            req-next(word-sep))
   => tokens;
   yield tokens[0];
end;

define parser lex-BEGIN-WORD (<token>)
   rule seq(opt-space, begin-word)
   => tokens;
   slot value :: <string> = tokens[1];
end;

define caching parser function-word :: <string>
   rule seq(choice(), req-next(word-sep)) => tokens;
   yield tokens[0];
end;

define parser lex-FUNCTION-WORD (<token>)
   rule seq(opt-space, function-word)
   => tokens;
   slot value :: <string> = tokens[1];
end;

define caching parser define-body-word :: <string>
   rule seq(choice(lit-class, lit-library, lit-method, lit-function, lit-module),
            req-next(word-sep))
   => tokens;
   yield tokens[0];
end;

define caching parser lex-DEFINE-BODY-WORD (<token>)
   rule seq(opt-space, define-body-word)
   => tokens;
   slot value :: <string> = tokens[1];
end;

define caching parser define-list-word :: <string>
   rule seq(choice(lit-constant, lit-variable, lit-generic, lit-domain),
            req-next(word-sep))
   => tokens;
   yield tokens[0];
end;

define parser lex-DEFINE-LIST-WORD (<token>)
   rule seq(opt-space, define-list-word)
   => tokens;
   slot value :: <string> = tokens[1];
end;

//
// Names, Symbols and Keywords
//

define parser name :: <string>
   rule choice(word, seq(backslash, word), operator-name) => token;
   yield token.concatenated-strings;
end;

define parser lex-NAME (<token>)
   label "name";
   rule seq(opt-space, name, req-next(word-sep)) => tokens;
   slot value :: <string> = tokens[1];
end;

define parser unreserved-name :: <string>
   rule seq(not-next(reserved-word), name) => tokens;
   yield tokens[1];
end;

define parser lex-UNRESERVED-NAME (<token>)
   label "name";
   rule seq(opt-space, unreserved-name, req-next(word-sep)) => tokens;
   slot value :: <string> = tokens[1];
end;

define parser ordinary-name :: <string>
   rule choice(unreserved-name, define-body-word, define-list-word) => token;
   yield token;
end;

define parser lex-ORDINARY-NAME (<token>)
   label "name";
   rule seq(opt-space, ordinary-name, req-next(word-sep)) => tokens;
   slot value :: <string> = tokens[1];
end;

define parser lex-CONSTRAINED-NAME (<token>)
   label "constrained name";
   rule seq(opt-space,
            choice(seq(name, colon, word, req-next(word-sep)),
                   seq(name, colon, binary-operator-with-sep),
                   seq(colon, word, req-next(word-sep))))
   => tokens;
end;

define parser operator-name :: <string>
   rule seq(backslash, choice(unary-operator, binary-operator)) => tokens;
   yield tokens.concatenated-strings;
end;

define parser macro-name :: <string>
   rule choice(ordinary-name, begin-word, function-word) => token;
   yield token;
end;

define parser lex-MACRO-NAME (<token>)
   label "name";
   rule seq(opt-space, macro-name, req-next(word-sep)) => tokens;
   slot value :: <string> = tokens[1];
end;

define parser lex-NAME-NOT-END (<token>)
   label "name";
   rule seq(opt-space,
            choice(macro-name, lit-define, lit-handler, lit-let,
                   lit-local, lit-macro, lit-otherwise),
            req-next(word-sep))
   => tokens;
   slot value :: <string> = tokens[1];
end;

define parser lex-SYMBOL (<token>)
   label "symbol";
   rule seq(opt-space,
            choice(seq(nil(#f), word, colon, req-next(colon-sep)),
                   seq(pound, string, req-next(quote-sep))))
   => tokens;
   slot value :: <string> = tokens[1][1];
end;

define caching parser word :: <string>
   rule choice(leading-alphabetic,
               seq(leading-numeric, alphabetic-character, leading-alphabetic),
               seq(leading-graphic, leading-alphabetic))
   => token;
   yield token.concatenated-strings;
end;

define parser leading-alphabetic :: <string>
   rule seq(alphabetic-character, opt-many(any-character)) => tokens;
   yield apply(concatenate, tokens[0], tokens[1] | #[""]);
end;

define parser leading-numeric :: <string>
   rule seq(numeric-character, opt-many(word-character-not-double-alphabetic))
   => tokens;
   yield apply(concatenate, tokens[0], tokens[1] | #[""]);
end;

define parser leading-graphic :: <string>
   rule seq(graphic-character, opt-many(word-character-not-alphabetic)) => tokens;
   yield apply(concatenate, tokens[0], tokens[1] | #[""]);
end;

define parser word-character-not-alphabetic :: <string>
   rule choice(numeric-character, graphic-character, special-character) => token;
   yield token;
end;

define parser word-character-not-double-alphabetic :: <string>
   rule choice(seq(alphabetic-character, word-character-not-alphabetic),
               numeric-character,
               graphic-character,
               special-character)
   => token;
   yield token.concatenated-strings;
end;

define caching parser any-character :: <string>
   rule choice(alphabetic-character,
               numeric-character,
               graphic-character,
               special-character)
   => token;
   yield token;
end;

define parser-method alphabetic-character (stream, context)
=> (char :: false-or(<string>), succ? :: <boolean>, ext :: <parse-extent>)
   label "alphabetic character";
   let char = read-element(stream, on-end-of-stream: #f);
   case
      char & char.alphabetic? => values(format-to-string("%c", char), #t, #f);
      ~char =>                   values(#f, #f, $eof);
      otherwise =>               values(#f, #f, #f);
   end case;
end parser-method;

define parser-method numeric-character (stream, context)
=> (char :: false-or(<string>), succ? :: <boolean>, ext :: <parse-extent>)
   label "numeric character";
   let char = read-element(stream, on-end-of-stream: #f);
   case
      char & char.decimal-digit? => values(format-to-string("%c", char), #t, #f);
      ~char =>                      values(#f, #f, $eof);
      otherwise =>                  values(#f, #f, #f);
   end case;
end parser-method;

define lexical-parsers
   graphic-character       in "!&*<>|^$%@_"  label "graphic character";
   special-character       in "-+~?/="       label "graphic character";
end;

//
// Operators
//

define lexical-parsers
   unary-operator          in "-~"           label "unary operator";
end;

define parser unary-operator-with-sep :: <string>
   rule choice(seq(minus,  req-next(minus-sep)),
               seq(not,    req-next(not-sep  )))
   => token;
   yield token[0];
end;

define parser lex-UNARY-OPERATOR (<token>)
   label "unary operator";
   rule seq(opt-space, unary-operator-with-sep) => tokens;
   slot value :: <string> = tokens[1];
end;

define parser binary-operator :: <string>
   label "binary operator";
   rule choice(identical, not-identical, not-equal, lt-equal, gt-equal, bind,
               plus, minus, star, slash, caret, equal, lt, gt, amp, vert-bar)
   => token;
   yield token;
end;

define parser binary-operator-with-sep :: <string>
   rule choice(seq(identical,       req-next(identical-sep     )),
               seq(not-identical,   req-next(not-identical-sep )),
               seq(not-equal,       req-next(not-equal-sep     )),
               seq(lt-equal,        req-next(lt-equal-sep      )),
               seq(gt-equal,        req-next(gt-equal-sep      )),
               seq(bind,            req-next(bind-sep          )),
               seq(plus,            req-next(plus-sep          )),
               seq(minus,           req-next(minus-sep         )),
               seq(star,            req-next(star-sep          )),
               seq(slash,           req-next(slash-sep         )),
               seq(caret,           req-next(caret-sep         )),
               seq(equal,           req-next(equal-sep         )),
               seq(lt,              req-next(lt-sep            )),
               seq(gt,              req-next(gt-sep            )),
               seq(amp,             req-next(amp-sep           )),
               seq(vert-bar,        req-next(vert-bar-sep      )))
   => token;
   yield token[0];
end;

define parser lex-BINARY-OPERATOR (<token>)
   label "binary operator";
   rule seq(opt-space, binary-operator-with-sep) => tokens;
   slot value :: <string> = tokens[1];
end;

//
// Character and String Literals
//

define parser lex-CHARACTER-LITERAL (<token>)
   label "character literal";
   rule seq(opt-space, apos, character, apos, req-next(apos-sep)) => tokens;
   slot value :: <string> = tokens[2];
end;

define parser character :: <string>
   rule choice(seq(nil(#"char"), not-next(apos), not-next(backslash), printing-character),
               seq(nil(#"esc"), backslash, escape-character))
   => token;
   yield select (token[0])
            #"char" => token[3];
            #"esc" => token[2];
         end select;
end;

define parser string :: <string>
   rule seq(quote, opt-many(string-character), quote) => tokens;
   yield tokens[1].concatenated-strings;
end;

define parser lex-STRING (<token>)
   label "string literal";
   rule seq(opt-space, string, req-next(quote-sep)) => tokens;
   slot value :: <string> = tokens[1];
end;

define parser string-character :: <string>
   rule choice(seq(nil(#"char"), not-next(quote), not-next(backslash), printing-character),
               seq(nil(#"esc"), backslash, escape-character))
   => token;
   yield select (token[0])
            #"char" => token[3];
            #"esc" => token[2];
         end select;
end;

define parser escape-character :: <string>
   rule choice(seq(nil(#"char"), esc-char),
               seq(nil(#"code"), lt, many(hex-digit), gt))
   => token;
   yield select (token[0])
            #"char" => translate-escape-char(token[1]);
            #"code" => translate-escape-code(token[2].concatenated-strings);
         end select;
end;

//
// Numbers
//

define parser lex-NUMBER (<token>)
   label "number";
   rule seq(opt-space, choice(integer, ratio, floating-point))
   => tokens;
   slot value = tokens[1];
end;

define parser integer (<token>)
   rule choice(seq(nil(#"bin"), binary-integer),
               seq(nil(#"oct"), octal-integer),
               seq(nil(#"dec"), opt(sign), decimal-integer),
               seq(nil(#"hex"), hex-integer))
   => token;
   slot base :: <symbol> = token[0];
   slot value :: <string>
      = if (token[0] = #"dec") concatenate(token[1] | "", token[2]) else token[1] end;
end;

define parser binary-integer :: <string>
   rule seq(bin, many(binary-digit)) => tokens;
   yield tokens[1].concatenated-strings;
end;

define parser octal-integer :: <string>
   rule seq(oct, many(octal-digit)) => tokens;
   yield tokens[1].concatenated-strings;
end;

define caching parser decimal-integer :: <string>
   rule many(decimal-digit) => tokens;
   yield tokens.concatenated-strings;
end;

define parser hex-integer :: <string>
   rule seq(hex, many(hex-digit)) => tokens;
   yield tokens[1].concatenated-strings;
end;

define lexical-parsers
   binary-digit   in "01"                 label "binary digit";
   octal-digit    in "01234567"           label "octal digit";
   decimal-digit  in "0123456789"         label "decimal digit";
   hex-digit      in "0123456789abcdef"   label "hexadecimal digit";
end lexical-parsers;

define parser ratio (<token>)
   rule seq(opt(sign), decimal-integer, slash, decimal-integer) => tokens;
   slot sign :: <string> = tokens[0] | "+";
   slot numerator :: <string> = tokens[1];
   slot denominator :: <string> = tokens[3];
end;

define parser floating-point (<token>)
   rule seq(opt(sign),
            choice(seq(nil(#"form1"), opt(decimal-integer), period, decimal-integer, opt(exponent)),
                   seq(nil(#"form2"), decimal-integer, period, req-next(period-sep), opt(decimal-integer), opt(exponent)),
                   seq(nil(#"form3"), decimal-integer, exponent)))
   => tokens;
   slot mantissa-sign :: <string> = tokens[0] | "+";
   slot mantissa-int :: <string> = tokens[1][1] | "0";
   slot mantissa-frac :: <string> = select (tokens[1][0])
                                       #"form1" => tokens[1][3];
                                       #"form2" => tokens[1][4];
                                       #"form3" => "0";
                                    end select;
   slot exp-sign :: <string> = select (tokens[1][0])
                                  #"form1" => tokens[1][4] & tokens[1][4].sign | "+";
                                  #"form2" => tokens[1][5] & tokens[1][5].sign | "+";
                                  #"form3" => tokens[1][2].sign;
                               end select;
   slot exp-value :: <string> = select (tokens[1][0])
                                   #"form1" => tokens[1][4] & tokens[1][4].value | "0";
                                   #"form2" => tokens[1][5] & tokens[1][5].value | "0";
                                   #"form3" => tokens[1][2].value;
                                end select;
end;

define parser exponent (<token>)
   rule seq(exp, opt(sign), decimal-integer) => tokens;
   slot sign :: <string> = tokens[1] | "+";
   slot value :: <string> = tokens[2];
end;

define lexical-parsers
   sign  in "+-"  label "\"+\" or \"-\"";
end lexical-parsers;
