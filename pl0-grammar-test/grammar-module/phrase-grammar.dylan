module: PL0-grammar

/*
Here is the PL/0 grammar in EBNF form.

program = block "." .

block = [ "const" ident "=" number {"," ident "=" number} ";"]
        [ "var" ident {"," ident} ";"]
        { "procedure" ident ";" block ";" } statement .

statement = [ ident ":=" expression | "call" ident |
            "begin" statement {";" statement } "end" |
            "if" condition "then" statement |
            "while" condition "do" statement ].

condition = "odd" expression |
            expression ("="|"#"|"<"|"<="|">"|">=") expression .

expression = [ "+"|"-"] term { ("+"|"-") term}.

term = factor {("*"|"/") factor}.

factor = ident | number | "(" expression ")".
*/

define parser program :: <block-token>
   rule seq(opt-space, \block, lex-PERIOD, not-next(char))
      => tokens;
   yield tokens[1];
end;

define parser \block (<token>)
   rule seq(opt(const-block), opt(var-block), opt-many(procedure-block), statement)
      => tokens;
end;

define parser const-block (<token>)
   rule seq(lex-CONST, const-defn, opt-many(seq(lex-COMMA, const-defn)), lex-SEMICOLON)
      => tokens;
end;

define parser const-defn (<token>)
   rule seq(lex-IDENT, lex-EQUAL, expression)
      => tokens;
end;

define parser var-block (<token>)
   rule seq(lex-VAR, lex-IDENT, opt-many(seq(lex-COMMA, lex-IDENT)), lex-SEMICOLON)
      => tokens;
end;

define parser procedure-block (<token>)
   rule seq(lex-PROCEDURE, lex-IDENT, lex-SEMICOLON, \block, lex-SEMICOLON)
      => tokens;
end;

define parser statement :: false-or(<token>)
   rule opt-choice(assignment, call, begin-end, if-then, while-do)
      => token;
   yield token;
end;

define parser assignment (<token>)
   rule seq(lex-IDENT, lex-COLON-EQUAL, expression)
      => tokens;
end;

define parser call (<token>)
   rule seq(lex-CALL, lex-IDENT)
      => tokens;
end;

define parser begin-end (<token>)
   rule seq(lex-BEGIN, statement, opt-many(seq(lex-SEMICOLON, statement)), lex-END)
      => tokens;
end;

define parser if-then (<token>)
   rule seq(lex-IF, condition, lex-THEN, statement)
      => tokens;
end;

define parser while-do (<token>)
   rule seq(lex-WHILE, condition, lex-DO, statement)
      => tokens;
end;

define parser condition :: type-union(<oddity-token>, <comparison-token>)
   rule choice(oddity, comparison)
      => token;
   yield token;
end;

define parser oddity (<token>)
   rule seq(lex-ODD, expression)
      => tokens;
end;

define parser comparison (<token>)
   rule seq(expression,
            choice(lex-EQUAL, lex-HASH, lex-LT, lex-LTE, lex-GT, lex-GTE),
            expression)
      => tokens;
end;

define parser expression (<token>)
   rule seq(opt-choice(lex-PLUS, lex-MINUS),
            term,
            opt-many(seq(choice(lex-PLUS, lex-MINUS), term)))
      => tokens;
end;

define parser term (<token>)
   rule seq(factor, opt-many(seq(choice(lex-STAR, lex-SLASH), factor)))
      => tokens;
end;

define parser factor :: type-union(<lex-IDENT-token>, <lex-NUMBER-token>, <expression-token>)
   rule choice(seq(nil(#f), lex-IDENT),
               seq(nil(#f), lex-NUMBER),
               seq(lex-LF-PAREN, expression, lex-RT-PAREN))
      => tokens;
   yield tokens[1];
end;
