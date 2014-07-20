module: dylan-grammar

//
// Program Structure
//

define parser source-record ()
   rule opt-seq(body, opt-space) => tokens;
end parser;

define parser body ()
   rule seq(constituents, opt(lex-SEMICOLON)) => tokens;
end parser;

define parser constituents ()
   rule seq(constituent, opt-many(seq(lex-SEMICOLON, constituent))) => tokens;
end parser;

define parser constituent ()
   rule choice(definition, local-declaration, expression) => tokens;
end parser;

define parser \macro ()
   rule choice(definition-macro-call, statement, function-macro-call) => tokens;
end parser;

//
// Property Lists
//

define parser comma-property-list ()
   rule seq(lex-COMMA, property-list) => tokens;
end parser;

define parser property-list ()
   rule seq(property, opt-many(seq(lex-COMMA, property))) => tokens;
end parser;

define parser property ()
   rule seq(lex-SYMBOL, value) => tokens;
end parser;

define parser value ()
   rule basic-fragment => tokens;
end parser;

//
// Fragments
//

define parser body-fragment ()
   rule choice(seq(statement, opt(non-statement-body-fragment)),
               non-statement-body-fragment) => tokens;
end parser;

define parser list-fragment ()
   rule choice(seq(statement, opt(non-statement-list-fragment)),
               non-statement-list-fragment) => tokens;
end parser;

define parser basic-fragment ()
   rule choice(seq(statement, opt(non-statement-basic-fragment)),
               non-statement-basic-fragment) => tokens;
end parser;

define parser non-statement-body-fragment ()
   rule choice(seq(definition, opt(semicolon-fragment)),
               seq(local-declaration, opt(semicolon-fragment)),
               seq(simple-fragment, opt(body-fragment)),
               seq(lex-COMMA, opt(body-fragment)),
               semicolon-fragment) => tokens;
end parser;

define parser semicolon-fragment ()
   rule seq(lex-SEMICOLON, opt(body-fragment)) => tokens;
end parser;

define parser non-statement-list-fragment ()
   rule seq(choice(simple-fragment, lex-COMMA), opt(list-fragment)) => tokens;
end parser;

define parser non-statement-basic-fragment ()
   rule seq(simple-fragment, opt(basic-fragment)) => tokens;
end parser;

define parser simple-fragment ()
   rule choice(function-macro-call, constant-fragment, variable-name,
               lex-BINARY-OPERATOR, lex-UNARY-OPERATOR, bracketed-fragment, 
               lex-POUND-WORD, lex-PERIOD, lex-DOUBLE-COLON, lex-ARROW,
               lex-DOUBLE-QUESTION, lex-QUESTION-EQUAL, lex-QUESTION,
               lex-ELLIPSIS, lex-DOUBLE-POUND, lex-OTHERWISE) => tokens;
end parser;

define parser bracketed-fragment ()
   rule choice(seq(lex-LF-PAREN, opt(body-fragment), lex-RT-PAREN),
               seq(lex-LF-BRACK, opt(body-fragment), lex-RT-BRACK),
               seq(lex-LF-BRACE, opt(body-fragment), lex-RT-BRACE)) => tokens;
end parser;

define parser constant-fragment ()
   rule choice(lex-NUMBER, lex-CHARACTER-LITERAL, lex-STRING, lex-SYMBOL,
               seq(lex-LF-LIST, constants, lex-PERIOD, constant, lex-RT-PAREN),
               seq(lex-LF-LIST, opt(constants), lex-RT-PAREN),
               seq(lex-LF-VECTOR, opt(constants), lex-RT-BRACK)) => tokens;
end parser;

//
// Definitions
//

define parser definition ()
   rule choice(definition-macro-call,
               seq(lex-DEFINE, lex-MACRO, macro-definition)) => tokens;
end parser;

define parser definition-macro-call ()
   rule seq(lex-DEFINE, opt-many(modifier),
            choice(seq(lex-DEFINE-BODY-WORD, opt(body-fragment), definition-tail),
                   seq(lex-DEFINE-LIST-WORD, opt(list-fragment)))) => tokens;
end parser;

define parser modifier ()
   rule lex-UNRESERVED-NAME => tokens;
end parser;

define parser definition-tail ()
   rule choice(seq(lex-END, lex-DEFINE-BODY-WORD, lex-MACRO-NAME),
               seq(lex-END, opt(lex-MACRO-NAME))) => tokens;
end parser;

//
// Local Declarations
//

define parser local-declaration ()
   rule choice(seq(lex-LET, bindings),
               seq(lex-LET, lex-HANDLER, condition, lex-EQUAL, \handler),
               seq(lex-LOCAL, local-methods)) => tokens;
end parser;

define parser condition ()
   rule choice(type, seq(lex-LF-PAREN, type, comma-property-list, lex-RT-PAREN)) => tokens;
end parser;

define parser \handler ()
   rule expression => tokens;
end parser;

define parser local-methods ()
   rule seq(opt(lex-METHOD),
            method-definition,
            opt-many(seq(lex-COMMA, method-definition))) => tokens;
end parser;

define parser bindings ()
   rule seq(choice(variable, seq(lex-LF-PAREN, variable-list, lex-RT-PAREN)),
            lex-EQUAL, expression) => tokens;
end parser;

define parser variable-list ()
   rule choice(seq(variables, opt-seq(lex-COMMA, lex-REST, variable-name)),
               seq(lex-REST, variable-name)) => tokens;
end parser;

define parser variables ()
   rule seq(variable, opt-many(seq(lex-COMMA, variable))) => tokens;
end parser;

define parser variable ()
   rule seq(variable-name, opt-seq(lex-DOUBLE-COLON, type)) => tokens;
end parser;

define parser variable-name ()
   rule lex-ORDINARY-NAME => tokens;
end parser;

define parser type ()
   rule operand => tokens;
end parser;

//
// Expressions
//

define parser expressions ()
   label "expression";
   rule seq(expression, opt-many(seq(lex-COMMA, expression))) => tokens;
end parser;

define parser expression ()
   rule seq(binary-operand, opt-many(seq(lex-BINARY-OPERATOR, binary-operand))) => tokens;
end parser;

define parser expression-no-symbol ()
   rule seq(binary-operand-no-symbol, opt-seq(lex-BINARY-OPERATOR, expression)) => tokens;
end parser;

define parser binary-operand-no-symbol ()
   rule seq(opt(lex-UNARY-OPERATOR), operand) => tokens;
end parser;

define parser binary-operand ()
   rule choice(lex-SYMBOL, seq(opt(lex-UNARY-OPERATOR), operand)) => tokens;
end parser;

define parser operand ()
   rule seq(leaf,
            opt-many(choice(seq(lex-LF-PAREN, opt(arguments), lex-RT-PAREN),
                            seq(lex-LF-BRACK, opt(arguments), lex-RT-BRACK),
                            seq(lex-PERIOD, variable-name)))) => tokens;
end parser;

define parser function-macro-call ()
   rule seq(lex-FUNCTION-WORD, lex-LF-PAREN, opt(body-fragment), lex-RT-PAREN) => tokens;
end parser;

define parser leaf ()
   rule choice(literal,
               statement,
               function-macro-call,
               variable-name,
               seq(lex-LF-PAREN, expression, lex-RT-PAREN)) => tokens;
end parser;

define parser arguments ()
   rule seq(argument, opt-many(seq(lex-COMMA, argument))) => tokens;
end parser;

define parser argument ()
   rule choice(seq(lex-SYMBOL, opt(expression)),
               expression-no-symbol) => tokens;
end parser;

define parser literal ()
   rule choice(lex-EMPTY-LIST, lex-NUMBER, lex-CHARACTER-LITERAL, string-literal,
               lex-TRUE, lex-FALSE, 
               seq(lex-LF-LIST, constants, lex-PERIOD, constant, lex-RT-PAREN),
               seq(lex-LF-LIST, opt(constants), lex-RT-PAREN),
               seq(lex-LF-VECTOR, opt(constants), lex-RT-BRACK)) => tokens;
end parser;

define parser string-literal ()
   rule many(lex-STRING) => tokens;
end parser;

define parser constants ()
   rule seq(constant, opt-many(seq(lex-COMMA, constant))) => tokens;
end parser;

define parser constant ()
   rule choice(literal, lex-SYMBOL) => tokens;
end parser;

//
// Statements
//

define parser statement ()
   label "statement";
   rule seq(lex-BEGIN-WORD, opt(body-fragment), end-clause) => tokens;
end parser;

define parser end-clause ()
   label "end clause";
   rule seq(lex-END, opt(lex-BEGIN-WORD)) => tokens;
end parser;

define parser case-body ()
   rule seq(cases, opt(lex-SEMICOLON)) => tokens;
end parser;

define parser cases ()
   rule seq(case-label,
            opt(constituents),
            opt-many(seq(lex-SEMICOLON, case-label, opt(constituents)))) => tokens;
end parser;

define parser case-label ()
   rule choice(seq(expressions, lex-ARROW),
               seq(lex-LF-PAREN, expression, lex-COMMA, expressions, lex-RT-PAREN,
                   lex-ARROW),
               seq(lex-OTHERWISE, opt(lex-ARROW))) => tokens;
end parser;

//
// Methods
//

define parser method-definition ()
   rule seq(variable-name, parameter-list, opt(body),
            lex-END, opt(lex-METHOD), opt(variable-name)) => tokens;
end parser;

define parser parameter-list ()
   rule seq(lex-LF-PAREN, opt(parameters), lex-RT-PAREN,
            choice(seq(lex-ARROW,
                       lex-LF-PAREN, opt(values-list), lex-RT-PAREN, opt(lex-SEMICOLON)),
                   seq(lex-ARROW,
                       variable, lex-SEMICOLON)),
                   opt(lex-SEMICOLON)) => tokens;
end parser;

define parser parameters ()
   rule choice(next-rest-key-parameter-list,
               seq(required-parameters,
                   opt-seq(lex-COMMA, next-rest-key-parameter-list))) => tokens;
end parser;

define parser next-rest-key-parameter-list ()
   rule choice(seq(lex-NEXT, variable-name,
                   opt-seq(lex-COMMA, rest-key-parameter-list)),
               rest-key-parameter-list) => tokens;
end parser;

define parser rest-key-parameter-list ()
   rule choice(seq(lex-REST, variable-name,
                   opt-seq(lex-COMMA, key-parameter-list)),
               key-parameter-list) => tokens;
end parser;

define parser key-parameter-list ()
   rule seq(lex-KEY, opt(keyword-parameters), opt-seq(lex-COMMA, lex-ALL-KEYS)) => tokens;
end parser;

define parser required-parameters ()
   rule seq(required-parameter, opt-many(seq(lex-COMMA, required-parameter))) => tokens;
end parser;

define parser required-parameter ()
   rule choice(seq(variable-name, lex-IDENTICAL, expression),
               variable) => tokens;
end parser;

define parser keyword-parameters ()
   rule seq(keyword-parameter, opt-many(seq(lex-COMMA, keyword-parameter))) => tokens;
end parser;

define parser keyword-parameter ()
   rule seq(opt(lex-SYMBOL), variable, opt(default)) => tokens;
end parser;

define parser default ()
   rule seq(lex-EQUAL, expression) => tokens;
end parser;

define parser values-list ()
   rule choice(seq(variables, opt-seq(lex-COMMA, lex-REST, variable)),
               seq(lex-REST, variable)) => tokens;
end parser;

//
// Macro Definitions
//

define parser macro-definition ()
   rule seq(lex-MACRO-NAME, main-rule-set, opt(aux-rule-sets),
            lex-END, opt(lex-MACRO), opt-seq(lex-MACRO-NAME)) => tokens;
end parser;

define parser main-rule-set ()
   rule choice(many(body-style-definition-rule),
               many(list-style-definition-rule),
               many(statement-rule),
               many(function-rule)) => tokens;
end parser;

define parser body-style-definition-rule ()
   rule seq(lex-LF-BRACE, lex-DEFINE, opt(definition-head), lex-MACRO-NAME,
            opt(pattern), opt(lex-SEMICOLON), lex-END, lex-RT-BRACE,
            lex-ARROW, rhs) => tokens;
end parser;

define parser list-style-definition-rule ()
   rule seq(lex-LF-BRACE, lex-DEFINE, opt(definition-head), lex-MACRO-NAME,
            opt(pattern), lex-RT-BRACE, lex-ARROW, rhs) => tokens;
end parser;

define parser rhs ()
   rule seq(lex-LF-BRACE, opt(template), lex-RT-BRACE, opt(lex-SEMICOLON)) => tokens;
end parser;

define parser definition-head ()
  rule many(choice(modifier, pattern-variable)) => tokens;
end parser;

define parser statement-rule ()
   rule seq(lex-LF-BRACE, lex-MACRO-NAME, opt(pattern), opt(lex-SEMICOLON),
            lex-END, lex-RT-BRACE, lex-ARROW, rhs) => tokens;
end parser;

define parser function-rule ()
   rule seq(lex-LF-BRACE, lex-MACRO-NAME, lex-LF-PAREN, opt(pattern), lex-RT-PAREN,
            lex-RT-BRACE, lex-ARROW, rhs) => tokens;
end parser;

//
// Patterns
//

define parser pattern ()
   rule seq(pattern-list, opt-many(seq(lex-SEMICOLON, pattern-list))) => tokens;
end parser;

define parser pattern-list ()
   rule choice(property-list-pattern,
               seq(pattern-sequence, opt-seq(lex-COMMA, pattern-list))) => tokens;
end parser;

define parser pattern-sequence ()
   rule many(simple-pattern) => tokens;
end parser;

define parser simple-pattern ()
   rule choice(binding-pattern, pattern-variable, lex-NAME-NOT-END, lex-ARROW,
               bracketed-pattern) => tokens;
end parser;

define parser bracketed-pattern ()
   rule choice(seq(lex-LF-PAREN, opt(pattern), lex-RT-PAREN),
               seq(lex-LF-BRACK, opt(pattern), lex-RT-BRACK),
               seq(lex-LF-BRACE, opt(pattern), lex-RT-BRACE)) => tokens;
end parser;

define parser binding-pattern ()
   rule choice(seq(pattern-variable, lex-DOUBLE-COLON, pattern-variable,
                   opt-seq(lex-EQUAL, pattern-variable)),
               seq(pattern-variable, lex-EQUAL, pattern-variable)) => tokens;
end parser;

define parser pattern-variable ()
   rule choice(seq(lex-QUESTION, lex-CONSTRAINED-NAME),
               seq(lex-QUESTION, lex-NAME),
               lex-ELLIPSIS) => tokens;
end parser;

define parser property-list-pattern ()
   rule choice(seq(lex-REST, pattern-variable,
                   opt-seq(lex-COMMA, lex-KEY, opt(pattern-keywords))),
               seq(lex-KEY, opt(pattern-keywords))) => tokens;
end parser;

define parser pattern-keywords ()
   rule choice(lex-ALL-KEYS,
               seq(pattern-keyword, opt-seq(lex-COMMA, pattern-keywords))) => tokens;
end parser;

define parser pattern-keyword ()
   rule seq(choice(lex-QUESTION, lex-DOUBLE-QUESTION),
            choice(lex-NAME, lex-CONSTRAINED-NAME),
            opt(default)) => tokens;
end parser;

//
// Templates
//

define parser template ()
   rule many(template-element) => tokens;
end parser;

define parser template-element ()
   rule choice(substitution, lex-PERIOD, lex-DOUBLE-COLON, lex-ARROW, lex-SYMBOL,
               lex-NAME, lex-NUMBER, lex-CHARACTER-LITERAL, lex-STRING, separator,
               lex-UNARY-OPERATOR, lex-POUND-WORD,
               seq(lex-LF-PAREN, opt(template), lex-RT-PAREN),
               seq(lex-LF-BRACK, opt(template), lex-RT-BRACK),
               seq(lex-LF-BRACE, opt(template), lex-RT-BRACE),
               seq(lex-LF-LIST, opt(template), lex-RT-PAREN),
               seq(lex-LF-VECTOR, opt(template), lex-RT-BRACK)) => tokens;
end parser;

define parser separator ()
   rule choice(lex-SEMICOLON, lex-COMMA, lex-BINARY-OPERATOR) => tokens;
end parser;

define parser substitution ()
   rule choice(seq(opt(name-prefix), lex-QUESTION, name-string-or-symbol, opt(name-suffix)),
               seq(lex-DOUBLE-QUESTION, lex-NAME, opt(separator), lex-ELLIPSIS),
               lex-ELLIPSIS,
               seq(lex-QUESTION-EQUAL, lex-NAME)) => tokens;
end parser;

define parser name-prefix ()
   rule seq(lex-STRING, lex-DOUBLE-POUND) => tokens;
end parser;

define parser name-suffix ()
   rule seq(lex-DOUBLE-POUND, lex-STRING) => tokens;
end parser;

define parser name-string-or-symbol ()
   rule choice(lex-NAME, lex-STRING, lex-SYMBOL) => tokens;
end parser;

//
// Auxiliary Rule Sets
//

define parser aux-rule-sets ()
   rule many(aux-rule-set) => tokens;
end parser;

define parser aux-rule-set ()
   rule seq(lex-SYMBOL, aux-rules) => tokens;
end parser;

define parser aux-rules ()
   rule many(aux-rule) => tokens;
end parser;

define parser aux-rule ()
   rule seq(lex-LF-BRACE, opt(pattern), lex-RT-BRACE, lex-ARROW, rhs) => tokens;
end parser;
