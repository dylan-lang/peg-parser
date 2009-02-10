module: PL0-grammar

define method print-object (o :: <lexeme>, s :: <stream>) => ()
   format(s, "{lexeme %=}", o.text);
end method;

/*
define method print-object (o :: <assignment-token>, s :: <stream>) => ()
end method;

define method print-object (o :: <begin-end-token>, s :: <stream>) => ()
end method;

define method print-object (o :: <block-token>, s :: <stream>) => ()
end method;

define method print-object (o :: <call-token>, s :: <stream>) => ()
end method;

define method print-object (o :: <comparison-token>, s :: <stream>) => ()
end method;

define method print-object (o :: <const-block-token>, s :: <stream>) => ()
end method;

define method print-object (o :: <const-defn-token>, s :: <stream>) => ()
end method;

define method print-object (o :: <expression-token>, s :: <stream>) => ()
end method;

define method print-object (o :: <if-then-token>, s :: <stream>) => ()
end method;

define method print-object (o :: <oddity-token>, s :: <stream>) => ()
end method;

define method print-object (o :: <procedure-block-token>, s :: <stream>) => ()
end method;

define method print-object (o :: <term-token>, s :: <stream>) => ()
end method;

define method print-object (o :: <var-block-token>, s :: <stream>) => ()
end method;

define method print-object (o :: <while-do-token>, s :: <stream>) => ()
end method;
*/
