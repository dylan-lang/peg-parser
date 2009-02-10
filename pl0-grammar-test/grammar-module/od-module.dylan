module: dylan-user

define module PL0-grammar
   // from common-dylan
   use common-dylan, exclude: {format-to-string};
   // from io
   use streams;
   use format, import: {format, format-to-string};
   use format-out;
   use print, import: {print-object, print};
   use pprint, import: {printing-logical-block, pprint-newline};
   // from strings
   use strings;
   // from peg-parser
   use peg-parser-client, export: all;
   use peg-parser;

   export parse-program;

   export
      <lex-EQUAL-token>,    
      <lex-GT-token>,         
      <lex-GTE-token>,        
      <lex-HASH-token>,       
      <lex-IDENT-token>,
      <lex-LT-token>,         
      <lex-LTE-token>,        
      <lex-MINUS-token>,      
      <lex-NUMBER-token>,
      <lex-PLUS-token>,       
      <lex-SLASH-token>,      
      <lex-STAR-token>; 

   export
      <assignment-token>,
      <begin-end-token>,
      <block-token>,
      <call-token>,
      <comparison-token>,
      <const-block-token>,
      <const-defn-token>,
      <expression-token>,
      <if-then-token>,
      <oddity-token>,
      <procedure-block-token>,
      <term-token>,
      <var-block-token>,
      <while-do-token>;
   
   export
      text;
end module;
