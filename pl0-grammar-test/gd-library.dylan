module: dylan-user

define library PL0-grammar-test
   use common-dylan;
   use io;
   use system;
   use string-extensions;
   use wrapper-streams, import: {basic-wrapper-stream};
   use peg-parser;
end library;
