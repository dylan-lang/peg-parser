module: dylan-user

define module main
   // from common-dylan
   use dylan;
   use common-extensions, exclude: {format-to-string};
   // from io
   use streams;
   use format-out;
   use standard-io;
   // from system
   use file-system;
   use date;

   use PL0-grammar;
end module;
