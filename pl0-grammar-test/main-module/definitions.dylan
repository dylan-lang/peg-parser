module: main

define function make-file-stream (name :: <string>) => (stream :: <stream>)
   make(<file-stream>, locator: name)
end function;

define method as (class == <string>, char :: <character>) => (string :: <string>)
   make(<string>, fill: char, size: 1)
end method;
