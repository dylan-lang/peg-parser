module: main

define function make-file-stream (name :: <string>) => (stream :: <stream>)
   make(<file-stream>, locator: name);
end function;