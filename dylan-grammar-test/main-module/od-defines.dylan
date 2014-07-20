module: main

define method make-file-stream (name :: <string>) => (stream :: <stream>)
   make(<file-stream>, locator: name);
end function;