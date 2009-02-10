module: main

// Gwydion Dylan's built-in read() doesn't return eos when it hits eos, so use
// correctly-behaving wrapper stream instead.

define class <positionable-basic-wrapper-stream>
   (<basic-wrapper-stream>, <positionable-stream>)
end class;

define function make-file-stream (name :: <string>) => (stream :: <stream>)
   let fs = make(<file-stream>, locator: name);
   make(<positionable-basic-wrapper-stream>, inner-stream: fs);
end function;
