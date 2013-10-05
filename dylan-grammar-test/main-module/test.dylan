module: main

define method main(name, arguments)
   let stream = make-file-stream(arguments.first);
   block ()
      let context = make(<parse-context>, stream: stream);
      // *parser-trace* := *standard-output*;
      // *parser-cache-hits* := #t;

      let run-time = current-date();
      let (res, succ?, f) = parse-source-file(stream, context);
      run-time := current-date() - run-time;

      let (d, h, m, s) = decode-duration(run-time);
      if (succ?)
         format-out("Success, %d seconds\n", s);
      else
         format-out("Failure\n");
      end if;
      if (f)
         format-out("Parsed to %x, expected %s\n", f.parse-position, f.parse-expected);
      end if;
      
      for (e keyed-by k in context.parser-cache-hits)
         format-out("%5d %s\n", e, k);
      end for;
   cleanup
      stream.close;
   end block;
end function main;

// Invoke our main() function.
main(application-name(), application-arguments());
