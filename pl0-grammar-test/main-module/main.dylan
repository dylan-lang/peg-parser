module: main

define function main(name, arguments)
   let stream = make-file-stream(arguments.first);
   block ()
      *parser-trace* := *standard-output*;
      *parser-cache-hits* := #f;

      let context = make(<parse-context>, cache-stream: stream);
      let run-time = current-date();
      let (result, success?, failure) = parse-program(stream, context);
      run-time := current-date() - run-time;
      let (d, h, m, sec) = decode-duration(run-time);

      if (success?)
         format-out("Success, %d seconds\n", sec);
      else
         format-out("Failure\n");
      end if;
      
      if (failure)
         format-out("Parsed to %x, expected %s\n",
                    failure.failure-position, failure.parse-expected);
      end if;
      
      for (count keyed-by parser in context.parser-cache-hits)
         format-out("%5d %s\n", count, parser);
      end for;
   cleanup
      stream.close;
   end block;
end function main;

// Invoke our main() function.
main(application-name(), application-arguments());
