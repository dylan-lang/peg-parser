peg-parser: peg-parser.lid library.dylan \
            parser-definers.dylan parser-rules.dylan parser-support.dylan
	d2c peg-parser.lid

clean:
	-rm -f *.o *.s *.a *.c *.mak *~
	-rm -rf .libs
