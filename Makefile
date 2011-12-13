MAKEFILE_DIR = $(lastword $(dir $(MAKEFILE_LIST)))
BASE_LIBDIR = $(abspath $(MAKEFILE_DIR)/..)

src = *.dylan

libs =	-L$(BASE_LIBDIR)/dynamic-binding

peg-parser: peg-parser.lid $(src)
	d2c $(libs) peg-parser.lid

clean:
	-rm -f *.o *.s *.a *.c *.mak *.lib.du *~
	-rm -rf .libs
