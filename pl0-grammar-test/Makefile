BASE_LIBDIR = ../../..

LIBRARIES = \
	$(BASE_LIBDIR)/utilities/peg-parser \
	$(BASE_LIBDIR)/utilities/dynamic-binding \
	$(BASE_LIBDIR)/files-and-io/wrapper-streams

libflags = $(addprefix -L,$(LIBRARIES))

test: gd-library.lid *.dylan main-module/*.dylan grammar-module/*.dylan
	d2c -g $(libflags) gd-library.lid

clean:
	-rm -f *.o *.s *.a *.c *.mak *.el *.lib.du pl0-grammar-test *~
	-rm -rf .libs
