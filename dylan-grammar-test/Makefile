test: gd-test.lid *.dylan main-module/*.dylan grammar-module/*.dylan
	d2c -L.. -L../../../files-and-io/wrapper-streams -L../../dynamic-binding gd-test.lid

clean:
	-rm -f *.o *.s *.a *.c *.mak *~
	-rm -rf .libs
