tests:
	./tests/run-tests.sbcl.sh

clean:
	rm -f *.fasl */*.fasl

.PHONY: tests clean

# End of Makefile
