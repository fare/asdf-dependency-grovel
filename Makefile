tests:
	./tests/run-tests.sbcl.sh tests

clean:
	rm -f *.fasl

.PHONY: tests clean

# End of Makefile
