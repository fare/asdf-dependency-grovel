#!/bin/sh
## Works with either SBCL or CCL

LISP="${1:-sbcl}"

ADG_TEST_DIR="$(dirname $0)"

$LISP --load ${ADG_TEST_DIR}/grovel-tests.lisp --eval '(uiop:quit (asdf-dependency-grovel-tester:test-result))'
STATUS=$?
rm -rf asdf-dependency-grovel-tmp-*
#$LISP --load ${ADG_TEST_DIR}/grovel-tests.lisp --eval '(asdf-dependency-grovel-tester:check-base-deps)' --eval '(uiop:quit)'
#rm -rf asdf-dependency-grovel-tmp-* groveled-components.lisp
exit $STATUS
