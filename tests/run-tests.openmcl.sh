#!/bin/sh

LISP="${1:-openmcl}"

cd `dirname $0`
$LISP --load `pwd`/grovel-tests.lisp --eval '(ccl:quit (asdf-dependency-grovel-tester:test-result))'
STATUS=$?
rm -rf asdf-dependency-grovel-tmp-*
#$LISP --load `pwd`/grovel-tests.lisp --eval '(asdf-dependency-grovel-tester:check-base-deps)' --eval '(ccl:quit)'
rm -rf asdf-dependency-grovel-tmp-* groveled-components.lisp
exit $STATUS
