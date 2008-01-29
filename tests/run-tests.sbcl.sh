#!/bin/sh

SBCL="${1:-sbcl}"

cd `dirname $0`
$SBCL --load `pwd`/grovel-tests.lisp --eval '(sb-ext:quit :unix-status (asdf-dependency-grovel-tester:test-result))'
STATUS=$?
rm -rf asdf-dependency-grovel-tmp-*
#$SBCL --load `pwd`/grovel-tests.lisp --eval '(asdf-dependency-grovel-tester:check-base-deps)' --eval '(sb-ext:quit)'
#rm -rf asdf-dependency-grovel-tmp-* groveled-components.lisp
exit $STATUS
