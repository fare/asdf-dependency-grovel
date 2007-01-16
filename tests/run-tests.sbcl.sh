#!/bin/sh

cd `dirname $0`
sbcl --load grovel-tests.lisp --eval '(sb-ext:quit :unix-status (asdf-dependency-grovel-tester:test-result))'
STATUS=$?
rm -rf asdf-dependency-grovel-tmp-* groveled-components.lisp
exit $STATUS
