#!/bin/sh

cd `dirname $0`
sbcl --load grovel-tests.lisp --eval '(sb-ext:quit :unix-status (test-asdf-dependency-grovel:test-result))'
STATUS=$?
rm -rf asdf-dependency-grovel-tmp-* groveled-components.lisp
exit $STATUS