all: test documentation
.PHONY:: test tests documentation

# default values:
ARTDIR = tests/artifacts/
DEPENDENCYDIR = $(abspath $(CURDIR)/..)/
THISDIR = $(abspath $(CURDIR))/

test tests:
	sbcl --noinform --no-userinit --non-interactive \
		--eval '(load #P"~/quicklisp/setup.lisp")' \
		--eval '(push "$(DEPENDENCYDIR)" ql:*local-project-directories*)' \
		--eval '(push #P"$(THISDIR)" asdf:*central-registry*)' \
		--eval '(ql:quickload :cl-naive-queue.tests)' \
		--eval '(cl-naive-tests:run)' \
		--eval '(cl-naive-tests:run)' \
		--eval '(cl-naive-tests:write-results cl-naive-tests:*suites-results* :format :text)' \
		--eval '(cl-naive-tests:save-results cl-naive-tests:*suites-results* :file "$(ARTDIR)junit-results.xml" :format :junit)' \
		--eval '(sb-ext:exit :code (if (cl-naive-tests:report) 0 200))'
documentation:
	make -C docs pdfs
