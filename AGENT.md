# cl-naive-code-analyzer

## Summary
cl-naive-code-analyzer is a Common Lisp library designed to statically
analyze one or more Lisp projects by extracting function and macro
definitions, call relationships, lambda lists, and variable usage
directly from source code. It performs structural parsing using
Eclector and stores analysis results for efficient querying across
project boundaries. Its primary goals are to support AI-assisted
context discovery, dead code detection, and future inference of
architectural patterns.

Code to analyze must compile successfully and have an .asd file to
load it. The library loads the project before analysis is started to
help fill in some gaps with the purely static approach. But once the
analysis meta data is harvested you only need to load the store to
query the information.

## Comments

Instructions about how and when to make comments.

1. Do not put any comments at the end of code lines like (....) ;comment. 
2. Do not put comments when the code is clear on its own already. For example (if (not x) 1 2) ;Checks if x is nil.
3. Do not put any comments before a ) even if its on a seperate line like below. My emacs configuration rolls up dangling ), which means if there is a preceding comment the ) becomes commented out.
```
;;coment
)
```

## Depth of analysis

1. If you are digging through code to find an issue and you pick up similar issues to the one that you are looking for mention them.
2. If you have a fix for an issue and you found similiar issues in the code suggest fixing those as well.

## Tests 

1. Dont' add additional comments to :expected output. Adding comments causes the tests to fail because you are deviating from the actual output. DUH!

### TEST COMMAND

DO NOT MUTATE THE DIRECTORIES in the command, use the command as is! The command has been tested inside the initialization script and runs successfully as IS!
```
sbcl --noinform --no-userinit --non-interactive \
		--eval '(load #P"~/quicklisp/setup.lisp")' \
		--eval '(push "~/source/" ql:*local-project-directories*)' \
		--eval '(push "/app/" ql:*local-project-directories*)' \
		--eval '(push #P"/app/" asdf:*central-registry*)' \
		--eval '(ql:quickload :cl-naive-code-analyzer.tests)' \
		--eval '(in-package :cl-naive-code-analyzer.tests)' \
		--eval '(cl-naive-tests:run)' \
 		--eval '(cl-naive-tests:report)'
```
## Code Style

1. Dont put flow functions like if, when, unless on ONE LINE.
2. Use full descriptive variable names.
3. Don't excede 80 chars on a line.
4. When defining class slots use constistent order of options ([slot name] [initarg] [accessor] [iniform] [documentation])
5. Format slots like the following:

```
 ([slot name] :initarg [initarg] 
              :accessor [accessor] 
			  :initform [iniform] 
			  :documentation 
			  [documentation])
```

## Where is the code

1. Code for the project is in /app/
2. Code for the dependecy projects are in /home/jule/source/

## Design Decisions/Particulars

These are in no particular order.

1. When writing out to the store we cannot write out symbols in the
   way that the lisp reader would read them back like
   my-package:my-symbol. The reason for that is that when the store is
   read later on the package might not exist. So we "export" symbols
   to (:name "my-symbol" :package "my-package") for safe reading
   later.

2. The cst and raw-body slots on analysis class is there to help us
   write analyzers and for possible debugging. The cst's stored in
   there are concrete-syntax-tree cst's produced by eclector when
   reading. We don't try to fight the peculuaralities of the way they
   are sturcture. Each analyzer must just extract what it needs based
   on how concrete-syntac-tree cst's work. In testing code
   specifically we need to be careful that we dont test cst structure
   because that will only confuse us and the tests.

3. Testing code some times uses analyze-string which is a poor version
   analyze-project. By that I mean that analyze-string does not deal
   with packagages properly because it does not have complete context
   of the a full project.

4. Testing code should use :: for symbols not exported, we dont want
   export symbols purely for testing purposes.

5. Testing code written should be run from within the
   cl-naive-code-analyzer.tests package so that comparisons of print
   output have the same package prefixes. The test command does this
   correctly.

## When introducing new functions

1. When it is a helper/utility function check if some thing similar
   has not been implemented already!
2. Check to see how the same thing is possibly being achieved
   elsewhere in the code.
   
## When changing existing functions.

1. Check to see if something similar has not done in the project code
   before and grok how those examples actually tell you about the
   project design and conventions.
2. It is ok to want to change project design and conventions when you
   have good reasons but then you have to articulate: a. Old VS New
   b. Impact on the rest of the project code because we want to stay
   consistant.

## Exploration

To explore stuff or to test individual functions without having to go
through a whole test cycle use the following command template.


```
sbcl --noinform --no-userinit --non-interactive \
		--eval '(load #P"~/quicklisp/setup.lisp")' \
		--eval '(push "~/source/" ql:*local-project-directories*)' \
		--eval '(push "/app/" ql:*local-project-directories*)' \
		--eval '(push #P"/app/" asdf:*central-registry*)' \
		--eval '(ql:quickload :cl-naive-code-analyzer)' \
		--eval '(in-package :cl-naive-code-analyzer)' \
		--eval '[code to run goes here. )]]))))'
```

Remember you have to output information you want with something like 
```(format t "Info: ~S~%" [thing])```

Also remember if it is a large code block you are inserting you do not have to worry about using \ command continuation because the code block is inbetween '...'.

For example:

```
sbcl --noinform --no-userinit --non-interactive \
	--eval '(load #P"~/quicklisp/setup.lisp")' \
	--eval '(push "~/source/" ql:*local-project-directories*)' \
	--eval '(push "/app/" ql:*local-project-directories*)' \
	--eval '(push #P"/app/" asdf:*central-registry*)' \
	--eval '(ql:quickload :cl-naive-code-analyzer)' \
	--eval '(let ((cst (cl-naive-code-analyzer::analyze-string
            "(defun simple (a &key (b (+ 1 1)))
\"A simple function with no arguments and a docstring.\"
(list 1 2 3))")))
  (cl-naive-code-analyzer::walk-cst-with-context
   cst
   (lambda (current-cst path tail)
     (declare (ignore path tail))
     (format nil "~S~%" cst))))' 
```
	
