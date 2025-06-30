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
		--eval '(format t "~S~%" (cl-naive-tests:run))' 	
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
