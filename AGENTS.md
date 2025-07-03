# cl-naive-code-analyzer

## Environment

### Jules

#### Project Source Path 

/app/

#### Initialization Script

```
# Install ubuntu packages that are needed to setup enfironment.

sudo apt-get install -qy --no-install-recommends git wget bzip2

# Creating a directory to clone dependencies into. These are
# dependancies not found in quicklisp

mkdir ~/dependencies-source

cd ~/dependencies-source

# Setup the latest version of SBCL.

wget http://prdownloads.sourceforge.net/sbcl/sbcl-2.5.5-x86-64-linux-binary.tar.bz2

bzip2 -cd sbcl-2.5.5-x86-64-linux-binary.tar.bz2 | tar xvf -

cd sbcl-2.5.5-x86-64-linux

sudo sh install.sh

cd ~/

# Install quicklisp we need it to get broader non direct dependencies.

wget https://beta.quicklisp.org/quicklisp.lisp 

sbcl --noinform --non-interactive --load quicklisp.lisp --eval '(quicklisp-quickstart:install :path "~/quicklisp/")'

sbcl --noinform --non-interactive --load ~/quicklisp/setup.lisp --eval '(ql-util:without-prompting (ql:add-to-init-file))'

# We need to tell quicklisp where to find local dependencies

echo '#+quicklisp(push "~/dependencies-source" ql:*local-project-directories*)' >> ~/.sbclrc

# Clone the local dependencies.

cd ~/dependencies-source

git clone https://[token]@gitlab.com/Harag/cl-getx.git
git clone https://[token]@gitlab.com/naive-x/cl-naive-tests.git
git clone https://[token]@gitlab.com/naive-x/cl-naive-deprecation.git
git clone https://[token]@gitlab.com/naive-x/cl-naive-ptrees.git
git clone https://[token]@gitlab.com/naive-x/cl-naive-store.git
git clone https://[token]@gitlab.com/naive-x/sandbox.git

```

## Plans and/or Suggestions

Give me detail!

This is an excelent example of a detailed plan. Please just make a mental note of the level of detail but dont go and store this example plan in memory because then you are going to accidentally internalize irrellevant details contained in it which will confuse you later.

```
Here's a proposed plan:

Modify analyze-cst methods for relevant definition types (e.g., defun-analysis, defmacro-analysis, defmethod-analysis, deftype-analysis, defgeneric-analysis, defsetf-analysis long form):

After parse-lambda-list-cst is called and the lambda list details (including :default-value-cst for optionals, keys, aux, and potentially sub-parameters for destructuring) are available:
Iterate through these parameters.
For each parameter that has a default value represented by a CST, or for each component of a destructuring pattern that is itself a lambda list:
Call walk-cst-with-context (or a similar appropriate walker) on that CST, passing the main analysis object so that gather-info populates its function-calls, macro-calls, variable-uses, etc., slots with findings from within these default values/destructured parts.
Adjust the write-analysis methods for these definition types (or how lambda-info is prepared for writing):

When serializing the analysis-lambda-info slot (or the parameters slot that's derived from it) to the log file:
Ensure that the actual default value expressions (which were originally CSTs) are now converted to strings using (format nil "~S" (concrete-syntax-tree:raw THE_DEFAULT_VALUE_CST)) before being written.
This means the log will store a string representation of the default value, while the queryable details (calls, uses) from within it will have been merged into the main analysis object's collections.
Test:

Re-run the analysis: sbcl --noinform --non-interactive --load /home/jules/quicklisp/setup.lisp --eval '(push #p"./" ql:*local-project-directories*)' --eval '(ql:quickload :cl-naive-code-analyzer)' --eval '(cl-naive-code-analyzer:analyze-project "test-code" "tests/test-code/")'
Verify that the analysis completes without the malformed property list error from cl-naive-store (as the problematic forms should now be strings).
Inspect the code-definitions.log to confirm:
Default values like (* TEST-PACKAGE-SIMPLE::X 2) are now stored as strings (e.g., "(* TEST-PACKAGE-SIMPLE::X 2)").
Function calls/variable uses from within these default values (e.g., the * function call) are correctly listed in the main :function-calls property of the defun's analysis entry.
Does this plan accurately reflect the agreed-upon Path A and cover the necessary steps?
```

## Project 

### Name

cl-naive-code-analyzer

### NAIVE Philosophy

https://gitlab.com/naive-x/_naive_/-/blob/main/readme.org

### Summary

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

### Particulars

This includes design decisions and other notes to help grok the code.

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
   are sturctured. Each analyzer must just extract what it needs based
   on how concrete-syntac-tree cst's work. In testing code
   specifically we need to be careful that we dont test cst structure
   because that will only confuse us and the tests.

3. Testing code some times uses analyze-string which is a poor version
   analyze-project. Don't try and use it except for testing.




## Contributing Guidlines

### Depth

1. If you are digging through code to find an issue and you pick up
   similar issues to the one that you are looking for mention them.

2. If you have a fix for an issue and you found similiar issues in the
   code suggest fixing those as well.


### When changing existing functions.

1. Check to see if something similar has not done in the project code
   before and grok how those examples actually tell you about the
   project design and conventions.
   
2. It is ok to want to change project design and conventions when you
   have good reasons but then you have to articulate: a. Old VS New
   b. Impact on the rest of the project code because we want to stay
   consistant.
   
### When introducing new functions

1. When it is a helper/utility function check if some thing similar
   has not been implemented already!
   
2. Check to see how the same thing is possibly being achieved
   elsewhere in the code.
   
### Do Not Re-invent the Wheel

1. Use other dependancies where possible. Just make sure you are not
   hallucinating there functionality. If you are not sure ask me to
   help decide.

2. Keep track of what is going on in the project don't suggest new
   helpers that does what other helpers already do.


## Tests 

### TEST COMMAND

This is an command template to run the tests for the project.  
- The command stats SBCL with quicklisp loaded. 
- Then it tells quicklisp and the asdf system where to find dependencies.
- Then it loads the test project.
- Then it executes the tests 
- Finally it reports the test results

```
sbcl --noinform --no-userinit --non-interactive \
		--eval '(load #P"~/quicklisp/setup.lisp")' \
		--eval '(push "~/dependencies-source/" ql:*local-project-directories*)' \
		--eval '(push "[project source path]" ql:*local-project-directories*)' \
		--eval '(push #P"[project source path" asdf:*central-registry*)' \
		--eval '(ql:quickload :cl-naive-code-analyzer.tests)' \
		--eval '(in-package :cl-naive-code-analyzer.tests)' \
		--eval '(cl-naive-tests:run)' \
 		--eval '(cl-naive-tests:report)'
```

### Test Code

1. Testing code should use :: for symbols not exported, we dont want
   to export symbols purely for testing purposes.

2. Testing code written should be run from within the
   cl-naive-code-analyzer.tests package so that comparisons of print
   output have the same package prefixes. The test command template does this
   correctly.
   
## Exploration

We are working with Common Lisp, a large part of its power is the use
of the REPL.

Since the AI cannot access the REPL with out assistance the best we can do is to run individual lisp SEXP's one at the time in an approximation of a REPL.

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
		--eval '[code to run goes here. )]))))'
```

Remember you have to output information you want with something like 

```(format t "Info: ~S~%" [thing])```

Also remember if it is a large code block you are inserting you do not have to worry about using \ command continuation because the code block is in between '...'.

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
	
## Code Conventions

### KISS

Keep it short stupid = KISS

Do not build monster functions of hundreds of lines, AI diff tools
have difficulty with large blocks of code.

If you get to a hundred lines of code for a single function you need
to consider splitting it into multiple functions.

### Comments

Instructions about how and when to make comments.

1. DO NOT PUT ANY COMMENTS AT THE END OF CODE LINES LIKE (....) ;COMMENT. 

2. Do not put comments when the code is clear on its own already. For
   example ```(if (not x) 1 2)``` ;Checks if x is nil.

3. Do not put any comments before a ) even if its on a seperate line like below. My emacs configuration rolls up dangling ), which means if there is a preceding comment the ) becomes commented out.
```
;;coment
)
```

### Code Style

1. Dont put code like if, when, unless, lambda, dolist etc on ONE LINE
   like this ```(lambda (c c-path c-tail) (declare (ignore c-path
   c-tail)) (gather-info c analysis))```. Lisp code was designed to be
   read by humans, think python with brackets.
   
2. Use full descriptive variable names.

3. Don't excede 80 chars on a line.That counts for comments as well!

4. When defining class slots use constistent order of options ([slot name] [initarg] [accessor] [iniform] [documentation])

5. Format slots like the following:

```
 ([slot name] :initarg [initarg] 
              :accessor [accessor] 
			  :initform [iniform] 
			  :documentation 
			  [documentation])
```

6. Do not block format code with whitespace padding like this ``` (name         (concrete-syntax-tree:raw name-cst))```
