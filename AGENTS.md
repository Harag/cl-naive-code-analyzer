# cl-naive-code-analyzer

## Environment

This section outlines the development environment setup, primarily for
AI agents like Jules.

### Jules

#### Project Source Path

The primary source code for this project within the agent's
environment is located at:

`/app/`

This path should be used when commands or configurations require a
reference to the project's root directory (e.g., in ASDF central
registry pushes or when specifying paths for analysis tools).

#### Initialization Script

The following script is executed to set up the necessary Common Lisp
environment. It includes:
- Installation of SBCL (Steel Bank Common Lisp), a high-performance
  Common Lisp implementation.
- Installation of Quicklisp, a library manager for Common Lisp.
- Configuration of Quicklisp to find local project dependencies.
- Cloning of essential local dependencies from GitLab.

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

Crafting a good plan is crucial for efficient collaboration. Please
provide detailed, step-by-step plans.

**Key Characteristics of a Good Plan:**

1.  **Clear Steps:** Break down the task into logical, numbered steps.
2.  **Sufficient Detail:** For each step, explain *what* needs to be
    done and *how*. Mention specific files, functions, or tools if
    applicable.
3.  **Assumptions:** If your plan relies on certain assumptions, state them.
4.  **Verification:** Include steps for testing or verifying the changes.
5.  **Tool Usage:** Briefly mention which tools you anticipate using for each step (e.g., `read_files`, `replace_with_git_merge_diff`, `run_in_bash_session`).

**Example of Desired Detail Level (General):**

Instead of: "Fix the bug in `utils.lisp`."

Prefer:
```
1. *Analyze the bug report and reproduce the issue.*
    - Read the bug description in issue #123.
    - Examine `utils.lisp` using `read_files` to understand the `calculate-score` function.
    - If necessary, use the "Exploration" command to run parts of the function with sample inputs.
2. *Identify the root cause.*
    - Based on the analysis, pinpoint the exact line(s) causing the incorrect score calculation.
3. *Implement the fix in `utils.lisp`.*
    - Use `replace_with_git_merge_diff` to modify the `calculate-score` function.
    - The fix will involve correcting the off-by-one error in the loop.
4. *Add a new test case in `tests/test-utils.lisp`.*
    - Use `replace_with_git_merge_diff` or `create_file_with_block` (if the test file needs significant additions).
    - The new test should specifically cover the scenario that caused bug #123.
5. *Run all tests.*
    - Use the "TEST COMMAND" provided in this document, adapting it for the `cl-naive-utils` project.
    - Ensure all tests, including the new one, pass.
6. *Submit the changes.*
    - Use `submit` with a clear commit message like "Fix: Correct off-by-one error in calculate-score."
```

**Regarding the previous example plan provided (for `analyze-cst`):**
That plan was an excellent example of the level of detail
required. The key is to break down the problem into manageable parts
and explain the approach for each, including how to verify the
solution. Please continue to provide plans with that level of
thoroughness.

## Project 

### Name

cl-naive-code-analyzer

### NAIVE Philosophy

https://gitlab.com/naive-x/_naive_/-/blob/main/readme.org

If you cannot access this let me know!

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
   because that will only confuse us and the tests. The `raw-body` slot
   should generally be treated as read-only after initial parsing,
   as modifications to it might not be reflected in the CST or other
   analysis artifacts.

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

This is a command template to run the tests for the project.
- The command starts SBCL with Quicklisp loaded.
- Then it tells Quicklisp and ASDF (Another System Definition
  Facility) where to find local dependencies.
- Then it loads the test system for the project (e.g., `cl-naive-code-analyzer.tests`).
- Then it executes the tests using `cl-naive-tests:run`.
- Finally, it reports the test results using `cl-naive-tests:report`.

**Note on `[project source path]`:** This placeholder refers to the
root directory of the project being tested. In the context of the
agent's environment, this is `/app/` (see "Environment" section). If
testing a different project (e.g., one of the dependencies), adjust
this path. For example, for `cl-naive-store`, it might be
`~/dependencies-source/cl-naive-store/`.  The
`asdf:*central-registry*` push should be a pathname designator, so
ensure it's like `#P"/app/"` or
`#P"~/dependencies-source/cl-naive-store/"`.

```
sbcl --noinform --no-userinit --non-interactive \
		--eval '(load #P"~/quicklisp/setup.lisp")' \
		--eval '(push "~/dependencies-source/" ql:*local-project-directories*)' \
		--eval '(push "[project source path]" ql:*local-project-directories*)' \
		--eval '(push #P"[project source path]" asdf:*central-registry*)' \
		--eval '(ql:quickload :cl-naive-code-analyzer.tests)' \
		--eval '(in-package :cl-naive-code-analyzer.tests)' \
		--eval '(cl-naive-tests:run)' \
 		--eval '(cl-naive-tests:report)'
```

### Test Code

1. Testing code should use `::` for symbols not exported (internal
   symbols). We avoid exporting symbols purely for testing purposes to
   maintain a clean public API.

2. Testing code written should be run from within the project's test
   package (e.g., `cl-naive-code-analyzer.tests`). This ensures that
   comparisons of print output have consistent package prefixes. The
   test command template handles this with the `(in-package ...)`
   form.

## Exploration (REPL Approximation)

Common Lisp development heavily relies on the interactive
Read-Eval-Print Loop (REPL). While direct REPL access isn't available
to the agent, we can approximate it by executing individual Lisp
S-expressions (SEXP) using SBCL from the command line.

This is useful for:
- Testing small code snippets.
- Inspecting the state of variables or data structures.
- Understanding the behavior of specific functions without a full test
  cycle.

Use the following command template:

```
sbcl --noinform --no-userinit --non-interactive \
		--eval '(load #P"~/quicklisp/setup.lisp")' \
		--eval '(push "~/dependencies-source/" ql:*local-project-directories*)' \
		--eval '(push "[project source path]" ql:*local-project-directories*)' \
		--eval '(push #P"[project source path]" asdf:*central-registry*)' \
		--eval '(ql:quickload :[system-to-load])' \ ;; e.g., :cl-naive-code-analyzer
		--eval '(in-package :[package-to-use])' \ ;; e.g., :cl-naive-code-analyzer
		--eval ''[code to run goes here. Ensure it is a single string argument, properly quoted. Example: (format t "Hello: ~S~%" (my-function 1 2))]''
```

**Key points for the exploration command:**
- **`[project source path]`**: Typically `/app/` for the main project,
  or a path under `~/dependencies-source/` for dependencies. Ensure
  the ASDF path is a pathname designator like `#P"/app/"`.
- **`:[system-to-load]`**: The ASDF system you need to load (e.g.,
  `:cl-naive-code-analyzer`, `:cl-naive-store`).
- **`:[package-to-use]`**: The package your code snippet should run in
  (e.g., `:cl-naive-code-analyzer`, `:cl-user`).
- **`'[code to run goes here...]'`**:
    - This entire SEXP must be passed as a *single string argument* to
      the `--eval` option.
    - Use `(format t "Info: ~S~%" [thing-to-print])` or similar to
      output results, as standard REPL output is not captured.
    - Ensure proper Lisp syntax, including matching parentheses. The
      `)]))))` in the original template was a placeholder; your actual
      code will determine the necessary parentheses.
    - For multi-line code blocks passed as a single string, you don't
      need shell line continuation characters (`\`).
    - Remember to escape characters within the Lisp string as needed
      for the shell, e.g., `\"` for double quotes.

**Example:**

```
sbcl --noinform --no-userinit --non-interactive \
	--eval '(load #P"~/quicklisp/setup.lisp")' \
	--eval '(push "~/dependencies-source/" ql:*local-project-directories*)' \
	--eval '(push "/app/" ql:*local-project-directories*)' \
	--eval '(push #P"/app/" asdf:*central-registry*)' \
	--eval '(ql:quickload :cl-naive-code-analyzer)' \
	--eval '(in-package :cl-naive-code-analyzer)' \
	--eval '(let ((cst (cl-naive-code-analyzer::analyze-string "(defun simple (a &key (b (+ 1 1))) \\"A simple function with no arguments and a docstring.\\" (list 1 2 3))"))) (format t "CST Info: ~S~%" (cl-naive-code-analyzer::walk-cst-with-context cst (lambda (current-cst path tail) (declare (ignore path tail)) (format nil "~S" current-cst)))))'
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

3. Do not put any comments before a ) even if its on a seperate line
   like below. My emacs configuration rolls up dangling ), which means
   if there is a preceding comment the ) becomes commented out.

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

4. When defining class slots use constistent order of options ([slot
   name] :initarg [initarg] :accessor [accessor] :initform [initform]
   :documentation "[documentation string]")

5. Format class slots as follows for readability. Each slot option
   should be on a new line, indented.

   **Example Class Definition:**
   ```common-lisp
   (defclass my-class ()
     ((first-slot :initarg :first-slot
                  :accessor first-slot
                  :initform nil
                  :documentation "This is the first slot.")
      (second-slot :initarg :second-slot
                   :accessor second-slot
                   :initform (list 1 2 3)
                   :documentation "This is the second slot, initialized with a list.")))
   ```

6. Do not block format code with excessive whitespace padding for
   alignment, e.g., avoid `(name (concrete-syntax-tree:raw name-cst))`
   if `(name (concrete-syntax-tree:raw name-cst))` is clear. The
   primary goal is readability and consistency, not strict column
   alignment if it makes diffs harder to read.


## Documentation and Examples

Good documentation and illustrative examples are highly valued. Please
ensure your contributions in this area align with the practices found
in other NAIVE projects (located in `~/dependencies-source`).

Key characteristics to aim for:
- **Docstrings:** All public functions, macros, classes, and variables
  should have clear and concise docstrings. Explain *what* the code
  does, its parameters, and what it returns. For complex functions,
  also explain the *why* or the algorithm if it's non-obvious.
- **Runnable Examples:** Where appropriate, include small, runnable
  examples within dedicated example files.
- **READMEs:** For larger features or modules, consider if updates to
  the project's README or other markdown documentation files are
  necessary.
- **Clarity and Conciseness:** Strive for clarity and avoid jargon
  where simpler terms suffice.

For instance, refer to the `cl-naive-store` project's `store.lisp`
file for examples of detailed docstrings and class definitions, or its
README for project-level documentation. If unsure, ask for a specific
example to emulate.

## Version Control

### Branching

- **Feature Branches:** Create branches for new features, preferably
  prefixed with `feature/` (e.g., `feature/new-parser`).
- **Bugfix Branches:** For bug fixes, use `bugfix/` (e.g.,
  `bugfix/issue-123-fix-off-by-one`).
- **Jules' Branches:** Your branches can be prefixed with `jules/`
  (e.g., `jules/refactor-utility-module`).
- Base your branches off the main development branch (e.g., `main` or
  `master`). Please ask if you are unsure which branch this is.

### Commit Messages

Please follow these guidelines for commit messages, based on Conventional Commits:
1.  **Format:** `<type>[optional scope]: <description>`
    *   `<type>`: Must be one of the following:
        *   `feat`: A new feature.
        *   `fix`: A bug fix.
        *   `docs`: Documentation only changes.
        *   `style`: Changes that do not affect the meaning of the
            code (white-space, formatting, missing semi-colons, etc).
        *   `refactor`: A code change that neither fixes a bug nor adds a feature.
        *   `perf`: A code change that improves performance.
        *   `test`: Adding missing tests or correcting existing tests.
        *   `build`: Changes that affect the build system or external
            dependencies (example scopes: gulp, broccoli, npm).
        *   `ci`: Changes to our CI configuration files and scripts
            (example scopes: Travis, Circle, BrowserStack, SauceLabs).
        *   `chore`: Other changes that don't modify src or test files.
    *   `[optional scope]`: A noun describing the section of the
        codebase affected (e.g., `parser`, `analyzer`, `tests`).
    *   `<description>`: Concise description of the change in present
        tense. Not capitalized. No period at the end.
2.  **Subject Line Length:** Keep the subject line to 50 characters or
    less if possible, but clarity is more important than strict
    length.
3.  **Body (Optional but Recommended):**
    *   A blank line separates the subject from the body.
    *   Explain the *what* and *why* of the change, not just the *how*
        (the code shows how).
    *   Wrap lines at 72 characters.
    *   Reference issues if applicable (e.g., `Closes #123`, `Fixes #456`).
    *   `BREAKING CHANGE`: If the commit introduces a breaking API
        change, start the body with `BREAKING CHANGE:` followed by a
        description of the change, justification, and migration notes.

**Example Commit Message (Simple):**
```
feat(parser): add support for new syntax element
```

**Example Commit Message (Detailed):**
```
fix(analyzer): correct off-by-one error in token processing

The previous logic for iterating over tokens could miss the last
token in certain edge cases. This commit adjusts the loop
boundary to ensure all tokens are processed.

Closes #789
```

**Example Commit Message (Breaking Change):**
```
refactor(api): rename process-data to handle-input-stream

BREAKING CHANGE: The function `process-data` has been renamed to
`handle-input-stream` to better reflect its purpose. Additionally,
the parameter order has changed from `(data options)` to
`(options data-stream)`.

To migrate, update all calls to the old function name and adjust
parameter order.
```

## Interaction and Error Handling

This section provides general guidelines for interaction and error
handling during development. Specific tool examples may be provided
for AI agents like Jules.

### Communication
- **Clarifications:** If a request is ambiguous or if you're unsure
  about the best approach, please ask for clarification before
  proceeding with extensive work. For AI agents, this might involve
  using a tool like `request_user_input`.
- **Updates:** Provide brief updates when you complete significant
  parts of a plan or if you encounter an unexpected blocker. For AI
  agents, tools like `message_user` or `plan_step_complete` can be
  used for this.
- **Suggestions:** Feel free to suggest alternative approaches or
  improvements if you see opportunities. Articulate these clearly.

### Error Handling (in Lisp Code)
- Strive to write robust code that anticipates potential issues.
- Use Common Lisp's condition system for signaling errors and warnings
  where appropriate.
- For this project, if you encounter a situation where user input is
  invalid or an operation cannot proceed as expected, it's generally
  preferred to:
    1. Signal a continuable error (`cerror`) if the situation might be
       resolvable by a handler.
    2. Signal a specific custom condition (derived from `error` or
       `warning`) if one is defined or appropriate for that type of
       issue.
    3. Signal a standard error (e.g., `error 'type-error
       :expected-type 'integer :datum some-value)`) if applicable.
- Avoid silent failures or returning `nil` ambiguously where an error
  condition is more appropriate.
- If unsure about the specific error handling strategy for a new piece
  of code, please ask.

### Tool or Environment Errors
- If a tool command (e.g., `run_in_bash_session`,
  `replace_with_git_merge_diff`) fails, please report the full error
  message and the command you attempted.
- If you suspect an environment issue (e.g., a dependency not
  loading), describe the symptoms clearly and any steps you've taken
  to diagnose it.
- Before reporting, if it's a simple issue like a typo in a command
  you constructed, please try to correct it first. If an error
  persists, provide the original command, the corrected one, and the
  resulting error.
