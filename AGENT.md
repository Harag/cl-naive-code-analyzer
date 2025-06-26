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

## Depth of analysis

1. If you are digging through code to find an issue and you pick up similar issues to the one that you are looking for mention them.
2. If you have a fix for an issue and you found similiar issues in the code suggest fixing those as well.

