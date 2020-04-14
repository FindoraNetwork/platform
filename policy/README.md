# Policy compiler

To build, you'll need Haskell Stack
https://docs.haskellstack.org/en/stable/README/

`./run.sh` will build and run the compiler.

Currently, the compiler takes the file as input on stdin, eg:

    $ ./run.sh <policy_examples/debt_policy_borrower_issues.fl

It will output a lot of debug info along with the compilation result.
The last section of output, headed with "Final result:", is the
completed translation.

