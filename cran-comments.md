## Test environments

* Windows 10 x64, R 4.1.1
* Mac x64, R 4.0.3

## R CMD check results

> checking installed package size ... NOTE
    installed size is  6.4Mb
    sub-directories of 1Mb or more:
      R         1.7Mb
      extdata   4.3Mb

0 errors v | 0 warnings v | 1 note x

These files are necessary and have already been reduced in size.

## Downstream dependencies

There is one downstream dependency, `fMRIscrub`. It continues to pass all checks.
## Tests

Passes all the tests in `tests/run_ciftiTools_tests.R`