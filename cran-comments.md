## Test environments

* Windows 10 x64, R 4.0.4
* Mac x64, R 4.0.3

## R CMD check results

> checking installed package size ... NOTE
    installed size is  5.0Mb
    sub-directories of 1Mb or more:
      extdata   4.3Mb

0 errors v | 0 warnings v | 1 note x

These files are necessary and have already been reduced in size.

## Downstream dependencies

None.

## Tests

Passes all the tests in `tests/run_ciftiTools_tests.R`

## Submission revision

The previous submission errored because the DESCRIPTION contained this line: `LazyData: true`. However, no data directory exists. This line has been removed.