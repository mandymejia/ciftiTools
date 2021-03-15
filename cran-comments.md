## Test environments

* Windows 10 x64, R 4.0.4
* Linux x64, R 3.6.0

## R CMD check results

On Windows:

  0 errors v | 0 warnings v | 0 notes v

  R CMD check succeeded

On macos-highsierra-release-cran:

> checking installed package size ... NOTE
    installed size is  5.0Mb
    sub-directories of 1Mb or more:
      extdata   4.8Mb

These files are necessary and have already been reduced in size.

## Downstream dependencies

None.

## Tests

Passes all the tests in `tests/run_ciftiTools_tests.R`