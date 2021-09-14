## Test environments

* Windows 10 x64, R 4.1.1
* Mac x64, R 4.0.3

## R CMD check results

> checking installed package size ... NOTE
    installed size is  6.4Mb
    sub-directories of 1Mb or more:
      R         1.6Mb
      extdata   4.3Mb

0 errors v | 0 warnings v | 1 note x

These files are necessary and have already been reduced in size.

## Downstream dependencies

None.

## Tests

Passes all the tests in `tests/run_ciftiTools_tests.R`

## Time since last submission

The package was last updated on CRAN on August 19, which is just below the 1-2 months suggested by CRAN policy. However, this new update fixes a major component of the package (subcortex visualization) which is completely broken on the current version on CRAN. We also expect to submit a paper about this package within the week, and would like to have a working and up-to-date version on CRAN. We do not expect needing to submit with such a short turnover again. Thanks!