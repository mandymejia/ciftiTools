## Test environments

* Windows x86_64-w64-mingw32/x64, R 4.2.2
* Mac x86_64-apple-darwin17.0, R 4.3.1

## R CMD check results

> checking installed package size ... NOTE
    installed size is  7.6Mb
    sub-directories of 1Mb or more:
      R         2.8Mb
      extdata   4.3Mb

0 errors v | 0 warnings v | 1 note x

These files are necessary and have already been reduced in size.

## Downstream dependencies

`fMRItools`, `fMRIscrub`, and `templateICAr` do not suffer additional warnings or errors with this new version of `ciftiTools`. 

## Tests

Passes all the tests in `tests/run_ciftiTools_tests.R`

## Previous submission

#### 0.13.3

  Lost braces in \itemize; meant \describe ?

\itemize has been replaced with \describe.