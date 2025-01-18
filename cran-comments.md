## Test environments

* Mac x86_64-apple-darwin17.0, R 4.4.0

## R CMD check results

> checking installed package size ... NOTE
    installed size is  7.3Mb
    sub-directories of 1Mb or more:
      R         1.8Mb
      extdata   4.3Mb
      help      1.1Mb

0 errors v | 0 warnings v | 1 note x

These files are necessary and have already been reduced in size.

## Downstream dependencies

`fMRItools`, `fMRIscrub`, and `templateICAr` do not suffer additional warnings or errors with this new version of `ciftiTools`. 

## Tests

Passes all the tests in `tests/run_ciftiTools_tests.R`

## Previous submission

  Quitting from lines 69-71 [unnamed-chunk-6] (ciftiTools_vignette.Rmd)
  Error: processing vignette 'ciftiTools_vignette.Rmd' failed with diagnostics:
  '"C:\Users\CRAN\Documents/Applications"' not found
  --- failed re-building 'ciftiTools_vignette.Rmd'

  SUMMARY: processing the following file failed:
    'ciftiTools_vignette.Rmd'

* The vignette has been modified to avoid this error. 