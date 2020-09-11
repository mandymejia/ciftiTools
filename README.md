
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ciftiTools

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/mandymejia/ciftiTools.svg?branch=master)](https://travis-ci.com/mandymejia/ciftiTools)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/mandymejia/ciftiTools?branch=master&svg=true)](https://ci.appveyor.com/project/mandymejia/ciftiTools)
[![Coveralls test
coverage](https://coveralls.io/repos/github/mandymejia/ciftiTools/badge.svg)](https://coveralls.io/r/mandymejia/ciftiTools?branch=master)
<!-- badges: end -->

Tools for reading and visualizing CIFTI brain imaging files. CIFTI files
contain brain imaging data in “gray-ordinates”, which includes all the
gray matter of the brain. The gray matter is divided into cortical
surface (left and right) and subcortical (cerebellum, basal ganglia, and
other deep gray matter). `ciftiTools` uses the Connectome Workbench to
read the data into R. It also provides visualization tools for the
surface and subcortical data, and processing functions to apply common
preprocessing steps (e.g. smoothing, resampling).

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mandymejia/ciftiTools")
```

## Example

Visualizing CIFTI data with `ciftiTools`:

``` r
library(ciftiTools)

# Tell ciftiTools where to find the Connectome Workbench folder or executable.
ciftiTools.setOption('wb_path', 'Desktop/workbench')

# Use example CIFTI and surface files.
cifti_fname <- system.file(
  "extdata", "Conte69.MyelinAndCorrThickness.32k_fs_LR.dtseries.nii", 
  package = "ciftiTools"
)
surfL_fname <- system.file(
  "extdata", "Conte69.L.inflated.32k_fs_LR.surf.gii", 
  package = "ciftiTools"
)
surfR_fname <- system.file(
  "extdata", "Conte69.R.inflated.32k_fs_LR.surf.gii", 
  package = "ciftiTools"
)

# Read in the CIFTI and surfaces, and resample to 8000 vertices per hemisphere.
cifti <- read_cifti(
  cifti_fname=cifti_fname, 
  surfL_fname=surfL_fname, 
  surfR_fname=surfR_fname, 
  resamp_res=8000
)

# Visualize the cortical surface data.
plot(cifti)
```

## Wishlist

  - slider in `view_cifti()` to change column/time
  - tests
  - CRAN
  - Neuroconductor
