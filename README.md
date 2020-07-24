
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ciftiTools

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/mandymejia/ciftiTools.svg?branch=master)](https://travis-ci.com/mandymejia/ciftiTools)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/mandymejia/ciftiTools?branch=master&svg=true)](https://ci.appveyor.com/project/mandymejia/ciftiTools)
<!-- badges: end -->

Tools for reading and visualizing CIFTI brain imaging files. CIFTI files
contain brain imaging data in “gray-ordinates”, which includes all the
gray matter of the brain. The gray matter is divided into cortical
surface (left and right) and subcortical (cerebellum, basal ganglia, and
other deep gray matter). `ciftiTools` uses the Connectome Workbench to
separate CIFTI files into two GIFTI files representing the cortical
surface data and one NIFTI file representing the subcortical data, and
reads the data into R. It also provides visualization tools for the
surface and subcortical data. `ciftiTools` also includes processing
functions to apply common preprocessing steps to the CIFTI data
(e.g. smoothing, resampling).

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

# Tell ciftiTools where to find the Connectome Workbench folder/executable.
ciftiTools.setOption('wb_path', 'Desktop/workbench')

# Read in the CIFTI file, and resample it to 8000 vertices per hemisphere.
cii <- read_cifti(
  cifti_fname='Desktop/data/my_CIFTI_file.dtseries.nii', 
  surfL_fname='Desktop/data/32k_inflated_left.gii', 
  surfR_fname='Desktop/data/32k_inflated_right.gii', 
  resamp_res=8000, 
  # The spheres are only required for resampling.
  sphereL_fname='Desktop/data/32k_left_sphere.gii', 
  sphereR_fname='Desktop/data/32k_right_sphere.gii'
)

# Visualize the surface.
plot(cii)
```

## Wishlist

  - static images in `view_cifti()`
  - slider in `view_cifti()` to change column/time
  - `write_cifti()`
  - `unflatten_cifti()`
  - tests
  - CRAN
  - Neuroconductor
