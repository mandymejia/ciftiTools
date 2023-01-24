
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ciftiTools

<!-- badges: start -->

[![R-CMD-check](https://github.com/mandymejia/ciftiTools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mandymejia/ciftiTools/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/mandymejia/ciftiTools/branch/master/graph/badge.svg)](https://app.codecov.io/gh/mandymejia/ciftiTools?branch=master)
<!-- badges: end -->

CIFTI files contain brain imaging data in “grayordinates,” which
represent the gray matter as cortical surface vertices (left and right)
and subcortical voxels (cerebellum, basal ganglia, and other deep gray
matter). `ciftiTools` provides a unified environment for reading,
writing, visualizing and manipulating CIFTI-format data. It supports the
“dscalar,” “dlabel,” and “dtseries” intents. Grayordinate data is read
in as a `"xifti"` object, which is structured for convenient access to
the data and metadata, and includes support for surface geometry files
to enable spatially-dependent functionality such as static or
interactive visualizations and smoothing.

## Citation

If you use `ciftiTools`, please cite our
[paper](https://doi.org/10.1016/j.neuroimage.2022.118877):

> Pham, D. D., Muschelli, J., & Mejia, A. F. (2022). ciftiTools: A
> package for reading, writing, visualizing, and manipulating CIFTI
> files in R. NeuroImage, 250, 118877.

You can also obtain citation information from within R like so:

``` r
citation("ciftiTools")
```

## Installation

You can install `ciftiTools` from [CRAN](https://cran.r-project.org/)
with:

``` r
install.packages("ciftiTools")
```

Additionally, most of the `ciftiTools` functions require the Connectome
Workbench, which can be installed from the [HCP
website](https://www.humanconnectome.org/software/get-connectome-workbench).

## Quick start guide

``` r
# Load the package and point to the Connectome Workbench --------
library(ciftiTools)
ciftiTools.setOption("wb_path", "path/to/workbench")

# Read and visualize a CIFTI file -------------------------------
cifti_fname <- ciftiTools::ciftiTools.files()$cifti["dtseries"]
surfL_fname <- ciftiTools.files()$surf["left"]
surfR_fname <- ciftiTools.files()$surf["right"]

xii <- read_cifti(
  cifti_fname, brainstructures="all", 
  surfL_fname=surfL_fname, surfR_fname=surfR_fname,
  resamp_res=4000
)

view_xifti_surface(xii) # or plot(xii)
# view_xifti_volume(xii) if subcortex is present

# Access CIFTI data ---------------------------------------------
cortexL <- xii$data$cortex_left
cortexL_mwall <- xii$meta$medial_wall_mask$left
cortexR <- xii$data$cortex_right
cortexR_mwall <- xii$meta$medial_wall_mask$right
# subcortVol <- xii$data$subcort
# subcortLabs <- xii$meta$subcort$labels
# subcortMask <- xii$meta$subcort$mask
surfL <- xii$surf$cortex_left
surfR <- xii$surf$cortex_right

# Create a `"xifti"` from data ----------------------------------
xii2 <- as.xifti(
  cortexL=cortexL, cortexL_mwall=cortexL_mwall,
  cortexR=cortexR, cortexR_mwall=cortexR_mwall,
  #subcortVol=subcortVol, subcortLabs=subcortLabs,
  #subcortMask=subcortMask,
  surfL=surfL, surfR=surfR
)

# Write a CIFTI file --------------------------------------------
write_cifti(xii2, "my_cifti.dtseries.nii")
```

## Vignette

See [this
link](https://htmlpreview.github.io/?https://github.com/mandymejia/ciftiTools/blob/master/vignettes/ciftiTools_vignette.html)
to view the tutorial vignette.

## Illustrations

<figure>
<img src="README_media/ciftiTools_summary.png" style="width:70.0%"
alt="ciftiTools graphical summary" />
<figcaption aria-hidden="true">ciftiTools graphical summary</figcaption>
</figure>

<figure>
<img src="README_media/xifti_structure.png" style="width:70.0%"
alt="“xifti” object structure" />
<figcaption aria-hidden="true">“xifti” object structure</figcaption>
</figure>

<figure>
<img src="README_media/surf_tour.gif" style="width:25.0%"
alt="Surfaces comparison. The “very inflated”, “inflated”, and “midthickness” surfaces are included in ciftiTools through the function load_surf. See the data acknowledgement section at the bottom of this README." />
<figcaption aria-hidden="true">Surfaces comparison. The “very inflated”,
“inflated”, and “midthickness” surfaces are included in ciftiTools
through the function <code>load_surf</code>. See the data
acknowledgement section at the bottom of this README.</figcaption>
</figure>

## FAQ

#### Why is a CIFTI file that has been read in called a `"xifti"`?

The `"xifti"` object is a general interface for not only CIFTI files,
but also GIFTI and NIFTI files. For example, we can plot a surface
GIFTI:

``` r
xii <- as.xifti(surfL=read_surf(ciftiTools.files()$surf["left"]))
plot(xii)
```

We can also convert metric GIFTI files and/or NIFTI files to CIFTI files
(or vice versa) using the `"xifti"` object as an intermediary.

#### How do I visualize cortical data without applying shading to the mesh geometry?

The 3D shading may make certain plots more difficult to interpret, if
the color scale varies from light to dark: darker regions might be in a
shadow, or their values might be higher. To skip shading, use the
argument `material=list(lit=FALSE)` to `view_xifti_surface`.

<img src="README_media/vxs_lit.png" style="width:15.0%"
alt="Lit surface plot" />
<img src="README_media/vxs_unlit.png" style="width:15.0%"
alt="Unlit surface plot" />

## Related R extensions

-   NIFTI files:
    [`oro.nifti`](https://CRAN.R-project.org/package=oro.nifti),
    [`RNifti`](https://CRAN.R-project.org/package=RNifti)
-   GIFTI files: [`gifti`](https://CRAN.R-project.org/package=gifti)
-   CIFTI files: [`cifti`](https://CRAN.R-project.org/package=cifti) can
    read in any CIFTI file, whereas `ciftiTools` provides a
    user-friendly interface for CIFTI files with the dscalar, dlabel,
    and dtseries intents only.
-   Other structural neuroimaging files:
    [`fsbrain`](https://CRAN.R-project.org/package=fsbrain)
-   xml files: [`xml2`](https://CRAN.R-project.org/package=xml2)
-   Interactive 3D rendering:
    [`rgl`](https://CRAN.R-project.org/package=rgl)

## Data acknowledgement

The following data are included in the package for convenience:

Example CIFTI files provided by
[NITRC](https://www.nitrc.org/projects/cifti/).

Cortical surfaces provided by the HCP, according to the [Data Use
Terms](https://www.humanconnectome.org/study/hcp-young-adult/document/wu-minn-hcp-consortium-open-access-data-use-terms):

> Data were provided \[in part\] by the Human Connectome Project,
> WU-Minn Consortium (Principal Investigators: David Van Essen and Kamil
> Ugurbil; 1U54MH091657) funded by the 16 NIH Institutes and Centers
> that support the NIH Blueprint for Neuroscience Research; and by the
> McDonnell Center for Systems Neuroscience at Washington University.

Several parcellations provided by [Thomas Yeo’s Computational Brain
Imaging Group
(CBIG)](https://github.com/ThomasYeoLab/CBIG/tree/master/stable_projects/brain_parcellation):

1.  Yeo, B. T. T. et al. The organization of the human cerebral cortex
    estimated by intrinsic functional connectivity. J Neurophysiol 106,
    1125–1165 (2011).
2.  Schaefer, A. et al. Local-Global Parcellation of the Human Cerebral
    Cortex from Intrinsic Functional Connectivity MRI. Cereb Cortex 28,
    3095–3114 (2018).
3.  Kong, R. et al. Individual-Specific Areal-Level Parcellations
    Improve Functional Connectivity Prediction of Behavior. Cerebral
    Cortex (2021).
