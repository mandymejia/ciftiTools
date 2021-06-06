# 3.0 (In-progress)

## Changes affecting users

* Add `combine_xifti`
* Add S3 method `dim` for `xifti` objects
* Add `legend_alllevels` arg to `view_xifti_surface`
* More control over legend file name in `view_xifti_surface`

## Notes for developers

None

# 2.2 (March 16, 2021)

## Changes affecting users

None

## Notes for developers

* Add `run_ciftiTools_tests.R` to `.Rbuildignore`

# 2.1 (March 15, 2021)

## Changes affecting users
* Search for "wb_path" during `ciftiTools.setOption` call, and never afterward
* Ignore Workbench warnings, for the most part
* Remove arguments concering intermediate files in `resample_cifti` and `read_cifti`
* Too many changes to document for `view_xifti_surface`!
* `transform_xifti` improvements
* Export `remove_xifti`
* Add `rotate_surf`, `select_xifti` and `concat_xifti`

# 2.0 (February 2, 2021)

## Changes affecting users
* Replace `surface_sigma` with `surf_FWHM` and likewise for volume
* Better handling of plotting surface or volume in `view_xifti`

# 1.6.1 (January 29, 2021)
## Changes affecting users
* Warn user (instead of raising an error) when the data has more medial wall vertices than non-medial wall vertices.
* New handling of color mode, default colors and color limits.
* Save color legends too, if saving surface plots.
* Label for widget slider.
* Move `fields` and `rgl` from Suggests to Imports.
* Remove `wb_path` argument. (Use `ciftiTools.setOption("wb_path", "path/to/workbench")`)
* `transform_xifti` to apply unary functions
* Add mathematical operations: `exp(cii) / 5 + cii2`
* `newdata_xifti` to substitute data

# 1.6 (November 10, 2020)

## Changes affecting users

* `smooth_cifti`
    * Add smoothing a `xifti` directly
    * `cifti_target_fname` is optional to `smooth_cifti`; will be auto-generated if `NULL`
    * Use the surfaces included with the `ciftiTools` package if not provided to `smooth_cifti` when corresponding cortex data exists
* `view_xifti_surface`
    * Rename arguments regarding interactivity
    * Add option to draw borders for qualitative coloring
    * Add color legend with names of labels for qualitative coloring
* Rename `surface` class to `surf`
* `as.xifti`
    * Use HCP 32k mwall when not provided, by default (add option to control)
    * Add option to add column names
* Retain more CIFTI metadata during resampling
* Reduce package size to < 5 for CRAN by resampling demo files.

## Notes for developers

* Add `run_ciftiTools_tests.R` (and add to .Rbuildignore)
* Do not try linking to RColorBrewer::brewer.pal.info because it is an object, not a function.
* Correct `ciftiTools.listOptions` and `ciftiTools_msg`
* Make `gifti_to_surf` an alias to `make_surf`

# 1.5 (October 2, 2020)

## Changes affecting users

* Add widget with slider for `view_xifti_surface` when more than one `idx` is requested!
    * Replace `mode` argument with `interactive` argument
    * Automatically use widget or video frames if more than one `idx`
* Resilience to missing "MetaData" entry in CIFTI$Matrix

# 1.4.2 (September 25, 2020)

## Changes affecting users

* `view_xifti_surface`
    * Option to draw vertices and edges
    * Automatically use (and resample to match data resolution) included surfaces, in most cases where surfaces were not provided
* `view_xifti_volume`
    * Fix error when data are integers
    * Check that image dimensions and transformation matrix match with MNI template before plotting
    * Use 9 slices instead of 12 by default (when not using `papayar`)
* New argument `mwall_values` to relevant functions. 
    * The values will be used to infer the medial wall if the medial wall is not explicitly specified
    * Default: `NA` and `NaN` 
    * This argument can be set to `NULL` to not infer the medial wall from the data
* Better reading & writing of NIFTI and GIFTI files
    * Export and import 4x4 transformation matrix with CIFTI and NIFTI files
        * Use the `TransformationMatrixIJKtoXYZ` in CIFTI files
        * Use the `sform` codes in NIFTI files
    * Export and import label table and column names with CIFTI and GIFTI files (for `dlabel` and `dscalar`)
    * Write NIFTI with `RNifti` instead of `oro.nifti`
* Corrected separating, writing and resampling `*.dlabel.nii` files
* Make `cifti_fname` in `read_cifti` optional (can read in just the surfaces)
* Warning if brainstructure is requested but not available (instead of error)
* Convert smoothed `dlabel` to `dscalar` (and add warning, because it's probably something you don't want to do)
* Documentation improvements

## Notes for developers

* Improved handling of bad arguments to `make_subcort`
* Split `info_cifti_raw` into `header_cifti` and `xml_cifti`
* Add the internal functions `remove_xifti`, `expect_equal_xifti` and `fix_gifti_mwall`
* `view_surf` is now a wrapper to `view_xifti_surface` instead of being its own function
* Move color functions to `utils_color`. 
* Make `ROY_BIG_BL` have an extra value to fix legend range. 
* Handle constant-valued data in `view_xifti_surface`
* Export color functions
* Add "hemisphere" metadata to \code{"surf"} objects, and check that it matches if it exists. For example, `as.xifti(surfL=surfR_gii)` will cause an error
* Move `rgl` and `fields` to Suggests
* Add a few new tests based on these changes

# 1.4 (August 31, 2020)

## Changes Affecting Users

* Spheres are no longer required to input for resampling
    * In-register spheres are generated automatically based on the cortical resolution
* Full label table list for `dlabel.nii` files
* Add `smooth_cifti`
* Add `resample_cifti_from_template`
* Add helpful error message if `plot(xii)` is called without any surface or subcortical data
* `view_xifti_surface` improvements:
    * Use column index or name as the default title
    * Improved qualitative color mode
    * Use qualitative color mode and label table colors if the CIFTI is a `dlabel.nii` file (intent 3007)
* `view_xifti_volume` improvements:
    * Fix `num.slices` argument
    * Add `...` for additional arguments
* Add `view_surface` for `"surface"` objects
* Add `summary` and `plot` methods for `"surface"` objects
* Warning if GIFTI version is too old (need the one on github for `writegii`)
* Tweaks to `view_xifti_surface` defaults
* Add `view_xifti_volume` without structural image
* If a function writes a file(s) as its main side effect, return a named character vector of the written file paths.
    * Previously some functions returned `TRUE` while others returned a `list` or `data.frame` of written file paths
* Out-of-mask values (medial wall for cortex and non-subcortical voxels for subcortex)
 are no longer inferred from the data values when making a `"xifti"` from GIFTI and NIFTI files. Previously, if a mask was not provided then constant `0`, `NA`, or `NaN` values were deemed out-of-mask. Now, `ciftiTools` uses ROI files to keep track of the out-of-mask values in `read_cifti_separate` and `resample_cifti`, and requires the masks to be explicitly provided in `as.xifti`
* Rename `make_surface` as `make_surf`
* Rename `is.surface` as `is.surf`
* Rename `side` argument as `hemisphere` argument (same choices: `"left"` or `"right"`)
* Fixed the alias functions, e.g. `readCIfTI` and `readcii` for `read_cifti`

## Demo files

* The demo files from NITRC are now included with `ciftiTools`: https://www.nitrc.org/frs/download.php/8541/cifti-2_test_data-1.2.zip

## Notes for Developers

* Add tests for most user functions
* Rename `write_xifti_components` as `write_cifti_components`
* Remove `metric_resample` and `surface_resample` (use `resample_gifti` directly)
* Rename `make_helper_spheres` as `write_spheres`, and do not export it
* Rename `unmask` as `unmask_vol` (to distinguish from `unmask_cortex`)
* Remove `data-raw` from R package build
* Require `gifti > 0.7.5`
* Clean up `onAttach` and `.Rbuildignore`

## Vignette!

It will be located here: https://htmlpreview.github.io/?https://github.com/mandymejia/ciftiTools/blob/master/vignettes/ciftiTools_vignette.html once we push to master. Until then, replace "master" with "1.4"

# 1.3 (August 24, 2020)

* introduce `"xifti"` object: can be made from CIFTI file, GIFTI surfaces, or new data
    * To make a `"xifti"` from data: `as.xifti(...)`
    * To make a `"xifti"` from a CIFTI file: `read_cifti(cifti_fname)`
    * To make a `"xifti"` from a GIFTI surface: `as.xifti(surfL=make_surf(surfL_fname))`
* `read_cifti_flat` now uses XML metadata directly (but still obtains XML via Connectome Workbench)
    * This allows for reading in more metadata such as time start/step for `.dtseries.nii`
* `write_surf_gifti` and `write_metric_gifti` as wrappers to the new `gifti::writegii`
    * Currently depends on `damondpham/gifti` but this will hopefully be merged to main repo soon
* `write_subcort_nifti` to write out subcortical components from `"xifti"` to a NIFTI file
* `write_cifti` can also write the surfaces attached to the `"xifti"`
* `"xifti"` objects can contain surfaces without any data. 
    * `view_xifti_surface` can view a surface without any data: `plot(as.xifti(surfL=left_surf))`
* `resample_surf` to resample a surface object 

# 1.2 (August 7, 2020)

* `read_cifti_flat` and `read_cifti_separate`
* flatten subcortical data in `"cifti"` object. unflattening functions.
* image view mode
* `write_cifti`
* better handling of medial wall
* Subcortical color table added for writing CIFTIs. Taken from: https://surfer.nmr.mgh.harvard.edu/fswiki/FsTutorial/AnatomicalROI/FreeSurferColorLUT

# 1.1 (July 24, 2020)

## New names

Most of the functions were renamed. In general, `cifti_[action]` is now `[action]_cifti`. So to open and visualize a CIFTI file, one would use `read_cifti()` followed by `view_cifti()`. To save a resampled version of a CIFTI file, one would use `resample_cifti()`. 

Arguments were also renamed for consistency across functions:

* Arguments referring to file names all end in `_fname` instead of beginning with `fname_`.
* The cortical data, `surf_L` or `cortex_left`, are now `cortexL` (likewise for right).
* The surface geometry data, `gifti_left` or `surf_left`, are now `surfL` (likewise for right).
* The helper sphere for resampling, `sphere_L`, is now `sphereL` (likewise for right).
* `sphere_orig_L` is now `sphereL_original` (likewise for right; likewise for other original files).
* `sphere_target_L` is now `sphereL_target` (likewise for right; likewise for other target files).
* The resampling resolution, `target_res` or `resample`, is now `resamp_res`.
* The Connectome Workbench argument `wb_cmd` is now `wb_path`.
* `outdir` is now `write_dir`.

## New functionality

There are a few new functions:

* `read_cifti_flat()` reads the cortical and surface data together as a single matrix. 
    * It is much faster than `read_cifti()`.
    * It works by using the Connectome Workbench command `-cifti-convert -to-gifti-ext`. 
    * `flatten_cifti()` converts a `"cifti"` object to this same matrix. 
    * `read_cifti()` with `flat==TRUE` will call `read_cifti_flat()`.
* `make_xifti()` creates a `"cifti"` object from its components.
    * Each component can be provided as a file name or R object.
* `view_cifti()` plots the CIFTI data.
    * It calls `view_cifti_surface()` unless no cortical data exists, in which case it will try `view_cifti_volume()`.
    * `view_cifti_surface()` was rewritten using the `rgl` package directly.

And, a few new arguments:

* `sep_keep` and `resamp_keep` indicate whether separated or resampled files should be deleted after reading in the CIFTI data (`read_cifti()`) or writing the resampled CIFTI file (`resample_cifti()`). Their default values are `FALSE`. in which case these files are written to a temporary directory (regardless of `write_dir`). 
* `sep_fnames` and `resamp_fnames` can be used to set the file names for the respective files made by `read_cifti` and `resample_cifti`.
    * The default separated and resampled file names have been updated; see `cifti_component_suffix`.

Other updates:

* Print execution times for each step in `separate_cifti()` and `resample_cifti()` if `verbose==TRUE`.
* Add option to set Connectome Workbench directory for the entire session with `ciftiTools.setOption('wb_path', 'path/to/workbench')`. 
    * Either the folder or executable path can be provided.
* Handling of spaces and parentheses in file names.
* The ROI data can be separated and resampled. However, the `"cifti"` object does not support ROIs, so they are ultimately not loaded. 
    * Please contact the developers if you are interested in this functionality.
    * A workaround would be to separate/resample the ROIs, and then read them in with `make_xifti()` (haven't tested this yet).
* Add option to store a zero-value threshold, `EPS`. Set it with `ciftiTools.setOption("EPS", 1e-8)`. This is used by `view_cifti()` and `flatten_cifti()` to detect constant zero brainordinates.
* Several function aliases, for example `readCIfTI()` and `readcii()` for `read_cifti()`.
* `plot()` method for `"cifti"` objects.

## Removed functionality

* The `overwrite` and `make_helper_files` arguments were removed. All functions will overwrite existing files, and this cannot be changed.
* A `"cifti"` object will only support one surface geometry model at a time.
    * `view_cifti()` can accept a surface GIFTI file directly. 
* The default brainstructures are `left` and `right` only (excludes the subcortical data).

## Notes for developers

* Replaced `oro.nifti::readNIfTI()` with `RNifti::readNifti()` for faster reading of subcortical data.
* Made formal classes for each CIFTI file component: `"cifti_surface"`, `"cifti_subcortical"`, `"cifti_cortex"`, `"cifti_volume"` and `"cifti_label"`.
* Replaced `read_separate_cifti()` with `read_cifti()`, which calls `separate_cifti()`, `resample_cifti_components()` (optionally), and `make_xifti()` in that order. 
* Rewrote `resample_cifti()`. The new version calls `separate_cifti()`, `resample_cifti_components()`, and then uses a template to create the new CIFTI file. (The first half is nearly identical to `read_separate_cifti()`.)
* Moved common argument/parameter descriptions to `rox_args_docs.R`. Use `@inheritParams` to reference these descriptions. This should make maintaining them across the entire package easier.
* Wrapper functions `separate_cifti_wrapper()` and `resample_cifti_wrapper()` to avoid duplicate code across `read_cifti()` and `resample_cifti()`. These should not be used by end-users, so they are not exported.
* Split utility functions into `utils.R` for general functions and `utils_cifti.R` for CIFTI-specific functions.