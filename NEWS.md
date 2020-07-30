# 1.1

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
* `make_cifti()` creates a `"cifti"` object from its components.
    * Each component can be provided as a file name or R object.
* `view_cifti()` plots the CIFTI data.
    * It calls `view_cifti_surface()` unless no cortical data exists, in which case it will try `view_cifti_volume()`.
    * `view_cifti_surface()` was rewritten using the `rgl` package directly.

And, a few new arguments:

* `sep_keep` and `resamp_keep` indicate whether separated or resampled files should be deleted after reading in the CIFTI data (`read_cifti()`) or writing the resampled CIFTI file (`resample_cifti()`). Their default values are `FALSE`. in which case these files are written to a temporary directory (regardless of `write_dir`). 
* `sep_fnames` and `resamp_fnames` can be used to set the file names for the respective files made by `read_cifti` and `resample_cifti`.
    * The default separated and resampled file names have been updated; see `separate_cifti_default_suffix`.

Other updates:

* Print execution times for each step in `separate_cifti()` and `resample_cifti()` if `verbose==TRUE`.
* Add option to set Connectome Workbench directory for the entire session with `ciftiTools.setOption('wb_path', 'path/to/workbench')`. 
    * Either the folder or executable path can be provided.
* Handling of spaces and parentheses in file names.
* The ROI data can be separated and resampled. However, the `"cifti"` object does not support ROIs, so they are ultimately not loaded. 
    * Please contact the developers if you are interested in this functionality.
    * A workaround would be to separate/resample the ROIs, and then read them in with `make_cifti()` (haven't tested this yet).
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
* Replaced `read_separate_cifti()` with `read_cifti()`, which calls `separate_cifti()`, `resample_cifti_components()` (optionally), and `make_cifti()` in that order. 
* Rewrote `resample_cifti()`. The new version calls `separate_cifti()`, `resample_cifti_components()`, and then uses a template to create the new CIFTI file. (The first half is nearly identical to `read_separate_cifti()`.)
* Moved common argument/parameter descriptions to `rox_args_docs.R`. Use `@inheritParams` to reference these descriptions. This should make maintaining them across the entire package easier.
* Wrapper functions `separate_cifti_wrapper()` and `resample_cifti_wrapper()` to avoid duplicate code across `read_cifti()` and `resample_cifti()`. These should not be used by end-users, so they are not exported.
* Split utility functions into `utils.R` for general functions and `utils_cifti.R` for CIFTI-specific functions.