# 1.1

## Since 1.0

For end-users:

* Replaced `oro.nifti::readNIfTI` with `RNifti::readNifti` for faster reading of subcortical data.
* Replaced `read_separate_cifti` with `read_cifti`, which calls `separate_cifti`, `resample_cifti_separate` (optionally), and `make_cifti` in that order. 
* Rewrote `resample_cifti`. The new version calls `separate_cifti`, `resample_cifti_separate`, and then uses a template to create the new CIFTI file. (The first half is nearly identical to `read_separate_cifti`.)
* Print execution times for `separate_cifti` and `resample_cifti` if `verbose==TRUE`.
* Removed `overwrite` and `make_helper_files` parameters: all functions will overwrite existing files, and this cannot be changed.
* Only one surface model is supported at a time.
* `sep_keep` and `resamp_keep` indicate whether separated or resampled files should be deleted after reading in the CIFTI data (`read_cifti`) or writing the resampled CIFTI file (`resample_cifti`). Their default values are `TRUE`.
* `sep_fnames` and `resamp_fnames` allow for custom file names for the intermediate files for `read_cifti` and `resample_cifti`.
* Re-named a few arguments to look similar between functions:
    * Arguments referring to file names all end in `_fname` instead of beginning with `fname_`
    * List/process/etc. "left" hemisphere components before "right" (e.g. argument order for make_helper_spheres has been swapped to match this).
    * `surf_L` and `cortex_left` are now `cortexL` (likewise for right)
    * `gifti_left` and `surf_left` are now `surfL` (likewise for right)
    * `sphere_L` is now `sphereL` (likewise for right)
    * `sphere_orig_L` is now `sphereL_original` (likewise for right; likewise for other original files)
    * `sphere_target_L` is now `sphereL_target` (likewise for right; likewise for other target files)
    * `target_res` in `make_helper_spheres` and `resample` in `resample_cifti` are now `resamp_res` (to match `read_cifti`)
    * `wb_cmd` is now `wb_path` because it could also just point to the folder. `wb_cmd` is used when it is converted to the executable exactly.
* Added `read_cifti_flat` to read cortical and surface data together as a single matrix (faster). Also added `flatten_cifti` to convert a cifti object to this same matrix. `read_cifti` with `flat==TRUE` will call `read_cifti_flat`.
* Made a formal `"cifti_surface"` class for surface geometry files. `make_cifti_surface` is a function that converts surface GIFTI files or file names into a `"cifti_surface"` object. Similar clases `"cifti_cortical"` [...]
* New default file names for separated CIFTI components. See `separate_cifti_default_suffix`.
* New `view_cifti_surface` function using the `rgl` package. Moved visualization of subcortical data to `view_cifti_volume` (I haven't tested this yet, though).
* Add option to set Connectome Workbench directory for the entire session with `ciftiTools.setOption('wb_path', 'path/to/workbench')`. Also, either the folder or executable path can be provided.
* Add option to store a zero-value threshold, `EPS`. Set it with `ciftiTools.setOption("EPS", 1e-8)`. This is used by `view_cifti` and `flatten_cifti` to detect constant zero brainordinates.
* The default brainstructures are `left` and `right` only (not the subcortical data).
* Add support for separating and resampling the ROI data. However, it is not loaded with the complete data (the CIFTI object does not yet support ROIs). A workaround would be to separate/resample the ROIs, and then read them in with `make_cifti_from_separate` (I haven't tested this yet).

For developers only:

* Moved common argument/parameter descriptions to `rox_args_docs.R`. Use `@inheritParams` to reference these descriptions. This should make maintaining them across the entire package easier.
* Wrapper functions `separate_cifti_wrapper` and `resample_cifti_wrapper` to avoid duplicate code across `read_cifti` and `resample_cifti`. These should not be used by users, so they are not exported.
