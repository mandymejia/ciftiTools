# 1.1

## Since 1.0

For end-users:

* Replaced oro.nifti::readNIfTI with RNifti::readNifti for faster reading of subcortical data.
* Replaced `cifti_read_separate` with `cifti_read`, which calls `cifti_separate`, `cifti_resample_separate` (optionally), and `cifti_make_from_separate` in that order. 
* Rewrote `cifti_resample`. The new version calls `cifti_separate`, `cifti_resample_separate`, and then uses a template to create the new CIFTI file. (The first half is nearly identical to `cifti_read_separate`.)
* Print execution times for `cifti_separate` and `cifti_resample` if `verbose==TRUE`.
* `sep_keep` and `resamp_keep` indicate whether separated or resampled files should be deleted after reading in the CIFTI data (`cifti_read`) or writing the resampled CIFTI file (`cifti_resample`). 
* `sep_kwargs` and `resamp_kwargs` allow for additional arguments to their respective functions. So we can use `cifti_read(cifti_fname, sep_kwargs=list(write_dir="sep_files", overwrite=FALSE), resamp_kwargs=list(write_dir="resamp_files", overwrite=TRUE), ...)` to write the separated files and the resampled files in different directories and have different overwriting behaviors.
* Re-named a few arguments to look similar between functions:
    * Arguments referring to file names all end in `_fname` instead of beginning with `fname_`
    * List/process/etc. "left" hemisphere components before "right" (e.g. argument order for make_helper_spheres has been swapped to match this).
    * `surf_L` and `cortex_left` are now `cortexL` (likewise for right)
    * `gifti_left` and `surf_left` are now `surfL` (likewise for right)
    * `sphere_L` is now `sphereL` (likewise for right)
    * `sphere_orig_L` is now `sphereL_original` (likewise for right; likewise for other original files)
    * `sphere_target_L` is now `sphereL_target` (likewise for right; likewise for other target files)
    * `target_res` in `make_helper_spheres` and `resample` in `cifti_resample` are now `resamp_res` (to match `cifti_read`)
    * Removed `make_helper_files` and `delete_helper_files` in favor of `sphere_target_keep` and `sphere_target_override`. This resembles e.g. `sep_keep` and `sep_kwargs$overwrite`. 
    * `wb_cmd` is now `wb_path` (because it could also just point to the folder).
    * ... (more to document)
* Added `cifti_read_flat` to read cortical and surface data together as a single matrix (faster). Also added `cifti_flatten` to convert a cifti object to this same matrix.
* Made a formal `"cifti_surface"` class for surface geometry files. `cifti_make_surface` is a function that converts surface GIFTI files into a `"cifti_surface"` object.
* New default file names for separated CIFTI components. See `cifti_separate_default_suffix`.
* New `cifti_view_surface` function using the `rgl` package. Moved visualization of subcortical data to `cifti_view_volume` (I haven't tested this yet, though).
* Add option to set Connectome Workbench directory for the entire session with `ciftiTools.setOption('wb_path', 'path/to/workbench')`. Also, either the folder or executable path can be provided.
* Add option to create new directories during any attempt to write a file in a directory that does not exist (instead of raising an error). Set it with `ciftiTools.setOption('make_dirs', 'TRUE')`. It is `FALSE` by default.
* Add option to store a zero-value threshold, `EPS`. Set it with `ciftiTools.setOption("EPS", 1e-8)`. This is used by `cifti_view` and `cifti_flatten` to detect constant zero brainordinates.
* The default brainstructures are `left` and `right` only (not the subcortical data).
* Add support for separating and resampling the ROI data. However, it is not loaded with the complete data (the CIFTI object does not yet support ROIs). A workaround would be to separate/resample the ROIs, and then read them in with `cifti_make_from_separate` (I haven't tested this yet).

For developers only:

* Moved common argument/parameter descriptions to `rox_args_docs.R`. Use `@inheritParams` to reference these descriptions. This should make maintaining them across the entire package easier.
* Wrapper functions `cifti_separate_wrapper` and `cifti_resample_wrapper` to avoid duplicate code across `cifti_read` and `cifti_resample`. These should not be used by users, so they are not imported.
