# 1.1

## Since 1.0
* Replaced oro.nifti::readNIfTI with RNifti::readNifti for speedup.
* Replaced `cifti_read_separate` with `cifti_read`, which calls `cifti_separate`, `cifti_resample_separate` (optionally), and `cifti_read_from_separate` in order. But resampling with `cifti_resample_separate` is still a work-in-progress.
* Re-named a few arguments to look similar between functions:
    * Arguments referring to file names all end in `_fname` instead of beginning with `fname_`
    * `surf_L` is now `cortexL` (likewise for right)
    * `gifti_left` is now `surfL` (likewise for right)
    * `sphere_orig_L` is now `sphereL_original` (likewise for right)
    * `sphere_target_L` is now `sphereL_target` (likewise for right)
    * ... (more to document)
* Added `cifti_read_flat` to read cortical and surface data together as a single matrix (faster). `cifti_flatten` to convert a cifti object to this same matrix.
* New `cifti_view_surface` using the `rgl` package.
* Add option to set Connectome Workbench directory for the entire session with `ciftiTools.setOption('wb_path', 'path/to/workbench')`. Also, either the folder or executable path can be provided.
