# #' Resample CIFTI data
# #'
# #' @description Performs spatial resampling of CIfTI data on the cortical surface
# #'
# #' @param c_original_fnames File paths of CIFTI file components in original resolution. Should be a named character vector with values
# #'  as the file names, and names as the file label: each name should be one of "cortexL", "cortexR", "subcortVol", or"subcortLab".
# #'  Alternatively, this can be the file name of the original CIfTI that was separated into files named with 
# #'  ciftiTools::cifti_separate_default_suffix .
# #' @param c_target_fnames File paths of CIFTI file components in target resolution to make. Should be a named character vector with values
# #'  as the file names, and names as the file label: each name should be one of "cortexL", "cortexR", "subcortVol", or"subcortLab".
# #'  Alternatively, this can be the file name of a hypothetical CIfTI file. Each separated file will be named by replacing the CIfTI extension with
# #'  ciftiTools::cifti_separate_default_suffix .
# #' @param c_original_fnames['cortexL'],c_original_fnames['cortexL'] (Optional) File path, or vector of multiple file paths, of GIFTI surface geometry file 
# #'  representing the left/right cortex. The path should be absolute, or relative to read_dir, which defaults to the current working directory.
# #' @param c_target_fnames['cortexL'],c_target_fnames['cortexL'] (Optional) File path, or vector of multiple file paths, of GIFTI surface geometry file 
# #'  representing the left/right cortex. The path should be absolute, or relative to write_dir, which defaults to the current working directory.
# #' @param res_target Target resolution (number of cortical surface vertices per hemisphere)
# #' @param sphereL_origin_fname,sphereR_origin_fname File path of [left/right]-hemisphere spherical GIFTI files in original resolution (compatible with cifti_orig)
# #' @param sphereL_target_fname,sphereR_target_fname File path of [left/right]-hemisphere spherical GIFTI files in targetinal resolution (compatible with cifti_Target)
# #' @param sphere_target_keep Should helper files be deleted at the end of this function call, if they were created?
# #' @param sphere_target_overwrite Logical indicating whether sphere[L/R]_target_fname should be overwritten if it already exists.
# #' @param overwrite Logical indicating whether c_target_fnames should be overwritten if it already exists.
# #' @param read_dir If the file names of \code{c_original_fnames} are relative, this is the directory to look for them in. If NULL (default),
# #'  use the current working directory. \code{read_dir} will not affect files specified with absolute paths.
# #' @param write_dir If the file names of \code{c_target_fnames} or surf[L/R]_origin_fname are relative, this is the directory to look for them in.
# #'  Defaults to the current working directory.
# #' @param sphere_target_dir If \code{sphere_target_keep} and the file names of \code{c_original_fnames} are relative, this is the directory to write and look for them in.
# #'  The default is "helper_files_resampling".
# #' @param wb_cmd Path to Connectome Workbench executable file, ending in 'wb_command' (Mac/linux) or 'wb_command.exe' (Windows).
# #'
# #' @return Logical indicating whether resampled CIFTI file was created.
# #' @export
# #'
# #' @details Performs resampling of CIFTI files using Connectome Workbench tools.  Several helper files must be created:
# #'
# #' Step 1: Create sphereL_target_fname and sphereR_target_fname, spherical GIFTI surface files for each hemisphere in target resolution.
# #' Step 2a: Use cifti-separate to separate cifti_orig into components (volumetric values and labels, surface values for each hemisphere, and ROI values for each hemisphere)
# #' Step 2b: Use metric-resample to resample the surfaces and ROIs created in Step 2a.
# #' Step 3: Use cifti-create to form a template CIFTI file in the target resolution based on the components created in Steps 2a and 2b.
# #' Step 4: Use cifti-resample to resample cifti_orig to the target resolution
# #'
# cifti_resample_separate <- function(c_original_fnames, c_target_fnames=NULL, res_target, 
#   sphereL_origin_fname, sphereR_origin_fname, 
#   sphereL_target_fname="Sphere.target.R.surf.gii", sphereR_target_fname="Sphere.target.L.surf.gii",
#   sphere_target_dir=NULL, sphere_target_keep=FALSE, overwrite=TRUE, write_dir=NULL, verbose=TRUE, wb_dir=NULL){

#   wb_cmd <- get_wb_cmd_path(wb_dir)

#   # Check arguments.
#   if(length(c_original_fnames == 1) & grepl("\\.d.*.nii$", cifti_fname)){ # .d*.nii or .d*.nii where * is tseries or scalar
#     c_original_fnames <- list("cortexL", "cortexR", "subcortVol", "subcortLab")
#     names(c_original_fnames) <- c_original_fnames
#     c_original_fnames <- sapply(c_original_fnames, cifti_separate_default_suffix)
#   } else {
#     names(c_original_fnames) <- match.arg(names(c_original_fnames), c("cortexL", "cortexR", "subcortVol", "subcortLab"), several.ok=TRUE)
#     stopifnot(length(unique(names(c_original_fnames))) == length(c_original_fnames))
#   }
#   stopifnot(is.numeric(res_target))
#   # surf[L/R]
#   if(identical(c_target_fnames['cortexL'], NULL)){ c_target_fnames['cortexL'] <- file.path(write_dir, 'surf.target.L.func.gii') }
#   if(identical(c_target_fnames['cortexR'], NULL)){ c_target_fnames['cortexR'] <- file.path(write_dir, 'surf.target.R.func.gii') }
#   if(is.null(sphere_target_dir)){
#     sphere_target_dir <- ifelse(sphere_target_keep, file.path(write_dir, "helper_files_resampling"), tempdir())
#   } else if(!file.exists(sphere_target_dir)){ 
#     dir.create(sphere_target_dir)
#   } 
#   read_dir <- check_dir(read_dir)
#   write_dir <- check_dir(write_dir, make=TRUE)
#   sphere_target_dir <- check_dir(sphere_target_dir, "helper_files_resampling", make=TRUE)

#   # Step 1. Create new spherical GIFTI files using wb_command -surface-create-sphere
#   if(!isAbsolutePath(sphereR_target_fname)){ sphereR_target_fname <- file.path(sphere_target_dir, sphereR_target_fname) }
#   if(!isAbsolutePath(sphereL_target_fname)){ sphereL_target_fname <- file.path(sphere_target_dir, sphereL_target_fname) }

#   sphere_target_exists <- file.exists(sphereR_target_fname, sphereL_target_fname)
#   if(sum(sphere_target_exists) == 1){ warning("One sphere target file exists but not the other. Overwriting the existing file.") }
#   if(sphere_target_overwrite | !all(sphere_target_exists)){
#     if(verbose) cat("\nCreating spherical surfaces in target resolution... \n")
#     make_helper_spheres(sphereR_target_fname, sphereL_target_fname, res_target, wb_cmd)
#   }

#   # Question: why 10k?
#   #gifti_target_L <- file.path(outdir, paste0('gifti',ii,'.target.L.surf.gii'))
#   validroiL_target <- file.path(write_dir, "valid-roi.10k.L.func.gii")
#   validroiR_target <- file.path(write_dir, "valid-roi.10k.R.func.gii")
#   #roiL_target <- file.path(sphere_target_dir, "roi.10k.L.func.gii")
#   #roiR_target <- file.path(sphere_target_dir, "roi.10k.R.func.gii")
#   surfL_target <- file.path(sphere_target_dir, "surf.target.L.func.gii")
#   surfR_target <- file.path(sphere_target_dir, "surf.target.R.func.gii")
#   if(make_helper_files){
#     if(verbose) cat("Resampling components to target resolution... \n")
#     system(paste(wb_cmd, "-metric-resample", c_original_fnames['cortexL'], sphereL_origin_fname, sphereL_target_fname, 
#       "BARYCENTRIC", c_target_fnames['cortexL'], "-current-roi", roiL_origin, "-valid-roi-out", validroiL_target, sep=" "))
#     system(paste(wb_cmd, "-metric-resample", c_target_fnames['cortexR'], sphereR_origin_fname, sphereR_target_fname, 
#       "BARYCENTRIC", c_target_fnames['cortexR'], "-current-roi", roiR_origin, "-valid-roi-out", validroiR_target, sep=" "))
#     # original with ROI
#     #system(paste(wb_cmd, "-metric-resample", c_original_fnames['cortexL'], sphereL_origin_fname, sphereL_target_fname, 
#     #  "BARYCENTRIC", c_target_fnames['cortexL'], "-current-roi", roiL_origin, "-valid-roi-out", validroiL_target, sep=" "))
#     #system(paste(wb_cmd, "-metric-resample", c_target_fnames['cortexR'], sphereR_origin_fname, sphereR_target_fname, 
#     #  "BARYCENTRIC", c_target_fnames['cortexR'], "-current-roi", roiR_origin, "-valid-roi-out", validroiR_target, sep=" "))
#     #system(paste(wb_cmd, "-metric-resample", roiL_origin,  sphereL_origin_fname, sphereL_target_fname, 
#     #  "BARYCENTRIC", roiL_target, sep=" "))
#     #system(paste(wb_cmd, "-metric-resample", roiR_origin,  sphere_orig_R, sphereR_target_fname, 
#     #  "BARYCENTRIC", roiR_target, sep=" "))
#   }

#   # Step 3. Create a template CIFTI file (e.g., *.dtseries.nii) in the target resolution

#   cifti_extn <- get_cifti_extn(cifti_orig)
#   cifti_template_target <- paste0("cifti.target.",cifti_extn)
#   # Step 3. Create a template CIFTI dense timeseries.
#   cifti_template_target <- file.path(sphere_target_dir, cifti_template_target)
#   # TO DO: switch
#   if(grepl("dtseries",cifti_extn)) create_cmd <- "-cifti-create-dense-timeseries"
#   if(grepl("dscalar",cifti_extn)) create_cmd <- "-cifti-create-dense-scalar"
#   if(grepl("dlabel",cifti_extn)) create_cmd <- "-cifti-create-label"
#   if(make_helper_files) {
#     if(verbose) cat("Creating template CIFTI file in target resolution... \n")
#     system(paste(wb_cmd, create_cmd, cifti_template_target, "-volume", vol_orig, labels_orig, "-left-metric", surfL_target, "-roi-left", roiL_target, "-right-metric", surfR_target, "-roi-right", roiR_target, sep=" "))
#   }


#   # Step 4.  Perform resampling with cifti-resample

#   #delete existing cifti_target
#   if(file.exists(cifti_target) & overwrite==TRUE) file.remove(cifti_target)

#   #peform resampling
#   if(verbose) cat("Resampling cifti_orig to target resolution... \n")
#   cmd = paste(wb_cmd, "-cifti-resample", cifti_orig, "COLUMN", cifti_template_target, "COLUMN BARYCENTRIC CUBIC", cifti_target, "-left-spheres",  sphereL_origin_fname, sphereL_target_fname, "-right-spheres", sphere_orig_R, sphereR_target_fname, sep=" ")
#   system(cmd)

#   #check whether operation successful and return result
#   if(verbose) cat("Checking if resampled CIFTI was written. \n")
#   file.exists(cifti_target)
# }

# #' Resample GIFTI data
# #'
# #' @details Uses Connectome Workbench surface-resample command to resample a surface file,
# #'     given two spherical surfaces that are in register. The BARYCENTRIC method is used
# #'     as it is generally recommended for anatomical surfaces, in order to minimize smoothing.
# #'
# #' @description Performs spatial resampling of GIFTI shape file (*.surf.gii)
# #'
# #' @param gifti_orig File path of GIFTI surface file in original resolution to resample
# #' @param gifti_target File path of GIFTI file in target resolution to be created
# #' @param sphere_orig File path of GIFTI sphere surface with the mesh that gifti_orig is currently on
# #' @param sphere_target File path of GIFTI sphere surface that is in register with sphere_orig and has the desired output mesh
# #' @param wb_cmd Path to Connectome Workbench executable file, ending in 'wb_command' (Mac/linux) or 'wb_command.exe' (Windows).
# #' @param make_helper_files If TRUE, make all the helper files required for resampling. Otherwise, all necessary helper files must be located in a subdirectory of outdir or current working directory named 'helper_files_resampling'.
# #' @param delete_helper_files If make_helper_files=TRUE, logical indicating whether those files should be deleted after resampling.
# #' @param overwrite Logical indicating whether gifti_target should be overwritten if it already exists.
# #'
# #' @return Logical indicating whether resampled GIFTI file was created.
# #' @export
# #'
# gifti_resample <- function(gifti_orig, gifti_target, sphere_orig, sphere_target, wb_cmd, make_helper_files=TRUE, delete_helper_files=FALSE, overwrite=FALSE){

#   #TO DO: Update this function with similar changes to cifti_resample (e.g. use of outdir argument, delete created files)
  
#   if(!file.exists(wb_cmd)) stop(paste0(wb_cmd, ' does not exist.  Check path and try again.'))

#   if(!file.exists(wb_cmd)) stop(paste0(wb_cmd, ' does not exist.  Check path and try again.'))

#   if(file.exists(gifti_target)){
#     if(!overwrite){
#       message('gifti_target already exists and will not be overwritten.  Set overwrite=TRUE to overwrite.')
#       return(NULL)
#     } else {
#       warning('gifti_target already exists. It will be overwritten since overwrite=TRUE.')
#     }
#   }

#   #delete existing gifti_target
#   if(file.exists(gifti_target) & overwrite==TRUE) file.remove(gifti_target)

#   if(is.null(outdir)){
#     outdir <- getwd()
#   } else if(!file.exists(outdir)){
#     stop('outdir does not exist, check and try again.')
#   }
#   #TO DO: Check that the user has write permissions in outdir
  
#   #TO DO: Test this line of code for Windows systems.  May need to try both options for winslash argument with a tryCatch or if statement to keep the one that works.
#   outdir <- normalizePath(outdir) #generate full path

    
#   #location of helper files
#   if(make_helper_files==FALSE & delete_helper_files==TRUE){
#     warning('Since make_helper_files is FALSE, I will not delete the helper files.  Setting delete_helper_files to FALSE.')
#     delete_helper_files <- FALSE
#   }
#   if(make_helper_files==FALSE) dir2 <- file.path(outdir,'helper_files_resampling') #expected location of existing helper files
#   if(make_helper_files==TRUE){
#     if(delete_helper_files) dir2 <- tempdir() #if helper files will be deleted, put them in a temporary directory
#     if(!delete_helper_files){
#       dir2 <- file.path(outdir,'helper_files_resampling') #location to save helper files
#       if(dir.exists(dir2)) warning('helper_files_resampling directory already exists and make_helper_files==TRUE. Helper files may be overwritten.')
#       if(!dir.exists(dir2)) dir.create(dir2) #create directory to make and keep helper files
#     }
#   }
  
#   # Step 1. Create new spherical GIFTI files using wb_command -surface-create-sphere
#   sphereR_target_fname <- file.path(dir2,'Sphere.target.R.surf.gii')
#   sphereL_target_fname <- file.path(dir2,'Sphere.target.L.surf.gii')
#   if(make_helper_files) {
#     if(verbose) cat('\nCreating spherical surfaces in target resolution... \n')
#     make_helper_spheres(sphereR_target_fname, sphereL_target_fname, target_res, wb_cmd)
#   }
  
#   #peform resampling
#   cmd = paste(wb_cmd, '-surface-resample', gifti_orig, sphere_orig, sphere_target, 'BARYCENTRIC', gifti_target, sep=' ')
#   system(cmd)

#   #check whether operation successful and return result
#   file.exists(gifti_target)
# }


# #' Generates GIFTI sphere surface files
# #'
# #' @description Generates GIFTI spherical surface files in a target resolution. These are required for resampling CIFTI
# #' and GIFTI files.  This function generates a pair of vertex-matched left and right spheres in the target resolution.
# #'
# #' @param sphereR_fname File path to right-hemisphere spherical GIFTI to be created
# #' @param sphereL_fname File path to left-hemisphere spherical GIFTI to be created
# #' @param target_res Target resolution of spherical GIFTIs to be created (approximate number of vertices per hemisphere)
# #' @param write_dir If a file name is relative, what directory should it be saved to? Defaults to the current working directory.
# #' @param wb_dir (Optional) Path to Connectome Workbench folder. If not provided, should be set with 
# #'  \code{ciftiTools.setOption('wb_path', 'path/to/workbench')}.
# #'
# #' @return Logical indicating whether output files exist
# #' @export
# #'
# make_helper_spheres <- function(sphereR_fname, sphereL_fname, target_res, write_dir=NULL, wb_dir=NULL){

#   wb_cmd <- get_wb_cmd_path(wb_dir)

#   # Check that write_dir is valid. Use the current working directory if no write_dir is given.
#   write_dir <- check_dir(write_dir)
  
#   sphereR_fname <- make_abs_path(sphereR_fname, write_dir)
#   sphereL_fname <- make_abs_path(sphereL_fname, write_dir)

#   system(paste(wb_cmd,'-surface-create-sphere', target_res, sphereR_fname, sep=' '))
#   system(paste(wb_cmd, '-surface-flip-lr', sphereR_fname, sphereL_fname, sep=' '))
#   system(paste(wb_cmd, '-set-structure', sphereR_fname, 'CORTEX_RIGHT', sep=' '))
#   system(paste(wb_cmd, '-set-structure', sphereL_fname, 'CORTEX_LEFT', sep=' '))

#   file.exists(sphereR_fname) & file.exists(sphereL_fname)
# }

