# visual studio: ctrl k + u to uncomment

# #' Resample CIFTI data
# #'
# #' @description Performs spatial resampling of CIFTI data on the cortical surface
# #'
# #' @param cifti_origin_files File path of CIFTI file in original resolution
# #' @param cifti_target_files File path of CIFTI file in target resolution to be created
# #' @param res_target Target resolution (number of cortical surface vertices per hemisphere)
# #' @param sphereL_origin_fname File path of left-hemisphere spherical GIFTI files in original resolution (compatible with cifti_orig)
# #' @param sphereR_origin_fname File path of right-hemisphere spherical GIFTI files in original resolution (compatible with cifti_orig)
# #' @param sphereL_target_fname File path of left-hemisphere spherical GIFTI files in Targetinal resolution (compatible with cifti_Target)
# #' @param sphereR_target_fname File path of right-hemisphere spherical GIFTI files in Targetinal resolution (compatible with cifti_Target)
# #' @param helper_dir Helper files to use. If not provided, these will be created.
# #' @param helper_keep Should helper files be deleted at the end of this function call, if they were created?
# #' @param overwrite Logical indicating whether cifti_target should be overwritten if it already exists.
# #' @param write_dir Location where to write output files. If NULL, use the current working directory.
# #' @param verbose Logical indicating whether progress updates should be displayed
# #' @param wb_cmd Path to Connectome Workbench executable file, ending in 'wb_command' (Mac/linux) or 'wb_command.exe' (Windows).
# #'
# #' @return Logical indicating whether resampled CIFTI file was created.
# #' @export
# #'
# #' @details Performs resampling of CIFTI files using Connectome Workbench tools.  Several helper files must be created:
# #'
# #' Step 1: Create sphereR_target_fname and sphereL_target_fname, spherical GIFTI surface files for each hemisphere in target resolution.
# #' Step 2a: Use cifti-separate to separate cifti_orig into components (volumetric values and labels, surface values for each hemisphere, and ROI values for each hemisphere)
# #' Step 2b: Use metric-resample to resample the surfaces and ROIs created in Step 2a.
# #' Step 3: Use cifti-create to form a template CIFTI file in the target resolution based on the components created in Steps 2a and 2b.
# #' Step 4: Use cifti-resample to resample cifti_orig to the target resolution
# #'
# cifti_resample_sep <- function(cifti_origin_files, cifti_target_files, res_target, 
#   sphereL_origin_fname, sphereR_origin_fname, 
#   sphereL_target_fname="Sphere.target.R.surf.gii", sphereR_target_fname="Sphere.target.L.surf.gii",
#   helper_dir=NULL, helper_keep=FALSE, overwrite=TRUE, write_dir=NULL, verbose=TRUE, wb_dir=NULL){

#   wb_cmd <- get_wb_cmd_path(wb_dir)

#   names(cifti_origin_files) <- match.arg(names(cifti_origin_files), c("cortexL", "cortexR", "subcortVol", "subcortLab"), several.ok=TRUE)
#   stopifnot(length(unique(names(cifti_origin_files))) == length(cifti_origin_files))

#   if(is.null(helper_dir)){
#     helper_dir <- ifelse(helper_keep, file.path(write_dir, "helper_files_resampling"), tempdir())
#   } 
#   if(!file.exists(helper_dir)){ dir.create(helper_dir) }

#   if(is.null(write_dir)){ write_dir <- getcwd() }
#   if(!file.exists(write_dir)){ dir.create(write_dir) }

#   #if(dir.exists(dir2)) warning('helper_files_resampling directory already exists and make_helper_files==TRUE. Helper files may be overwritten.')

#   # Step 1. Create new spherical GIFTI files using wb_command -surface-create-sphere
#   if(!isAbsolutePath(sphereR_target_fname)){ sphereR_target_fname <- file.path(helper_dir, sphereR_target_fname) }
#   if(!isAbsolutePath(sphereL_target_fname)){ sphereL_target_fname <- file.path(helper_dir, sphereL_target_fname) }

#   need_helper_files <- !all(file.exists(sphereR_target_fname, sphereL_target_fname))
#   if(need_helper_files) {
#     if(verbose) cat('\nCreating spherical surfaces in target resolution... \n')
#     make_helper_spheres(sphereR_target_fname, sphereL_target_fname, res_target, wb_cmd)
#   }

#   # Question: why 10k?
#   validroiL_target <- file.path(helper_dir, 'valid-roi.10k.L.func.gii')
#   validroiR_target <- file.path(helper_dir, 'valid-roi.10k.R.func.gii')
#   roiL_target <- file.path(helper_dir, 'roi.10k.L.func.gii')
#   roiR_target <- file.path(helper_dir, 'roi.10k.R.func.gii')
#   surfL_target <- file.path(helper_dir, 'surf.target.L.func.gii')
#   surfR_target <- file.path(helper_dir, 'surf.target.R.func.gii')
#   if(make_helper_files){
#     if(verbose) cat('Resampling components to target resolution... \n')
#     system(paste(wb_cmd, '-metric-resample', surfL_origin, sphereL_origin_fname, sphereL_target_fname, 'BARYCENTRIC', surfL_target, '-current-roi', roiL_origin, '-valid-roi-out', validroiL_target, sep=' '))
#     system(paste(wb_cmd, '-metric-resample', surfR_origin, sphere_orig_R, sphereR_target_fname, 'BARYCENTRIC', surfR_target, '-current-roi', roiR_origin, '-valid-roi-out', validroiR_target, sep=' '))
#     system(paste(wb_cmd, '-metric-resample', roiL_origin,  sphereL_origin_fname, sphereL_target_fname, 'BARYCENTRIC', roiL_target, sep=' '))
#     system(paste(wb_cmd, '-metric-resample', roiR_origin,  sphere_orig_R, sphereR_target_fname, 'BARYCENTRIC', roiR_target, sep=' '))
#   }

#   # Step 3. Create a template CIFTI file (e.g., *.dtseries.nii) in the target resolution

#   cifti_extn <- get_cifti_extn(cifti_orig)
#   cifti_template_target <- paste0('cifti.target.',cifti_extn)
#   # Step 3. Create a template CIFTI dense timeseries.
#   cifti_template_target <- file.path(helper_dir, cifti_template_target)
#   if(grepl('dtseries',cifti_extn)) create_cmd <- '-cifti-create-dense-timeseries'
#   if(grepl('dscalar',cifti_extn)) create_cmd <- '-cifti-create-dense-scalar'
#   if(grepl('dlabel',cifti_extn)) create_cmd <- '-cifti-create-label'
#   if(make_helper_files) {
#     if(verbose) cat('Creating template CIFTI file in target resolution... \n')
#     system(paste(wb_cmd, create_cmd, cifti_template_target, '-volume', vol_orig, labels_orig, '-left-metric', surfL_target, '-roi-left', roiL_target, '-right-metric', surfR_target, '-roi-right', roiR_target, sep=' '))
#   }


#   # Step 4.  Perform resampling with cifti-resample

#   #delete existing cifti_target
#   if(file.exists(cifti_target) & overwrite==TRUE) file.remove(cifti_target)

#   #peform resampling
#   if(verbose) cat('Resampling cifti_orig to target resolution... \n')
#   cmd = paste(wb_cmd, '-cifti-resample', cifti_orig, 'COLUMN', cifti_template_target, 'COLUMN BARYCENTRIC CUBIC', cifti_target, '-left-spheres',  sphereL_origin_fname, sphereL_target_fname, '-right-spheres', sphere_orig_R, sphereR_target_fname, sep=' ')
#   system(cmd)

#   #check whether operation successful and return result
#   if(verbose) cat('Checking if resampled CIFTI was written. \n')
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
# #' @param sphere_R File path to right-hemisphere spherical GIFTI to be created
# #' @param sphere_L File path to left-hemisphere spherical GIFTI to be created
# #' @param target_res Target resolution of spherical GIFTIs to be created (approximate number of vertices per hemisphere)
# #' @param wb_cmd Path to Connectome Workbench executable file, ending in 'wb_command' (Mac/linux) or 'wb_command.exe' (Windows).
# #'
# #' @return Logical indicating whether output files exist
# #' @export
# #'
# make_helper_spheres <- function(sphere_R, sphere_L, target_res, wb_cmd){

#   if(!file.exists(wb_cmd)) stop(paste0(wb_cmd, ' does not exist.  Check path and try again.'))

#   system(paste(wb_cmd,'-surface-create-sphere', target_res, sphere_R, sep=' '))
#   system(paste(wb_cmd, '-surface-flip-lr', sphere_R, sphere_L, sep=' '))
#   system(paste(wb_cmd, '-set-structure', sphere_R, 'CORTEX_RIGHT', sep=' '))
#   system(paste(wb_cmd, '-set-structure', sphere_L, 'CORTEX_LEFT', sep=' '))

#   file.exists(sphere_R) & file.exists(sphere_L)
# }

