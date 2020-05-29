#' Resample CIFTI data
#'
#' @description Performs spatial resampling of CIFTI data on the cortical surface
#'
#' @param cifti_orig File path of CIFTI file in original resolution
#' @param cifti_target File path of CIFTI file in target resolution to be created
#' @param sphere_orig_L File path of left-hemisphere spherical GIFTI files in original resolution (compatible with cifti_orig)
#' @param sphere_orig_R File path of right-hemisphere spherical GIFTI files in original resolution (compatible with cifti_orig)
#' @param target_res Target resolution (number of cortical surface vertices per hemisphere)
#' @param wb_cmd Path to Connectome Workbench executable file, ending in 'wb_command' (Mac/linux) or 'wb_command.exe' (Windows).
#' @param make_helper_files If TRUE, make all the helper files required for resampling. Otherwise, all necessary helper files must be located in a subdirectory of current working directory named 'helper_files_resampling'.
#' @param delete_helper_files If make_helper_files=TRUE, logical indicating whether those files should be deleted after resampling.
#' @param overwrite Logical indicating whether cifti_target should be overwritten if it already exists.
#' @param verbose Logical indicating whether progress updates should be displayed
#'
#' @return Logical indicating whether resampled CIFTI file was created.
#' @export
#'
#' @details Performs resampling of CIFTI files using Connectome Workbench tools.  Several helper files must be created:
#'
#' Step 1: Create sphere_target_R and sphere_target_L, spherical GIFTI surface files for each hemisphere in target resolution.
#' Step 2a: Use cifti-separate to separate cifti_orig into components (volumetric values and labels, surface values for each hemisphere, and ROI values for each hemisphere)
#' Step 2b: Use metric-resample to resample the surfaces and ROIs created in Step 2a.
#' Step 3: Use cifti-create to form a template CIFTI file in the target resolution based on the components created in Steps 2a and 2b.
#' Step 4: Use cifti-resample to resample cifti_orig to the target resolution
#'
cifti_resample <- function(cifti_orig, cifti_target, sphere_orig_L, sphere_orig_R, target_res, wb_cmd=NULL, make_helper_files=TRUE, delete_helper_files=FALSE, overwrite=FALSE, verbose=TRUE){

  wb_cmd <- check_wb_cmd(wb_cmd)

  if(file.exists(cifti_target)){
    if(!overwrite){
      message('cifti_target already exists and will not be overwritten.  Set overwrite=TRUE to overwrite.')
      return(NULL)
    } else {
      warning('cifti_target already exists. It will be overwritten since overwrite=TRUE.')
    }
  }

  #location of cifti_orig and write location of cifti_target
  dir <- dirname(cifti_orig)

  #location of helper files
  if(make_helper_files==FALSE & delete_helper_files==TRUE){
    warning('Since make_helper_files is FALSE, I will not delete the helper files.  Setting delete_helper_files to FALSE.')
    delete_helper_files <- FALSE
  }
  if(make_helper_files==FALSE) dir2 <- 'helper_files_resampling' #expected location of existing helper files
  if(make_helper_files==TRUE){
    if(delete_helper_files) dir2 <- tempdir() #if helper files will be deleted, put them in a temporary directory
    if(!delete_helper_files){
      dir2 <- 'helper_files_resampling' #location to save helper files
      if(dir.exists(dir2)) warning('helper_files_resampling directory already exists and make_helper_files==TRUE. Helper files may be overwritten.')
      if(!dir.exists(dir2)) dir.create(dir2) #create directory to make and keep helper files
    }
  }

  # Step 1. Create new spherical GIFTI files using wb_command -surface-create-sphere
  sphere_target_R <- file.path(dir2,'Sphere.target.R.surf.gii')
  sphere_target_L <- file.path(dir2,'Sphere.target.L.surf.gii')
  if(make_helper_files) {
    if(verbose) cat('\nCreating spherical surfaces in target resolution... \n')
    make_helper_spheres(sphere_target_R, sphere_target_L, target_res, wb_cmd)
  }

  # Step 2. Create CIFTI space with target number of vertices

  # First, use -cifti-separate to get separate CORTEX_LEFT, CORTEX_RIGHT and both medial walls in original resolution

  vol_orig <- file.path(dir2, 'vol.orig.nii')
  labels_orig <- file.path(dir2, 'labels.orig.nii')
  surf_orig_L <- file.path(dir2, 'surf.orig.L.func.gii')
  surf_orig_R <- file.path(dir2, 'surf.orig.R.func.gii')
  roi_orig_L <- file.path(dir2, 'roi.orig.L.func.gii')
  roi_orig_R <- file.path(dir2, 'roi.orig.R.func.gii')
  if(make_helper_files) {
    if(verbose) cat('Separating cifti_orig into components... \n')
    system(paste(wb_cmd, '-cifti-separate', cifti_orig, 'COLUMN -volume-all', vol_orig, '-label', labels_orig, '-metric CORTEX_LEFT', surf_orig_L, '-roi', roi_orig_L, '-metric CORTEX_RIGHT', surf_orig_R, '-roi', roi_orig_R, sep=' '))
  }

  # Then, use wb_commmand -metric-resample to create versions of these in the target resolution

  validroi_target_L <- file.path(dir2, 'valid-roi.10k.L.func.gii')
  validroi_target_R <- file.path(dir2, 'valid-roi.10k.R.func.gii')
  roi_target_L <- file.path(dir2, 'roi.10k.L.func.gii')
  roi_target_R <- file.path(dir2, 'roi.10k.R.func.gii')
  surf_target_L <- file.path(dir2, 'surf.target.L.func.gii')
  surf_target_R <- file.path(dir2, 'surf.target.R.func.gii')
  if(make_helper_files){
    if(verbose) cat('Resampling components to target resolution... \n')
    system(paste(wb_cmd, '-metric-resample', surf_orig_L, sphere_orig_L, sphere_target_L, 'BARYCENTRIC', surf_target_L, '-current-roi', roi_orig_L, '-valid-roi-out', validroi_target_L, sep=' '))
    system(paste(wb_cmd, '-metric-resample', surf_orig_R, sphere_orig_R, sphere_target_R, 'BARYCENTRIC', surf_target_R, '-current-roi', roi_orig_R, '-valid-roi-out', validroi_target_R, sep=' '))
    system(paste(wb_cmd, '-metric-resample', roi_orig_L,  sphere_orig_L, sphere_target_L, 'BARYCENTRIC', roi_target_L, sep=' '))
    system(paste(wb_cmd, '-metric-resample', roi_orig_R,  sphere_orig_R, sphere_target_R, 'BARYCENTRIC', roi_target_R, sep=' '))
  }

  # Step 3. Create a template CIFTI file (e.g., *.dtseries.nii) in the target resolution

  cifti_extn <- get_cifti_extn(cifti_orig)
  cifti_template_target <- paste0('cifti.target.',cifti_extn)
  # Step 3. Create a template CIFTI dense timeseries.
  cifti_template_target <- file.path(dir2, cifti_template_target)
  if(grepl('dtseries',cifti_extn)) create_cmd <- '-cifti-create-dense-timeseries'
  if(grepl('dscalar',cifti_extn)) create_cmd <- '-cifti-create-dense-scalar'
  if(grepl('dlabel',cifti_extn)) create_cmd <- '-cifti-create-label'
  if(make_helper_files) {
    if(verbose) cat('Creating template CIFTI file in target resolution... \n')
    system(paste(wb_cmd, create_cmd, cifti_template_target, '-volume', vol_orig, labels_orig, '-left-metric', surf_target_L, '-roi-left', roi_target_L, '-right-metric', surf_target_R, '-roi-right', roi_target_R, sep=' '))
  }


  # Step 4.  Perform resampling with cifti-resample

  #delete existing cifti_target
  if(file.exists(cifti_target) & overwrite==TRUE) file.remove(cifti_target)

  #peform resampling
  if(verbose) cat('Resampling cifti_orig to target resolution... \n')
  cmd = paste(wb_cmd, '-cifti-resample', cifti_orig, 'COLUMN', cifti_template_target, 'COLUMN BARYCENTRIC CUBIC', cifti_target, '-left-spheres',  sphere_orig_L, sphere_target_L, '-right-spheres', sphere_orig_R, sphere_target_R, sep=' ')
  system(cmd)

  #check whether operation successful and return result
  if(verbose) cat('Checking if resampled CIFTI was written. \n')
  file.exists(cifti_target)
}

#' Resample GIFTI data
#'
#' @details Uses Connectome Workbench surface-resample command to resample a surface file,
#'     given two spherical surfaces that are in register. The BARYCENTRIC method is used
#'     as it is generally recommended for anatomical surfaces, in order to minimize smoothing.
#'
#' @description Performs spatial resampling of GIFTI shape file (*.surf.gii)
#'
#' @param gifti_orig File path of GIFTI surface file in original resolution to resample
#' @param gifti_target File path of GIFTI file in target resolution to be created
#' @param sphere_orig File path of GIFTI sphere surface with the mesh that gifti_orig is currently on
#' @param sphere_target File path of GIFTI sphere surface that is in register with sphere_orig and has the desired output mesh
#' @param wb_cmd Path to Connectome Workbench executable file, ending in 'wb_command' (Mac/linux) or 'wb_command.exe' (Windows).
#' @param overwrite Logical indicating whether gifti_target should be overwritten if it already exists.
#'
#' @return Logical indicating whether resampled GIFTI file was created.
#' @export
#'
gifti_resample <- function(gifti_orig, gifti_target, sphere_orig, sphere_target, wb_cmd, overwrite=FALSE){

  if(file.exists(gifti_target)){
    if(!overwrite){
      message('gifti_target already exists and will not be overwritten.  Set overwrite=TRUE to overwrite.')
      return(NULL)
    } else {
      warning('gifti_target already exists. It will be overwritten since overwrite=TRUE.')
    }
  }

  #delete existing gifti_target
  if(file.exists(gifti_target) & overwrite==TRUE) file.remove(gifti_target)

  #peform resampling
  cmd = paste(wb_cmd, '-surface-resample', gifti_orig, sphere_orig, sphere_target, 'BARYCENTRIC', gifti_target, sep=' ')
  system(cmd)

  #check whether operation successful and return result
  file.exists(gifti_target)
}


#' Generates GIFTI sphere surface files
#'
#' @description Generates GIFTI spherical surface files in a target resolution. These are required for resampling CIFTI
#' and GIFTI files.  This function generates a pair of vertex-matched left and right spheres in the target resolution.
#'
#' @param sphere_R File path to right-hemisphere spherical GIFTI to be created
#' @param sphere_L File path to left-hemisphere spherical GIFTI to be created
#' @param target_res Target resolution of spherical GIFTIs to be created (approximate number of vertices per hemisphere)
#' @param wb_cmd Path to Connectome Workbench executable file, ending in 'wb_command' (Mac/linux) or 'wb_command.exe' (Windows).
#'
#' @return Logical indicating whether output files exist
#' @export
#'
make_helper_spheres <- function(sphere_R, sphere_L, target_res, wb_cmd){
  system(paste(wb_cmd,'-surface-create-sphere', target_res, sphere_R, sep=' '))
  system(paste(wb_cmd, '-surface-flip-lr', sphere_R, sphere_L, sep=' '))
  system(paste(wb_cmd, '-set-structure', sphere_R, 'CORTEX_RIGHT', sep=' '))
  system(paste(wb_cmd, '-set-structure', sphere_L, 'CORTEX_LEFT', sep=' '))

  file.exists(sphere_R) & file.exists(sphere_L)
}

