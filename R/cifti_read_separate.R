#' Reads in CIFTI/GIFTI data and (optionally) resamples  
#'
#' @description Separates CIFTI data into cortical (left and right) and subcortical structures and reads in the data within each structure. Optionally reads in gifti surface model files also. Optionally resamples cortical and surface model data.
#'
#' @param fname_cifti File path of CIFTI-format data (ending in .d*.nii).
#' @param fname_gifti_left (Optional) File path, or vector of multiple file paths, of GIFTI surface geometry file representing left cortex
#' @param fname_gifti_right (Optional) File path, or vector of multiple file paths, of GIFTI surface geometry file representing right cortex
#' @param surf_names Character vector containing descriptive names of each GIFTI surface geometry provided (e.g. midthickness, inflated, etc.). Should match the length of fname_gifti_left and/or fname_gifti_left if they are provided. Otherwise, ignored.
#' @param brainstructures A vector indicating which brain structure(s) to include: 'left' (left cortical surface), 'right' (right cortical surface), and/or 'subcortical' (subcortical and cerebellar gray matter)
#' @param wb_cmd Path to Connectome Workbench executable file, ending in 'wb_command' (Mac/linux) or 'wb_command.exe' (Windows).
#' @param make_helper_files If TRUE, make all the helper files required for resampling. Otherwise, all necessary helper files must be located in a subdirectory of current working directory named 'helper_files_resampling'.
#' @param delete_helper_files If make_helper_files=TRUE, logical indicating whether those files should be deleted after resampling.
#' @param outdir Location where to write output files. If NULL, use the current working directory.
#' @param resample Target resolution for resampling (number of cortical surface vertices per hemisphere). If NULL, do not perform resampling.
#' @param sphere_orig_L File path of left-hemisphere spherical GIFTI files in original resolution (compatible with cifti_orig). Must be provided if resample provided.
#' @param sphere_orig_R File path of right-hemisphere spherical GIFTI files in original resolution (compatible with cifti_orig). Must be provided if resample provided.
#' @param verbose Should occasional updates be printed?
#'
#' @return An object of type 'cifti', a list containing up to 4 elements: CORTEX_LEFT, CORTX_RIGHT, VOL and LABELS.  LABELS contains the brain structure labels (usually 3-21) of the subcortical elements.
#' @export
#' @importFrom gifti readGIfTI
#' @importFrom oro.nifti readNIfTI
#'
#' @details This function uses a system wrapper for the 'wb_command' executable. The user must first download and install the Connectome Workbench,
#' available from https://www.humanconnectome.org/software/get-connectome-workbench. The 'wb_cmd' argument is the full file path to the 'wb_command' executable file.
#'
#' The subcortical brain structure labels (LABELS element of returned list) take values 3-21 and represent:
#' 3 Accumbens-L
#' 4 Accumbens-R
#' 5 Amygdala-L
#' 6 Amygdala-R
#' 7 Brain Stem
#' 8 Caudate-L
#' 9 Caudate-R
#' 10 Cerebellum-L
#' 11 Cerebellum-R
#' 12 Diencephalon-L
#' 13 Diencephalon-R
#' 14 Hippocampus-L
#' 15 Hippocampus-R
#' 16 Pallidum-L
#' 17 Pallidum-R
#' 18 Putamen-L
#' 19 Putamen-R
#' 20 Thalamus-L
#' 21 Thalamus-R
#'
cifti_read_separate <- function(fname_cifti, fname_gifti_left=NULL, fname_gifti_right=NULL, 
  surf_names=NULL, brainstructures=c('left','right','subcortical'), wb_cmd=NULL, 
  make_helper_files=TRUE, delete_helper_files=FALSE, outdir=NULL, 
  resample=NULL, sphere_orig_L, sphere_orig_R, verbose=FALSE) {

  wb_cmd <- get_wb_cmd_path(wb_cmd)
  if (!file.exists(wb_cmd)) stop(paste0(wb_cmd, ' does not exist.  Check path and try again.'))

  if (is.null(outdir)) {
    outdir <- getwd()
  } else if (!file.exists(outdir)) {
    stop('outdir does not exist, check and try again.')
  }
  #TO DO: Check that the user has write permissions in outdir
  
  #TO DO: Test this line of code for Windows systems.  May need to try both options for winslash argument with a tryCatch or if statement to keep the one that works.
  outdir <- normalizePath(outdir) #generate full path
  
  do_left <- ('left' %in% brainstructures)
  do_right <- ('right' %in% brainstructures)
  do_sub <- ('subcortical' %in% brainstructures)

  ### Check surface argument compatibility
  do_left_surf <- (!is.null(fname_gifti_left))
  do_right_surf <- (!is.null(fname_gifti_right))
  if (do_left_surf) { 
    if (is.null(surf_names)) surf_names <- paste0('surface', 1:length(fname_gifti_left))
    if (length(fname_gifti_left) != length(surf_names)) stop('Length of fname_gifti_left and surf_names must match.') 
  }
  if (do_right_surf) { 
    if (is.null(surf_names)) surf_names <- paste0('surface', 1:length(fname_gifti_right))
    if (length(fname_gifti_right) != length(surf_names)) stop('Length of fname_gifti_right and surf_names must match.') 
  }
  num_surf <- length(surf_names) #number of surface types provided

  ### Outline of steps:
  ### 1. Use -cifti-separate to separate the CIFTI file into left cortex, right cortex, subcortical volumetric data, and subcortical labels
  ### 2. If resampling is to be performed:
  ###    a) Generate spheres in target resolution using -surface-create-sphere
  ###    b) Use -metric-resample to resample surface/cortex files into target resolution
  ### 3. Read in each surface and subcortical data/labels
  
  ############################################################
  ### 1. Use -cifti-separate to separate the CIFTI file into left cortex, right cortex, subcortical volumetric data, and subcortical labels
  
  ### Check whether original cifti file exists
  if (!file.exists(fname_cifti)) stop('fname_cifti does not exist')
  
  # Make full file paths
  fname_cifti <- normalizePath(fname_cifti)
  if (do_left_surf) fname_gifti_left <- normalizePath(fname_gifti_left) 
  if (do_right_surf) fname_gifti_right <- normalizePath(fname_gifti_right) 
  if (!is.null(resample)) sphere_orig_L <- normalizePath(sphere_orig_L)
  if (!is.null(resample)) sphere_orig_R <- normalizePath(sphere_orig_R)
  
  # Create file names for separated cifti components
  basename_cifti <- basename(fname_cifti) #extract file name component of file path to cifti data
  extn <- get_cifti_extn(fname_cifti)  #get extension of cifti file (e.g. "dtseries.nii", "dscalar.nii")
  if (do_left) surf_L <- gsub(extn,'L.func.gii',basename_cifti, fixed=TRUE)
  if (do_right) surf_R <- gsub(extn,'R.func.gii',basename_cifti, fixed=TRUE)
  if (do_sub) {
    fname_vol <- gsub(extn,'nii',basename_cifti, fixed=TRUE)
    fname_labels <- gsub(extn,'labels.nii',basename_cifti, fixed=TRUE)
  }
  
  ### Check whether separated files already exist in outdir <-- THIS IS DANGEROUS. HCP SUBJECTS ALL HAVE SAME FILE NAME, SO SEPARATED FILE COULD EXIST BUT FOR ANOTHER SUBJECT/SESSION.
  #need_left <- need_right <- need_sub <- FALSE
  #all_files <- list.files(outdir)
  #if (do_left) if (!(surf_L %in% all_files)) need_left <- TRUE
  #if (do_right) if (!(surf_R %in% all_files)) need_right <- TRUE
  #if (do_sub) if (!(fname_vol %in% all_files & fname_labels %in% all_files)) need_sub <- TRUE

  ### Construct system command to create needed files

  if (do_left) surf_L <- file.path(outdir,surf_L)
  if (do_right) surf_R <- file.path(outdir,surf_R)
  if (do_sub) fname_vol <- file.path(outdir,fname_vol)
  if (do_sub) fname_labels <- file.path(outdir,fname_labels)
  
  #TO DO: Deal with the possibility of spaces in file paths. 
  #I tried adding quotes around the file paths, but that resulted in an error (e.g., "Unable to open ~/tfMRI_MOTOR_LR_Atlas.L.func.gii for writing.")
  #We may be able to add a "\ " to indicate an explicit space in the file path
  #Would need to make sure it works on Windows and Linux

  cmd_left <- cmd_right <- cmd_sub <- NULL
  if (do_left) cmd_left <- paste('-metric CORTEX_LEFT', surf_L, sep=' ')
  if (do_right) cmd_right <- paste('-metric CORTEX_RIGHT', surf_R, sep=' ')
  if (do_sub) cmd_sub <- paste('-volume-all', fname_vol, '-label', fname_labels, sep=' ')

  if (do_left | do_right | do_sub) {
    cmd <- paste(wb_cmd, '-cifti-separate', fname_cifti, 'COLUMN', cmd_left, cmd_right, cmd_sub, sep=' ')
    system(cmd)
  }
  
  #TO DO: Considering using system2() instead of system()
  
  ############################################################
  ### 2. If resampling is to be performed:
  ###    a) Generate spheres in target resolution
  ###    b) Use -metric-resample to resample surface/cortex files into target resolution
  ###    c) Use -surface-resample to resample gifti files (if provided) into target resolution

  if (!is.null(resample)) {
    
    #location of helper files
    if (make_helper_files==FALSE & delete_helper_files==TRUE) {
      warning('Since make_helper_files is FALSE, I will not delete the helper files.  Setting delete_helper_files to FALSE.')
      delete_helper_files <- FALSE
    }
    if (make_helper_files==FALSE) dir2 <- file.path(outdir,'helper_files_resampling') #expected location of existing helper files
    if (make_helper_files==TRUE) {
      if (delete_helper_files) dir2 <- tempdir() #if helper files will be deleted, put them in a temporary directory
      if (!delete_helper_files) {
        dir2 <- file.path(outdir,'helper_files_resampling') #location to save helper files
        if (dir.exists(dir2)) warning('helper_files_resampling directory already exists and make_helper_files==TRUE. Helper files may be overwritten.')
        if (!dir.exists(dir2)) dir.create(dir2) #create directory to make and keep helper files
      }
    }
    
    #a) Generate spheres in target resolution using -surface-create-sphere
    sphere_target_R <- file.path(dir2,'Sphere.target.R.surf.gii')
    sphere_target_L <- file.path(dir2,'Sphere.target.L.surf.gii')
    if (make_helper_files) {
      if (verbose) cat('Creating spherical surfaces in target resolution... \n')
      make_helper_spheres(sphere_target_R, sphere_target_L, target_res=resample, wb_cmd)
    }
    
    
    #b) Use -metric-resample to resample surface/cortex files into target resolution
    
    ## TO DO: Use cifti_resample function (or a simplified version of it) for this part?
    
    surf_target_L <- file.path(outdir, 'surf.target.L.func.gii')
    surf_target_R <- file.path(outdir, 'surf.target.R.func.gii')

    if (do_left) {
      if (verbose) cat('Resampling components to target resolution... \n')
      system(paste(wb_cmd, '-metric-resample', surf_L, sphere_orig_L, sphere_target_L, 'BARYCENTRIC', surf_target_L, sep=' '))
    }
    
    if (do_right) {
      if (verbose) cat('Resampling components to target resolution... \n')
      system(paste(wb_cmd, '-metric-resample', surf_R, sphere_orig_R, sphere_target_R, 'BARYCENTRIC', surf_target_R, sep=' '))
    }
    
    surf_L <- surf_target_L
    surf_R <- surf_target_R
    
    
    #c) Use -surface-resample to resample gifti files (if provided) into target resolution
    
    ## TO DO: Use gifti_resample function for this part?
    
    ### Read in GIFTI surface geometry files if provided
    
    #left hemisphere
    if (do_left_surf) {
      for(ii in 1:num_surf) {
        gifti_target_L <- file.path(outdir, paste0('gifti',ii,'.target.L.surf.gii'))
        cmd = paste(wb_cmd, '-surface-resample', fname_gifti_left[ii], sphere_orig_L, sphere_target_L, 'BARYCENTRIC', gifti_target_L, sep=' ')
        system(cmd)
        fname_gifti_left[ii] <- gifti_target_L #replace gifti file name with resampled file
      }
    }
    
    #right hemisphere
    if (do_right_surf) {
      for(ii in 1:num_surf) {
        gifti_target_R <- file.path(outdir, paste0('gifti',ii,'.target.R.surf.gii'))
        cmd = paste(wb_cmd, '-surface-resample', fname_gifti_right[ii], sphere_orig_R, sphere_target_R, 'BARYCENTRIC', gifti_target_R, sep=' ')
        system(cmd)
        fname_gifti_right[ii] <- gifti_target_R #replace gifti file name with resampled file
      }
    }
    
    
  }


  ### Read in gifti and nifti files
  result <- vector('list', length=4)
  names(result) <- c('CORTEX_LEFT','CORTEX_RIGHT','VOL','LABELS')
  if (do_left) {
    if (verbose) cat('Reading in left cortical data ... \n')
    result$CORTEX_LEFT <- do.call(cbind, readGIfTI(surf_L)$data)
  }
  if (do_right) {
    if (verbose) cat('Reading in right cortical data ... \n')
    result$CORTEX_RIGHT <- do.call(cbind, readGIfTI(surf_R)$data)
  }
  if (do_sub) {
    if (verbose) cat('Reading in volumetric data ... \n')
    result$VOL <- readNIfTI(fname_vol, reorient=FALSE)
    result$LABELS <- readNIfTI(fname_labels, reorient=FALSE)
    result$LABELS[result$LABELS > 0] <- result$LABELS[result$LABELS > 0] + 2 #shift by 2 to be consistent with Matlab ft_read_cifti function, which labels 1=CORTEX_LEFT and 2=CORTEX_RIGHT
  }


  if (do_left_surf) {

    result$SURF_LEFT <- vector('list', num_surf)
    names(result$SURF_LEFT) <- surf_names

    for(ii in 1:num_surf) {
      surf_left_ii <- readGIfTI(fname_gifti_left[ii])$data
      verts_left_ii <- surf_left_ii$pointset
      faces_left_ii <- surf_left_ii$triangle
      if (min(faces_left_ii)==0) faces_left_ii <- faces_left_ii + 1 #start vertex indexing at 1 instead of 0
      surf_left_ii <- list(vertices = verts_left_ii, faces = faces_left_ii)
      class(surf_left_ii) <- 'surface'
      result$SURF_LEFT[[ii]] <- surf_left_ii
    }
    rm(surf_left_ii, verts_left_ii, faces_left_ii)
  } else {
    result$SURF_LEFT <- NULL
  }

  if (do_right_surf) {

    result$SURF_RIGHT <- vector('list', num_surf)
    names(result$SURF_RIGHT) <- surf_names

    for(ii in 1:num_surf) {
      surf_right_ii <- readGIfTI(fname_gifti_right[ii])$data
      verts_right_ii <- surf_right_ii$pointset
      faces_right_ii <- surf_right_ii$triangle
      if (min(faces_right_ii)==0) faces_right_ii <- faces_right_ii + 1 #start vertex indexing at 1 instead of 0
      surf_right_ii <- list(vertices = verts_right_ii, faces = faces_right_ii)
      class(surf_right_ii) <- 'surface'
      result$SURF_RIGHT[[ii]] <- surf_right_ii
    }
    rm(surf_right_ii, verts_right_ii, faces_right_ii)
  } else {
    result$SURF_RIGHT <- NULL
  }

  class(result) <- 'cifti'
  return(result)
}






