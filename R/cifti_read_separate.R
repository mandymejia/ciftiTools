#' Reads in CIFTI data
#'
#' @description Separates CIFTI data into cortical (left and right) and subcortical structures and reads in the data within each structure.
#'
#' @param fname_cifti File path of CIFTI-format data (ending in .d*.nii).
#' @param fname_gifti_left (Optional) File path, or vector of multiple file paths, of GIFTI surface geometry file representing left cortex
#' @param fname_gifti_right (Optional) File path, or vector of multiple file paths, of GIFTI surface geometry file representing right cortex
#' @param surf_names Character vector containing descriptive names of each GIFTI surface geometry provided (e.g. midthickness, inflated, etc.). Should match the length of fname_gifti_left and/or fname_gifti_left if they are provided. Otherwise, ignored.
#' @param brainstructures A vector indicating which brain structure(s) to include: 'left' (left cortical surface), 'right' (right cortical surface), and/or 'subcortical' (subcortical and cerebellar gray matter)
#' @param wb_cmd Path to Connectome Workbench executable file, ending in 'wb_command' (Mac/linux) or 'wb_command.exe' (Windows).
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
cifti_read_separate <- function(fname_cifti, fname_gifti_left=NULL, fname_gifti_right=NULL, surf_names='surface', brainstructures=c('left','right','subcortical'), wb_cmd){

  do_left <- ('left' %in% brainstructures)
  do_right <- ('right' %in% brainstructures)
  do_sub <- ('subcortical' %in% brainstructures)

  ### Check surface argument compatibility
  do_left_surf <- (!is.null(fname_gifti_left))
  do_right_surf <- (!is.null(fname_gifti_right))
  if(do_left_surf){ if(length(fname_gifti_left) != length(surf_names)) stop('Length of fname_gifti_left and surf_names must match.') }
  if(do_right_surf){ if(length(fname_gifti_right) != length(surf_names)) stop('Length of fname_gifti_left and surf_names must match.') }

  ### Separate the CIFTI file into left cortex, right cortex, subcortical volumetric data, and subcortical labels
  dir <- dirname(fname_cifti) #extract directory component of file path to cifti data
  fname_cifti <- basename(fname_cifti) #extract file name component of file path to cifti data
  extn <- get_cifti_extn(fname_cifti)  #get extension of cifti file (e.g. "dtseries.nii", "dscalar.nii")
  if(do_left) fname_left <- gsub(extn,'L.func.gii',fname_cifti, fixed=TRUE)
  if(do_right) fname_right <- gsub(extn,'R.func.gii',fname_cifti, fixed=TRUE)
  if(do_sub) {
    fname_vol <-gsub(extn,'nii.gz',fname_cifti, fixed=TRUE)
    fname_labels <- gsub(extn,'labels.nii.gz',fname_cifti, fixed=TRUE)
  }


  ### Check whether separated files already exist
  all_files <- list.files(dir)
  if(!(fname_cifti %in% all_files)) stop('fname_cifti does not exist')
  need_left <- need_right <- need_sub <- FALSE
  if(do_left) if(!(fname_left %in% all_files)) need_left <- TRUE
  if(do_right) if(!(fname_right %in% all_files)) need_right <- TRUE
  if(do_sub) if(!(fname_vol %in% all_files & fname_labels %in% all_files)) need_sub <- TRUE


  ### Construct system command to create needed files
  cmd_left <- cmd_right <- cmd_sub <- NULL
  if(need_left) cmd_left <- paste('-metric CORTEX_LEFT', file.path(dir,fname_left), sep=' ')
  if(need_right) cmd_right <- paste('-metric CORTEX_RIGHT', file.path(dir,fname_right), sep=' ')
  if(need_sub) cmd_sub <- paste('-volume-all', file.path(dir,fname_vol), '-label', file.path(dir,fname_labels), sep=' ')

  if(need_left | need_right | need_sub){
    cmd <- paste(wb_cmd, '-cifti-separate', file.path(dir,fname_cifti), 'COLUMN', cmd_left, cmd_right, cmd_sub, sep=' ')
    system(cmd)
  }


  ### Read in gifti and nifti files
  result <- vector('list', length=4)
  names(result) <- c('CORTEX_LEFT','CORTEX_RIGHT','VOL','LABELS')
  if(do_left) {
    dat_left <- readGIfTI(file.path(dir,fname_left))$data #list of length T, each element of length nvox
    nvox <- length(dat_left[[1]])
    ntime <- length(dat_left)
    result$CORTEX_LEFT <- matrix(unlist(dat_left), nrow=nvox, ncol=ntime) #form data matrix
  }
  if(do_right) {
    dat_left <- readGIfTI(file.path(dir,fname_right))$data #list of length T, each element of length nvox
    nvox <- length(dat_left[[1]])
    ntime <- length(dat_left)
    result$CORTEX_RIGHT <- matrix(unlist(dat_left), nrow=nvox, ncol=ntime) #form data matrix
  }
  if(do_sub){
    result$VOL <- readNIfTI(file.path(dir,fname_vol), reorient=FALSE)
    result$LABELS <- readNIfTI(file.path(dir,fname_labels), reorient=FALSE)
    result$LABELS[result$LABELS > 0] <- result$LABELS[result$LABELS > 0] + 2 #shift by 2 to be consistent with Matlab ft_read_cifti function, which labels 1=CORTEX_LEFT and 2=CORTEX_RIGHT
  }

  ### Read in GIFTI surface geometry files if provided
  do_left_surf <- (!is.null(fname_gifti_left))
  do_right_surf <- (!is.null(fname_gifti_right))
  num_surf <- length(surf_names) #number of surface types provided

  if(do_left_surf){

    result$SURF_LEFT <- vector('list', num_surf)
    names(result$SURF_LEFT) <- surf_names

    for(ii in 1:num_surf){
      surf_left_ii <- readGIfTI(fname_gifti_left[ii])$data
      verts_left_ii <- surf_left_ii$pointset
      faces_left_ii <- surf_left_ii$triangle
      if(min(faces_left_ii)==0) faces_left_ii <- faces_left_ii + 1 #start vertex indexing at 1 instead of 0
      surf_left_ii <- list(vertices = verts_left_ii, faces = faces_left_ii)
      class(surf_left_ii) <- 'surface'
      result$SURF_LEFT[[ii]] <- surf_left_ii
    }
  } else {
    result$SURF_LEFT <- NULL
  }

  if(do_right_surf){

    result$SURF_RIGHT <- vector('list', num_surf)
    names(result$SURF_RIGHT) <- surf_names

    for(ii in 1:num_surf){
      surf_right_ii <- readGIfTI(fname_gifti_right[ii])$data
      verts_right_ii <- surf_right_ii$pointset
      faces_right_ii <- surf_right_ii$triangle
      if(min(faces_right_ii)==0) faces_right_ii <- faces_right_ii + 1 #start vertex indexing at 1 instead of 0
      surf_right_ii <- list(vertices = verts_right_ii, faces = faces_right_ii)
      class(surf_right_ii) <- 'surface'
      result$SURF_RIGHT[[ii]] <- surf_right_ii
    }
  } else {
    result$SURF_RIGHT <- NULL
  }

  class(result) <- 'cifti'
  return(result)
}






