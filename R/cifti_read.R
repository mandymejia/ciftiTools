#' Reads in CIFTI data
#'
#' @description Wrapper function to read CIfTI data by separating it into GIfTI and NIfTI files, then reading each in.
#'
#' @param fname_cifti File path of CIFTI-format data (ending in .d*.nii).
#' @param brainstructures A vector indicating which brain structure(s) to obtain: \code{"left"} (left cortical surface), 
#'  \code{"right"} (right cortical surface), and/or \code{"subcortical"} (subcortical and cerebellar gray matter). The
#'  default is \code{c('left','right','subcortical')} (all brain structures).
#' @param fname_surfaceL (Optional) File path, or vector of multiple file paths, of GIFTI surface geometry file 
#'  representing left cortex
#' @param fname_surfaceR (Optional) File path, or vector of multiple file paths, of GIFTI surface geometry file 
#'  representing right cortex
#' @param surf_names (Optional) Character vector containing descriptive names of each GIFTI surface geometry provided 
#'  (e.g. midthickness, inflated, etc.). Should match the length of fname_surfaceL and/or fname_surfaceL if they are 
#'  provided. Otherwise, ignored.
#' @param fnames_sep_files A named character vector or list indicating where to save the separated GIfTI and NIfTI 
#'  files. Each name should match a file created by \code{cifti_separate()}: "cortexL", "cortexR", "subcortVol", or 
#'  "subcortLab".
#' @param keep_sep_files If FALSE (Default), new files made by the \code{cifti_separate()} call are deleted.
#' @param overwrite_sep_files (For cifti_separate) If a NIfTI or GIfTI file already exists, should it be overwritten? 
#'  Default is FALSE.
#' @param wb_dir (Optional) Path to Connectome Workbench folder. If not provided, should be set by option ...
#' @param verbose Should occasional updates be printed? Default is FALSE.
#'
#' @return An object of type 'cifti', a list containing at least 4 elements: CORTEX_LEFT, CORTX_RIGHT, VOL and LABELS.
#'  LABELS contains the brain structure labels (usually 3-21) of the subcortical elements. If surface geometry files
#'  were provided in the arguments, the list will also contain SURF_LEFT and SURF_RIGHT.
#' @export
#'
#' @details This function uses a system wrapper for the 'wb_command' executable. The user must first download and 
#'  install the Connectome Workbench, available from https://www.humanconnectome.org/software/get-connectome-workbench. 
#'  The 'wb_cmd' argument is the full file path to the 'wb_command' executable file.
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
cifti_read <- function(fname_cifti, brainstructures=c("left","right","subcortical"), fname_surfaceL=NULL, 
  fname_surfaceR=NULL, surf_names="surface", fnames_sep_files=NULL, keep_sep_files=FALSE, overwrite_sep_files=FALSE, 
  wb_dir=NULL, verbose=FALSE){

  # Separate the CIfTI file path into directory, file name, and extension components.
  dir_cifti <- dirname(fname_cifti) 
  bname_cifti <- basename(fname_cifti) 
  extn_cifti <- get_cifti_extn(bname_cifti)  # "dtseries.nii" or "dscalar.nii"
  all_files <- list.files(dir_cifti)
  if(!(bname_cifti %in% all_files)) stop("fname_cifti does not exist")

  # Determine which brainstructures to obtain.
  for(i in 1:length(brainstructures)){
    brainstructures[i] <- match.arg(brainstructures[i], c("left","right","subcortical"))
  }
  stopifnot(length(unique(names(brainstructures))) == length(names(brainstructures)))

  # Format the names of the separate NIfTI and GIfTI files.
  sep_files <- c("cortexL", "cortexR", "subcortVol", "subcortLab")
  # Define the default names.
  fnames_sep_files_defaults <- list(cortexL="L.func.gii", cortexR="R.func.gii", subcortVol="nii", 
    subcortLab="labels.nii")
  for(i in 1:length(fnames_sep_files_defaults)){
    fnames_sep_files_defaults[[i]] <- gsub(extn_cifti, fnames_sep_files_defaults[[i]], fname_cifti, fixed=TRUE)
  }
  # Use the default names wherever no argument was provided.
  if(identical(fnames_sep_files, NULL)){
    fnames_sep_files <- fnames_sep_files_defaults
  } else {
    for(i in 1:length(fnames_sep_files)){
      names(fnames_sep_files)[i] <- match.arg(names(fnames_sep_files), c("cortexL", "cortexR", "subcortVol", "subcortLab"))
    }
    stopifnot(length(unique(names(fnames_sep_files))) == length(names(fnames_sep_files)))
    for(i in 1:length(sep_files)){
      sep_file <- sep_files[i]
      if(!(sep_file %in% names(fnames_sep_files))){ 
        fnames_sep_files[[sep_file]] <- fnames_sep_files_defaults[[sep_file]]
      }
    }
  }
  names(fnames_sep_files) <- paste0("fname_", names(fnames_sep_files))
  sep_files_existed <- file.exists(unlist(fnames_sep_files))

  # Separate the CIfTI file.
  if(verbose){ print("Separating CIfTI file.") }
  cifti_separate_args <- c(
    list(fname_cifti=fname_cifti, brainstructures=brainstructures, dir=".", wb_dir=wb_dir, overwrite=overwrite_sep_files), 
    fnames_sep_files
  )  
  cifti_separate_result <- do.call(cifti_separate, cifti_separate_args)
  stopifnot(cifti_separate_result %in% c(NA, 0))

  # Read the CIfTI file.
  if(verbose){ print("Reading GIfTI and NIfTI files.") }
  cifti_read_from_separate_args <- c(
    list(fname_surfaceL=fname_surfaceL, fname_surfaceR=fname_surfaceR, surf_names=surf_names, dir=".", wb_dir=wb_dir), 
    fnames_sep_files
  )  
  result <- do.call(cifti_read_from_separate, cifti_read_from_separate_args)

  # Delete the separated files, unless otherwise requested. Do not delete files that existed before.
  if(!keep_sep_files){
    for(f in fnames_sep_files[!(sep_files_existed)]){
      file.remove(f)
      if(file.exists(paste0(f, ".data"))){
        file.remove(paste0(f, ".data"))
      }
    }
  }

  return(result)
}
