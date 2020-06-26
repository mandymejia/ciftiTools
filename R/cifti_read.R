#' Reads in CIFTI data
#'
#' @description Wrapper function to read CIfTI data by separating it into GIfTI and NIfTI files (\code{cifti_separate}), 
#'  optionally resampling them (\code{cifti_resample_separate}) (NOT YET IMPLEMENTED), and then reading each in
#'  (\code{cifti_read_from_separate}).
#'
#' @param cifti_fname File path of CIfTI-format data (ending in .d*.nii) to read in.
#' @param brainstructures A vector indicating which brain structure(s) to obtain: \code{"left"} (left cortical surface), 
#'  \code{"right"} (right cortical surface), and/or \code{"subcortical"} (subcortical and cerebellar gray matter). The
#'  default is \code{c("left","right","subcortical")} (all brain structures).
#' @param sep_kwargs (Optional) Additional arguments to cifti_separate in the form of a list, e.g. 
#'  \code{list(overwrite=FALSE, write_dir="separated_cifti_files")} . Note that \code{cifti_read} will save the location
#'  of the files from cifti_separate
#' @param sep_keep If new separated files were made by this function call, should they be deleted once they are read in?
#'  Default is FALSE (delete them). If \code{sep_kwargs["overwrite"]==FALSE} and the separated files already exist,
#'  they will be read in and will not be deleted even if \code{sep_keep==FALSE}.
#' @param resamp_res Target resolution for resampling (number of cortical surface vertices per hemisphere). If NULL or 
#'  FALSE, do not perform resampling. NOT YET IMPLEMENTED.
#' @param resamp_kwargs (Optional) Additional arguments to cifti_resample_separate in the form of a list.
#' @param read_from_separate_kwargs (Optional) Additional arguments to cifti_read_from_separate in the form of a list.
#' @param surfL_fname,surfR_fname (Optional) File path, or vector of multiple file paths, of GIFTI surface geometry file 
#'  representing the left/right cortex. The path should be absolute, or relative to 
#'  \code{read_from_separate_kwargs["read_dir"]}, which defaults to the current working directory.
#' @param surf_label (Optional) Character vector containing descriptive names of each GIFTI surface geometry provided 
#'  (e.g. "midthickness", "inflated", etc.). Should match the lengths of \code{surfL_fname} and/or \code{surfR_fname} 
#'  if they are provided. Otherwise, ignored.
#' @param wb_dir (Optional) Path to Connectome Workbench folder. If not provided, should be set with 
#'  \code{ciftiTools.setOption('wb_path', 'path/to/workbench')}.
#' @param verbose Should occasional updates be printed? Default is FALSE.
#'
#' @return An object of type 'cifti', a list containing at least 4 elements: CORTEX_LEFT, CORTX_RIGHT, VOL and LABELS.
#'  LABELS contains the brain structure labels (usually 3-21) of the subcortical elements. If surface geometry files
#'  were provided in the arguments, the list will also contain SURF_LEFT and SURF_RIGHT.
#' @export
#'
#' @details This function uses a system wrapper for the 'wb_command' executable. The user must first download and 
#'  install the Connectome Workbench, available from https://www.humanconnectome.org/software/get-connectome-workbench. 
#'  The 'wb_dir' argument is the full file path to the Connectome Workbench folder. (The full file path to the 'wb_cmd' 
#'  executable also works.)
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
cifti_read <- function(cifti_fname, brainstructures=c("left","right","subcortical"), 
  sep_kwargs=NULL, sep_keep=FALSE, # cifti_separate
  resamp_res=NULL, resamp_kwargs=NULL, # cifti_resample
  read_from_separate_kwargs=NULL, surfL_fname=NULL, surfR_fname=NULL, surf_label=NULL, # cifti_read_from_separate
  wb_dir=NULL, verbose=FALSE){

  ################
  # cifti_separate
  ################

  if(verbose){ cat("Separating CIfTI file.") }
  
  # Check that the cifti_separate arguments are valid.
  sep_kwargs_allowed <- names(as.list(args(ciftiTools::cifti_separate)))
  sep_kwargs_allowed <- sep_kwargs_allowed[1:(length(sep_kwargs_allowed)-1)] # last is empty
  if(!is.null(sep_kwargs)){
    names(sep_kwargs) <- match.arg(names(sep_kwargs), sep_kwargs_allowed, several.ok=TRUE)
    stopifnot(length(unique(names(sep_kwargs))) == length(names(sep_kwargs)))
  } else {
    sep_kwargs <- vector(length=0, mode="list")
  }
  # It will read cifti_fname or sep_kwargs["cifti_fname"]. Raise an error if both are provided and they differ.
  if("cifti_fname" %in% sep_kwargs){
    if(!identical(cifti_fname, sep_kwargs$cifti_fname)){
      stop("cifti_fname argument to cifti_read did not match sep_kwargs entry. Only one is needed.")
    }
  } else {
    if(identical(cifti_fname, NULL)){ stop("cifti_fname must be provided directly to cifti_read or as an entry in sep_kwargs.") }
    sep_kwargs$cifti_fname <- cifti_fname
  }
  # Do cifti_separate.
  sep_result <- do.call(cifti_separate, sep_kwargs) # column names are "label", "fname", and "existed"
  files_to_read <- sep_result$fname
  names(files_to_read) <- sep_result$label

  #########################
  # cifti_resample_separate
  #########################

  if(identical(resamp_res, NULL) | identical(resamp_res, FALSE)){
    if(verbose){ cat("Resampling CIfTI file.") }

    # Check that the cifti_resample_separate arguments are valid.
    resamp_kwargs_allowed <- names(as.list(args(ciftiTools::cifti_resample_separate)))
    resamp_kwargs_allowed <- resamp_kwargs_allowed[1:(length(resamp_kwargs_allowed)-1)] # last is empty
    if(!is.null(resamp_kwargs)){
      names(resamp_kwargs) <- match.arg(names(resamp_kwargs), resamp_kwargs_allowed, several.ok=TRUE)
      stopifnot(length(unique(names(resamp_kwargs))) == length(names(resamp_kwargs)))
    } else {
      resamp_kwargs <- vector(length=0, mode="list")
    }
    # To-do: Populate resamp_kwargs with required arguments.
    resamp_result <- do.call(cifti_resample_separate, resamp_kwargs)
    files_to_read <- resamp_result$fname
    names(files_to_read) <- resamp_result$label
  }

  ##########################
  # cifti_read_from_separate
  ##########################

  # Read the CIfTI file from the separated files.
  if(verbose){ print("Reading GIfTI and NIfTI files to form the CIfTI.") }
  # Note: read_dir will only affect the surfaces because the cifti file paths are absolute.
  read_from_separate_kwargs <- c(
    files_to_read,
    list(surfL_fname=surfL_fname, surfR_fname=surfR_fname, read_dir=NULL, surf_label=surf_label, wb_dir=wb_dir)
  )
  result <- do.call(cifti_read_from_separate, read_from_separate_kwargs)

  ########
  # Finish
  ########

  # Delete the separated files, unless otherwise requested. Do not delete files that existed before.
  if(!sep_keep){
    for(f in sep_result$fname[!(sep_result$existed)]){
      file.remove(f)
      if(file.exists(paste0(f, ".data"))){
        file.remove(paste0(f, ".data"))
      }
    }
  }

  return(result)
}
