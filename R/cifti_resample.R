#' Resample CIFTI data
#'
#' @description Performs spatial resampling of CIFTI data on the cortical surface
#'
#' @param cifti_original_fname A CIFTI file to resample.
#' @param cifti_target_fname The file name to save the resampled CIFTI.
#' @param sep_kwargs (Optional) Additional arguments to cifti_resample in the form of a list, e.g. 
#'  \code{list(overwrite=FALSE, write_dir="separated_cifti_files")} . Note that \code{cifti_read} will save the location
#'  of the files from cifti_resample
#' @param sep_keep If new separated files were made by this function call, should they be deleted once they are read in?
#'  Default is FALSE (delete them). If \code{sep_kwargs["overwrite"]==FALSE} and the separated files already exist,
#'  they will be read in and will not be deleted even if \code{sep_keep==FALSE}.
#' @param resamp_res Target resolution for resampling (number of cortical surface vertices per hemisphere). If NULL or 
#'  FALSE, do not perform resampling. NOT YET IMPLEMENTED.
#' @param sphereL_fname,sphereR_fname Helper sphere files needed for resampling
#' @param resamp_kwargs (Optional) Additional arguments to cifti_resample_separate in the form of a list.
#' @param resamp_keep Should files made by \code{cifti_resample} be kept? Default is FALSE (delete after).
#' @param overwrite If \code{cifti_target_fname} exists, should it be replaced? Default is \code{FALSE}.
#' @param write_dir Where to place the resampled CIFTI. Defaults to the current working directory.
#' @param wb_path (Optional) Path to Connectome Workbench folder. If not provided, should be set with 
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
#'  The 'wb_path' argument is the full file path to the Connectome Workbench folder. (The full file path to the 'wb_cmd' 
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
cifti_resample <- function(cifti_original_fname, cifti_target_fname=NULL, 
  sep_kwargs=NULL, sep_keep=FALSE, #cifti_separate
  resamp_res=NULL, sphereL_fname=NULL, sphereR_fname=NULL, resamp_kwargs=NULL, resamp_keep=FALSE, # cifti_resample
  overwrite=FALSE, write_dir=NULL, wb_path=NULL, verbose=FALSE){

  wb_cmd <- get_wb_cmd_path(wb_path)

  #######
  # setup
  #######

  brainstructures <- c("left","right","subcortical")
  ROI_brainstructures <- brainstructures

  original_to_target_fname <- function(original_fname, res_target){
    bname <- basename(original_fname)
    paste("resampled", res_target, bname, sep="_")
  }
  if(is.null(cifti_target_fname)){ cifti_target_fname <- original_to_target_fname(cifti_original_fname, resamp_res) }
  write_dir <- check_dir(write_dir)
  cifti_target_fname <- make_abs_path(cifti_target_fname, write_dir)
  if(file.exists(cifti_target_fname) & !overwrite){ return(invisible) }

  ################
  # cifti_separate
  ################

  if(verbose){ cat("Separating CIFTI file.\n") }
  
  # Check that the cifti_separate arguments are valid.
  sep_kwargs_allowed <- names(as.list(args(ciftiTools::cifti_separate)))
  sep_kwargs_allowed <- sep_kwargs_allowed[1:(length(sep_kwargs_allowed)-1)] # last is empty
  if(!is.null(sep_kwargs)){
    names(sep_kwargs) <- match.arg(names(sep_kwargs), sep_kwargs_allowed, several.ok=TRUE)
    stopifnot(length(unique(names(sep_kwargs))) == length(names(sep_kwargs)))
  } else {
    sep_kwargs <- vector("list", 0)
  }
  sep_kwargs <- c(sep_kwargs, list(brainstructures=brainstructures, ROI_brainstructures=ROI_brainstructures))
  # It will read cifti_original_fname or sep_kwargs["cifti_fname"]. Raise an error if both are provided and they differ.
  if("cifti_original_fname" %in% sep_kwargs){
    if(!identical(cifti_original_fname, sep_kwargs$cifti_fname)){
      stop("cifti_original_fname argument to cifti_read did not match sep_kwargs entry. Only one is needed.")
    }
  } else {
    if(identical(cifti_original_fname, NULL)){ stop("cifti_original_fname must be provided directly to cifti_read or as an entry in sep_kwargs.") }
    sep_kwargs$cifti_fname <- cifti_original_fname
  }
  # If sep_keep==FALSE, use a temporary directory
  if(!sep_keep){
    if(!is.null(sep_kwargs$write_dir)){
      if(verbose){ cat("Warning: using temporary directory instead of sep_kwargs$write_dir because sep_keep is FALSE.\n") }
    }
    sep_kwargs$write_dir <- tempdir()
  }
  # Do cifti_separate.
  sep_kwargs[sapply(sep_kwargs, is.null)] <- NULL
  sep_result <- do.call(cifti_separate, sep_kwargs) # column names are "label", "fname", and "existed"

  # Organize the files to (resample and) read
  files_to_read <- as.list(sep_result$fname)
  names(files_to_read) <- sep_result$label

  #########################
  # cifti_resample_separate
  #########################

  if(!identical(resamp_res, NULL) & !identical(resamp_res, FALSE)){
    if(verbose){ cat("Resampling CIFTI file.\n") }

    if(("left" %in% brainstructures) & is.null(sphereL_fname)){
      stop("To resample the left cortex, `sphereL_fname` must be provided to `cifti_read`.")
    }
    if(("right" %in% brainstructures) & is.null(sphereR_fname)){
      stop("To resample the right cortex, `sphereR_fname` must be provided to `cifti_read`.")
    }
    # Check that the cifti_resample_separate arguments are valid.
    resamp_kwargs_allowed <- names(as.list(args(ciftiTools::cifti_resample_separate)))
    resamp_kwargs_allowed <- resamp_kwargs_allowed[1:(length(resamp_kwargs_allowed)-1)] # last is empty
    if(!is.null(resamp_kwargs)){
      names(resamp_kwargs) <- match.arg(names(resamp_kwargs), resamp_kwargs_allowed, several.ok=TRUE)
      stopifnot(length(unique(names(resamp_kwargs))) == length(names(resamp_kwargs)))
    } else {
      resamp_kwargs <- vector(length=0, mode="list")
    }
    resamp_kwargs$res_target <- resamp_res
    original_fnames <- files_to_read[!grepl("subcort", names(files_to_read))]
    names(original_fnames) <- paste0(names(original_fnames), "_original_fname")
    original_fnames <- c(original_fnames, list(
      surfL_original_fname=surfL_fname, surfR_original_fname=surfR_fname,
      sphereL_original_fname=sphereL_fname, sphereR_original_fname=sphereR_fname
    ))
    original_fnames[names(resamp_kwargs)] <- NULL # remove duplicates, keeping user-inputs over known files. (other way?)
    resamp_kwargs <- c(resamp_kwargs, original_fnames)
    # If resamp_keep==FALSE, use a temporary directory.
    if(!resamp_keep){
      if(!is.null(resamp_kwargs$write_dir)){
        if(verbose){ cat("Warning: using temporary directory instead of resamp_kwargs$write_dir because resamp_keep is FALSE.\n") }
      }
      resamp_kwargs$write_dir <- tempdir()
    }
    # Do cifti_resample_separate .
    resamp_kwargs[sapply(resamp_kwargs, is.null)] <- NULL
    resamp_result <- do.call(cifti_resample_separate, resamp_kwargs)

    # Replace cortical data to read in.
    for(i in 1:length(files_to_read)){
      if(names(files_to_read)[i] %in% resamp_result$label){
        files_to_read[i] <- resamp_result$fname[resamp_result$label == names(files_to_read)[i]]
      }
    }
    # Replace surface geometry to read in.
    if(length(surfL_fname) > 0){ surfL_fname <- resamp_result$fname[grepl("surfL", resamp_result$label)] }
    if(length(surfR_fname) > 0){ surfR_fname <- resamp_result$fname[grepl("surfR", resamp_result$label)] }
  }

  ########################
  # resample with template
  ########################

  # Create a template CIFTI dense timeseries.
  if(verbose) cat('Creating template CIFTI file in target resolution... \n')
  cifti_template_fname <- make_abs_path(paste0("template_", basename(cifti_original_fname)), tempdir())
  cifti_extn <- get_cifti_extn(cifti_original_fname)
  if(grepl('dtseries',cifti_extn)) create_cmd <- '-cifti-create-dense-timeseries'
  if(grepl('dscalar',cifti_extn)) create_cmd <- '-cifti-create-dense-scalar'
  if(grepl('dlabel',cifti_extn)) create_cmd <- '-cifti-create-label'
  system(paste(
    wb_cmd, create_cmd, cifti_template_fname, 
    '-volume', files_to_read$subcortVol, files_to_read$subcortLab, 
    '-left-metric', files_to_read$cortexL, 
      ifelse("ROIcortexL" %in% names(files_to_read), paste('-roi-left', files_to_read$ROIcortexL), NULL), 
    '-right-metric', files_to_read$cortexR, 
      ifelse("ROIcortexR" %in% names(files_to_read), paste('-roi-right', files_to_read$ROIcortexR), NULL)
  ))

  if(verbose) cat('Resampling cifti_original_fname to target resolution... \n')
  sphereL_target_fname <- original_to_target_fname(sphereL_fname, resamp_res)
  sphereR_target_fname <- original_to_target_fname(sphereR_fname, resamp_res)
  sphereL_target_fname <- make_abs_path(sphereL_target_fname, resamp_kwargs$write_dir)
  sphereR_target_fname <- make_abs_path(sphereR_target_fname, resamp_kwargs$write_dir)
  stopifnot(all(file.exists(c(sphereL_target_fname, sphereR_target_fname))))
  cmd = paste(wb_cmd, 
    '-cifti-resample', cifti_original_fname, 'COLUMN', cifti_template_fname, 
    'COLUMN BARYCENTRIC CUBIC', cifti_target_fname, 
    '-left-spheres',  sphereL_fname, sphereL_target_fname, 
    '-right-spheres', sphereR_fname, sphereR_target_fname, sep=' ')
  cmd_code <- system(cmd)
  if(cmd_code != 0){
    stop(paste0("The Connectome Workbench command failed with code ", cmd_code, 
      ". The command was:\n", cmd))
  }

  file.remove(cifti_template_fname)

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

  # TO-DO: Delete resampled files.

  return(invisible())
}
