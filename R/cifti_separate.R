#' Separates and saves CIFTI data to NIfTI and GIfTI files.
#'
#' @description Separates a CIFTI file into GIfTIs for the cortical (left and right) structures, and NIfTIs for the 
#'  subcortical structures. This uses the -cifti-separate command from Connectome Workbench.
#'
#' @param cifti_fname File path of CIFTI-format data (ending in .d*.nii).
#' @param brainstructures A vector indicating which brain structure(s) to obtain: \code{"left"} (left cortical surface), 
#'  \code{"right"} (right cortical surface), and/or \code{"subcortical"} (subcortical and cerebellar gray matter). The
#'  default is \code{c('left','right','subcortical')} (all brain structures).
#' @param cortexL_fname,cortexR_fname (Optional) where to save the left and right cortex GIfTIs. If not provided, 
#'  defaults to \code{"*[L/R].func.gii"}, where * is the same directory and file name component of \code{cifti_fname}.
#' @param subcortVol_fname,subcortLab_fname (Optional) where to save the subcortical volume and label NIfTIs. If not 
#'  provided, defaults to \code{"[/.labels].nii.gz"}, where * is the same directory and file name component of 
#'  \code{cifti_fname}.
#' @param overwrite If a NIfTI or GIfTI file already exists, should it be overwritten? Default is FALSE.
#' @param write_dir If a file name is relative, what directory should it be saved to? Defaults to the current working directory.
#' @param wb_dir (Optional) Path to Connectome Workbench folder. If not provided, should be set by option ...
#'
#' @return Nothing
#' @export
#'
#' @details This function uses a system wrapper for the 'wb_command' executable. The user must first download and 
#'  install the Connectome Workbench, available from https://www.humanconnectome.org/software/get-connectome-workbench. 
#'  The 'wb_dir' argument is the path to the Connectome Workbench folder.
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
cifti_separate <- function(cifti_fname, brainstructures=c("left","right","subcortical"), 
  cortexL_fname=NULL, cortexR_fname=NULL, subcortVol_fname=NULL, subcortLab_fname=NULL, 
  overwrite=TRUE, write_dir=NULL, wb_dir=NULL){

  wb_cmd <- get_wb_cmd_path(wb_dir)

  cifti_fname <- make_abs_path(cifti_fname)
  if(!file.exists(cifti_fname)) stop('cifti_fname does not exist.')

  # Separate the CIfTI file path into directory, file name, and extension components.
  bname_cifti <- basename(cifti_fname) 
  extn_cifti <- get_cifti_extn(bname_cifti)  # "dtseries.nii" or "dscalar.nii"

  if(is.null(write_dir)){ 
    write_dir <- getwd()
  } else {
    if(!dir.exists(write_dir)){ stop("write_dir does not exist, check and try again.") }
    # TO DO: dir.create?
  }
  #TO DO: Check that the user has write permissions in outdir

  brainstructures <- match.arg(brainstructures, c("left","right","subcortical"), several.ok=TRUE)
  stopifnot(length(unique(brainstructures)) == length(brainstructures))
  do <- c("left","right","subcortical") %in% brainstructures
  names(do) <- c("left", "right", "sub")

  # Use default file names if not provided. Write relative paths to write_dir.
  if(do['left']){
    if(is.null(cortexL_fname)){ cortexL_fname <- gsub(extn_cifti, "L.func.gii", bname_cifti, fixed=TRUE) }
    cortexL_fname <- make_abs_path(cortexL_fname, write_dir)
  }
  if(do['right']){
    if(is.null(cortexR_fname)){ cortexR_fname <- gsub(extn_cifti, "R.func.gii", bname_cifti, fixed=TRUE) }
    cortexR_fname <- make_abs_path(cortexR_fname, write_dir)
  }
  if(do['sub']){
    if(is.null(subcortVol_fname)){ subcortVol_fname <- gsub(extn_cifti, "nii", bname_cifti, fixed=TRUE) }
    if(is.null(subcortLab_fname)){ subcortLab_fname <- gsub(extn_cifti, "labels.nii", bname_cifti, fixed=TRUE) }
    subcortVol_fname <- make_abs_path(subcortVol_fname, write_dir)
    subcortLab_fname <- make_abs_path(subcortLab_fname, write_dir)
  }

  sep_files <- data.frame(
    label = c("cortexL", "cortexR", "subcortVol", "subcortLab"),
    fname = c(cortexL_fname, cortexR_fname, subcortVol_fname, subcortLab_fname),
    stringsAsFactors=FALSE
  )
  sep_files$existed <- file.exists(sep_files$fname)

  # use overwrite argument?

  # Run the command if overwrite==TRUE, or if any desired file does not exist.
  run_cmd <- overwrite | any(!file.exists(c(cortexL_fname, cortexR_fname, subcortVol_fname, subcortLab_fname)))
  if(!run_cmd){
    cmd_code <- NA
  } else {
    # Build the Connectome Workbench command. 
    cmd <- paste(wb_cmd, "-cifti-separate", cifti_fname, "COLUMN", sep=" ")
    if(do['left']){
      cmd <- paste(cmd, '-metric CORTEX_LEFT', cortexL_fname)
    }
    if(do['right']){
      cmd <- paste(cmd, '-metric CORTEX_RIGHT', cortexR_fname)
    }
    if(do['sub']){
      cmd <- paste(cmd, '-volume-all', subcortVol_fname)
      cmd <- paste(cmd, '-label', subcortLab_fname)
    }
    cmd_code <- system(cmd)
    if(cmd_code != 0){
      stop(paste0("The Connectome Workbench command failed with code ", cmd_code, 
        ". The command was:\n", cmd))
    }
  }

  invisible(sep_files)
}