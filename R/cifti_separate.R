#' Separates and saves CIFTI data to NIfTI and GIfTI files.
#'
#' @description Separates a CIFTI file into GIfTIs for the cortical (left and right) structures, and NIfTIs for the 
#'  subcortical structures. This uses the -cifti-separate command from Connectome Workbench.
#'
#' @param cifti_fname File path of CIFTI-format data (ending in .d*.nii) to read in.
#' @param brainstructures A vector indicating which brain structure(s) to obtain: \code{"left"} (left cortical surface), 
#'  \code{"right"} (right cortical surface), and/or \code{"subcortical"} (subcortical and cerebellar gray matter). The
#'  default is \code{c("left","right","subcortical")} (all brain structures).
#' @param cortexL_fname,cortexR_fname (Optional) the files to save the left and right cortex GIfTIs to. If not provided, 
#'  defaults to \code{"*[L/R].func.gii"}, where * is the file name component of \code{cifti_fname}. If the path is 
#'  relative, they will be saved in \code{write_dir}.
#' @param subcortVol_fname,subcortLab_fname (Optional) where to save the subcortical volume and label NIfTIs. If not 
#'  provided, defaults to \code{"[/.labels].nii.gz"}, where * is the file name component of \code{cifti_fname}. If the
#'  path is relative, they will be saved in \code{write_dir}.
#' @param ROI_brainstructures Which ROIs should be obtained? NULL (default) to not get any ROIs. This should be a subset of the
#'  \code{brainstructures} argument.
#' @param ROIcortexL_fname,ROIcortexR_fname File names for cortex ROIs.
#' @param ROIsubcortVol_fname File name for volume ROI.
#' @param overwrite If all NIfTI or GIfTI files already exists, should they be overwritten? Default is TRUE. 
#' @param write_dir If a file name is relative, what directory should it be saved to? Defaults to the current working directory.
#' @param wb_path (Optional) Path to Connectome Workbench folder. If not provided, should be set with 
#'  \code{ciftiTools.setOption('wb_path', 'path/to/workbench')}.
#'
#' @return A data frame with column names "label", "fname", and "existed", and rows corresponding to each separate file.
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
cifti_separate <- function(cifti_fname, brainstructures=c("left","right","subcortical"), 
  cortexL_fname=NULL, cortexR_fname=NULL, subcortVol_fname=NULL, subcortLab_fname=NULL, 
  ROI_brainstructures=NULL, ROIcortexL_fname=NULL, ROIcortexR_fname=NULL, ROIsubcortVol_fname=NULL, 
  overwrite=TRUE, write_dir=NULL, wb_path=NULL){

  wb_cmd <- get_wb_cmd_path(wb_path)

  cifti_fname <- make_abs_path(cifti_fname)
  if(!file.exists(cifti_fname)) stop('cifti_fname does not exist.')

  # Get the components of the CIFTI file path.
  bname_cifti <- basename(cifti_fname) 
  extn_cifti <- get_cifti_extn(bname_cifti)  # "dtseries.nii" or "dscalar.nii"

  # Check that write_dir is valid. Use the current working directory if no write_dir is given.
  write_dir <- check_dir(write_dir)

  # Check that the brainstructures are valid.
  brainstructures <- match.arg(brainstructures, c("left","right","subcortical"), several.ok=TRUE)
  stopifnot(length(unique(brainstructures)) == length(brainstructures))
  do <- c("left","right","subcortical") %in% brainstructures
  names(do) <- c("left", "right", "sub")

  if(!is.null(ROI_brainstructures)){
    ROI_brainstructures <- match.arg(ROI_brainstructures, brainstructures, several.ok=TRUE)
    stopifnot(length(unique(ROI_brainstructures)) == length(ROI_brainstructures))
  }
  ROI_do <- c("left","right","subcortical") %in% ROI_brainstructures
  names(ROI_do) <- c("left", "right", "sub")

  # Use default file names if not provided. Relative paths will be placed in write_dir.
  default_fname <- function(label, extn_cifti, bname_cifti){ 
    gsub(extn_cifti, cifti_separate_default_suffix(label), bname_cifti, fixed=TRUE)
  }
  if(do['left']){
    if(is.null(cortexL_fname)){ cortexL_fname <- default_fname("cortexL", extn_cifti, bname_cifti) }
    cortexL_fname <- make_abs_path(cortexL_fname, write_dir)
    if(ROI_do['left']){
      if(is.null(ROIcortexL_fname)){ ROIcortexL_fname <- default_fname("ROIcortexL", extn_cifti, bname_cifti) }
      ROIcortexL_fname <- make_abs_path(ROIcortexL_fname, write_dir)
    } else { ROIcortexL_fname <- "" }
  } else { cortexL_fname <- ROIcortexL_fname <- "" }
  if(do['right']){
    if(is.null(cortexR_fname)){ cortexR_fname <- default_fname("cortexR", extn_cifti, bname_cifti) }
    cortexR_fname <- make_abs_path(cortexR_fname, write_dir)
    if(ROI_do['right']){
      if(is.null(ROIcortexR_fname)){ ROIcortexR_fname <- default_fname("ROIcortexR", extn_cifti, bname_cifti) }
      ROIcortexR_fname <- make_abs_path(ROIcortexR_fname, write_dir)
    } else { ROIcortexR_fname <- "" }
  } else { cortexR_fname <- ROIcortexR_fname <- "" }
  if(do['sub']){
    if(is.null(subcortVol_fname)){ subcortVol_fname <- default_fname("subcortVol", extn_cifti, bname_cifti) }
    subcortVol_fname <- make_abs_path(subcortVol_fname, write_dir)
    if(is.null(subcortLab_fname)){ subcortLab_fname <- default_fname("subcortLab", extn_cifti, bname_cifti) }
    subcortLab_fname <- make_abs_path(subcortLab_fname, write_dir)
    if(ROI_do['sub']){
      if(is.null(ROIsubcortVol_fname)){ ROIsubcortVol_fname <- default_fname("ROIsubcort", extn_cifti, bname_cifti) }
      ROIsubcortVol_fname <- make_abs_path(ROIsubcortVol_fname, write_dir)
    } else { ROIsubcortVol_fname <- "" }
  } else { 
    subcortVol_fname <- subcortLab_fname <- ROIsubcortVol_fname <- "" 
  }

  # Collect the absolute paths to each file in a data.frame to return later. Also record whether each existed before the
  # workbook command.
  sep_files <- data.frame(
    label = c("cortexL", "cortexR", "subcortVol", "subcortLab", 
      "ROIcortexL", "ROIcortexR", "ROIsubcortVol"),
    fname = c(cortexL_fname, cortexR_fname, subcortVol_fname, subcortLab_fname,
      ROIcortexL_fname, ROIcortexR_fname, ROIsubcortVol_fname),
    stringsAsFactors=FALSE
  )
  sep_files <- sep_files[sep_files$fname != "",]
  sep_files$existed <- file.exists(sep_files$fname)

  # Run the command if overwrite==TRUE, or if any desired file does not exist.
  run_cmd <- overwrite | any(!sep_files$existed)
  if(!run_cmd){
    cmd_code <- NA
  } else {
    # Build the Connectome Workbench command. 
    cmd <- paste(wb_cmd, "-cifti-separate", cifti_fname, "COLUMN")
    if(do['left']){
      cmd <- paste(cmd, '-metric CORTEX_LEFT', cortexL_fname)
      if(ROI_do['left']){ cmd <- paste(cmd, '-roi', ROIcortexL_fname) }
    }
    if(do['right']){
      cmd <- paste(cmd, '-metric CORTEX_RIGHT', cortexR_fname)
      if(ROI_do['right']){ cmd <- paste(cmd, '-roi', ROIcortexR_fname) }
    }
    if(do['sub']){
      cmd <- paste(cmd, '-volume-all', subcortVol_fname)
      if(ROI_do['sub']){ cmd <- paste(cmd, '-roi', ROIsubcortVol_fname) }
      cmd <- paste(cmd, '-label', subcortLab_fname)
    }
    # Run it! Raise an error if it fails.
    cmd_code <- system(cmd)
    if(cmd_code != 0){
      stop(paste0("The Connectome Workbench command failed with code ", cmd_code, 
        ". The command was:\n", cmd))
    }
  }

  invisible(sep_files) # column names are "label", "fname", and "existed"
}