#' Separate a CIFTI as its NIFTI and GIFTI component files
#'
#' @description Separates a CIFTI file into GIFTIs for the cortical 
#'  (left and right) structures, and NIFTIs for the 
#'  subcortical structures. 
#'  This uses the \code{-cifti-separate} command from Connectome Workbench.
#'
#' @inheritParams cifti_fname_Param
#' @inheritParams brainstructures_Param_LR
#' @param cortexL_fname,cortexR_fname (Optional) the files to save the left and right cortex GIFTIs to. If not provided, 
#'  defaults to \code{"*[L/R].func.gii"}, where * is the file name component of \code{cifti_fname}. If the path is 
#'  relative, they will be saved in \code{write_dir}.
#' @param subcortVol_fname,subcortLabs_fname (Optional) where to save the subcortical volume and labels NIFTIs. If not 
#'  provided, defaults to \code{"[/.labels].nii.gz"}, where * is the file name component of \code{cifti_fname}. If the
#'  path is relative, they will be saved in \code{write_dir}.
#' @param ROI_brainstructures Which ROIs should be obtained? NULL (default) to not get any ROIs. This should be a subset of the
#'  \code{brainstructures} argument.
#' @param ROIcortexL_fname,ROIcortexR_fname File names for cortex ROIs.
#' @param ROIsubcortVol_fname File name for volume ROI.
#' @inheritParams write_dir_Param_generic
#' @inheritParams wb_path_Param
#'
#' @return A data frame with column names "label" and "fname", and rows corresponding to each separate file.
#' @export
#'
#' @details This function uses a system wrapper for the 'wb_command' executable. The user must first download and 
#'  install the Connectome Workbench, available from https://www.humanconnectome.org/software/get-connectome-workbench. 
#'  The 'wb_path' argument is the full file path to the Connectome Workbench folder. (The full file path to the 'wb_cmd' 
#'  executable also works.)
#'
separate_cifti <- function(cifti_fname, brainstructures=c("left","right"), 
  cortexL_fname=NULL, cortexR_fname=NULL, subcortVol_fname=NULL, subcortLabs_fname=NULL, 
  ROI_brainstructures=NULL, ROIcortexL_fname=NULL, ROIcortexR_fname=NULL, ROIsubcortVol_fname=NULL, 
  write_dir=NULL, wb_path=NULL) {

  cifti_fname <- format_path(cifti_fname)
  if (!file.exists(cifti_fname)) {
    stop(paste("cifti_fname", cifti_fname, "does not exist."))
  }

  # Get the components of the CIFTI file path.
  bname_cifti <- basename(cifti_fname) 
  extn_cifti <- get_cifti_extn(bname_cifti)  # "dtseries.nii" or "dscalar.nii"

  # Validate the brainstructures.
  brainstructures <- match_input(
    brainstructures, c("left","right","subcortical","all"),
    user_value_label="brainstructures"
  )
  if ("all" %in% brainstructures) { 
    brainstructures <- c("left","right","subcortical")
  }
  do <- c("left","right","subcortical") %in% brainstructures
  names(do) <- c("left", "right", "sub")
  # Validate the ROI brainstructures.
  if (!is.null(ROI_brainstructures)) {
    ROI_brainstructures <- match.arg(ROI_brainstructures, brainstructures, several.ok=TRUE)
    stopifnot(length(unique(ROI_brainstructures)) == length(ROI_brainstructures))
  }
  ROI_do <- c("left","right","subcortical") %in% ROI_brainstructures
  names(ROI_do) <- c("left", "right", "sub")

  # Use default file names if not provided. Relative paths will be placed in write_dir.
  default_fname <- function(label, extn_cifti, bname_cifti) { 
    gsub(extn_cifti, cifti_component_suffix(label), bname_cifti, fixed=TRUE)
  }
  if (do['left']) {
    if (is.null(cortexL_fname)) { cortexL_fname <- default_fname("cortexL", extn_cifti, bname_cifti) }
    cortexL_fname <- format_path(cortexL_fname, write_dir, mode=2)
    if (ROI_do['left']) {
      if (is.null(ROIcortexL_fname)) { ROIcortexL_fname <- default_fname("ROIcortexL", extn_cifti, bname_cifti) }
      ROIcortexL_fname <- format_path(ROIcortexL_fname, write_dir, mode=2)
    } else { ROIcortexL_fname <- "" }
  } else { cortexL_fname <- ROIcortexL_fname <- "" }

  if (do['right']) {
    if (is.null(cortexR_fname)) { cortexR_fname <- default_fname("cortexR", extn_cifti, bname_cifti) }
    cortexR_fname <- format_path(cortexR_fname, write_dir, mode=2)
    if (ROI_do['right']) {
      if (is.null(ROIcortexR_fname)) { ROIcortexR_fname <- default_fname("ROIcortexR", extn_cifti, bname_cifti) }
      ROIcortexR_fname <- format_path(ROIcortexR_fname, write_dir, mode=2)
    } else { ROIcortexR_fname <- "" }
  } else { cortexR_fname <- ROIcortexR_fname <- "" }

  if (do['sub']) {
    if (is.null(subcortVol_fname)) { subcortVol_fname <- default_fname("subcortVol", extn_cifti, bname_cifti) }
    subcortVol_fname <- format_path(subcortVol_fname, write_dir, mode=2)
    if (is.null(subcortLabs_fname)) { subcortLabs_fname <- default_fname("subcortLabs", extn_cifti, bname_cifti) }
    subcortLabs_fname <- format_path(subcortLabs_fname, write_dir, mode=2)
    if (ROI_do['sub']) {
      if (is.null(ROIsubcortVol_fname)) { ROIsubcortVol_fname <- default_fname("ROIsubcort", extn_cifti, bname_cifti) }
      ROIsubcortVol_fname <- format_path(ROIsubcortVol_fname, write_dir, mode=2)
    } else { ROIsubcortVol_fname <- "" }
    
  } else { 
    subcortVol_fname <- subcortLabs_fname <- ROIsubcortVol_fname <- "" 
  }

  # Collect the paths to each file in a data.frame to return later. 
  sep_files <- data.frame(
    label = c("cortexL", "cortexR", "subcortVol", "subcortLabs", 
      "ROIcortexL", "ROIcortexR", "ROIsubcortVol"),
    fname = c(cortexL_fname, cortexR_fname, subcortVol_fname, subcortLabs_fname,
      ROIcortexL_fname, ROIcortexR_fname, ROIsubcortVol_fname),
    stringsAsFactors=FALSE
  )
  sep_files <- sep_files[sep_files$fname != "",]

  if (length(unique(sep_files$fname)) != nrow(sep_files)) {
    print(sep_files)
    stop(paste0(
      "The file paths for the separated components are printed above. ",
      "Some file paths were identical, but ",
      "the same path cannot be used to write out different components. ",
      "Check if identical file names were specified, or if any provided ",
      "file name overlapped with a default file name.\n\n"
    ))
  }

  # Build the Connectome Workbench command. 
  cmd <- paste("-cifti-separate", sys_path(cifti_fname), "COLUMN")
  if (do['left']) {
    cmd <- paste(cmd, '-metric CORTEX_LEFT', sys_path(cortexL_fname))
    if (ROI_do['left']) { cmd <- paste(cmd, '-roi', sys_path(ROIcortexL_fname)) }
  }
  if (do['right']) {
    cmd <- paste(cmd, '-metric CORTEX_RIGHT', sys_path(cortexR_fname))
    if (ROI_do['right']) { cmd <- paste(cmd, '-roi', sys_path(ROIcortexR_fname)) }
  }
  if (do['sub']) {
    cmd <- paste(cmd, '-volume-all', sys_path(subcortVol_fname))
    if (ROI_do['sub']) { cmd <- paste(cmd, '-roi', sys_path(ROIsubcortVol_fname)) }
    cmd <- paste(cmd, '-label', sys_path(subcortLabs_fname))
  }
  run_wb_cmd(cmd, wb_path)

  invisible(sep_files) # column names are "label" and "fname"
}

#' @rdname separate_cifti
#' @export
separateCIfTI <- separatecii <- function(
  cifti_fname, brainstructures=c("left","right"), 
  cortexL_fname=NULL, cortexR_fname=NULL, subcortVol_fname=NULL, subcortLabs_fname=NULL, 
  ROI_brainstructures=NULL, ROIcortexL_fname=NULL, ROIcortexR_fname=NULL, ROIsubcortVol_fname=NULL, 
  write_dir=NULL, wb_path=NULL){

  separate_cifti(
    cifti_fname, brainstructures, 
    cortexL_fname, cortexR_fname, subcortVol_fname, subcortLabs_fname, 
    ROI_brainstructures, ROIcortexL_fname, ROIcortexR_fname, ROIsubcortVol_fname, 
    write_dir, wb_path
  )
}