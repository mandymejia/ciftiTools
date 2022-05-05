#' Separate CIFTI: file names
#' 
#' File paths for writing GIFTI and NIFTI files from a CIFTI or \code{"xifti"}
#' 
#' @param intent 3002 (default), 3006, or 3007
#' @inheritParams brainstructures_Param_LR
#' @param ROI_brainstructures Which ROIs should be obtained? \code{"all"} 
#'  (default) to obtain ROIs for each of the \code{brainstructures}. \code{NULL} 
#'  to not obtain any ROIs. This should be a subset of \code{brainstructures}.
#' @param cortexL_fname,cortexR_fname (Optional) GIFTI file names 
#'  (*.\[func/label\].gii) to save the \[left/right\] cortex data to. If not provided, 
#'  defaults to \code{"*[L/R].\[func/label\].gii"}, where * is the file name
#'  component of \code{cifti_fname}. Will be written in \code{write_dir}. 
#' 
#'  dtseries and dscalar files should use "func", whereas dlabel files should 
#'  use "label".
#' @param ROIcortexL_fname,ROIcortexR_fname (Optional) GIFTI file names
#'  (*.\[func/label\].gii) to save the \[left/right\] cortex ROI to. If not provided, 
#'  defaults to \code{"*ROI_[L/R].\[func/label\].gii"}, where * is the file name component
#'  of \code{cifti_fname}. The cortical ROIs typically represent the medial wall 
#'  mask, with values of 1 for in-ROI (non-medial wall) vertices and 0 for 
#'  out-of-ROI (medial wall) vertices. Will be written in \code{write_dir}.
#' 
#'  dtseries and dscalar files should use "func", whereas dlabel files should
#'  use "label".
#' @param subcortVol_fname,subcortLabs_fname (Optional) NIFTI file names to save
#'  the subcortical \[volume/labels\] to. If not provided, defaults to 
#'  \code{"*[/.labels].nii"}, where * is the file name component of 
#'  \code{cifti_fname}. Will be written in \code{write_dir}.
#' @param ROIsubcortVol_fname (Optional) NIFTI file names to save
#'  the subcortical ROI to. If not provided, defaults to 
#'  \code{"*ROI.nii"}, where * is the file name component of 
#'  \code{cifti_fname}. The subcortical ROI typically represents the volumetric
#'  mask for the entire subcortical structure, with values of 1 for in-ROI 
#'  (in subcortex) voxels and 0 for out-of-ROI (not in subcortex) voxels. Will 
#'  be written in \code{write_dir}.
#' @inheritParams write_dir_Param_generic
#' 
#' @return List of three: \code{do}, \code{ROI_do}, and \code{sep_fnames}
#' 
#' @keywords internal
separate_cifti_files <- function(
  intent=3006,
  brainstructures=c("left","right"), 
  cortexL_fname=NULL, cortexR_fname=NULL, 
  subcortVol_fname=NULL, subcortLabs_fname=NULL, 
  ROI_brainstructures="all", 
  ROIcortexL_fname=NULL, ROIcortexR_fname=NULL, 
  ROIsubcortVol_fname=NULL, 
  write_dir=NULL){

  # Get the brainstructures. 
  brainstructures <- match_input(
    brainstructures, c("left","right","subcortical","all"),
    user_value_label="brainstructures"
  )
  if ("all" %in% brainstructures) { 
    brainstructures <- c("left","right","subcortical")
  }
  do <- c("left","right","subcortical") %in% brainstructures
  names(do) <- c("left", "right", "sub")

  # Get the ROI brainstructures.
  if (!is.null(ROI_brainstructures)) {
    ROI_brainstructures <- match_input(ROI_brainstructures, c(brainstructures, "all"), user_value_label="ROI_brainstructures")
    if ("all" %in% ROI_brainstructures) { 
      ROI_brainstructures <- brainstructures
    }
    stopifnot(length(unique(ROI_brainstructures)) == length(ROI_brainstructures))
  }
  ROI_do <- c("left","right","subcortical") %in% ROI_brainstructures
  names(ROI_do) <- c("left", "right", "sub")

  default_fname <- function(x){ paste0("sep.", cifti_component_suffix(x)) }

  if (do['left']) {
    # Left cortex
    if (is.null(cortexL_fname)) { cortexL_fname <- default_fname("cortexL") }
    cortexL_fname <- format_path(cortexL_fname, write_dir, mode=2)
    if (intent == 3007) { cortexL_fname <- gsub(".func.gii", ".label.gii", cortexL_fname, fixed=TRUE) }
    if (ROI_do['left']) {
      if (is.null(ROIcortexL_fname)) { ROIcortexL_fname <- default_fname("ROIcortexL") }
      ROIcortexL_fname <- format_path(ROIcortexL_fname, write_dir, mode=2)
    } else { ROIcortexL_fname <- NULL }
  } else { cortexL_fname <- ROIcortexL_fname <- NULL }

  if (do['right']) {
    # Right cortex
    if (is.null(cortexR_fname)) { cortexR_fname <- default_fname("cortexR") }
    cortexR_fname <- format_path(cortexR_fname, write_dir, mode=2)
    if (intent == 3007) { cortexR_fname <- gsub(".func.gii", ".label.gii", cortexR_fname, fixed=TRUE) }
    if (ROI_do['right']) {
      if (is.null(ROIcortexR_fname)) { ROIcortexR_fname <- default_fname("ROIcortexR") }
      ROIcortexR_fname <- format_path(ROIcortexR_fname, write_dir, mode=2)
    } else { ROIcortexR_fname <- NULL }
  } else { cortexR_fname <- ROIcortexR_fname <- NULL }

  if (do['sub']) {
    # Subcortex
    if (is.null(subcortVol_fname)) { subcortVol_fname <- default_fname("subcortVol") }
    subcortVol_fname <- format_path(subcortVol_fname, write_dir, mode=2)
    if (is.null(subcortLabs_fname)) { subcortLabs_fname <- default_fname("subcortLabs") }
    subcortLabs_fname <- format_path(subcortLabs_fname, write_dir, mode=2)
    if (ROI_do['sub']) {
      if (is.null(ROIsubcortVol_fname)) { ROIsubcortVol_fname <- default_fname("ROIsubcort") }
      ROIsubcortVol_fname <- format_path(ROIsubcortVol_fname, write_dir, mode=2)
    } else { ROIsubcortVol_fname <- NULL }
  } else { 
    subcortVol_fname <- subcortLabs_fname <- ROIsubcortVol_fname <- NULL 
  }

  sep_fnames <- unlist(list(
    cortexL = cortexL_fname,
    ROIcortexL = ROIcortexL_fname,
    cortexR = cortexR_fname,
    ROIcortexR = ROIcortexR_fname,
    subcortVol = subcortVol_fname,
    subcortLabs = subcortLabs_fname,
    ROIsubcortVol = ROIsubcortVol_fname
  ))

  # Stop if any file paths are identical.
  if (length(unique(sep_fnames)) != length(sep_fnames)) {
    print(sep_fnames)
    stop(paste0(
      "The file paths for the separated xifti/CIFTI components are above. ",
      "Some file paths were identical, but ",
      "the same path cannot be used to write out different components. ",
      "Check if identical file names were specified, or if any provided ",
      "file name overlapped with a default file name.\n\n"
    ))
  }

  list(do=do, ROI_do=ROI_do, sep_fnames=sep_fnames)
}

#' Separate a CIFTI file
#'
#' Separate a CIFTI file into GIFTI files for the cortical data and NIFTI files 
#'  for the subcortical data and labels. ROIs can also be written to indicate 
#'  the medial wall mask (cortex) and volume mask (subcortex). This uses the
#'  Connectome Workbench command \code{-cifti-separate}.
#' 
#' Time unit, start, and step (dtseries files) will not be written to the GIFTI/NIFTIs. 
#'  Column names (dscalar files) will not be written to the GIFTIs, as well as label 
#'  names and colors (dlabel files). (Haven't checked the NIFTIs yet.)
#' 
#'  ROI/medial wall behavior: If there are 32k vertices in the left cortex with
#'  3k representing the medial wall, then both \code{cortexL_fname} and 
#'  \code{ROIcortexL_fname} will have 32k entries, 3k of which having a value of
#'  0 indicating the medial wall. The non-medial wall entries will have the 
#'  data values in \code{cortexL_fname} and a value of 1 in 
#'  \code{ROIcortexL_fname}. Thus, exporting \code{ROIcortexL_fname} is vital if 
#'  the data values include 0, because 0-valued non-medial wall vertices and
#'  medial wall vertices cannot be distinguished from one another within
#'  \code{cortexL_fname} alone.
#'
#' 
#' @inheritParams cifti_fname_Param
#' @inheritParams separate_cifti_files
#'
#' @return A named character vector with the file paths to the written 
#'  NIFTI and GIFTI files
#' 
#' @family writing
#' @export
#'
#' @section Connectome Workbench:
#' This function interfaces with the \code{"-cifti-separate"} Workbench command.
#' 
separate_cifti <- function(cifti_fname, 
  brainstructures=c("left","right"), 
  cortexL_fname=NULL, cortexR_fname=NULL, 
  subcortVol_fname=NULL, subcortLabs_fname=NULL, 
  ROI_brainstructures="all", 
  ROIcortexL_fname=NULL, ROIcortexR_fname=NULL, 
  ROIsubcortVol_fname=NULL, 
  write_dir=NULL) {

  # Check if CIFTI exists.
  cifti_fname <- format_path(cifti_fname)
  if (!file.exists(cifti_fname)) {
    stop(paste("The `cifti_fname` \"", cifti_fname, "\" does not exist."))
  }

  # Read metadata.
  cifti_info <- info_cifti(cifti_fname)

  # Get the components of the CIFTI file path.
  bname_cifti <- basename(cifti_fname) 
  extn_cifti <- get_cifti_extn(bname_cifti)
  
  # Convert extension to numerical intent
  check_cifti_type(cifti_info$cifti$intent, extn_cifti)
  if (extn_cifti %in% c("dtseries.nii", "dscalar.nii", "dlabel.nii")) {
    intent_cifti <- supported_intents()$value[
      match(extn_cifti, supported_intents()$extension)
    ]
  } else {
    intent_cifti <- 3006
  }

  # Get output files: which ones to write, and their names
  x <- separate_cifti_files(
    intent = intent_cifti,
    brainstructures=brainstructures,
    cortexL_fname=cortexL_fname, cortexR_fname=cortexR_fname,
    subcortVol_fname=subcortVol_fname, subcortLabs_fname=subcortLabs_fname,
    ROI_brainstructures=ROI_brainstructures,
    ROIcortexL_fname=ROIcortexL_fname, ROIcortexR_fname=ROIcortexR_fname,
    ROIsubcortVol_fname=ROIsubcortVol_fname, write_dir=write_dir
  )
  do <- x$do; ROI_do <- x$ROI_do; sep_fnames <- x$sep_fnames; rm(x)

  brainstructures <- names(do)[do]
  brainstructures[brainstructures=="sub"] <- "subcortical"

  # Modify output file names
  fix_sep_fname <- function(x){ file.path(
    dirname(x),
    gsub(extn_cifti, basename(x), bname_cifti, fixed=TRUE)
  ) }
  sep_fnames <- vapply(sep_fnames, fix_sep_fname, "")

  # Check brainstructures
  bs_present <- brainstructures %in% cifti_info$cifti$brainstructures
  if (!all(bs_present)) {
    warning(paste0(
      "Only the following brainstructures are present in the CIFTI file: ",
      paste(cifti_info$cifti$brainstructures, collapse=", "), "\n"
    ))
    brainstructures <- brainstructures[bs_present]
  }
  do <- c("left","right","subcortical") %in% brainstructures
  names(do) <- c("left", "right", "sub")

  # Build the Connectome Workbench command. 
  what <- switch(as.character(cifti_info$cifti$intent),
    `3002` = "metric", `3006` = "metric", `3007` = "label", "metric"
  )
  cmd <- paste("-cifti-separate", sys_path(cifti_fname), "COLUMN")
  if (do['left']) {
    cmd <- paste(cmd, paste0('-',what,' CORTEX_LEFT'), sys_path(sep_fnames["cortexL"]))
    if (ROI_do['left']) { cmd <- paste(cmd, '-roi', sys_path(sep_fnames["ROIcortexL"])) }
  }
  if (do['right']) {
    cmd <- paste(cmd, paste0('-',what,' CORTEX_RIGHT'), sys_path(sep_fnames["cortexR"]))
    if (ROI_do['right']) { cmd <- paste(cmd, '-roi', sys_path(sep_fnames["ROIcortexR"])) }
  }
  if (do['sub']) {
    cmd <- paste(cmd, '-volume-all', sys_path(sep_fnames["subcortVol"]))
    if (ROI_do['sub']) { cmd <- paste(cmd, '-roi', sys_path(sep_fnames["ROIsubcortVol"])) }
    cmd <- paste(cmd, '-label', sys_path(sep_fnames["subcortLabs"]))
  }

  # Run it.
  run_wb_cmd(cmd)

  invisible(sep_fnames)
}

#' @rdname separate_cifti
#' @export
separateCIfTI <- function(cifti_fname, 
  brainstructures=c("left","right"), 
  cortexL_fname=NULL, cortexR_fname=NULL, 
  subcortVol_fname=NULL, subcortLabs_fname=NULL, 
  ROI_brainstructures="all", 
  ROIcortexL_fname=NULL, ROIcortexR_fname=NULL, 
  ROIsubcortVol_fname=NULL, 
  write_dir=NULL){

  separate_cifti(
    cifti_fname=cifti_fname, brainstructures=brainstructures, 
    cortexL_fname=cortexL_fname, cortexR_fname=cortexR_fname, 
    subcortVol_fname=subcortVol_fname, subcortLabs_fname=subcortLabs_fname, 
    ROI_brainstructures=ROI_brainstructures, 
    ROIcortexL_fname=ROIcortexL_fname, ROIcortexR_fname=ROIcortexR_fname, 
    ROIsubcortVol_fname=ROIsubcortVol_fname, 
    write_dir=write_dir
  )
}

#' @rdname separate_cifti
#' @export
separatecii <- function(cifti_fname, 
  brainstructures=c("left","right"), 
  cortexL_fname=NULL, cortexR_fname=NULL, 
  subcortVol_fname=NULL, subcortLabs_fname=NULL, 
  ROI_brainstructures="all", 
  ROIcortexL_fname=NULL, ROIcortexR_fname=NULL, 
  ROIsubcortVol_fname=NULL, 
  write_dir=NULL){

  separate_cifti(
    cifti_fname=cifti_fname, brainstructures=brainstructures, 
    cortexL_fname=cortexL_fname, cortexR_fname=cortexR_fname, 
    subcortVol_fname=subcortVol_fname, subcortLabs_fname=subcortLabs_fname, 
    ROI_brainstructures=ROI_brainstructures, 
    ROIcortexL_fname=ROIcortexL_fname, ROIcortexR_fname=ROIcortexR_fname, 
    ROIsubcortVol_fname=ROIsubcortVol_fname, 
    write_dir=write_dir
  )
}