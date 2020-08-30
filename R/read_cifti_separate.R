#' Read in CIFTI Data
#'
#' @description Read a CIFTI file by separating it into GIFTI and NIFTI files 
#'  (\code{\link{separate_cifti}}), optionally resampling them, 
#'  and then reading each separated 
#'  component into R (\code{\link{make_xifti}}).
#'
#' @inheritParams cifti_fname_Param
#' @inheritParams surfL_fname_Param
#' @inheritParams surfR_fname_Param
#' @inheritParams brainstructures_Param_LR
#' @inheritParams ROI_brainstructures_Param_LR
#' @inheritParams resamp_res_Param_optional
#' @inheritParams sep_keep_Param
#' @inheritParams sep_fnames_Param
#' @inheritParams resamp_keep_Param
#' @inheritParams resamp_fnames_Param
#' @inheritParams write_dir_Param_intermediate
#' @inheritParams verbose_Param_TRUE
#' @inheritParams wb_path_Param
#'
#' @return A \code{"xifti"} object. See \code{\link{is.xifti}}.
#'
#' @details This function uses a system wrapper for the "wb_command"
#'  executable. The user must first download and install the Connectome
#'  Workbench, available from
#'  \url{https://www.humanconnectome.org/software/get-connectome-workbench}.
#'  The \code{wb_path} argument is the path to the Connectime Workbench folder
#'  or executable.
#'
#' @keywords internal
#' 
read_cifti_separate <- function(
  cifti_fname, 
  surfL_fname=NULL, surfR_fname=NULL,
  brainstructures=c("left","right"), ROI_brainstructures=NULL,
  resamp_res=NULL, 
  sep_keep=FALSE, sep_fnames=NULL,
  resamp_keep=FALSE, resamp_fnames=NULL,
  write_dir=NULL, verbose=TRUE, wb_path=NULL) {

  # ----------------------------------------------------------------------------
  # Setup ----------------------------------------------------------------------
  # ----------------------------------------------------------------------------

  if (sep_keep) {
    write_dir_sep <- write_dir
  } else {
    write_dir_sep <- tempdir()
  }
  if (resamp_keep) {
    write_dir_resamp <- write_dir
  } else {
    write_dir_resamp <- tempdir()
  }

  brainstructures <- match_input(
    brainstructures, c("left","right","subcortical","all"),
    user_value_label="brainstructures"
  )
  if ("all" %in% brainstructures) { 
    brainstructures <- c("left","right","subcortical")
  }
  if (!is.null(ROI_brainstructures)) {
    ROI_brainstructures <- match_input(ROI_brainstructures, brainstructures,
      user_value_label="ROI_brainstructures")
  }

  stopifnot(is.null(resamp_res) || resamp_res>0)

  if (verbose) { exec_time <- Sys.time() }

  # ----------------------------------------------------------------------------
  # info_cifti() ---------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  cifti_info <- info_cifti(cifti_fname, wb_path)

  if (!all(brainstructures %in% cifti_info$cifti$brainstructures)) {
    stop(paste0(
      "Only the following brainstructures are present in the CIFTI file:",
      paste(cifti_info$cifti$brainstructures, collapse=", ")
    ))
  }

  if (!("left" %in% brainstructures)) {
    original_res <- length(cifti_info$cortex$medial_wall_mask$left)
  } else {
    original_res <- length(cifti_info$cortex$medial_wall_mask$right)
  }
  stopifnot(original_res > 0)

  # ----------------------------------------------------------------------------
  # separate_cifti() -----------------------------------------------------------
  # ----------------------------------------------------------------------------

  if (verbose) { cat("Separating CIFTI file.\n") }

  sep_result <- separate_cifti_wrapper(
    cifti_fname=cifti_fname,
    brainstructures=brainstructures, ROI_brainstructures=ROI_brainstructures,
    sep_fnames=sep_fnames, write_dir=write_dir_sep, wb_path=wb_path
  )

  to_read <- sep_result$fname
  names(to_read) <- sep_result$label

  if (verbose) {
    print(Sys.time() - exec_time)
    exec_time <- Sys.time()
  }

  # ----------------------------------------------------------------------------
  # resample_cifti_separate() --------------------------------------------------
  # ----------------------------------------------------------------------------

  do_resamp <- !(is.null(resamp_res) || identical(resamp_res, FALSE))
  # Do not resample the subcortical data.
  to_resample <- to_read[!grepl("subcort", names(to_read))]

  do_resamp <- do_resamp && length(to_resample) > 0
  if (do_resamp) {
    if (verbose) { cat("Resampling CIFTI file.\n") }

    # Do resample_cifti_separate.
    resamp_result <- resample_cifti_wrapper(
      original_res=original_res, resamp_res=resamp_res, 
      original_fnames=to_resample, resamp_fnames=resamp_fnames,
      surfL_fname=surfL_fname, surfR_fname=surfR_fname,
      read_dir=NULL, write_dir=write_dir_resamp, wb_path=wb_path
    )

    # Replace resampled files.
    to_read_resampled <- names(to_read)[names(to_read) %in% resamp_result$label]
    to_read[to_read_resampled] <- resamp_result$fname[
      resamp_result$label %in% to_read_resampled]

    if (!is.null(surfL_fname)) {
      surfL_fname <- resamp_result$fname[resamp_result$label == "surfL"]
    }
    if (!is.null(surfR_fname)) {
      surfR_fname <- resamp_result$fname[resamp_result$label == "surfR"]
    }

    if (verbose) {
      print(Sys.time() - exec_time)
      exec_time <- Sys.time()
    }
  }

  # ----------------------------------------------------------------------------
  # make_xifti() ---------------------------------------------------------------
  # ----------------------------------------------------------------------------

  if (is.null(resamp_res)) {
    to_read <- c(list(cifti_info=cifti_info), to_read)
  }

  # ROIs are not supported yet.
  is_ROI <- grepl("ROI", names(to_read))
  if(any(is_ROI)){
    warning(paste(
      "ROIs are not supported by ciftiTools yet.",
      "The separated ROI file(s) has been created but will not be read in.\n"
    ))
  }
  to_read <- to_read[!is_ROI]
  to_read <- as.list(to_read)

  if (!is.null(surfL_fname)) { to_read$surfL <- surfL_fname }
  if (!is.null(surfR_fname)) { to_read$surfR <- surfR_fname }

  # Read the CIFTI file from the separated files.
  if (verbose) { cat("Reading GIFTI and NIFTI files to form the CIFTI.\n") }
  xifti <- do.call(make_xifti, to_read)

  if (endsWith(cifti_fname, ".dlabel.nii")) {
    if ("left" %in% brainstructures) { xifti$data$cortex_left <- xifti$data$cortex_left + 1 }
    if ("right" %in% brainstructures) { xifti$data$cortex_right <- xifti$data$cortex_right + 1 }
    if ("subcortical" %in% brainstructures) { xifti$data$subcort <- xifti$data$subcort + 1 }
  }

  if ("left" %in% brainstructures || "right" %in% brainstructures) {
    xifti$meta$cortex$resamp_res <- resamp_res
  }
  xifti$meta$cifti <- cifti_info$cifti

  if (verbose) {
    print(Sys.time() - exec_time)
    exec_time <- Sys.time()
  }


  xifti
}