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
#' @inheritParams sphereL_fname_Param
#' @inheritParams sphereR_fname_Param
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
read_cifti_separate <- function(
  cifti_fname, 
  surfL_fname=NULL, surfR_fname=NULL,
  brainstructures=c("left","right"), ROI_brainstructures=NULL,
  resamp_res=NULL, sphereL_fname=NULL, sphereR_fname=NULL,
  sep_keep=FALSE, sep_fnames=NULL,
  resamp_keep=FALSE, resamp_fnames=NULL,
  write_dir=NULL, verbose=TRUE, wb_path=NULL) {

  # ----------------------------------------------------------------------------
  # Setup ----------------------------------------------------------------------
  # ----------------------------------------------------------------------------

  # [TO DO]: more extensive preliminary check
  if (!is.null(resamp_res)) {
    if (is.null(sphereL_fname) | is.null(sphereR_fname)) {
      stop("`sphereL_fname` and `sphereR_fname` are required for resampling.")
    }
  }

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

  if (verbose) { exec_time <- Sys.time() }

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
      resamp_res=resamp_res, original_fnames=to_resample,
      resamp_fnames=resamp_fnames,
      sphereL_fname=sphereL_fname, sphereR_fname=sphereR_fname,
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
    to_read <- c(list(cifti_map=map_cifti(cifti_fname)), to_read)
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
  result <- do.call(make_xifti, to_read)

  if (verbose) {
    print(Sys.time() - exec_time)
    exec_time <- Sys.time()
  }

  # ----------------------------------------------------------------------------
  # Finish ---------------------------------------------------------------------
  # ----------------------------------------------------------------------------

  ## [TO DO]: The files are in tempdir(), so no need to manually delete?
  # Delete the separated files, unless otherwise requested.
  # if (!sep_keep) {
  #   for(f in sep_result$fname) {
  #     file.remove(f)
  #     if (file.exists(paste0(f, ".data"))) {
  #       file.remove(paste0(f, ".data"))
  #     }
  #   }
  # }
  # # Same for resampled files.
  # if (do_resamp && !resamp_keep) {
  #   for(f in resamp_result$fname) {
  #     file.remove(f)
  #     if (file.exists(paste0(f, ".data"))) {
  #       file.remove(paste0(f, ".data"))
  #     }
  #   }
  # }

  result
}