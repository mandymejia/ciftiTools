#' Read a CIFTI file with optional resampling
#'
#' Read a CIFTI file by writing each component into a GIFTI and NIFTI file
#'  (\code{\link{separate_cifti}}), optionally resampling the GIFTIs, 
#'  (\code{\link{resample_gifti}}), and then reading each separated 
#'  component into R (\code{\link{make_xifti}}). Surfaces can also be provided; 
#'  they will be resampled along with the CIFTI for viewing. 
#' 
#' The subcortical component (NIFTI) is not resampled.
#'
#' @inheritSection Connectome_Workbench_Description Connectome Workbench Requirement
#' 
#' @inheritParams cifti_fname_Param
#' @inheritParams surfL_fname_Param
#' @inheritParams surfR_fname_Param
#' @inheritParams brainstructures_Param_LR
#' @inheritParams resamp_res_Param_optional
#' @inheritParams sep_keep_Param
#' @inheritParams sep_fnames_Param
#' @inheritParams resamp_keep_Param
#' @inheritParams resamp_fnames_Param
#' @inheritParams write_dir_Param_intermediate
#' @param mwall_values If the medial wall locations are not indicated in the
#'  CIFTI, use these values to infer the medial wall mask. Default: 
#'  \code{c(NA, NaN)}. If \code{NULL}, do not attempt to infer the medial wall.
#' @inheritParams verbose_Param_TRUE
#'
#' @return A \code{"xifti"} object. See \code{\link{is.xifti}}.
#' 
#' @keywords internal
#' 
#' 
read_cifti_separate <- function(
  cifti_fname, 
  surfL_fname=NULL, surfR_fname=NULL,
  brainstructures=c("left","right"),
  resamp_res=NULL, 
  sep_keep=FALSE, sep_fnames=NULL,
  resamp_keep=FALSE, resamp_fnames=NULL,
  write_dir=NULL, 
  mwall_values=c(NA, NaN), verbose=TRUE) {

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
  ROI_brainstructures <- brainstructures

  stopifnot(is.null(resamp_res) || resamp_res>0)

  if (verbose) { exec_time <- Sys.time() }

  # ----------------------------------------------------------------------------
  # info_cifti() ---------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  cifti_info <- info_cifti(cifti_fname)
  bs_present <- brainstructures %in% cifti_info$cifti$brainstructures
  if (!all(bs_present)) {
    warning(paste0(
      "Only the following brainstructures are present in the CIFTI file: ",
      paste(cifti_info$cifti$brainstructures, collapse=", "), "\n"
    ))
    brainstructures <- ROI_brainstructures <- brainstructures[bs_present]
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

  to_read <- separate_cifti_wrapper(
    cifti_fname=cifti_fname,
    brainstructures=brainstructures, ROI_brainstructures=ROI_brainstructures,
    sep_fnames=sep_fnames, write_dir=write_dir_sep
  )

  if (verbose) {
    print(Sys.time() - exec_time)
    exec_time <- Sys.time()
  }

  # ----------------------------------------------------------------------------
  # Handle medial wall values --------------------------------------------------
  # ----------------------------------------------------------------------------

  if (!is.null(mwall_values)) {
    if ("left" %in% brainstructures) {
      fix_gifti_mwall(
        to_read["cortexL"], to_read["cortexL"], 
        to_read["ROIcortexL"], to_read["ROIcortexL"], 
        mwall_values
      )
    }
    if ("right" %in% brainstructures) {
      fix_gifti_mwall(
        to_read["cortexR"], to_read["cortexR"], 
        to_read["ROIcortexR"], to_read["ROIcortexR"], 
        mwall_values
      )
    }
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
      read_dir=NULL, write_dir=write_dir_resamp
    )

    # Replace resampled files.
    to_read[names(to_read) %in% names(resamp_result)] <- resamp_result[names(to_read)[names(to_read) %in% names(resamp_result)]]

    if (!is.null(surfL_fname)) { surfL_fname <- resamp_result["surfL"] }
    if (!is.null(surfR_fname)) { surfR_fname <- resamp_result["surfR"] }

    if (verbose) {
      print(Sys.time() - exec_time)
      exec_time <- Sys.time()
    }
  }

  # ----------------------------------------------------------------------------
  # make_xifti() ---------------------------------------------------------------
  # ----------------------------------------------------------------------------

  to_read <- as.list(to_read)
  
  to_read["mwall_values"] <- list(mwall_values=mwall_values)
  to_read$cifti_info$cifti <- cifti_info$cifti
  # Erase misc metadata and replace with the name of the resampled file
  to_read$cifti_info$cifti$misc <- list(resampled_fname=cifti_fname)

  # Rename ROI arguments
  names(to_read)[names(to_read) == "ROIcortexL"] <- "cortexL_mwall"
  names(to_read)[names(to_read) == "ROIcortexR"] <- "cortexR_mwall"
  names(to_read)[names(to_read) == "ROIsubcortVol"] <- "subcortMask"

  if (!is.null(surfL_fname)) { to_read["surfL"] <- surfL_fname }
  if (!is.null(surfR_fname)) { to_read["surfR"] <- surfR_fname }

  # Read the CIFTI file from the separated files.
  if (verbose) { cat("Reading GIFTI and NIFTI files to form the CIFTI.\n") }
  xifti <- do.call(make_xifti, to_read)

  if ("left" %in% brainstructures || "right" %in% brainstructures) {
    xifti$meta$cortex$resamp_res <- resamp_res
  }

  if (verbose) {
    print(Sys.time() - exec_time)
    exec_time <- Sys.time()
  }

  xifti
}