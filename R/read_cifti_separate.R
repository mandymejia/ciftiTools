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
#' 
#' @inheritParams cifti_fname_Param
#' @inheritParams surfL_fname_Param
#' @inheritParams surfR_fname_Param
#' @inheritParams brainstructures_Param_LR
#' @inheritParams idx_Param
#' @inheritParams resamp_res_Param_optional
#' @inheritParams resamp_method_Param
#' @inheritParams resamp_area_Param
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
  cifti_fname, surfL_fname=NULL, surfR_fname=NULL,
  brainstructures=c("left","right"), idx=NULL,
  resamp_res=NULL, resamp_method=c("barycentric", "adaptive"),
  areaL_original_fname=NULL, areaR_original_fname=NULL,
  mwall_values=c(NA, NaN), verbose=TRUE) {

  # Setup ----------------------------------------------------------------------

  # Write separated and resampled intermediate/helper files to a temp. dir.
  write_dir_sep <- write_dir_resamp <- tempdir()

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

  # info_cifti() ---------------------------------------------------------------  
  
  cifti_info <- info_cifti(cifti_fname)
  bs_present <- brainstructures %in% cifti_info$cifti$brainstructures
  if (!all(bs_present)) {
    warning(paste0(
      "Only the following brainstructures are present in the CIFTI file: ",
      paste(cifti_info$cifti$brainstructures, collapse=", "), "\n"
    ))
    brainstructures <- ROI_brainstructures <- brainstructures[bs_present]
  }

  # Determine the original cortical resolution.
  original_res <- infer_resolution(cifti_info)
  if (!is.null(original_res) && any(original_res < 2 & original_res > 0)) {
    warning("The CIFTI resolution is already too low (< 2 vertices). Skipping resampling.")
    do_resamp <- FALSE
  }

  # separate_cifti() -----------------------------------------------------------

  if (verbose) { cat("Separating CIFTI file.\n") }

  to_read <- separate_cifti_wrapper(
    cifti_fname=cifti_fname,
    brainstructures=brainstructures, ROI_brainstructures=ROI_brainstructures,
    sep_fnames=NULL, write_dir=write_dir_sep
  )

  if (verbose) {
    print(Sys.time() - exec_time)
    exec_time <- Sys.time()
  }

  # Handle medial wall values --------------------------------------------------

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

  # resample_cifti_separate() --------------------------------------------------

  do_resamp <- !(is.null(resamp_res) || identical(resamp_res, FALSE))

  # Do not resample the subcortical data.
  to_resample <- to_read[!grepl("subcort", names(to_read))]

  do_resamp <- do_resamp && length(to_resample) > 0
  if (do_resamp) {
    if (verbose) { cat("Resampling CIFTI file.\n") }

    # Do resample_cifti_separate.
    resamp_result <- resample_cifti_wrapper(
      original_res=original_res, 
      resamp_res=resamp_res, resamp_method=resamp_method,
      areaL_original_fname=areaL_original_fname, 
      areaR_original_fname=areaR_original_fname,
      original_fnames=to_resample, resamp_fnames=NULL,
      surfL_original_fname=NULL, surfR_original_fname=NULL,
      read_dir=NULL, write_dir=write_dir_resamp
    )

    # Replace resampled files.
    to_read[names(to_read) %in% names(resamp_result)] <- resamp_result[names(to_read)[names(to_read) %in% names(resamp_result)]]

    #if (!is.null(surfL_fname)) { surfL_fname <- resamp_result["surfL"] }
    #if (!is.null(surfR_fname)) { surfR_fname <- resamp_result["surfR"] }

    if (verbose) {
      print(Sys.time() - exec_time)
      exec_time <- Sys.time()
    }
  }

  # make_xifti() ---------------------------------------------------------------  
  
  to_read["mwall_values"] <- list(mwall_values=mwall_values)
  to_read$cifti_info <- cifti_info
  # Erase misc metadata and replace with the name of the resampled file
  to_read$cifti_info$cifti$misc <- list(resampled_fname=cifti_fname)

  # Rename ROI arguments
  names(to_read)[names(to_read) == "ROIcortexL"] <- "cortexL_mwall"
  names(to_read)[names(to_read) == "ROIcortexR"] <- "cortexR_mwall"
  names(to_read)[names(to_read) == "ROIsubcortVol"] <- "subcortMask"

  # Read the CIFTI file from the separated files.
  if (verbose) { cat("Reading GIFTI and/or NIFTI files to form the CIFTI.\n") }

  if (!is.null(idx)) {
    to_read$idx <- idx
    if (!is.null(to_read$cifti_info$cifti$names)) {
      to_read$cifti_info$cifti$names <- to_read$cifti_info$cifti$names[idx]
    }
    if (!is.null(to_read$cifti_info$cifti$labels)) {
      to_read$cifti_info$cifti$labels <- to_read$cifti_info$cifti$labels[idx]
    }
  }
  xifti <- do.call(make_xifti, to_read)

  if (!is.null(surfL_fname) | !is.null(surfR_fname)) { 
    if(verbose) { cat("Adding surface(s).\n") }
  }
  if (!is.null(surfL_fname)) { xifti <- add_surf(xifti, surfL=surfL_fname) }
  if (!is.null(surfR_fname)) { xifti <- add_surf(xifti, surfR=surfR_fname) }

  if ("left" %in% brainstructures || "right" %in% brainstructures) {
    xifti$meta$cortex["resamp_res"] <- list(resamp_res)
  }
  if ("subcortical" %in% brainstructures) {
    xifti$meta$subcort[c("labels", "mask", "trans_mat")] <- to_read$cifti_info$subcort[c("labels", "mask", "trans_mat")]
  }

  if (verbose) {
    print(Sys.time() - exec_time)
    exec_time <- Sys.time()
  }

  xifti
}