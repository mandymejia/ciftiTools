#' Resample CIFTI Data
#'
#' Performs spatial resampling of CIFTI data on the cortical surface
#'  by separating it into GIFTI and NIFTI files, resampling the GIFTIs, and then 
#'  putting them together. (The subcortex is not resampled.) 
#'
#' @param cifti_original_fname A CIFTI file to resample.
#' @param cifti_target_fname The file name to save the resampled CIFTI.
#' @param surfL_original_fname,surfR_original_fname (Optional) File path of 
#'  existing GIFTI surface geometry file representing the left/right cortex. 
#'  One or both can be provided. These will be resampled too, and are convenient
#'  for visualizing the resampled data.
#' @param surfL_target_fname,surfR_target_fname (Optional) File path for
#'  the resampled GIFTI surface geometry file representing the left/right 
#'  cortex. If NULL (default), will use default names: see 
#'  \code{resample_cifti_default_fname}.
#' @inheritParams resamp_res_Param_required
#' @inheritParams sep_fnames_Param
#' @inheritParams sep_keep_Param
#' @inheritParams resamp_fnames_Param
#' @inheritParams resamp_keep_Param
#' @inheritParams write_dir_Param_intermediate
#' @param mwall_values If the medial wall locations are not indicated in the
#'  CIFTI, use these values to infer the medial wall mask. Default: 
#'  \code{c(NA, NaN)}. If \code{NULL}, do not attempt to infer the medial wall.
#' 
#'  Correctly indicating the medial wall locations is important for resampling,
#'  because the medial wall mask is taken into account during resampling
#'  calculations.
#' @inheritParams verbose_Param_TRUE
#' @inheritParams wb_path_Param
#'
#' @return A named character vector of written files: \code{"cifti"} and
#'  potentially \code{"surfL"} (if \code{surfL_original_fname} was provided) 
#'  and/or \code{"surfR"} (if \code{surfR_original_fname} was provided).
#' @inheritSection Connectome_Workbench_Description Connectome Workbench Requirement
#' 
#' @export
#'
resample_cifti <- function(
  cifti_original_fname, cifti_target_fname, 
  surfL_original_fname=NULL, surfR_original_fname=NULL,
  surfL_target_fname=NULL, surfR_target_fname=NULL,
  resamp_res, 
  sep_keep=FALSE, sep_fnames=NULL, #separate_cifti
  resamp_keep=FALSE, resamp_fnames=NULL, # resample_cifti_components
  write_dir=NULL, 
  mwall_values=c(NA, NaN),
  verbose=TRUE, wb_path=NULL) {

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

  stopifnot(resamp_res > 0)

  cifti_target_fname <- format_path(cifti_target_fname, write_dir, mode=2)

  if (verbose) { exec_time <- Sys.time() }

  # ----------------------------------------------------------------------------
  # info_cifti() ---------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  cifti_info <- info_cifti(cifti_original_fname, wb_path)
  
  brainstructures <- ROI_brainstructures <- cifti_info$cifti$brainstructures

  if (!("left" %in% brainstructures || "right" %in% brainstructures)) {
    stop("The CIFTI does not have cortical data, so there's nothing to resample.")
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

  to_cif <- separate_cifti_wrapper(
    cifti_fname=cifti_original_fname, 
    brainstructures=brainstructures, ROI_brainstructures=ROI_brainstructures,
    sep_fnames=sep_fnames, write_dir=write_dir_sep, wb_path=wb_path
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
        to_cif["cortexL"], to_cif["cortexL"], 
        to_cif["ROIcortexL"], to_cif["ROIcortexL"], 
        mwall_values
      )
    }
    if ("right" %in% brainstructures) {
      fix_gifti_mwall(
        to_cif["cortexR"], to_cif["cortexR"], 
        to_cif["ROIcortexR"], to_cif["ROIcortexR"], 
        mwall_values
      )
    }
  }

  # ----------------------------------------------------------------------------
  # resample_cifti_components() ------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Do not resample the subcortical data.
  to_resample <- to_cif[!grepl("subcort", names(to_cif))]
  if (verbose) { cat("Resampling CIFTI file.\n") }

  # Do resample_cifti_components.
  resamp_result <- resample_cifti_wrapper(
    original_res=original_res, resamp_res=resamp_res, 
    original_fnames=to_resample, resamp_fnames=resamp_fnames, 
    surfL_fname=surfL_original_fname, surfR_fname=surfR_original_fname,
    surfL_target_fname=surfL_target_fname, 
    surfR_target_fname=surfR_target_fname,
    read_dir=NULL, write_dir=write_dir_resamp, wb_path=wb_path
  )

  # Replace resampled files.
  to_cif[names(to_cif) %in% names(resamp_result)] <- resamp_result[names(to_cif)[names(to_cif) %in% names(resamp_result)]]

  # Copy resampled surface files to desired file paths.
  if (!is.null(surfL_original_fname)) { 
    surfL_target_fname_old <- resamp_result["surfL"]
    surfL_target_fname <- format_path(basename(surfL_target_fname_old), write_dir, mode=2)
    file.copy(surfL_target_fname_old, surfL_target_fname)
  }
  if (!is.null(surfR_original_fname)) { 
    surfR_target_fname_old <- resamp_result["surfR"]
    surfR_target_fname <- format_path(basename(surfR_target_fname_old), write_dir, mode=2)
    file.copy(surfR_target_fname_old, surfR_target_fname)
  }

  if (verbose) { 
    print(Sys.time() - exec_time)
    exec_time <- Sys.time()
  }

  # ----------------------------------------------------------------------------
  # Put together ---------------------------------------------------------------
  # ----------------------------------------------------------------------------

  # Create target CIFTI dense timeseries.
  if (verbose) cat("Merging components into a CIFTI file... \n")
  to_cif <- to_cif[names(to_cif) != "ROIsubcortVol"]
  wcfs_kwargs <- c(list(cifti_fname=cifti_target_fname), as.list(to_cif))
  do.call(write_cifti_from_separate, wcfs_kwargs)
  
  if (verbose) { 
    print(Sys.time() - exec_time)
    exec_time <- Sys.time()
  }

  out <- unlist(list(
    cifti=cifti_target_fname, 
    surfL=surfL_target_fname, surfR=surfR_target_fname
  ))
}

#' @rdname resample_cifti
#' @export
resampleCIfTI <- function(
  cifti_original_fname, cifti_target_fname, 
  surfL_original_fname=NULL, surfR_original_fname=NULL,
  surfL_target_fname=NULL, surfR_target_fname=NULL,
  resamp_res, 
  sep_keep=FALSE, sep_fnames=NULL, #separate_cifti
  resamp_keep=FALSE, resamp_fnames=NULL, # resample_cifti_components
  write_dir=NULL, verbose=TRUE, wb_path=NULL) {

  resample_cifti(
    cifti_original_fname, cifti_target_fname, 
    surfL_original_fname, surfR_original_fname,
    surfL_target_fname, surfR_target_fname,
    resamp_res,
    sep_keep, sep_fnames, #separate_cifti
    resamp_keep, resamp_fnames, # resample_cifti_components
    write_dir, verbose, wb_path
  ) 
}

#' @rdname resample_cifti
#' @export
resamplecii <- function(
  cifti_original_fname, cifti_target_fname, 
  surfL_original_fname=NULL, surfR_original_fname=NULL,
  surfL_target_fname=NULL, surfR_target_fname=NULL,
  resamp_res,
  sep_keep=FALSE, sep_fnames=NULL, #separate_cifti
  resamp_keep=FALSE, resamp_fnames=NULL, # resample_cifti_components
  write_dir=NULL, verbose=TRUE, wb_path=NULL) {

  resample_cifti(
    cifti_original_fname, cifti_target_fname, 
    surfL_original_fname, surfR_original_fname,
    surfL_target_fname, surfR_target_fname,
    resamp_res,
    sep_keep, sep_fnames, #separate_cifti
    resamp_keep, resamp_fnames, # resample_cifti_components
    write_dir, verbose, wb_path
  ) 
}

#' @rdname resample_cifti
#' @export
resample_xifti <- function(
  cifti_original_fname, cifti_target_fname, 
  surfL_original_fname=NULL, surfR_original_fname=NULL,
  surfL_target_fname=NULL, surfR_target_fname=NULL,
  resamp_res,
  sep_keep=FALSE, sep_fnames=NULL, #separate_cifti
  resamp_keep=FALSE, resamp_fnames=NULL, # resample_cifti_components
  write_dir=NULL, verbose=TRUE, wb_path=NULL) {

  resample_cifti(
    cifti_original_fname, cifti_target_fname, 
    surfL_original_fname, surfR_original_fname,
    surfL_target_fname, surfR_target_fname,
    resamp_res,
    sep_keep, sep_fnames, #separate_cifti
    resamp_keep, resamp_fnames, # resample_cifti_components
    write_dir, verbose, wb_path
  ) 
}