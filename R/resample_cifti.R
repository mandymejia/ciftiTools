#' Resample CIFTI Data
#'
#' Performs spatial resampling of CIFTI data on the cortical surface
#'  by separating it into GIFTI and NIFTI files, resampling the GIFTIs, and then 
#'  putting them together. (The subcortex is not resampled.) 
#' 
#'  Can accept a \code{"xifti"} object as well as a path to a CIFTI-file.
#'
#' @inheritSection Connectome_Workbench_Description Connectome Workbench Requirement
#' 
#' @param x The CIFTI file name or \code{"xifti"} object to resample. If
#'  \code{NULL}, the result will be a \code{"xifti"} with resampled surfaces 
#'  given by \code{surfL_original_fname} and \code{surfR_original_fname}.
#' @param cifti_target_fname File name for the resampled CIFTI. Will be placed
#'  in \code{write_dir}. If \code{NULL}, will be written to "resampled.d*.nii". 
#'  \code{write_dir} will be appended to the beginning of the path.
#' @param surfL_original_fname,surfR_original_fname (Optional) Path to a GIFTI 
#'  surface geometry file representing the left/right cortex. One or both can be
#'  provided. These will be resampled too, and are convenient for visualizing
#'  the resampled data. 
#' 
#'  If \code{x} is a \code{"xifti"} object with surfaces, these arguments
#'  will override the surfaces in the \code{"xifti"}.
#' @param surfL_target_fname,surfR_target_fname (Optional) File names for the
#'  resampled GIFTI surface geometry files. Will be placed in \code{write_dir}. 
#'  If \code{NULL} (default), will use default names created by 
#'  \code{\link{resample_cifti_default_fname}}. 
#' @inheritParams resamp_res_Param_required
#' @param write_dir Where to write the resampled CIFTI (and surfaces if present.)
#'  If \code{NULL} (default), will use the current working directory if \code{x}
#'  was a CIFTI file, and a temporary directory if \code{x} was a \code{"xifti"}
#'  object.
#' @param mwall_values If the medial wall locations are not indicated in the
#'  CIFTI, use these values to infer the medial wall mask. Default: 
#'  \code{c(NA, NaN)}. If \code{NULL}, do not attempt to infer the medial wall.
#' 
#'  Correctly indicating the medial wall locations is important for resampling,
#'  because the medial wall mask is taken into account during resampling
#'  calculations.
#' @inheritParams verbose_Param_TRUE
#'
#' @return A named character vector of written files: \code{"cifti"} and
#'  potentially \code{"surfL"} (if \code{surfL_original_fname} was provided) 
#'  and/or \code{"surfR"} (if \code{surfR_original_fname} was provided).
#' 
#' @export
#'
resample_cifti <- function(
  x=NULL, cifti_target_fname=NULL, 
  surfL_original_fname=NULL, surfR_original_fname=NULL,
  surfL_target_fname=NULL, surfR_target_fname=NULL,
  resamp_res, write_dir=NULL, mwall_values=c(NA, NaN), verbose=TRUE) {

  # Handle if no data ----------------------------------------------------------
  if (is.null(x)) {
    if (is.null(surfL_original_fname) && is.null(surfR_original_fname)) {
      warning("`x`, `surfL_original_fname` and `surfR_original_fname` were all NULL: Nothing to resample!\n")
      return(NULL)
    }
    return(read_cifti(
      surfL_fname=surfL_original_fname, 
      surfR_fname=surfR_original_fname,
      resamp_res=resamp_res
    ))
  }
  
  if (all(vapply(x$data, is.null, FALSE))) {
    x <- add_surf(x, surfL=surfL_original_fname, surfR=surfR_original_fname)
    if (!is.null(x$surf$cortex_left)) {
      x$surf$cortex_left <- resample_surf(x$surf$cortex_left, resamp_res, "left")
    }
    if (!is.null(x$surf$cortex_right)) {
      x$surf$cortex_right <- resample_surf(x$surf$cortex_right, resamp_res, "right")
    }
    return(x)
  }

  # Args check -----------------------------------------------------------------
  input_is_xifti <- is.xifti(x, messages=FALSE)
  if (is.null(write_dir)) { 
    write_dir <- ifelse(input_is_xifti, tempdir(), getwd())
  }
  stopifnot(resamp_res > 0)
  surfL_return <- surfR_return <- FALSE

  if (verbose) { exec_time <- Sys.time() }

  # Setup ----------------------------------------------------------------------
  if (input_is_xifti) {
    # Check intent. Treat unknown itents as dscalar.
    x_intent <- x$meta$cifti$intent
    if (!is.null(x_intent) && (x_intent %in% supported_intents()$value)) {
      x_extn <- supported_intents()$extension[supported_intents()$value == x_intent]
    } else {
      warning("The CIFTI intent was unknown, so resampling as a dscalar.")
      x_extn <- "dscalar.nii"
    }

    # Write out the CIFTI.
    cifti_original_fname <- file.path(tempdir(), paste0("to_resample.", x_extn))
    write_cifti(x, cifti_original_fname, verbose=FALSE)

    # Set the target CIFTI file name.
    if (is.null(cifti_target_fname)) {
      cifti_target_fname <- gsub(
        "to_resample.", "resampled.", cifti_original_fname, fixed=TRUE
      )
    } else {
      cifti_target_fname <- format_path(write_dir, cifti_target_fname, mode=2)
    }

    # Get the surfaces present.
    if (is.null(surfL_original_fname) && !is.null(x$surf$cortex_left)) {
      surfL_return <- TRUE
      surfL_original_fname <- file.path(tempdir(), "left.surf.gii")
      write_surf_gifti(x$surf$cortex_left, surfL_original_fname, hemisphere="left")
    }
    if (is.null(surfR_original_fname) && !is.null(x$surf$cortex_right)) {
      surfR_return <- TRUE
      surfR_original_fname <- file.path(tempdir(), "right.surf.gii")
      write_surf_gifti(x$surf$cortex_right, surfR_original_fname, hemisphere="right")
    }

    cifti_info <- x$meta
    brainstructures <- vector("character")
    if (!is.null(x$data$cortex_left)) { brainstructures <- c(brainstructures, "left") }
    if (!is.null(x$data$cortex_right)) { brainstructures <- c(brainstructures, "right") }
    if (!is.null(x$data$subcort)) { brainstructures <- c(brainstructures, "subcort") }
    ROI_brainstructures <- brainstructures

  } else {
    # Check that the original file is valid.
    cifti_original_fname <- x
    stopifnot(file.exists(cifti_original_fname))
    cifti_info <- info_cifti(cifti_original_fname)
    brainstructures <- ROI_brainstructures <- cifti_info$cifti$brainstructures
    # Set the target CIFTI file name.
    if (is.null(cifti_target_fname)) {
      cifti_target_fname <- file.path(
        write_dir, paste0("resampled.", get_cifti_extn(cifti_original_fname))
      )
    }
  }

  # Check that at least one surface is present.
  if (!("left" %in% brainstructures || "right" %in% brainstructures)) {
    warning("The CIFTI does not have cortical data, so there's nothing to resample.")
    if (input_is_xifti) { return(x) } else { return(NULL) }
  }

  # Determine the original cortical resolution.
  if (!("left" %in% brainstructures)) {
    original_res <- length(cifti_info$cortex$medial_wall_mask$left)
  } else {
    original_res <- length(cifti_info$cortex$medial_wall_mask$right)
  }
  if (original_res < 2) {
    warning("The CIFTI resolution is already too low (< 2 vertices).")
    if (input_is_xifti) { return(x) } else { return(NULL) }
  }

  # Separate the CIFTI ---------------------------------------------------------

  if (verbose) { cat("Separating CIFTI file.\n") }

  to_cif <- separate_cifti_wrapper(
    cifti_fname=cifti_original_fname, 
    brainstructures=brainstructures, ROI_brainstructures=ROI_brainstructures,
    sep_fnames=NULL, write_dir=tempdir()
  )

  if (verbose) { 
    print(Sys.time() - exec_time)
    exec_time <- Sys.time()
  }

  # Handle medial wall values --------------------------------------------------

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

  # resample_cifti_components() ------------------------------------------------
  
  # Do not resample the subcortical data.
  to_resample <- to_cif[!grepl("subcort", names(to_cif))]
  if (verbose) { cat("Resampling CIFTI file.\n") }

  # Do resample_cifti_components.
  resamp_result <- resample_cifti_wrapper(
    original_res=original_res, resamp_res=resamp_res, 
    original_fnames=to_resample, resamp_fnames=NULL, 
    surfL_fname=surfL_original_fname, surfR_fname=surfR_original_fname,
    surfL_target_fname=surfL_target_fname, 
    surfR_target_fname=surfR_target_fname,
    read_dir=NULL, write_dir=tempdir()
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

  # Put together ---------------------------------------------------------------

  # Create target CIFTI dense timeseries.
  if (verbose) cat("Merging components into a CIFTI file... \n")
  to_cif <- to_cif[names(to_cif) != "ROIsubcortVol"]
  wcfs_kwargs <- c(list(cifti_fname=cifti_target_fname), as.list(to_cif))
  do.call(write_cifti_from_separate, wcfs_kwargs)
  
  if (verbose) { 
    print(Sys.time() - exec_time)
    exec_time <- Sys.time()
  }

  # Return results -------------------------------------------------------------
  if (input_is_xifti) {
    read_xifti_args <- list(
      cifti_fname = cifti_target_fname, 
      brainstructures = brainstructures
    )
    if (surfL_return) { read_xifti_args$surfL_fname <- surfL_target_fname }
    if (surfR_return) { read_xifti_args$surfR_fname <- surfR_target_fname }
    return(do.call(read_xifti, read_xifti_args))
  } else {
    return(unlist(list(
      cifti=cifti_target_fname, 
      surfL=surfL_target_fname, surfR=surfR_target_fname
    )))
  }
}

#' @rdname resample_cifti
#' @export
resampleCIfTI <- function(
  x, cifti_target_fname, 
  surfL_original_fname=NULL, surfR_original_fname=NULL,
  surfL_target_fname=NULL, surfR_target_fname=NULL,
  resamp_res, write_dir=NULL, mwall_values=c(NA, NaN), verbose=TRUE) {

  resample_cifti(
    x=x, cifti_target_fname=cifti_target_fname, 
    surfL_original_fname=surfL_original_fname, surfR_original_fname=surfR_original_fname,
    surfL_target_fname=surfL_target_fname, surfR_target_fname=surfR_target_fname,
    resamp_res=resamp_res, write_dir=write_dir, mwall_values=mwall_values, verbose=verbose
  ) 
}

#' @rdname resample_cifti
#' @export
resamplecii <- function(
  x, cifti_target_fname, 
  surfL_original_fname=NULL, surfR_original_fname=NULL,
  surfL_target_fname=NULL, surfR_target_fname=NULL,
  resamp_res, write_dir=NULL, mwall_values=c(NA, NaN), verbose=TRUE) {

  resample_cifti(
    x=x, cifti_target_fname=cifti_target_fname, 
    surfL_original_fname=surfL_original_fname, surfR_original_fname=surfR_original_fname,
    surfL_target_fname=surfL_target_fname, surfR_target_fname=surfR_target_fname,
    resamp_res=resamp_res, write_dir=write_dir, mwall_values=mwall_values, verbose=verbose
  ) 
}

#' @rdname resample_cifti
#' @export
resample_xifti <- function(
  x, cifti_target_fname, 
  surfL_original_fname=NULL, surfR_original_fname=NULL,
  surfL_target_fname=NULL, surfR_target_fname=NULL,
  resamp_res, write_dir=NULL, mwall_values=c(NA, NaN), verbose=TRUE) {

  resample_cifti(
    x=x, cifti_target_fname=cifti_target_fname, 
    surfL_original_fname=surfL_original_fname, surfR_original_fname=surfR_original_fname,
    surfL_target_fname=surfL_target_fname, surfR_target_fname=surfR_target_fname,
    resamp_res=resamp_res, write_dir=write_dir, mwall_values=mwall_values, verbose=verbose
  ) 
}