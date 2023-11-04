#' \code{remap_cifti} wrapper
#'
#' Wrapper for \code{remap_cifti}. Calls \code{resample_cifti_components} using
#'  the original file names listed in the \code{original_fnames} argument and
#'  the target file names listed in the \code{remap_fnames} argument.
#'
#' @inheritParams original_fnames_Param_remapped
#' @param remap_fnames Where to write the resampled files. This is a named list
#'  where each entry's name is a file type label, and each entry's value
#'  is a file name indicating where to write the corresponding resampled file.
#'  The recognized file type labels are: "cortexL", "cortexR",
#'  "ROIcortexL", "ROIcortexR", "validROIcortexL", and "validROIcortexR".
#'
#'  Entry values can be \code{NULL}, in which case a default file name will be
#'  used: see \code{\link{resample_cifti_default_fname}}. Default file names
#'  will also be used for files that need to be resampled/written but without a
#'  corresponding entry in \code{remap_fnames}.
#'
#'  Entries in \code{remap_fnames} will be ignored if they are not needed
#'  based on \code{[ROI_]brainstructures}. For example, if
#'  \code{brainstructures="left"}, then \code{remap_fnames$cortexR} will be
#'  ignored if specified.
#'
#'  The \code{write_dir} argument can be used to place each resampled file in
#'  the same directory.
#' @param remap_method \code{"adaptive"} (default) or \code{"adaptive"}
#'  resampling. These options correspond to the Workbench command options
#'  \code{"BARYCENTRIC"} and \code{"ADAP_BARY_AREA"}, respectively.
#'
#'  For remapping between fs_LR group data and FreeSurfer fsaverage group data,
#'  adaptive resampling should be used.
#' @inheritParams area_original_Param
#' @inheritParams area_target_Param
#' @inheritParams read_dir_Param_separated
#' @inheritParams write_dir_Param_generic
#'
#' @return The return value of the \code{resample_cifti_components} call
#'
#' @keywords internal
#'
remap_cifti_wrapper <- function(
  original_fnames, remap_fnames=NULL,
  remap_method=c("adaptive", "barycentric"),
  areaL_original_fname=NULL, areaR_original_fname=NULL,
  areaL_target_fname=NULL, areaR_target_fname=NULL,
  sphereL_original_fname=NULL, sphereR_original_fname=NULL,
  sphereL_target_fname=NULL, sphereR_target_fname=NULL,
  read_dir=NULL, write_dir=NULL) {

  # Get kwargs.
  resamp_kwargs <- list(
    resamp_method=remap_method,
    areaL_original_fname=areaL_original_fname,
    areaR_original_fname=areaR_original_fname,
    areaL_target_fname=areaL_target_fname,
    areaR_target_fname=areaR_target_fname,
    sphereL_original_fname=sphereL_original_fname,
    sphereR_original_fname=sphereR_original_fname,
    sphereL_target_fname=sphereL_target_fname,
    sphereR_target_fname=sphereR_target_fname,
    read_dir=read_dir, write_dir=write_dir
  )

  # Get expected file names.
  expected_labs <- get_kwargs(resample_cifti_components)
  expected_labs <- expected_labs[grepl("fname", expected_labs, fixed=TRUE)]
  expected_labs <- unique(gsub("_.*", "", expected_labs))

  # Check and add original file names to the kwargs.
  if (!is.null(original_fnames)) {
    match_input(names(original_fnames), expected_labs,
      user_value_label="original_fnames")
    resamp_kwargs[paste0(names(original_fnames), "_original_fname")] <- original_fnames
  }
  # Check and add resampled/target file names to the kwargs.
  if (!is.null(remap_fnames)) {
    match_input(names(remap_fnames), expected_labs,
      user_value_label="remap_fnames")
    resamp_kwargs[paste0(names(remap_fnames), "_target_fname")] <- remap_fnames
  }

  # Do resample_cifti_components.
  resamp_kwargs[vapply(resamp_kwargs, is.null, FALSE)] <- NULL
  do.call(resample_cifti_components, resamp_kwargs)
}

#' Remap CIFTI data
#'
#' Remap CIFTI data between two different spaces, such as between FreeSurfer
#'  fsaverage group data and fs_LR group data.
#'
#'  Can accept a \code{"xifti"} object as well as a path to a CIFTI-file. If
#'  the input \code{"xifti"} object has surface geometry, it will be removed.
#'
#' @param x The CIFTI file name or \code{"xifti"} object to resample.
#' @param cifti_target_fname File name for the resampled CIFTI. Will be placed
#'  in \code{write_dir}. If \code{NULL}, will be written to "resampled.d*.nii".
#'  \code{write_dir} will be appended to the beginning of the path.
#' @param write_dir Where to write the resampled CIFTI (and surfaces if present.)
#'  If \code{NULL} (default), will use the current working directory if \code{x}
#'  was a CIFTI file, and a temporary directory if \code{x} was a \code{"xifti"}
#'  object.
#' @param remap_method \code{"adaptive"} (default) or \code{"adaptive"}
#'  resampling. These options correspond to the Workbench command options
#'  \code{"BARYCENTRIC"} and \code{"ADAP_BARY_AREA"}, respectively.
#'
#'  For remapping fs_LR group data to fsaverage, barycentric should be
#'  used. For remapping FreeSurfer fsaverage group data to fs_LR, adaptive
#'  should be used.
#' @param areaL_original_fname,areaL_target_fname File paths to the left cortex
#'  surfaces to use for vertex area correction during adaptive resampling.
#'  Required if \code{remap_method} is \code{"adaptive"}.
#' @param sphereL_original_fname,sphereL_target_fname File paths to the sphere
#'  surfaces in the original and target spaces, for the left cortex.
#' @param areaR_original_fname,areaR_target_fname,sphereR_original_fname,sphereR_target_fname
#'  See the correxponding arguments for the left cortex.
#' @param mwall_values If the medial wall locations are not indicated in the
#'  CIFTI, and if \code{ROIcortexL/R_original_fname} is not provided, then use
#'  these values to infer the medial wall mask. Default: \code{c(NA, NaN)}. If
#'  \code{NULL}, do not attempt to infer the medial wall.
#'
#'  Correctly indicating the medial wall locations is important for remapping,
#'  because the medial wall mask is taken into account during remapping
#'  calculations.
#' @inheritParams verbose_Param_TRUE
remap_cifti <- function(
  x, cifti_target_fname=NULL,
  remap_method=c("adaptive", "barycentric"),
  areaL_original_fname=NULL, areaR_original_fname=NULL,
  areaL_target_fname=NULL, areaR_target_fname=NULL,
  sphereL_original_fname=NULL, sphereR_original_fname=NULL,
  sphereL_target_fname=NULL, sphereR_target_fname=NULL,
  write_dir=NULL, mwall_values=c(NA, NaN), verbose=TRUE) {

  input_is_xifti <- is.xifti(x, messages=FALSE)

  # Args check -----------------------------------------------------------------
  if (is.null(write_dir) & input_is_xifti) { write_dir <- tempdir() }
  stopifnot(file.exists(areaL_original_fname))
  stopifnot(file.exists(areaR_original_fname))
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

    # Drop the surfaces, if any. 
    if (!is.null(x$surf$cortex_left)) {
      cat("Removing left cortex surface from `x`.")
      remove_xifti(x, "cortex_left")
    }
    if (!is.null(x$surf$cortex_right)) {
      cat("Removing right cortex surface from `x`.")
      remove_xifti(x, "cortex_right")
    }

    # Write out the CIFTI.
    cifti_original_fname <- file.path(tempdir(), paste0("to_remap.", x_extn))
    write_cifti(x, cifti_original_fname, verbose=FALSE)

    # Set the target CIFTI file name.
    if (is.null(cifti_target_fname)) {
      cifti_target_fname <- basename(gsub(
        "to_remap.", "remapped.", cifti_original_fname, fixed=TRUE
      ))
    }

    cifti_info <- x$meta
    brainstructures <- vector("character")
    if (!is.null(x$data$cortex_left)) { brainstructures <- c(brainstructures, "left") }
    if (!is.null(x$data$cortex_right)) { brainstructures <- c(brainstructures, "right") }
    if (!is.null(x$data$subcort)) { brainstructures <- c(brainstructures, "subcortical") }
    ROI_brainstructures <- brainstructures

    original_res <- infer_resolution(x)
    if (!is.null(original_res) && any(original_res < 2 & original_res > 0)) {
      warning("The CIFTI resolution is already too low (< 2 vertices). Skipping resampling.")
      return(x)
    }

  } else {
    # Check that the original file is valid.
    cifti_original_fname <- x
    stopifnot(file.exists(cifti_original_fname))
    cifti_info <- info_cifti(cifti_original_fname)
    brainstructures <- ROI_brainstructures <- cifti_info$cifti$brainstructures
    # Check that the resolutions match
    # Set the target CIFTI file name.
    if (is.null(cifti_target_fname)) {
      cifti_target_fname <- paste0("remapped.", get_cifti_extn(cifti_original_fname))
    }

    original_res <- infer_resolution(cifti_info)
    if (!is.null(original_res) && any(original_res < 2 & original_res > 0)) {
      warning("The CIFTI resolution is already too low (< 2 vertices). Skipping resampling.")
      return(NULL)
    }
  }
  cifti_target_fname <- format_path(cifti_target_fname, write_dir, mode=2)

  # Check that at least one surface is present.
  if (!("left" %in% brainstructures || "right" %in% brainstructures)) {
    warning("The CIFTI does not have cortical data, so there's nothing to remap.")
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
  remap_result <- remap_cifti_wrapper(
    remap_method=remap_method,
    areaL_original_fname=areaL_original_fname,
    areaR_original_fname=areaR_original_fname,
    areaL_target_fname=areaL_target_fname,
    areaR_target_fname=areaR_target_fname,
    sphereL_original_fname=sphereL_original_fname,
    sphereR_original_fname=sphereR_original_fname,
    sphereL_target_fname=sphereL_target_fname,
    sphereR_target_fname=sphereR_target_fname,
    original_fnames=to_resample, remap_fnames=NULL,
    read_dir=NULL, write_dir=tempdir()
  )

  # Replace remapped files.
  to_cif[names(to_cif) %in% names(remap_result)] <- remap_result[names(to_cif)[names(to_cif) %in% names(remap_result)]]

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
    return(do.call(read_xifti, read_xifti_args))
  } else {
    return(unlist(list(cifti=cifti_target_fname)))
  }
}

#' @rdname remap_cifti
#' @export
remapCIfTI <- function(
  x, cifti_target_fname=NULL,
  remap_method=c("adaptive", "barycentric"),
  areaL_original_fname=NULL, areaR_original_fname=NULL,
  areaL_target_fname=NULL, areaR_target_fname=NULL,
  sphereL_original_fname=NULL, sphereR_original_fname=NULL,
  sphereL_target_fname=NULL, sphereR_target_fname=NULL,
  write_dir=NULL, mwall_values=c(NA, NaN), verbose=TRUE) {

  remap_cifti(
    x=x, cifti_target_fname=cifti_target_fname,
    remap_method=remap_method,
    areaL_original_fname=areaL_original_fname, areaR_original_fname=areaR_original_fname,
    areaL_target_fname=areaL_target_fname, areaR_target_fname=areaR_target_fname,
    sphereL_original_fname=sphereL_original_fname, sphereR_original_fname=sphereR_original_fname,
    sphereL_target_fname=sphereL_target_fname, sphereR_target_fname=sphereR_target_fname,
    write_dir=write_dir, mwall_values=mwall_values, verbose=verbose
  )
}

#' @rdname remap_cifti
#' @export
remapcii <- function(
  x, cifti_target_fname=NULL,
  remap_method=c("adaptive", "barycentric"),
  areaL_original_fname=NULL, areaR_original_fname=NULL,
  areaL_target_fname=NULL, areaR_target_fname=NULL,
  sphereL_original_fname=NULL, sphereR_original_fname=NULL,
  sphereL_target_fname=NULL, sphereR_target_fname=NULL,
  write_dir=NULL, mwall_values=c(NA, NaN), verbose=TRUE) {

  remap_cifti(
    x=x, cifti_target_fname=cifti_target_fname,
    remap_method=remap_method,
    areaL_original_fname=areaL_original_fname, areaR_original_fname=areaR_original_fname,
    areaL_target_fname=areaL_target_fname, areaR_target_fname=areaR_target_fname,
    sphereL_original_fname=sphereL_original_fname, sphereR_original_fname=sphereR_original_fname,
    sphereL_target_fname=sphereL_target_fname, sphereR_target_fname=sphereR_target_fname,
    write_dir=write_dir, mwall_values=mwall_values, verbose=verbose
  )
}

#' @rdname remap_cifti
#' @export
remap_xifti <- function(
  x, cifti_target_fname=NULL,
  remap_method=c("adaptive", "barycentric"),
  areaL_original_fname=NULL, areaR_original_fname=NULL,
  areaL_target_fname=NULL, areaR_target_fname=NULL,
  sphereL_original_fname=NULL, sphereR_original_fname=NULL,
  sphereL_target_fname=NULL, sphereR_target_fname=NULL,
  write_dir=NULL, mwall_values=c(NA, NaN), verbose=TRUE) {

  remap_cifti(
    x=x, cifti_target_fname=cifti_target_fname,
    remap_method=remap_method,
    areaL_original_fname=areaL_original_fname, areaR_original_fname=areaR_original_fname,
    areaL_target_fname=areaL_target_fname, areaR_target_fname=areaR_target_fname,
    sphereL_original_fname=sphereL_original_fname, sphereR_original_fname=sphereR_original_fname,
    sphereL_target_fname=sphereL_target_fname, sphereR_target_fname=sphereR_target_fname,
    write_dir=write_dir, mwall_values=mwall_values, verbose=verbose
  )
}
