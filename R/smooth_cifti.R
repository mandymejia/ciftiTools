#' Smooth CIFTI data
#'
#' Spatially smooth the metric data of a CIFTI file or \code{"xifti"} object.
#' 
#' If the CIFTI is a ".dlabel" file (intent 3007), then it will be converted
#'  to a ".dscalar" file because the values will no longer be integer indices.
#'  Unless the label values were ordinal, this is probably not desired so a
#'  warning will be printed.
#' 
#'  Can accept a \code{"xifti"} object as well as a path to a CIFTI-file.
#' 
#'  Surfaces are required for each hemisphere in the CIFTI. If they are not 
#'  provided, the default inflated surfaces will be used. 
#' 
#'  Conversion for sigma: \eqn{\sigma * 2 * sqrt(2*log(2)) = FWHM}
#' 
#' @param x The CIFTI file name or \code{"xifti"} object to smooth.
#' @param cifti_target_fname File name for the smoothed CIFTI. If
#'  \code{NULL}, will be written to "smoothed.d*.nii" in the current working
#'  directory if \code{x} was a CIFTI file, and in a temporary directory if 
#'  \code{x} was a \code{"xifti"} object.
#' @param surf_FWHM,vol_FWHM The full width at half maximum (FWHM) parameter
#'  for the gaussian surface or volume smoothing kernel, in mm. Default: \code{5}
#' @param surfL_fname,surfR_fname (Required if the corresponding cortex is 
#'  present) Surface GIFTI files for the left and right cortical surfaces. If not
#'  provided, the default surfaces will be used.
#' @param cerebellum_fname (Optional) Surface GIFTI file for the cerebellar surface
#' @param subcortical_zeroes_as_NA,cortical_zeroes_as_NA Should zero-values in 
#'  the subcortical volume or cortex be treated as NA? Default: \code{FALSE}.
#' @param subcortical_merged Smooth across subcortical structure boundaries?
#'  Default: \code{FALSE}.
#'
#' @return The \code{cifti_target_fname}, invisibly
#' 
#' @family common
#' @export
#'
#' @section Connectome Workbench:
#' This function interfaces with the \code{"-cifti-smoothing"} Workbench command.
#' 
smooth_cifti <- function(
  x, cifti_target_fname=NULL,
  surf_FWHM=5, vol_FWHM=5,
  surfL_fname=NULL, surfR_fname=NULL, cerebellum_fname=NULL,
  subcortical_zeroes_as_NA=FALSE, cortical_zeroes_as_NA=FALSE,
  subcortical_merged=FALSE){

  # Args check -----------------------------------------------------------------
  input_is_xifti <- is.xifti(x, messages=FALSE)
  surfL_return <- surfR_return <- FALSE

  # Setup ----------------------------------------------------------------------
  if (input_is_xifti) {
    # Check intent. Treat unknown intents as dscalar.
    x_intent <- x$meta$cifti$intent
    if (!is.null(x_intent) && (x_intent %in% supported_intents()$value)) {
      x_extn <- supported_intents()$extension[supported_intents()$value == x_intent]
    } else {
      warning("The CIFTI intent was unknown, so smoothing as a dscalar.")
      x_extn <- "dscalar.nii"
    }

    # Write out the CIFTI.
    cifti_original_fname <- file.path(tempdir(), paste0("to_smooth.", x_extn))
    write_cifti(x, cifti_original_fname, verbose=FALSE)

    # Set the target CIFTI file name.
    if (is.null(cifti_target_fname)) {
      cifti_target_fname <- gsub(
        "to_smooth.", "smoothed.", cifti_original_fname, fixed=TRUE
      )
    }

    # Get the surfaces present.
    if (is.null(surfL_fname) && !is.null(x$surf$cortex_left)) {
      surfL_return <- TRUE
      surfL_fname <- file.path(tempdir(), "left.surf.gii")
      write_surf_gifti(x$surf$cortex_left, surfL_fname, hemisphere="left")
    }
    if (is.null(surfR_fname) && !is.null(x$surf$cortex_right)) {
      surfR_return <- TRUE
      surfR_fname <- file.path(tempdir(), "right.surf.gii")
      write_surf_gifti(x$surf$cortex_right, surfR_fname, hemisphere="right")
    }

    cifti_info <- x$meta
    brainstructures <- vector("character")
    if (!is.null(x$data$cortex_left)) { brainstructures <- c(brainstructures, "left") }
    if (!is.null(x$data$cortex_right)) { brainstructures <- c(brainstructures, "right") }
    if (!is.null(x$data$subcort)) { brainstructures <- c(brainstructures, "subcort") }
  
  } else {
    # Check that the original file is valid.
    cifti_original_fname <- x
    stopifnot(file.exists(cifti_original_fname))
    cifti_info <- info_cifti(cifti_original_fname)
    brainstructures <- cifti_info$cifti$brainstructures
    # Set the target CIFTI file name.
    if (is.null(cifti_target_fname)) {
      cifti_target_fname <- file.path(
        getwd(), paste0("smoothed.", get_cifti_extn(cifti_original_fname))
      )
    }
  }

  # If the input is a .dlabel file, the target should be .dscalar not .dlabel. -
  fix_dlabel <- FALSE
  if (!is.null(cifti_info$cifti$intent) && cifti_info$cifti$intent == 3007) {
    warning(paste(
      "Smoothing a label file will convert the labels to their numeric",
      "indices. Coercing `cifti_target_fname` to a \".dscalar\" file.\n"
    ))
    fix_dlabel <- TRUE
    cifti_target_fname <- gsub(
      "dlabel.nii", "dscalar.nii", 
      cifti_target_fname, fixed=TRUE
    )
  }

  # Build the Connectome Workbench command. 
  cmd <- paste(
    "-cifti-smoothing", 
    sys_path(cifti_original_fname), 
    surf_FWHM / (2*sqrt(2*log(2))),
    vol_FWHM / (2*sqrt(2*log(2))),
    "COLUMN",
    sys_path(cifti_target_fname)
  )

  # Add default surface(s) where missing ---------------------------------------
  # If cortex data is present but its surface geometry is missing, use the
  #  surface included with `ciftiTools.`
  if ("left" %in% brainstructures && is.null(surfL_fname)) {
    ciftiTools_warn(paste(
      "No left surface provided to `smooth_cifti`,",
      "so using the surface included in `ciftiTools`."
    ))

    if (!is.xifti(x, messages=FALSE)) { x <- read_cifti(x, brainstructures=brainstructures) }

    ## Try in this order: `resamp_res`, medial wall mask, data length
    if (!is.null(x$meta$cifti$resamp_res)) {
      x_res <- x$meta$cifti$resamp_res
    } else if (!is.null(x$meta$cortex$medial_wall_mask$left)) {
      x_res <- length(x$meta$cortex$medial_wall_mask$left)
    } else {
      if (!is.null(x$data$cortex_left) && !is.null(x$data$cortex_right)) {
        if (nrow(x$data$cortex_left) != nrow(x$data$cortex_right)) {
          stop(paste(
            "The cortex resolution needs to be known to resample the cortex surface",
            "for use in smoothing. But, there was no resampling resolution",
            "or left medial wall mask in the `xifti`. Furthermore, the number of",
            "data vertices differed between the left and right cortices, meaning",
            "the cortex resolution cannot be inferred in any way."
          ))
        }
      }
      warning(paste(
        "No resampling resolution or left medial wall mask in the `xifti`.",
        "Using the number of left cortex vertices. This may cause an error if",
        "medial wall values were masked out."
      ))
      x_res <- nrow(x$data$cortex_left)
    }

    surfL_fname <- file.path(tempdir(), "left.surf.gii")
    surfL_fname <- resample_gifti(
      ciftiTools.files()$surf["left"], 
      surfL_fname, hemisphere="left", file_type="surface", resamp_res=x_res
    )
  }

  ## Try in this order: `resamp_res`, medial wall mask, data length
  if ("right" %in% brainstructures && is.null(surfR_fname)) {
    ciftiTools_warn(paste(
      "No right surface provided to `smooth_cifti`,",
      "so using the surface included in `ciftiTools`."
    ))

    if (!is.xifti(x, messages=FALSE)) { x <- read_cifti(x, brainstructures=brainstructures) }

    if (!is.null(x$meta$cifti$resamp_res)) {
      x_res <- x$meta$cifti$resamp_res
    } else if (!is.null(x$meta$cortex$medial_wall_mask$right)) {
      x_res <- length(x$meta$cortex$medial_wall_mask$right)
    } else {
      if (!is.null(x$data$cortex_right) && !is.null(x$data$cortex_right)) {
        if (nrow(x$data$cortex_right) != nrow(x$data$cortex_right)) {
          stop(paste(
            "The cortex resolution needs to be known to resample the cortex surface",
            "for use in smoothing. But, there was no resampling resolution",
            "or right medial wall mask in the `xifti`. Furthermore, the number of",
            "data vertices differed between the right and right cortices, meaning",
            "the cortex resolution cannot be inferred in any way."
          ))
        }
      }
      warning(paste(
        "No resampling resolution or right medial wall mask in the `xifti`.",
        "Using the number of right cortex vertices. This may cause an error if",
        "medial wall values were masked out."
      ))
      x_res <- nrow(x$data$cortex_right)
    }

    surfR_fname <- file.path(tempdir(), "right.surf.gii")
    surfR_fname <- resample_gifti(
      ciftiTools.files()$surf["right"], 
      surfR_fname, hemisphere="right", file_type="surface", resamp_res=x_res
    )
  }

  # Build and run command ------------------------------------------------------
  if (!is.null(surfL_fname)) { cmd <- paste(cmd, "-left-surface", sys_path(surfL_fname)) }
  if (!is.null(surfR_fname)) { cmd <- paste(cmd, "-right-surface", sys_path(surfR_fname)) }
  if (!is.null(cerebellum_fname)) { cmd <- paste(cmd, "-cerebellum-surface", sys_path(cerebellum_fname)) }  

  if (subcortical_zeroes_as_NA) { cmd <- paste(cmd, "-fix-zeros-volume") }
  if (cortical_zeroes_as_NA) { cmd <- paste(cmd, "-fix-zeros-surface") }

  if (subcortical_merged) { cmd <- paste(cmd, "-merged-volume") }

  run_wb_cmd(cmd)

  # Fix .dlabel output ---------------------------------------------------------
  if (fix_dlabel) {
    old_target_fname <- cifti_target_fname
    cifti_target_fname <- gsub("dlabel", "dscalar", old_target_fname)
    names_fname <- tempfile()
    cat(names(cifti_info$cifti$labels), file = names_fname, sep = "\n")
    run_wb_cmd(paste(
      "-cifti-change-mapping", old_target_fname, 
      "ROW", cifti_target_fname,
      "-scalar", "-name-file", names_fname
    ))
  }
  
  # Return results -------------------------------------------------------------
  if (input_is_xifti) {
    read_xifti_args <- list(
      cifti_fname = cifti_target_fname, 
      brainstructures = brainstructures
    )
    if (surfL_return) { read_xifti_args$surfL_fname <- surfL_fname }
    if (surfR_return) { read_xifti_args$surfR_fname <- surfR_fname }
    return(do.call(read_xifti, read_xifti_args))
  } else {
    return(invisible(cifti_target_fname))
  }
}

#' @rdname smooth_cifti
#' @export
smoothCIfTI <- function(
  x, cifti_target_fname=NULL,
  surf_FWHM=5, vol_FWHM=5,
  surfL_fname=NULL, surfR_fname=NULL, cerebellum_fname=NULL,
  subcortical_zeroes_as_NA=FALSE, cortical_zeroes_as_NA=FALSE,
  subcortical_merged=FALSE){

  smooth_cifti(
    x=x, cifti_target_fname=cifti_target_fname,
    surf_FWHM=surf_FWHM, vol_FWHM=vol_FWHM,
    surfL_fname=surfL_fname, surfR_fname=surfR_fname, cerebellum_fname=cerebellum_fname,
    subcortical_zeroes_as_NA=subcortical_zeroes_as_NA, cortical_zeroes_as_NA=cortical_zeroes_as_NA,
    subcortical_merged=subcortical_merged
  )
}

#' @rdname smooth_cifti
#' @export
smoothcii <- function(
  x, cifti_target_fname=NULL,
  surf_FWHM=5, vol_FWHM=5,
  surfL_fname=NULL, surfR_fname=NULL, cerebellum_fname=NULL,
  subcortical_zeroes_as_NA=FALSE, cortical_zeroes_as_NA=FALSE,
  subcortical_merged=FALSE){

  smooth_cifti(
    x=x, cifti_target_fname=cifti_target_fname,
    surf_FWHM=surf_FWHM, vol_FWHM=vol_FWHM,
    surfL_fname=surfL_fname, surfR_fname=surfR_fname, cerebellum_fname=cerebellum_fname,
    subcortical_zeroes_as_NA=subcortical_zeroes_as_NA, cortical_zeroes_as_NA=cortical_zeroes_as_NA,
    subcortical_merged=subcortical_merged
  )
}

#' @rdname smooth_cifti
#' @export
smooth_xifti <- function(
  x, cifti_target_fname=NULL,
  surf_FWHM=5, vol_FWHM=5,
  surfL_fname=NULL, surfR_fname=NULL, cerebellum_fname=NULL,
  subcortical_zeroes_as_NA=FALSE, cortical_zeroes_as_NA=FALSE,
  subcortical_merged=FALSE){

  smooth_cifti(
    x=x, cifti_target_fname=cifti_target_fname,
    surf_FWHM=surf_FWHM, vol_FWHM=vol_FWHM,
    surfL_fname=surfL_fname, surfR_fname=surfR_fname, cerebellum_fname=cerebellum_fname,
    subcortical_zeroes_as_NA=subcortical_zeroes_as_NA, cortical_zeroes_as_NA=cortical_zeroes_as_NA,
    subcortical_merged=subcortical_merged
  )
}