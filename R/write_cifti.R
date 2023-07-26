#' Write a CIFTI file from a \code{"xifti"} object
#'
#' Write out a \code{"xifti"} object as a CIFTI file and (optionally) GIFTI 
#'  surface files. 
#' 
#' @inheritParams xifti_Param
#' @inheritParams cifti_fname_Param
#' @param surfL_fname,surfR_fname If the \[left/right\] surface is present, it 
#'  will be a written to a GIFTI file at this file path. If \code{NULL} 
#'  (default), do not write out the surface.
#' @inheritParams verbose_Param_TRUE
#'
#' @return Named character vector of the written files
#' 
#' @family common
#' @family writing
#' @export
#'
#' @section Connectome Workbench:
#' This function interfaces with the \code{"-cifti-create-dense-timeseries"},
#'  \code{"-cifti-create-dense-scalar"}, or \code{"-cifti-create-label"} Workbench
#'  Command, depending on the input.
#' 
write_cifti <- function(
  xifti, cifti_fname, surfL_fname=NULL, surfR_fname=NULL,
  verbose=TRUE) {

  # Infer extension from name, and add it to `xifti`.
  extn_cifti <- get_cifti_extn(cifti_fname)
  if (extn_cifti %in% c("dtseries.nii", "dlabel.nii", "dscalar.nii")) {
    intent_cifti <- supported_intents()$value[
      match(extn_cifti, supported_intents()$extension)
    ]

    if (!is.null(xifti$meta$cifti$intent)) {
      if (intent_cifti != xifti$meta$cifti$intent) {
        ciftiTools_warn(paste0(
          "The `xifti` intent is ", xifti$meta$cifti$intent, ", but writing a ", 
          gsub(".nii", "", extn_cifti), " (", intent_cifti, 
          ") instead to match the output file name.\n"
        ))
      }
    } else {
      xifti$meta$cifti$intent <- intent_cifti
    }

  } else if (!is.null(xifti$meta$cifti$intent)) {
    # Add correct file extension.
    extn_cifti <- supported_intents()$extension[
      match(xifti$meta$cifti$intent, supported_intents()$value)
    ]
    cifti_fname <- paste0(cifti_fname, ".", extn_cifti)
  }

  # Problem with label xifti that has NA/NaN data
  if (extn_cifti=="dlabel.nii") {
    if (any(is.na(as.matrix(xifti)))) {
      stop(
        "Cannot write out label data with NA/NaN values. ",
        "Create a new label for NA/NaN, impute these values, or ", 
        "otherwise get rid them to write out the file."
      )
    }
  }

  # Label xifti's with multiple columns having different label tables need to be 
  #   written one column at a time, because GIFTI/NIFTI files only permit one
  #   label table at a time, even for multi-column data.
  # [TO DO]

  sep_fnames <- write_xifti2(xifti=xifti, write_dir=tempdir(), verbose=verbose)

  if (verbose) { cat("Creating CIFTI file from separated components.\n") }
  wcfs_kwargs <- list(
    cifti_fname=cifti_fname,
    timestep = xifti$meta$cifti$time_step, 
    timestart = xifti$meta$cifti$time_start,
    names = xifti$meta$cifti$names
  )
  if ("cortexL" %in% names(sep_fnames)) {
    wcfs_kwargs$cortexL_fname <- sep_fnames["cortexL"]
    if ("ROIcortexL" %in% names(sep_fnames)) {
      wcfs_kwargs$ROIcortexL_fname <- sep_fnames["ROIcortexL"]
    }
  }
  if ("cortexR" %in% names(sep_fnames)) {
    wcfs_kwargs$cortexR_fname <- sep_fnames["cortexR"]
    if ("ROIcortexR" %in% names(sep_fnames)) {
      wcfs_kwargs$ROIcortexR_fname <- sep_fnames["ROIcortexR"]
    }
  }
  if ("subcortVol" %in% names(sep_fnames)) {
    wcfs_kwargs$subcortVol_fname <- sep_fnames["subcortVol"]
    if ("subcortLabs" %in% names(sep_fnames)) {
      wcfs_kwargs$subcortLabs_fname <- sep_fnames["subcortLabs"]
    }
  }
  do.call(write_cifti_from_separate, wcfs_kwargs)

  # Surfaces
  do_left_surf <- !is.null(surfL_fname) && !is.null(xifti$surf$cortex_left)
  do_right_surf <- !is.null(surfR_fname) && !is.null(xifti$surf$cortex_right)
  if (do_left_surf || do_right_surf) {
    if (verbose) { cat("Writing surface geometry GIFTI(s).\n") }

    if (do_left_surf) {
      write_surf_gifti(xifti$surf$cortex_left, surfL_fname, "left")
    }
    if (do_right_surf) {
      write_surf_gifti(xifti$surf$cortex_right, surfR_fname, "right")
    }
  }

  out <- unlist(list(
    cifti=cifti_fname, 
    surfL=surfL_fname, surfR=surfR_fname
  ))
}

#' @rdname write_cifti
#' @export
writeCIfTI <- function(
  xifti, cifti_fname, 
  surfL_fname=NULL, surfR_fname=NULL,
  verbose=TRUE) {
  write_cifti(
    xifti=xifti, cifti_fname=cifti_fname, 
    surfL_fname=surfL_fname, surfR_fname=surfR_fname,
    verbose=verbose
  )
}

#' @rdname write_cifti
#' @export
writecii <- function(
  xifti, cifti_fname, 
  surfL_fname=NULL, surfR_fname=NULL,
  verbose=TRUE) {
  write_cifti(
    xifti=xifti, cifti_fname=cifti_fname, 
    surfL_fname=surfL_fname, surfR_fname=surfR_fname,
    verbose=verbose
  )
}

#' @rdname write_cifti
#' @export
write_xifti <- function(
  xifti, cifti_fname, 
  surfL_fname=NULL, surfR_fname=NULL,
  verbose=TRUE) {
  write_cifti(
    xifti=xifti, cifti_fname=cifti_fname, 
    surfL_fname=surfL_fname, surfR_fname=surfR_fname,
    verbose=verbose
  )
}