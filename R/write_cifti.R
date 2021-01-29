#' Write CIFTI component files from a \code{"xifti"} object.
#'
#' Write metric GIFTIs for the cortical surface data and NIFTIs for the
#'  subcortical labels and mask in a \code{"xifti"} object. Each present
#'  brainstructure will be written; if a brainstructure is absent the 
#'  corresponding file is not written.
#' 
#' @inheritParams xifti_Param
#' @param extn_cifti The CIFTI extension e.g. "dtseries.nii"
#' @inheritParams write_dir_Param_generic
#' @param mwall_fill Value to use for the medial wall in the cortex GIFTIs. 
#'  Default: \code{NA}.
#' @param subcort_fill Value to use for out-of-mask voxels in the subcortex. 
#'  Default: \code{0}.
#' @inheritParams verbose_Param_FALSE
#'
#' @return List of written files
#' @importFrom RNifti writeNifti
#'
#' @keywords internal
#' 
write_cifti_components <- function(
  xifti, extn_cifti, write_dir=NULL, 
  mwall_fill=NA, subcort_fill=0,
  verbose=FALSE) {
  # Check arguments.
  stopifnot(is.xifti(xifti))
  stopifnot(length(mwall_fill)==1)

  # Get intermediate file names.
  if (is.null(write_dir)) { write_dir <- getwd() }
  sep_names <- c(
    "cortexL", "ROIcortexL",
    "cortexR", "ROIcortexR", 
    "subcortVol", "subcortLabs" #"ROIsubcortVol"
  )
  sep_fnames <- vapply(sep_names, cifti_component_suffix, "")
  sep_fnames["cortexL"] <- gsub("func", "label", sep_fnames["cortexL"])
  sep_fnames["cortexR"] <- gsub("func", "label", sep_fnames["cortexR"])
  sep_fnames <- vapply(
    sep_fnames, 
    function(x){format_path(paste0("sep.", x), write_dir, mode=2)},
    ""
  )
  names(sep_fnames) <- sep_names

  if (extn_cifti == "dlabel.nii") {
    intent <- "label"
    data_type <- "INT32"
    label_table <- xifti$meta$cifti$labels
    if (length(label_table) > 1) {
      if (length(unique(label_table)) > 1) {
        warning(paste(
          "CIFTI files support a different label table for each column,",
          "but GIFTI files only support a single label table. Writing the",
          "`xifti` requires exporting the cortical data to GIFTI files.",
          "Using the label table for the first column."
        ))
      }
    }
    label_table <- label_table[[1]]
    col_names <- names(xifti$meta$cifti$labels)
  } else if (extn_cifti == "dscalar.nii") {
    col_names <- xifti$meta$cifti$names
    intent <- data_type <- label_table <- NULL
  } else {
    intent <- data_type <- label_table <- col_names <- NULL
  }

  # Left cortex
  if (!is.null(xifti$data$cortex_left)){
    if (verbose) {cat("Writing left cortex.\n")}
    # Add back medial wall.
    if (is.null(xifti$meta$cortex$medial_wall_mask$left)) {
      mwall <- rep(TRUE, nrow(xifti$data$cortex_left))
    } else {
      mwall <- xifti$meta$cortex$medial_wall_mask$left
    }
    cdat <- unmask_cortex(xifti$data$cortex_left, mwall)

    # Write data and ROI.
    write_metric_gifti(
      cdat, sep_fnames["cortexL"], "left", data_type = data_type,
      intent=intent, label_table = label_table, col_names = col_names
    )
    write_metric_gifti(
      as.numeric(mwall), sep_fnames["ROIcortexL"], 
      "left", data_type = "FLOAT32"
    )
  } else {
    sep_fnames <- sep_fnames[!grepl("cortexL", names(sep_fnames))]
  }

  ## Right cortex: add back medial wall.
  if (!is.null(xifti$data$cortex_right)){
    if (verbose) {cat("Writing right cortex.\n")}
    # Add back medial wall.
    if (is.null(xifti$meta$cortex$medial_wall_mask$right)) {
      mwall <- rep(TRUE, nrow(xifti$data$cortex_right))
    } else {
      mwall <- xifti$meta$cortex$medial_wall_mask$right
    }
    cdat <- unmask_cortex(xifti$data$cortex_right, mwall)

    # Write data and ROI.
    write_metric_gifti(
      cdat, sep_fnames["cortexR"], "right", data_type = data_type,
      intent=intent, label_table = label_table, col_names = col_names
    )
    write_metric_gifti(
      as.numeric(mwall), sep_fnames["ROIcortexR"], 
      "right", data_type = "FLOAT32"
    )
  } else {
    sep_fnames <- sep_fnames[!grepl("cortexR", names(sep_fnames))]
  }

  ## Subcortex: unmask to get volumetric array.
  if (!is.null(xifti$data$subcort)) {
    if (verbose) {cat("Writing subcortical data and labels.\n")}
    write_subcort_nifti(
      xifti$data$subcort, 
      xifti$meta$subcort$labels, 
      xifti$meta$subcort$mask, 
      xifti$meta$subcort$trans_mat,
      sep_fnames["subcortVol"], 
      sep_fnames["subcortLabs"], 
      #sep_fnames["ROIsubcortVol"],
      fill=0
    )
  } else {
    sep_fnames <- sep_fnames[!grepl("subcort", names(sep_fnames))]
  }

  invisible(sep_fnames)
}

#' Write a CIFTI file from a \code{"xifti"} object.
#'
#' Write out a \code{"xifti"} object as a CIFTI file and (optionally) GIFTI 
#'  surface files. 
#' 
#' @inheritSection Connectome_Workbench_Description Connectome Workbench Requirement
#' 
#' @inheritParams xifti_Param
#' @inheritParams cifti_fname_Param
#' @param surfL_fname,surfR_fname If the [left/right] surface is present, it 
#'  will be a written to a GIFTI file at this file path. If \code{NULL} 
#'  (default), do not write out the surface.
#' @inheritParams verbose_Param_TRUE
#'
#' @return Named character vector of the written files
#' 
#' @export
#'
write_cifti <- function(
  xifti, cifti_fname, surfL_fname=NULL, surfR_fname=NULL,
  verbose=TRUE) {

  extn_cifti <- get_cifti_extn(cifti_fname)
  sep_fnames <- write_cifti_components(
    xifti=xifti, extn_cifti=extn_cifti,
    write_dir=tempdir(), 
    verbose=verbose
  )

  if (verbose) { cat("Creating CIFTI file from separated components.\n") }
  wcfs_kwargs <- list(
    cifti_fname=cifti_fname,
    timestep = xifti$meta$cifti$time_step, 
    timestart = xifti$meta$cifti$time_start
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