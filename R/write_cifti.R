#' Write out a GIFTI File
#' 
#' Write GIFTI-format data.
#' 
#' @param data A data matrix to write out as a GIFTI, i.e. a V (vertices) by T
#'  (measurements) matrix correpsonding to the left or right cortex.
#' @param out_fname The path to the GIFTI file to write
# #' @param intent The NIFTI intent. Default is \code{"NIFTI_INTENT_NORMAL"}. See:
# #'  https://nifti.nimh.nih.gov/nifti-1/documentation/nifti1fields/nifti1fields_pages/group__NIFTI1__INTENT__CODES.html/document_view
#' @param data_type A vector corresponding to the NIFTI types of \code{data}.
#'  For example, "NIFTI_TYPE_INT32" and "NIFTI_TYPE_FLOAT32". If \code{NULL}
#'  (default), the data_type will be inferred. 
#' @param ... Other vectors of options to 
#'  \code{\link[freesurferformats]{gifti_xml}}, for example "encoding" and
#'  "endian". 
#' 
#' @importFrom freesurferformats gifti_writer
#'
#' @keywords internal
#' 

#' Write Out Each Present Component in a "xifti"
#'
#' @inheritParams xifti_Param
#' @inheritParams write_dir_Param_generic
#' @param mwall_fill Value to use for the medial wall in the cortex GIFTIs. 
#'  Default: \code{NA}.
#' @param subcort_fill Value to use for out-of-mask voxels in the subcortex. 
#'  Default: \code{0}.
#' @inheritParams verbose_Param_FALSE
#' @inheritParams wb_path_Param
#'
#' @return List of written files
#' @importFrom RNifti writeNifti
#'
#' @keywords internal
#' 
write_xifti_components <- function(
  xifti, write_dir=NULL, 
  mwall_fill=NA, subcort_fill=0,
  verbose=FALSE, wb_path=NULL) {
  # Check arguments.
  stopifnot(is.xifti(xifti))
  stopifnot(length(mwall_fill)==1)

  # Get intermediate file names.
  if (is.null(write_dir)) { write_dir <- getwd() }
  sep_names <- c("cortexL", "cortexR", "subcortVol", "subcortLabs")
  sep_fnames <- lapply(sep_names, cifti_component_suffix)
  sep_fnames <- lapply(
    sep_fnames, 
    function(x){format_path(paste0("sep.", x), write_dir, mode=2)}
  )
  names(sep_fnames) <- sep_names

  print(sep_names)

  # Write the intermediate files.
  # TO DO: is it possible to indicate the medial wall?
  ## Left cortex: add back medial wall.
  if (!is.null(xifti$data$cortex_left)){
    if (verbose) {cat("Writing left cortex.\n")}
    if (is.null(xifti$meta$cortex$medial_wall_mask$left)) {
      mwall <- rep(TRUE, nrow(xifti$data$cortex_left))
    } else {
      mwall <- xifti$meta$cortex$medial_wall_mask$left
    }
    cdat <- unmask_cortex(xifti$data$cortex_left, mwall)
    write_metric_gifti(cdat, sep_fnames$cortexL, "left")
  } else {
    sep_fnames$cortexL <- NULL
  }

  ## Right cortex: add back medial wall.
  if (!is.null(xifti$data$cortex_right)){
    if (verbose) {cat("Writing right cortex.\n")}
    if (is.null(xifti$meta$cortex$medial_wall_mask$right)) {
      mwall <- rep(TRUE, nrow(xifti$data$cortex_right))
    } else {
      mwall <- xifti$meta$cortex$medial_wall_mask$right
    }
    cdat <- unmask_cortex(xifti$data$cortex_right, mwall)
    write_metric_gifti(cdat, sep_fnames$cortexR, "right")
  } else {
    sep_fnames$cortexR <- NULL
  }

  ## Subcortex: unmask to get volumetric array.
  if (!is.null(xifti$data$subcort)) {
    if (verbose) {cat("Writing subcortical data and labels.\n")}
    write_subcort_nifti(
      xifti$data$subcort, 
      xifti$meta$subcort$labels, 
      xifti$meta$subcort$mask, 
      sep_fnames$subcortVol, 
      sep_fnames$subcortLabs, 
      fill=0,
      wb_path
    )
  } else {
    sep_fnames$subcortVol <- sep_fnames$subcortLabs <- NULL
  }

  return(sep_fnames)
}

#' Write CIFTI
#'
#' Write out a CIFTI file from a "xifti" object. 
#'
#' @inheritParams xifti_Param
#' @inheritParams cifti_fname_Param
#' @param surfL_fname,surfR_fname If the left or right surface is present, it 
#'  will be a written to a GIFTI file at this file path. If \code{NULL} 
#'  (default), do not write out the surface file.
#' @inheritParams verbose_Param_TRUE
#' @inheritParams wb_path_Param
#'
#' @return Logical indicating whether the CIFTI file (and any surfaces) was 
#'  successfully written
#' @export
#'
write_cifti <- function(
  xifti, cifti_fname, surfL_fname=NULL, surfR_fname=NULL,
  verbose=TRUE, wb_path=NULL) {

  sep_fnames <- write_xifti_components(
    xifti=xifti, write_dir=tempdir(), 
    verbose=verbose, wb_path=wb_path
  )

  if (verbose) { cat("Creating CIFTI file from separated components.\n") }
  write_cifti_from_separate(
    cifti_fname=cifti_fname, 
    cortexL_fname=sep_fnames$cortexL, 
    cortexR_fname=sep_fnames$cortexR,
    subcortVol_fname=sep_fnames$subcortVol, 
    subcortLabs_fname=sep_fnames$subcortLabs,
    timestep=xifti$meta$cifti$time_step, timestart=xifti$meta$cifti$time_start,
    wb_path=wb_path
  )

  do_left_surf <- !is.null(surfL_fname) && !is.null(xifti$surf$cortex_left)
  do_right_surf <- !is.null(surfR_fname) && !is.null(xifti$surf$cortex_right)

  if (do_left_surf || do_right_surf) {
    if (verbose) { cat("Writing surface geometry GIFTI(s).\n") }

    if (do_left_surf) {
      write_surf_gifti(xifti$surf$cortex_left, surfL_fname, "left")
    }
    if (do_right_surf) {
      write_surf_gifti(xifti$surf$cortex_right, surfL_fname, "right")
    }
  }

  invisible(TRUE)
}

#' @rdname write_cifti
#' @export
writeCIfTI <- writecii <- write_xifti <- function(
  xifti, cifti_fname, 
  surfL_fname=NULL, surfR_fname=NULL,
  verbose=TRUE, wb_path=NULL) {
  write_cifti(
    xifti=xifti, cifti_fname=cifti_fname, 
    surfL_fname=surfL_fname, surfR_fname=surfR_fname,
    verbose=verbose, wb_path=wb_path
  )
}