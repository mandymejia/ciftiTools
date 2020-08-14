#' Write out a GIFTI File
#' 
#' Write GIFTI-format data.
#' 
#' @param data A data matrix to write out as a GIFTI, i.e. a V (vertices) by T
#'  (measurements) matrix correpsonding to the left or right cortex.
#' @param out_fname The path to the GIFTI file to write
# #' @param intent The NIFTI intent. Default is \code{"NIFTI_INTENT_NORMAL"}. See:
# #'  https://nifti.nimh.nih.gov/nifti-1/documentation/nifti1fields/nifti1fields_pages/group__NIFTI1__INTENT__CODES.html/document_view
#' @param datatype A vector corresponding to the NIFTI types of \code{data}.
#'  For example, "NIFTI_TYPE_INT32" and "NIFTI_TYPE_FLOAT32". If \code{NULL}
#'  (default), the datatype will be inferred. 
#' @param ... Other vectors of options to 
#'  \code{\link[freesurferformats]{gifti_xml}}, for example "encoding" and
#'  "endian". 
#' 
#' @importFrom freesurferformats gifti_writer
#'
#' @keywords internal
#' 
write_gifti_component_of_cifti <- function(data, out_fname, datatype=NULL, ...) {
  # Check arguments.
  if (is.list(data)) { stop("Only a single data matrix is supported.") }
  stopifnot(is.matrix(data))
  
  # Get data type.
  if (is.null(datatype)) { 
    non_integer <- max(abs(data - round(data)))
    datatype <- ifelse(
      non_integer!=0 || is.na(non_integer),
      "NIFTI_TYPE_FLOAT32", 
      "NIFTI_TYPE_INT32"
    )
  } 
  stopifnot(length(datatype) == 1)
  stopifnot(datatype %in% c("NIFTI_TYPE_INT32", "NIFTI_TYPE_FLOAT32"))

  # Convert data columns to list.
  data <- split(t(data), seq(ncol(data)))
  names(data) <- rep("data", length(data))

  # We need to use ASCII encoding if the data type is integers.
  ## Why? Wasn't this a problem for gifti::write_gifti?
  ## [TO DO]: Check this.
  gifti_writer_wrapper <- function(encoding, ...){
    encoding <- "ascii"
    gifti_writer(...)
  }
  writer_FUN <- switch(datatype,
    NIFTI_TYPE_INT32 = gifti_writer_wrapper,
    NIFTI_TYPE_FLOAT32 = gifti_writer
  )

  # Write.
  invisible(
    writer_FUN(filepath=out_fname, data_array=data, ...)
  )
}

#' Write Out Each Present Component in a "xifti"
#'
#' @inheritParams xifti_Param
#' @inheritParams write_dir_Param_generic
#' @param medial_wall_val Value to use for the medial wall in the cortex GIFTIs. 
#'  Default: \code{NA}.
#' @param subcortex_fill_val Value to use for out-of-mask voxels in the subcortex. 
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
  medial_wall_val=NA, subcortex_fill_val=0,
  verbose=FALSE, wb_path=NULL) {
  # Check arguments.
  stopifnot(is.xifti(xifti))
  stopifnot(length(medial_wall_val)==1)

  # Get intermediate file names.
  if (is.null(write_dir)) { write_dir <- getwd() }
  sep_names <- c("cortexL", "cortexR", "subcortVol", "subcortLab")
  sep_fnames <- lapply(sep_names, cifti_component_suffix)
  sep_fnames <- lapply(
    sep_fnames, 
    function(x){format_path(paste0("sep.", x), write_dir, mode=2)}
  )
  names(sep_fnames) <- sep_names

  # Write the intermediate files.
  # TO DO: is it possible to indicate the medial wall?
  ## Left cortex: add back medial wall.
  if (!is.null(xifti$data$cortex_left)){
    if (verbose) {cat("Writing left cortex.\n")}
    cdat <- matrix(medial_wall_val,
      nrow=length(xifti$meta$cortex$medial_wall_mask$left), 
      ncol=ncol(xifti$data$cortex_left)
    )
    cdat[xifti$meta$cortex$medial_wall_mask$left,] <- xifti$data$cortex_left
    write_gifti_component_of_cifti(cdat, sep_fnames$cortexL)
  } else {
    sep_fnames$cortexL <- NULL
  }

  ## Right cortex: add back medial wall.
  if (!is.null(xifti$data$cortex_right)){
    if (verbose) {cat("Writing right cortex.\n")}
    cdat <- matrix(medial_wall_val,
      nrow=length(xifti$meta$cortex$medial_wall_mask$right), 
      ncol=ncol(xifti$data$cortex_right)
    )
    cdat[xifti$meta$cortex$medial_wall_mask$right,] <- xifti$data$cortex_right
    write_gifti_component_of_cifti(cdat, sep_fnames$cortexR)
  } else {
    sep_fnames$cortexR <- NULL
  }

  ## Subcortex: unmask to get volumetric array.
  if (!is.null(xifti$data$subcort)) {
    if (verbose) {cat("Writing subcortex.\n")}
    ## (first pad the mask if the cropping is known.)
    if (!any(is.na(do.call(rbind, xifti$meta$subcort$mask_padding)))) {
      mask <- pad_vol(
        xifti$meta$subcort$mask, 
        xifti$meta$subcort$mask_padding, 
        FALSE
      )
    } else {
      mask <- xifti$meta$subcort$mask
    }
    ## Data.
    writeNifti(
      unmask(xifti$data$subcort, mask, fill=subcortex_fill_val), 
      sep_fnames$subcortVol
    )
    ## Labels..
    writeNifti(
      unmask(as.numeric(xifti$meta$subcort$labels), mask, fill=subcortex_fill_val), 
      sep_fnames$subcortLab
    )
    # Add back subcortical label information.
    # https://www.humanconnectome.org/software/workbench-command/-volume-help
    subcort_lab_list <- system.file("subcort_label_list.txt", package="ciftiTools")
    cmd <- paste(
      "-volume-label-import", 
      sys_path(sep_fnames$subcortLab), 
      sys_path(subcort_lab_list), 
      sys_path(sep_fnames$subcortLab)
    )
    run_wb_cmd(cmd, wb_path)
  } else {
    sep_fnames$subcortVol <- sep_fnames$subcortLab <- NULL
  }

  return(sep_fnames)
}

#' Write CIFTI
#'
#' Write out a CIFTI file from a "xifti" object. Each brainstructure must be
#'  present.
#'
#' @inheritParams xifti_Param
#' @inheritParams cifti_fname_Param
#' @param timestep If a dense time series (dtseries.nii) file is being written,
#'  this is the time between measurements. If \code{NULL}, use the Connectome
#'  Workbench default (1.0).
#' @param timestart If a dense time series (dtseries.nii) file is being written,
#'  this is starting time. If \code{NULL}, use the Connectome Workbench default 
#'  (0.0).
#' @inheritParams verbose_Param_TRUE
#' @inheritParams wb_path_Param
#'
#' @return Logical indicating whether the CIFTI file was successfully written
#' @export
#'
write_cifti <- function(
  xifti, cifti_fname, 
  timestep=NULL, timestart=NULL,
  verbose=TRUE, wb_path=NULL) {
  stopifnot(!any(sapply(xifti$data, is.null)))

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
    subcortLab_fname=sep_fnames$subcortLab,
    timestep=timestep, timestart=timestart,
    wb_path=wb_path
  )
}

#' @rdname write_cifti
#' @export
writeCIfTI <- writecii <- write_xifti <- function(
  xifti, cifti_fname, 
  verbose=TRUE, wb_path=NULL) {
  write_cifti(
    xifti=xifti, cifti_fname=cifti_fname, 
    verbose=verbose, wb_path=wb_path
  )
}