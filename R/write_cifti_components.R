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
write_gifti_component_of_cifti <- function(data, out_fname, datatype=NULL, ...) {
  if (is.list(data)) { stop("Only a single data matrix is supported.") }
  stopifnot(is.matrix(data))
  
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

  # Convert to list
  data <- split(t(data), seq(ncol(data)))
  names(data) <- rep("data", length(data))

  # Patch
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

#' Save cifti object as cifti file
#'
#' @param xifti A "xifti" object.
# #' @param cifti_fname Name of cifti file to write, ending in .dtseries.nii, .dscalar.nii or .dlabel.nii
#' @inheritParams verbose_Param_FALSE
#' @inheritParams wb_path_Param
#'
#' @return Logical indicating whether CIFTI file was created.
#' @export
# #' @importFrom oro.nifti writeNIfTI
#' @importFrom RNifti writeNifti
#'
write_cifti_components <- function(xifti, verbose=FALSE, wb_path=NULL) {
  stopifnot(is.xifti(xifti))
  stopifnot(!any(sapply(xifti$data, is.null)))

  wb_cmd <- get_wb_cmd_path(wb_path)

  # Get intermediate file names.
  write_dir <- file.path(
    tempdir()#, paste0(sub("\\..*", "", basename(cifti_fname)), "-components")
  )
  if (!dir.exists(write_dir)) { dir.create(write_dir) }
  sep_names <- c("cortexL", "cortexR", "subcortVol", "subcortLab")
  sep_fnames <- lapply(sep_names, cifti_component_suffix)
  sep_fnames <- lapply(sep_fnames, function(x){file.path(write_dir, paste0("sep.", x))})
  names(sep_fnames) <- sep_names

  # Write the intermediate files.
  # TO DO: how to indicate medial wall?
  if (verbose) {cat("Writing left cortex.\n")}
  cortex_dat <- matrix(NA,
    nrow=length(xifti$meta$cortex$medial_wall_mask$left), 
    ncol=ncol(xifti$data$cortex_left)
  )
  cortex_dat[xifti$meta$cortex$medial_wall_mask$left,] <- xifti$data$cortex_left
  write_gifti_component_of_cifti(cortex_dat, sep_fnames$cortexL)
  
  if (verbose) {cat("Writing right cortex.\n")}
  cortex_dat <- matrix(NA,
    nrow=length(xifti$meta$cortex$medial_wall_mask$right), 
    ncol=ncol(xifti$data$cortex_right)
  )
  cortex_dat[xifti$meta$cortex$medial_wall_mask$right,] <- xifti$data$cortex_right
  write_gifti_component_of_cifti(cortex_dat, sep_fnames$cortexR)
  
  if (verbose) {cat("Writing subcortex.\n")}
  if (!any(is.na(do.call(rbind, xifti$meta$subcort$mask_padding)))) {
    mask <- pad_vol(xifti$meta$subcort$mask, xifti$meta$subcort$mask_padding, FALSE)
  } else {
    mask <- xifti$meta$subcort$mask
  }
  writeNifti(
    unmask(xifti$data$subcort, mask, fill=0), 
    sep_fnames$subcortVol
  )
  writeNifti(
    unmask(as.numeric(xifti$meta$subcort$labels), mask, fill=0), 
    sep_fnames$subcortLab
  )

  return(sep_fnames)
}
