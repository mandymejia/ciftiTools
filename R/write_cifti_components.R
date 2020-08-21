#' Write CIFTI Surface Data to GIFTI
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