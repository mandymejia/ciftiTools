#' Write out a GIFTI File
#' 
#' Write GIFTI-format data.
#' 
#' @param data A data matrix to write out as a GIFTI, i.e. a V (vertices) by T
#'  (measurements) matrix correpsonding to the left or right cortex.
#' @param out_fname The path to the GIFTI file to write
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
  
  if (!is.null(datatype)) { 
    stopifnot(length(datatype) == 1)
    stopifnot(datatype %in% c("NIFTI_TYPE_INT32", "NIFTI_TYPE_FLOAT32"))
  } else {
    datatype <- ifelse(
      max(abs(data - round(data))) == 0, 
      "NIFTI_TYPE_INT32", 
      "NIFTI_TYPE_FLOAT32"
    )
  }

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
# '
#' @param xifti A "xifti" object.
#' @param intent The NIFTI intent. https://nifti.nimh.nih.gov/nifti-1/documentation/nifti1fields/nifti1fields_pages/group__NIFTI1__INTENT__CODES.html/document_view
#' @param fname_out Name of cifti file to write, ending in .dtseries.nii, .dscalar.nii or .dlabel.nii
#' @inheritParams wb_path_Param
#'
#' @return Logical indicating whether CIFTI file was created.
#' @export
#' @importFrom RNifti writeNifti
# '
write_xifti <- function(xifti, intent="NIFTI_INTENT_VECTOR", fname_out, wb_path=NULL) {
  stop("Does not work yet.")
  stopifnot(check_xifti(xifti))
  stopifnot(any(sapply(xifti$data, is.null)))

  wb_cmd <- get_wb_cmd_path(wb_path)

  write_dir <- file.path(
    tempdir(), paste0(basename(fname_out), "-separated")
  )
  sep_names <- c("cortexL", "cortexR", "subcortVol", "subcortLabs")
  sep_fnames <- lapply(sep_fnames, separate_cifti_default_suffix)
  sep_fnames <- lapply(sep_fnames, function(x){file.path(write_dir, x)})
  names(sep_fnames) <- sep_fnames

  write_gifti_component_of_cifti(xifti$data$cortex_left, sep_names$cortexL)
  write_gifti_component_of_cifti(xifti$data$cortex_left, sep_names$cortexR)
  writeNifti(
    unmask(xifti$data$subcort, xifti$meta$subcort$mask, fill=0), 
    sep_names$subcortVol
  )
  writeNifti(
    unmask(xifti$meta$subcort$labels, xifti$meta$subcort$mask, fill=0), 
    sep_names$subcortLab
  )

  # ...

  # cifti_extn <- get_cifti_extn(fname_out)
  # if (grepl('dtseries',cifti_extn)) create_cmd <- '-cifti-create-dense-timeseries'
  # if (grepl('dscalar',cifti_extn)) create_cmd <- '-cifti-create-dense-scalar'
  # if (grepl('dlabel',cifti_extn)) create_cmd <- '-cifti-create-label'

  # if (!is.null(cifti$CORTEX_LEFT)) cmd_left <- paste

  # system(paste(wb_cmd, create_cmd, fname_out,
  #              '-volume', vol_orig, labels_orig,
  #              '-left-metric', surf_target_L,
  #              '-roi-left', roi_target_L,
  #              '-right-metric', surf_target_R,
  #              '-roi-right', roi_target_R, sep=' '))
  # }

}
