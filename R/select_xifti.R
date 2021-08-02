#' Select columns of a \code{"xifti"} object
#' 
#' Select column indices to keep in a \code{"xifti"}. Can also be used to
#'  reorder the columns.
#' 
#' @param xifti The \code{"xifti"} object.
#' @param idx The column indices to keep, in order.
#' @param add_meta Add \code{idx} to \code{xifti$meta$cifti$misc[[add_meta]]}
#'  for reference. Default: \code{"select"}. If \code{NULL} or an empty string,
#'  do not add a metadata entry.
#' 
#' @return The \code{"xifti"} with only the selected columns.
#' 
#' @family manipulating
#' @export
select_xifti <- function(xifti, idx, add_meta="select") {
  # Check input.
  stopifnot(is.xifti(xifti))
  idx <- as.numeric(idx)
  if (any(idx < 1) || !all(idx == round(idx))) { 
    stop("`idx` are the indices to select, so they should be positive integers.")
  }
  T_ <- ncol_xifti(xifti)
  if (T_ < 1) { warning("No columns to select!"); return(xifti) }
  if (any(idx > T_)) { stop("At least one `idx` greater than the number of columns in the xifti (", T_, ").") }

  # Subset each relevant component.
  for (bs in names(xifti$data)) {
    if (!is.null(xifti$data[[bs]])) { xifti$data[[bs]] <- xifti$data[[bs]][,idx,drop=FALSE] }
  }
  if (!is.null(xifti$meta$cifti$names)) {
    xifti$meta$cifti$names <- xifti$meta$cifti$names[idx]
  }
  if (!is.null(xifti$meta$cifti$labels)) {
    xifti$meta$cifti$labels <- xifti$meta$cifti$labels[idx]
  }
  if (length(add_meta) > 0) {
    add_meta <- as.character(add_meta)
    if (length(add_meta) > 1) { warning("Using the first entry of `add_meta`.") }
    xifti$meta$cifti$misc[[add_meta]] <- idx
  }

  xifti
}