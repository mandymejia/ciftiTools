#' Concatenate \code{"xifti"} objects
#' 
#' Concatenate \code{"xifti"} objects along the columns. They must have the same
#'  brainstructures. The first \code{"xifti"}'s metadata will be retained, including
#'  its intent.
#' 
#' @param ...,xifti_list Provide as arguments the \code{"xifti"} objects to 
#'  concatenate, OR the single argument \code{xifti_list} which should be a 
#'  list of \code{"xifti"} objects. (If \code{xifti_list} is provided all other
#'  inputs will be ignored.)
#' 
#' @return The concatenated \code{"xifti"} object. 
#' 
#' @export
concat_xifti <- function(..., xifti_list=NULL) {
  if (is.null(xifti_list)) { xifti_list <- list(...) }
  if (!all(vapply(xifti_list, is.xifti, FALSE, messages=FALSE))) {
    stop("At least one input was not a `'xifti'`.")
  }

  xifti_out <- xifti_list[[1]]
  L <- length(xifti_list)

  for (bs in names(xifti_out$data)) {
    bs_sizes <- unique(vapply(xifti_list, function(x){ifelse(is.null(x$data[[bs]]), 0, nrow(x$data[[bs]]))}, 0))
    if (length(bs_sizes) > 1) { stop(bs, " dims are not identical across the `'xifti'`.") }
    if (bs_sizes > 0) { xifti_out$data[[bs]] <- do.call(cbind, lapply(xifti_list, function(x){x$data[[bs]]})) }
  }

  # Check names.
  xifti_names <- lapply(xifti_list, function(x){x$meta$cifti$names})
  if (!all(vapply(xifti_names, is.null, FALSE))) {
    for (ii in seq(L)) {
      if (is.null(xifti_list[[ii]]$meta$cifti$names)) {
        xifti_list[[ii]]$meta$cifti$names <- rep("", ncol_xifti(xifti_list[[ii]]))
      }
    }
    if (is.null(xifti_out$meta$cifti$intent) || xifti_out$meta$cifti$intent != 3002) {
      xifti_out$meta$cifti$names <- do.call(c, lapply(xifti_list, function(x){x$meta$cifti$names})) 
    } else {
      # Will overwrite if this component already exists...
      xifti_out$meta$cifti$misc$names <- do.call(c, lapply(xifti_list, function(x){x$meta$cifti$names})) 
    }
  }

  # Check labels.
  # A mixture of label & non-label xiftis shouldn't happen anyway, since the medial
  #   wall is typically masked out for non-label xiftis but not in label xiftis.
  xifti_labels <- lapply(xifti_list, function(x){x$meta$cifti$labels})
  if (!all(vapply(xifti_labels, is.null, FALSE))) {
    for (ii in seq(L)) {
      if (is.null(xifti_list[[ii]]$meta$cifti$labels)) {
        xifti_list[[ii]]$meta$cifti$labels <- rep(list(NULL), ncol(xifti_list[[ii]]))
      }
    }
    if (is.null(xifti_out$meta$cifti$intent) || xifti_out$meta$cifti$intent == 3007) {
      xifti_out$meta$cifti$labels <- do.call(c, lapply(xifti_list, function(x){x$meta$cifti$labels})) 
    } else {
      # Will overwrite if this component already exists...
      xifti_out$meta$cifti$misc$labels <- do.call(c, lapply(xifti_list, function(x){x$meta$cifti$labels})) 
    }
  }

  xifti_out
}