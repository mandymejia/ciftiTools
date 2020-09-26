#' Flatten a \code{"xifti"} object
#'
#' Concatenates the data in a \code{"xifti"} object into a single matrix. 
#'
#' If all brain structures are present, and if 
#'  \code{is.null(medial_wall)} and \code{subcort_order=="alphabetical"}
#'  (the default arguments), then the result will be identical to the matrix
#'  obtained with the Connectome Workbench command 
#'  \code{-cifti-export-dense-mapping}.
#'
#' @inheritParams xifti_Param
#' @param medial_wall Should vertices for the medial wall be included with the
#'  cortical data? If \code{NULL} (default), do not include them. If any other 
#'  value, use that value to fill rows corresponding to the medial wall mask.
#' @param subcort_order "alphabetical" (default) to match the Connectome 
#'  Worbench command \code{-cifti-export-dense-mapping}, or "spatial" which is 
#'  already the ordering used in the \code{"xifti"}.
#' 
#' @return The flattened \code{"xifti"}, a data matrix
#' 
#' @keywords internal
#'
flatten_xifti <- function(xifti, medial_wall=NULL, subcort_order="alphabetical") {
  if (!is.xifti(xifti)) { stop("The input \"xifti\" object is invalid.") }

  if (!is.null(medial_wall)) {
    stopifnot(length(medial_wall) == 1)
    if (!is.null(xifti$data$cortex_left)) {
      cortex_dat <- matrix(medial_wall,
        nrow=length(xifti$meta$cortex$medial_wall_mask$left), 
        ncol=ncol(xifti$data$cortex_left)
      )
      cortex_dat[xifti$meta$cortex$medial_wall_mask$left,] <- xifti$data$cortex_left
      xifti$data$cortex_left <- cortex_dat
    }
    if (!is.null(xifti$data$cortex_right)) {
      cortex_dat <- matrix(medial_wall,
        nrow=length(xifti$meta$cortex$medial_wall_mask$right), 
        ncol=ncol(xifti$data$cortex_right)
      )
      cortex_dat[xifti$meta$cortex$medial_wall_mask$right,] <- xifti$data$cortex_right
      xifti$data$cortex_right <- cortex_dat
    }
  }

  if (subcort_order=="alphabetical" && !is.null(xifti$data$subcort)) {
    alpha_order <- order(xifti$meta$subcort$labels)
    xifti$data$subcort <- xifti$data$subcort[alpha_order,, drop=FALSE]
  }

  do.call(rbind, xifti$data)
}