#' Flatten a "xifti"
#'
#' Concatenates the data in a "xifti" object into a single matrix. 
#'
#' @param xifti The "xifti" object
#' @param medial_wall For the cortical data, should vertices for the
#'  medial wall be included? If \code{NULL} (default), do not include medial
#'  wall vertices. If any other value, use that value to fill rows corresponding
#'  to the medial wall mask.
#' @param subcort_order "alphabetical" (default) to match 
#'  \code{-cifti-export-dense-mapping}, or "spatial" which is already the
#'  ordering used in the "xifti".
#'  
#' @return The flattened "xifti", a data matrix.
#' @export
#'
flatten_xifti <- function(xifti, medial_wall=NULL, subcort_order="alphabetical") {
  if (!(check_xifti(xifti))) { stop("Not a \"xifti\" object.") }

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