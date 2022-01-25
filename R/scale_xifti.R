#' Scale CIFTI
#' 
#' Scale CIFTI data. Similar to \code{\link[base]{scale}}.
#' 
#' @inheritParams xifti_Param
#' @param center,scale Arguments to \code{\link[base]{scale}}.
#' 
#' @return The input \code{"xifti"} with scaled columns.
#' 
#' @keywords export
scale_xifti <- function(x, center=TRUE, scale=TRUE){
  stopifnot(is.xifti(x))

  if (!is.null(x$meta$cifti$intent)) {
    if (x$meta$cifti$intent == 3007) {
      ciftiTools_warn("Converting to `dscalar` before scaling.\n")
      x <- convert_xifti(x, "dscalar")
    }
  }

  x <- newdata_xifti(x,
    scale(as.matrix(x), center=center, scale=scale)
  )
}