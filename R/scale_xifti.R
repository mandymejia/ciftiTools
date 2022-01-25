#' Scale CIFTI
#' 
#' Scale CIFTI data. Similar to \code{\link[base]{scale}}.
#' 
#' @inheritParams xifti_Param
#' @param center,scale Arguments to \code{\link[base]{scale}}.
#' 
#' @return The input \code{"xifti"} with scaled columns.
#' 
#' @export
scale_xifti <- function(xifti, center=TRUE, scale=TRUE){
  stopifnot(is.xifti(xifti))

  if (!is.null(xifti$meta$cifti$intent)) {
    if (xifti$meta$cifti$intent == 3007) {
      ciftiTools_warn("Converting to `dscalar` before scaling.\n")
      xifti <- convert_xifti(xifti, "dscalar")
    }
  }

  xifti <- newdata_xifti(
    xifti,
    scale(as.matrix(xifti), center=center, scale=scale)
  )
}