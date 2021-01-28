#' Fix a \code{"xifti"} object
#' 
#' Make adjustments to a putative \code{"xifti"} so that it is valid. Each
#'  adjustment is reported.
#' 
#' Right now it only coerces the data to numeric.
#' 
#' @param xifti The xifti
#' @param verbose Report each adjustment? Default: \code{TRUE}
#' @return The fixed \code{"xifti"}
#' @keywords internal
#' 
fix_xifti <- function(xifti, verbose=TRUE) {
  xifti2 <- transform_xifti(xifti, FUN=as.numeric)
  if (!identical(xifti, xifti2) && verbose) { cat("Coerced to numeric.\n") }
  xifti2
}