#' Fix a \code{"xifti"}
#' 
#' Make adjustments to a putative \code{"xifti"} so that it is valid. Each
#'  adjustment is reported.
#' 
#' Right now it only coerces the data to numeric matrices.
#' 
#' @inheritParams xifti_Param
#' @param verbose Report each adjustment? Default: \code{TRUE}
#' @return The fixed \code{"xifti"}
#' 
#' @export
#' 
fix_xifti <- function(xifti, verbose=TRUE) {
  
  bs <- names(xifti$data)[!vapply(xifti$data, is.null, FALSE)]
  for (b in bs) {
    if (!is.matrix(xifti$data[[b]])) {
      cat("Coercing", b, "data to a matrix with one column\n")
      xifti$data[[b]] <- as.matrix(xifti$data[[b]])
    }
    if (!is.numeric(xifti$data[[b]])) {
      cat("Coercing", b, "data to numeric.\n")
      class(xifti$data[[b]]) <- "numeric"
    }
  }

  xifti
}