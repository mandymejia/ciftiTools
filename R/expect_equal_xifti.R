#' Expect these CIFTIs to match
#' 
#' Raise an error if the CIFTIs do not match. 
#' 
#' \code{cifti$intent} and \code{cifti$brainstructures} are only compared if they
#'  exist for both files. \code{cifti$misc} is never compared. 
#' 
#' @param cii1 The first CIFTI
#' @param cii2 The second CIFTI
#' 
#' @return \code{TRUE}, invisibly
#' 
#' @keywords internal
#' 
expect_equal_xifti <- function(cii1, cii2) {

  if (!requireNamespace("testthat", quietly = TRUE)) {
    stop("Package \"testthat\" needed to use `view_xifti_surface`. Please install it.", call. = FALSE)
  }

  testthat::expect_equal(cii1$data, cii2$data)
  testthat::expect_equal(cii1$meta$subcort$labels, cii2$meta$subcort$labels)
  testthat::expect_equal(cii1$meta$subcort$mask, cii2$meta$subcort$mask)
  testthat::expect_equal(cii1$meta$cortex$medial_wall_mask$left, cii2$meta$cortex$medial_wall_mask$left)
  testthat::expect_equal(cii1$meta$cortex$medial_wall_mask$right, cii2$meta$cortex$medial_wall_mask$right)
  testthat::expect_equal(cii1$meta$cifti$names, cii2$meta$cifti$names)
  testthat::expect_equal(cii1$meta$cifti$labels, cii2$meta$cifti$labels)

  if (!is.null(cii1$meta$cifti$intent) && !is.null(cii2$meta$cifti$intent)) {
    testthat::expect_equal(cii1$meta$cifti$intent, cii2$meta$cifti$intent)
  }

  if (!is.null(cii1$meta$cifti$brainstructure) && !is.null(cii2$meta$cifti$brainstructure)) {
    testthat::expect_equal(cii1$meta$cifti$brainstructure, cii2$meta$cifti$brainstructure)
  }
}