#' Expect these \code{"xifti"}s to match
#' 
#' Raise an error if the \code{"xifti"}s do not match. 
#' 
#' \code{cifti$intent} is only compared if it
#'  exists for both files. \code{cifti$brainstructures} and \code{cifti$misc} are not compared. 
#' 
#' @param xii1 The first \code{"xifti"}
#' @param xii2 The second \code{"xifti"}
#' 
#' @return \code{TRUE}, invisibly
#' 
#' @keywords internal
#' 
expect_equal_xifti <- function(xii1, xii2) {

  if (!requireNamespace("testthat", quietly = TRUE)) {
    stop("Package \"testthat\" needed to use `view_xifti_surface`. Please install it.", call. = FALSE)
  }

  testthat::expect_equal(xii1$data, xii2$data)
  testthat::expect_equal(xii1$meta$subcort$labels, xii2$meta$subcort$labels)
  testthat::expect_equal(xii1$meta$subcort$mask, xii2$meta$subcort$mask)
  testthat::expect_equal(xii1$meta$cortex$medial_wall_mask$left, xii2$meta$cortex$medial_wall_mask$left)
  testthat::expect_equal(xii1$meta$cortex$medial_wall_mask$right, xii2$meta$cortex$medial_wall_mask$right)
  testthat::expect_equal(xii1$meta$cifti$names, xii2$meta$cifti$names)
  testthat::expect_equal(xii1$meta$cifti$labels, xii2$meta$cifti$labels)

  if (!is.null(xii1$meta$cifti$intent) && !is.null(xii2$meta$cifti$intent)) {
    testthat::expect_equal(xii1$meta$cifti$intent, xii2$meta$cifti$intent)
  }

  # [TO DO]: Define this?
  # if (!is.null(xii1$meta$cifti$brainstructure) && !is.null(xii2$meta$cifti$brainstructure)) {
  #   testthat::expect_equal(xii1$meta$cifti$brainstructure, xii2$meta$cifti$brainstructure)
  # }
}