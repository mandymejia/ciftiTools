#' Set \code{"xifti"} column names
#' 
#' Change the column names of a \code{"dscalar"} or \code{"dlabel"} 
#'  \code{"xifti"} object.
#' 
#' @param xifti A \code{"dscalar"} or \code{"dlabel"} \code{"xifti"} object.
#' @param names The new column names, as a character vector with length
#'  equal to the same number of columns in \code{xifti}.
#' 
#' @return \code{xifti} with the new column names.
#' @export 
#' 
set_names_xifti <- function(xifti, names){
  # Check input
  stopifnot(is.xifti(xifti))
  stopifnot(is.character(names))
  stopifnot(ncol(xifti) == length(names))

  # Get xifti intent
  xifti_intent <- c("dtseries", "dscalar", "dlabel")[
    match(xifti$meta$cifti$intent, c(3002, 3006, 3007))
  ]

  # Stop if not dscalar or dlabel
  if (length(xifti_intent) == 0 ) {
    stop("`xifti` must have intent 3006 (dscalar) or 3007 (dlabel).")
  } else if (xifti_intent == "dtseries") {
    stop(
      "`xifti` must have intent 3006 (dscalar) or 3007 (dlabel). ",
      "dtseries `xifti` do not have column names. Maybe you'd like to use ",
      "`convert_xifti` to convert it to a dscalar and then add names."
    )
  }

  # Set names
  xifti$meta$cifti$names <- names
  if (xifti_intent == "dlabel") {
    names(xifti$meta$cifti$labels) <- names
  }

  # Return
  xifti
}