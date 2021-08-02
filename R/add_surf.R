#' Add surface(s) to a \code{"xifti"} object
#' 
#' Add left or right cortical surface geometry to a \code{"xifti"} object.
#' 
#' \code{surfL} will be added to \code{xifti$surf$cortex_left} and \code{surfR} 
#'  will be added to \code{xifti$surf$cortex_right}. Any existing surfaces will 
#'  be overwritten.
#' 
#' @inheritParams xifti_Param
#' @inheritParams surfL_Param_optional
#' @inheritParams surfR_Param_optional
#' 
#' @return the \code{"xifti"} object with added surface geometry components.
#' 
#' @family surfing
#' @export
#'
add_surf <- function(xifti, surfL=NULL, surfR=NULL) {
  if (!is.xifti(xifti)) { stop("The input \"xifti\" object is invalid.") }

  # Left.
  if (!is.null(surfL)) {
    if (!is.null(xifti$surf$cortex_left)) { 
      ciftiTools_msg("Overwriting existing geometry for left cortex.\n") 
    }
    xifti$surf$cortex_left <- make_surf(surfL, "left")
  }

  # Right.
  if (!is.null(surfR)) {
    if (!is.null(xifti$surf$cortex_right)) { 
      ciftiTools_msg("Overwriting existing geometry for right cortex.\n") 
    }
    xifti$surf$cortex_right <- make_surf(surfR, "right")
  }

  # Check.
  if (!is.xifti(xifti)) { stop("The resulting \"xifti\" object was invalid.") }

  xifti
}