#' Add surface(s) to a \code{"xifti"}
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
#' @family manipulating
#' @export
#'
add_surf <- function(xifti, surfL=NULL, surfR=NULL) {
  if (!is.xifti(xifti)) { stop("The input \"xifti\" object is invalid.") }

  resamp_res <- infer_resolution(xifti)

  # Left.
  if (!is.null(surfL)) {
    if (!is.null(xifti$surf$cortex_left)) { 
      ciftiTools_msg("Overwriting existing geometry for left cortex.\n") 
    }
    if (is.character(surfL) && length(surfL)==1 && surfL %in% c("very inflated", "inflated", "midthickness")) {
      xifti$surf$cortex_left <- load_surf("left", surfL, resamp_res)
    } else {
      z <- read_surf(surfL, "left")
      if (nrow(z$vertices) != resamp_res) { z <- resample_surf(z, resamp_res, "left") }
      xifti$surf$cortex_left <- z
    }
  }

  # Right.
  if (!is.null(surfR)) {
    if (!is.null(xifti$surf$cortex_right)) { 
      ciftiTools_msg("Overwriting existing geometry for right cortex.\n") 
    }
    if (is.character(surfR) && length(surfR)==1 && surfR %in% c("very inflated", "inflated", "midthickness")) {
      xifti$surf$cortex_right <- load_surf("right", surfR, resamp_res=resamp_res)
    } else {
      z <- read_surf(surfR, "right")
      if (nrow(z$vertices) != resamp_res) { z <- resample_surf(z, resamp_res, "right") }
      xifti$surf$cortex_right <- z
    }
  }

  # Check.
  if (!is.xifti(xifti)) { stop("The resulting \"xifti\" object was invalid.") }

  xifti
}