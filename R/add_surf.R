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
  if (all(resamp_res %in% c(0, NA, NaN))) { 
    resamp_res <- NULL
  }

  # Left.
  if (!is.null(surfL)) {
    if (!is.null(xifti$surf$cortex_left)) { 
      ciftiTools_msg("Overwriting existing geometry for left cortex.\n") 
    }
    if (is.null(resamp_res) || resamp_res[1] %in% c(0, NA, NaN)) {
      warning("Could not infer left cortex resolution, so the left surface cannot be added.")
    } else {
      if (is.character(surfL) && length(surfL)==1 && surfL %in% c("very inflated", "inflated", "midthickness")) {
        xifti$surf$cortex_left <- load_surf("left", surfL, resamp_res[1])
      } else {
        z <- read_surf(surfL, "left")
        if (nrow(z$vertices) != resamp_res[1]) { 
          z <- resample_surf(z, resamp_res[1], "left") 
          if ((nrow(z$vertices) != resamp_res[1])) {
            warning("The left surface was resampled, but still might not match the data resolution.")
          }
        }
        xifti$surf$cortex_left <- z
      }
    }
  }

  # Right.
  if (!is.null(surfR)) {
    if (!is.null(xifti$surf$cortex_right)) { 
      ciftiTools_msg("Overwriting existing geometry for right cortex.\n") 
    }
    if (is.null(resamp_res) || resamp_res[1] %in% c(0, NA, NaN)) {
      warning("Could not infer right cortex resolution, so the right surface cannot be added.")
    } else {
      if (is.character(surfR) && length(surfR)==1 && surfR %in% c("very inflated", "inflated", "midthickness")) {
        xifti$surf$cortex_right <- load_surf("right", surfR, resamp_res[1])
      } else {
        z <- read_surf(surfR, "right")
        if (nrow(z$vertices) != resamp_res[1]) { 
          z <- resample_surf(z, resamp_res[1], "right") 
          if ((nrow(z$vertices) != resamp_res[1])) {
            warning("The right surface was resampled, but still might not match the data resolution.")
          }
        }
        xifti$surf$cortex_right <- z
      }
    }
  }

  # Check.
  if (!is.xifti(xifti)) { stop("The resulting \"xifti\" object was invalid.") }

  xifti
}