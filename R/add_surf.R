#' Add surface(s) to a \code{"xifti"}
#'
#' Add left or right cortical surface geometry to a \code{"xifti"} object.
#'
#' \code{surfL} will be added to \code{xifti$surf$cortex_left} and \code{surfR}
#'  will be added to \code{xifti$surf$cortex_right}. Any existing surfaces will
#'  be overwritten.
#' 
#' If the resolutions of the data and surfaces do not match, the surfaces will
#'  be resampled to match the resolution of the data. The barycentric resampling
#'  method, which is recommended for anatomical surfaces, will be used. 
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
  if (all(resamp_res %in% c(NA, NaN))) {
    resamp_res <- NULL
  }

  # Left.
  if (!is.null(surfL)) {
    if (!is.null(xifti$surf$cortex_left)) {
      ciftiTools_msg("Overwriting existing geometry for left cortex.\n")
    }
    rr1 <- resamp_res[1]
    if (is.null(resamp_res) || rr1 %in% c(NA, NaN)) {
      warning("Could not infer left cortex resolution, so the left surface cannot be added.")
    } else {
      if (rr1==0) { rr1 <- NULL }
      if (is.character(surfL) && length(surfL)==1 && surfL %in% c("very inflated", "inflated", "midthickness")) {
        xifti$surf$cortex_left <- z <- load_surf("left", surfL, rr1)
      } else {
        z <- read_surf(surfL, "left")
        if (!is.null(rr1) && (nrow(z$vertices) != rr1)) {
          z <- resample_surf(z, rr1, "left")
        }
        xifti$surf$cortex_left <- z
      }
      if (!is.null(rr1) && (nrow(z$vertices) != rr1)) {
        warning("The left surface might not match the data resolution, even after any resampling.")
      }
    }
  }

  # right.
  if (!is.null(surfR)) {
    if (!is.null(xifti$surf$cortex_right)) {
      ciftiTools_msg("Overwriting existing geometry for right cortex.\n")
    }
    rr2 <- resamp_res[2]
    if (is.null(resamp_res) || rr2 %in% c(NA, NaN)) {
      warning("Could not infer right cortex resolution, so the right surface cannot be added.")
    } else {
      if (rr2==0) { rr2 <- NULL }
      if (is.character(surfR) && length(surfR)==1 && surfR %in% c("very inflated", "inflated", "midthickness")) {
        xifti$surf$cortex_right <- z <- load_surf("right", surfR, rr2)
      } else {
        z <- read_surf(surfR, "right")
        if (!is.null(rr2) && (nrow(z$vertices) != rr2)) {
          z <- resample_surf(z, rr2, "right")
        }
        xifti$surf$cortex_right <- z
      }
      if (!is.null(rr2) && (nrow(z$vertices) != rr2)) {
        warning("The right surface might not match the data resolution, even after any resampling.")
      }
    }
  }

  # Check.
  if (!is.xifti(xifti)) { stop("The resulting \"xifti\" object was invalid.") }

  xifti
}
