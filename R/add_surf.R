#' Add surface(s) to "xifti"
#' 
#' Add left or right cortical surface geometry to a "xifti" object.
#' 
#' @inheritParams xifti_Param
#' @param surfL,surfR (Optional) [Left/right] brain surface model. Can be a file
#'  path for GIFTI data or an object from \code{\link[gifti]{readgii}}.
#' 
#' @return The "xifti" with added surface geometry components
#' 
#' @export
#'
add_surf <- function(xifti, surfL=NULL, surfR=NULL) {
  if (!is.xifti(xifti)) { stop("The input \"xifti\" object is invalid.") }

  if (!is.null(surfL)) {
    if (!is.null(xifti$surf$cortex_left)) { 
      ciftiTools_msg("Overwriting existing geometry for left cortex.\n") 
    }
    xifti$surf$cortex_left <- make_surf(surfL)
  }
  if (!is.null(surfR)) {
    if (!is.null(xifti$surf$cortex_right)) { 
      ciftiTools_msg("Overwriting existing geometry for right cortex.\n") 
    }
    xifti$surf$cortex_right <- make_surf(surfR)
  }

  if (!is.xifti(xifti)) { stop("The resulting \"xifti\" object was invalid.") }
  return(xifti)
}