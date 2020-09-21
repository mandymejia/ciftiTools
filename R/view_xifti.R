#' View a \code{"xifti"} object
#' 
#' Switch for \code{\link{view_xifti_surface}} or \code{\link{view_xifti_volume}}
#'
#' @inheritParams xifti_Param
#' @param what Either \code{"surface"} or \code{"volume"}. If \code{NULL} 
#'  (default), view the surface if cortex data is present in the \code{"xifti"} 
#'  object, and the subcortical volume otherwise.
#' @param ... Additional arguments to pass to either view function.
#'
#' @return The return value of \code{view_xifti_surface} or
#'  \code{view_xifti_volume}.
#'
#' @export
#'
view_xifti <- function(xifti, what=NULL, ...) {
  stopifnot(is.xifti(xifti))
  if (is.null(what)) {
    can_do_left <- !is.null(xifti$data$cortex_left)
    can_do_right <- !is.null(xifti$data$cortex_right)
    can_do_sub <- !is.null(xifti$data$subcort)
    what <- ifelse(
      can_do_left || can_do_right,
      "surface",
      ifelse(can_do_sub, "volume", "error")
    )
  }
  if (what == "surface") {
    return(view_xifti_surface(xifti, ...))
  } else if (what == "volume") {
    return(view_xifti_volume(xifti, ...))
  } else {
    stop(paste(
      "No valid cortical surface, and no valid subcortical data.",
      "Did you forget to provide surfL/surfR?",
      "Or, did you forget to read in the subcortical data too?"
    ))
  }
}

#' S3 method: use \code{view_xifti} to plot a \code{"xifti"} object
#'
#' @inheritParams x_Param_xifti
#' @param ... Additional arguments to \code{\link{view_xifti}}, except
#'  \code{what}, which will be set to \code{NULL}.
#'
#' @method plot xifti
#' 
#' @export
#' 
plot.xifti <- function(x, ...){
  view_xifti(x, what=NULL, ...)
}

#' @rdname view_xifti
#' @export
view_cifti <- function(xifti, what=NULL, ...){
  view_xifti(xifti, what=what, ...)
}

#' @rdname view_xifti
#' @export
viewCIfTI <- function(xifti, what=NULL, ...){
  view_xifti(xifti, what=what, ...)
}

#' @rdname view_xifti
#' @export
viewcii <- function(xifti, what=NULL, ...){
  view_xifti(xifti, what=what, ...)
}
