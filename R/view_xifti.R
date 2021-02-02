#' View a \code{"xifti"} object
#' 
#' Switch for \code{\link{view_xifti_surface}} or \code{\link{view_xifti_volume}}
#'
#' @inheritParams xifti_Param
#' @param what Either \code{"surface"} or \code{"volume"}. \code{NULL} will infer
#'  based on the contents of the \code{"xifti"}: if there is data, plot the 
#'  surface cortex data if present, and the volumetric subcortical data 
#'  otherwise. If there is no data, plot the surface geometry if present, and
#'  do nothing otherwise.
#' @param ... Additional arguments to pass to either view function.
#'
#' @return The return value of \code{view_xifti_surface} or
#'  \code{view_xifti_volume}.
#'
#' @export
#'
view_xifti <- function(xifti, what=NULL, ...) {
  stopifnot(is.xifti(xifti))
  
  has_left <- !is.null(xifti$data$cortex_left)
  has_right <- !is.null(xifti$data$cortex_right)
  has_sub <- !is.null(xifti$data$subcort)
  has_surfL <- !is.null(xifti$surf$cortex_left)
  has_surfR <- !is.null(xifti$surf$cortex_right)

  if (is.null(what)) { 
    if (has_left | has_right) { 
      what <- "surface" 
    } else if (has_sub) {
      what <- "volume"
    } else if (has_surfL | has_surfR) {
      what <- "surface"
    } else {
      cat("Nothing to plot: the `xifti` is empty.\n")
      return(NULL)
    }
  } else {
    what <- match.arg(what, c("surface", "volume"))
    if (what == "surface" && !any(c(has_left, has_right, has_surfL, has_surfR))) {
      stop("No cortical data nor surface geometry are present in the `xifti`, so the surface cannot be plotted.")
    }
    if (what == "volume" && !has_sub) {
      stop("No subcortical data are present in the `xifti`, so the volume cannnot be plotted.")
    }
  }

  return(switch(
    what, 
    surface = view_xifti_surface(xifti, ...),
    volume = view_xifti_volume(xifti, ...)
  ))
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
