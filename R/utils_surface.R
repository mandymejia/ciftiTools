#' Summarise cifti objects
#'
#' Summary method for class "surface"
#'
#' @param object Object of class "surface". 
#'  See \code{\link{is.surf}} and \code{\link{make_surf}}.
#' @param ... further arguments passed to or from other methods.
#' 
#' @export
#' 
#' @method summary surface
summary.surface <- function(object, ...) {
  out <- list(
    vertices = nrow(object$vertices),
    faces = nrow(object$faces),
    hemisphere = object$hemisphere
  )
  class(out) <- "summary.surface"
  return(out)
}

#' @rdname summary.surface
#' @export
#' 
#' @param x bject of class "surface". 
#' 
#' @method print summary.surface
#' 
print.summary.surface <- function(x, ...) {
  cat("Vertices: ", x$vertices, "\n")
  cat("Faces: ", x$faces, "\n")
  if (!is.null(x$hemisphere)) { cat("Hemisphere: ", x$hemisphere, "\n") }
}

#' @rdname summary.surface
#' @export
#' 
#' @method print surface
#' 
print.surface <- function(x, ...) {
  print.summary.surface(summary(x))
}