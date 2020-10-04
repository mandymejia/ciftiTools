#' Summarise cifti objects
#'
#' Summary method for class "surf"
#'
#' @param object Object of class "surf". 
#'  See \code{\link{is.surf}} and \code{\link{make_surf}}.
#' @param ... further arguments passed to or from other methods.
#' 
#' @export
#' 
#' @method summary surf
summary.surf <- function(object, ...) {
  out <- list(
    vertices = nrow(object$vertices),
    faces = nrow(object$faces),
    hemisphere = object$hemisphere
  )
  class(out) <- "summary.surf"
  return(out)
}

#' @rdname summary.surf
#' @export
#' 
#' @param x bject of class "surf". 
#' 
#' @method print summary.surf
#' 
print.summary.surf <- function(x, ...) {
  cat("Vertices: ", x$vertices, "\n")
  cat("Faces: ", x$faces, "\n")
  if (!is.null(x$hemisphere)) { cat("Hemisphere: ", x$hemisphere, "\n") }
}

#' @rdname summary.surf
#' @export
#' 
#' @method print surf
#' 
print.surf <- function(x, ...) {
  print.summary.surf(summary(x))
}