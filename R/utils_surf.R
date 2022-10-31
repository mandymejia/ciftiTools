#' Summarize a \code{"surf"} object
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
  cat("Vertices:   ", x$vertices, "\n")
  cat("Faces:      ", x$faces, "\n")
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

#' Boundary Mask for Region on Surface
#'
#' Identify the vertices within `boundary_width` edges of the input mask on a
#'  triangular mesh.
#'
#' @param faces a \eqn{V \times 3} matrix of integers. Each row defines a face
#'  by the index of three vertices.
#' @param mask A length \eqn{V} logical vector indicating if each vertex is
#'  within the input mask.
#' @param boundary_width a positive integer. Vertices no more than this number
#'  of edges from any vertex in the input mask will be placed in the boundary 
#'  mask.
#'
#' @return The boundary mask, a length-\eqn{V} logical vector. \code{TRUE} 
#'  indicates vertices within the boundary mask.
#'
#' @keywords internal
surf_mask_boundary <- function(faces, mask, boundary_width){
  s <- ncol(faces)
  v <- max(faces)
  # For quads, surf_mask_boundary() would count opposite vertices on a face as
  #   adjacent--that's probably not desired.
  stopifnot(s == 3)

  stopifnot(boundary_width > 0)

  bmask <- rep(FALSE, v)
  # Begin with the input mask.
  verts_adj_previous <- which(mask)
  for (ii in seq(1, boundary_width)) {
    # Identify vertices not in the mask, but adjacent to it.
    # Adjacency is defined by sharing a face.
    faces_nmask <- rowSums(matrix(faces %in% verts_adj_previous, ncol=s))
    faces_adj <- faces_nmask > 0 & faces_nmask < s
    verts_adj <- unique(as.vector(faces[faces_adj,]))
    verts_adj <- verts_adj[!(verts_adj %in% verts_adj_previous)]
    # Add those vertices to the boundary mask, and use them as the mask in
    #   the next iteration.
    bmask[verts_adj] <- TRUE
    verts_adj_previous <- verts_adj
  }

  bmask
}