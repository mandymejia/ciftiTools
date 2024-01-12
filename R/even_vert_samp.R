#' Evenly sample vertices of mesh
#'
#' Get a subset of the mesh vertices that are spatially evenly-sampled, by
#'  resampling the mesh and choosing the original vertices closest (Euclidian
#'  distance) to the new vertices.
#'
#' @param surf A \code{"surf"} object
#' @param n_vert The desired number of vertices in the evenly-spaced sample.
#'  Note that the actual size of the subset will likely be close to but not
#'  exactly \code{n_vert} because it depends on the size of the resampled
#'  surface.
#'
#' @return An integer vector giving the indices of the vertices in the subset.
#'
#' @family surface-related
#' @export
even_vert_samp <- function(surf, n_vert) {
  stopifnot(is.surf(surf))
  stopifnot(is.numeric(n_vert) && length(n_vert)==1)
  stopifnot(n_vert>0 && n_vert==round(n_vert))

  vert_og <- t(surf$vertices) # 3 x original_res
  vert_rs <- t(resample_surf(surf, n_vert)$vertices) # 3 x n_vert

  # The size of the resampled surface is not exactly `n_vert`.
  n_vert <- ncol(vert_rs)

  vmask <- rep(NA, n_vert)
  for (ii in seq(n_vert)) {
    vdist <- colSums((vert_og - vert_rs[,ii])^2)
    vmask[ii] <- which.min(vdist)
  }

  vmask
}
