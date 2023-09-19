#' Parcellation borders
#' 
#' Identify vertices which lie on the border of different parcels.
#' 
#' @param parc Integer vector the same length as the number of vertices. Each
#'  entry indicates the parcel that vertex belongs to.
#' @param surf The surface which the vertices belong to, or just the \code{"faces"}
#'  component (\eqn{F \times 3} matrix where each row indicates the vertices which
#'  comprise a face). If not provided, the (resampled) default \code{hemisphere} 
#'  surface included with \code{ciftiTools} will be used.
#' @param hemisphere Only used to choose which default surface to use if 
#'  \code{is.null(surf)}. Should be \code{"left"} (default) or \code{"right"}.
#' 
#' @return Logical vector the same length as \code{parc} indicating if the
#'  vertex lies on a border.
#' 
#' @export
parc_borders <- function(parc, surf=NULL, hemisphere=c("left", "right")) {
  stopifnot(is.vector(parc))
  parc <- as.numeric(as.factor(parc))

  if (is.null(surf)) {
    hemisphere <- match.arg(hemisphere, c("left", "right"))
    surf <- load_surf(hemisphere)
    if (nrow(surf$vertices) != length(parc)) {
      surf <- resample_surf(surf, length(parc))
      if (nrow(surf$vertices) != length(parc)) {
        stop(paste(
          "`parc` has", length(parc), "vertices; the surface could not be resampled to match."
        ))
      }
    }
  }

  x <- matrix(parc[as.vector(surf$faces)], ncol=ncol(surf$faces))
  x <- (x[,1] != x[,2]) | (x[,2] != x[,3])
  x <- unique(as.vector(surf$faces[x,]))
  isBorder <- rep(FALSE, length(parc))
  isBorder[x] <- TRUE
  isBorder
}