#' Edit mask on surface
#' 
#' Erode, dilate, or get the borders of a mask along the cortical surface
#' 
#' The depth of the edit is determined by the number of edges between the
#'  vertices. To erode or dilate based on spatial distance (mm), see
#'  \code{-cifti-dilate} and \code{-cifti-erode}.
#' 
#' @param x,mwall Vector of the data mask to edit, and the medial wall mask.
#'  These can be specified in two ways. First, \code{mwall} can be a logical vector
#'  with each entry corresponding to a vertex as the cortical surface, and
#'  using \code{FALSE} values to indicate medial wall vertices. In this first case,
#'  \code{x} should then be a logical vector with each entry corresponding to a 
#'  \code{TRUE} value in \code{mwall}. \code{TRUE} values in \code{x} should 
#'  indicate the mask to be edited.
#' 
#'  Second, \code{mwall} can be \code{NULL} (default) in which case \code{x}
#'  should then be a logical vector with each entry corresponding to a vertex
#'  on the cortical surface. \code{TRUE} values in \code{x} should indicate the 
#'  mask to be edited.
#' 
#'  In either case, \code{xii$data$cortex_left[,1]} and 
#'  \code{xii$meta$cortex$medial_wall_mask$left} should work.
#' @param surf,hemisphere Provide one: the surface in the same resolution as the
#'  data, or the name of the hemisphere of the surface to resample and use 
#'  (default: resample the left surface).
#' @param do \code{"erode"} (default), \code{"dilate"}, or \code{"borders"}. 
#'  \code{"erode"} removes faces with at least one vertex not inside the mask.
#'  \code{"dilate"} adds faces with at least one vertex inside the mask.
#'  \code{"borders"} obtains the vertices inside the mask which share a face
#'  with at least one vertex not inside the mask.
#' @param depth How many iterations of the edit? Default: \code{1}. Does not
#'  apply to \code{"borders"}.
#' 
#' @return \code{x} after erosion or dilation.
#' 
#' @family surface-related
#' @export
#' 
edit_mask_surf <- function(
  x, mwall=NULL, surf=NULL, hemisphere=c("left", "right"), 
  do=c("erode", "dilate", "borders"), depth=1){

  # Check arguments.
  x <- as.logical(x)
  if (!is.null(mwall)) {
    mwall <- as.logical(mwall)
    stopifnot(length(x) == sum(mwall))
    y <- vector("logical", length(mwall))
    y[mwall] <- x; y[!mwall] <- NA
    x <- y
  }
  if (is.null(surf)) { 
    hemisphere <- match.arg(hemisphere, c("left", "right"))
  } else {
    stopifnot(is.surf(surf))
  }
  do <- match.arg(do, c("erode", "dilate", "borders"))
  if (do != "borders") {
    depth <- round(as.numeric(depth))
    if (depth == 0) { return(x) }
    if (depth < 0) { 
      do <- switch(do, erode="dilate", dilate="erode")
      depth <- -depth
    }
  }

  # Infer resolution
  res <- ifelse(is.null(mwall), length(x), length(mwall))
  
  # Get and/or check surface
  if (is.null(surf)) {
    surf <- load_surf(hemisphere, resamp_res=res)
    if (res != nrow(surf$vertices)) {
      stop("Could not resample ", hemisphere, " surface to match the inferred resolution: ", res, ".")
    }
  } else {
    if (res != nrow(surf$vertices)) {
      stop("The inferred resolution: ", res, " did not match the provided surface.")
    }
  }

  for (dd in seq(depth)) {
    x2 <- x
    x2[is.na(x2)] <- do != "dilate"
    y <- rowSums(matrix(x2[as.vector(surf$faces)], ncol=ncol(surf$faces)))
    if (do=="dilate") { y <- y > 0 } else { y <- y < 3 }
    y <- seq(length(x2)) %in% unique(as.vector(surf$faces[y,]))
    if (do == "erode") {
      x2 <- x2 & (!y)
    } else if (do == "dilate") {
      x2 <- y
    } else if (do == "borders") {
      x2 <- x2 & y
    } else { stop() }
    x[!is.na(x)] <- x2[!is.na(x)]
  }

  if (!is.null(mwall)) { x <- x[mwall] }

  x
}

#' @rdname edit_mask_surf
#' @export
erode_mask_surf <- function(
  x, mwall=NULL, surf=NULL, hemisphere=c("left", "right"), depth=1){

  edit_mask_surf(
    x, mwall=mwall, surf=surf, hemisphere=hemisphere, 
    do="erode", depth=depth
  )
}

#' @rdname edit_mask_surf
#' @export
dilate_mask_surf <- function(
  x, mwall=NULL, surf=NULL, hemisphere=c("left", "right"), depth=1){

  edit_mask_surf(
    x, mwall=mwall, surf=surf, hemisphere=hemisphere, 
    do="dilate", depth=depth
  )
}