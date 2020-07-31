#' Undo a mask
#' 
#' Applies a mask to vectorized data to yield its volumetric representation.
#'  The mask and data should have compatible dimensions: the number of rows in
#'  \code{dat} should equal the number of locations within the \code{mask}.
#' 
#' @param dat Data matrix with locations along the rows and measurements along 
#'  the columns. If only one set of measurements were made, this may be a 
#'  vector.
#' @param mask Volumetric brain mask for subcortical locations.
#' @param fill The value for locations outside the mask. Default: \code{NA}.
#'
#' @return The 3D or 4D unflattened volume array
#'
unmask <- function(dat, mask, fill=NA) {

  # Check that dat is a vector or matrix.
  if (is.vector(dat)) { dat <- matrix(dat, ncol=1) }
  stopifnot(length(dim(dat)) == 2)

  # Check that mask is numeric {0, 1} or logical, and is 3D.
  if (is.numeric(mask)) {
    mask_vals <- unique(as.vector(mask))
    stopifnot(length(mask_vals) <= 2)
    stopifnot(all(mask_vals %in% c(0,1)))
  }
  mask <- as.logical(mask)
  stopifnot(length(dim(mask)) == 3)

  # Other checks.
  stopifnot(is.vector(fill) && length(fill)==1)
  stopifnot(sum(mask) == nrow(dat))

  # Make volume and fill.
  vol <- array(fill, dim=c(dim(mask), ncol(dat)))
  for(ii in 1:ncol(dat)) {
    vol[,,,ii][mask] <- dat[,ii]
  }

  vol
}

#' Convert sparse coordinate list to non-sparse volume.
#' 
#' Converts a sparse coordinate list to its non-sparse volumetric representation.
#' 
#' @param coords The sparse coordinate list. Should be a data.frame or matrix
#'  with voxels along the rows and three or four columns. The first three 
#'  columns should be integers indicating the spatial coordinates of the voxel.
#'  If the fourth column is present, it will be the value used for that voxel.
#'  If it is absent, the value will be \code{TRUE} or \code{1} if \code{fill}
#'  is not those values, and \code{FALSE} or \code{0} if \code{fill} is. The
#'  data type will be the same as that of \code{fill}.
#'  \code{fill}. The fourth column must be logical or numeric.
#' @param fill Fill value for the volume. Must be logical or numeric. Default: 
#'  \code{FALSE}.
#' 
#' @return The volumetric data.
#' @export 
#' 
coordlist_to_vol <- function(coords, fill=FALSE){
  stopifnot(length(fill)==1)
  if (is.logical(fill)) {
    logical_vals=TRUE
  } else if (is.numeric(fill)) {
    logical_vals=FALSE
  } else { stop("Fill value must be logical or numeric.") }

  stopifnot(is.matrix(coords) || is.data.frame(coords))
  stopifnot(dim(coords)[2] %in% c(3,4))
  if (dim(coords)[2] == 3) {
    val <- ifelse(
      as.numeric(fill) != 1,
      ifelse(logical_vals, TRUE, 1),
      ifelse(logical_vals, FALSE, 0)
    )
    coords <- cbind(coords, rep(val, nrow(coords)))
  } else {
    if (any(coords[,4] == fill, na.rm=TRUE)) { 
      warning("The fill value occurs in the data.") 
    }
  }

  vol <- array(fill, dim=sapply(coords[,1:3], max, na.rm=TRUE))
  vol[as.matrix(coords[,1:3])] = coords[,4]
  vol
}