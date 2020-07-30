#' Unmask masked/vectorized data.
#' 
#' Unmask masked/vectorized data to obtain the volumetric data.
#' 
#' @param dat The data matrix, with voxels along the rows.
#' @param mask The 3D volumetric mask.
#' @param fill Fill value for out-of-mask locations. Default: \code{NA}.
#' 
#' @return The volumetric data.
#' @export
#' 
unmask_dat <- function(dat, mask, fill=NA){
  if (is.vector(dat)) { dat <- as.matrix(dat, ncol=1) }
  vol <- array(fill, dim=c(dim(mask), ncol(dat)))
  for (ii in 1:ncol(dat)) {
    vol[,,,ii] <- dat[,ii]
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