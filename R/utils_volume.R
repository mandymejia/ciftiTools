#' Undo a volumetric mask
#' 
#' Un-applies a mask to vectorized data to yield its volumetric representation.
#'  The mask and data should have compatible dimensions: the number of rows in
#'  \code{dat} should equal the number of locations within the \code{mask}.
#'  This is used for the subcortical CIFTI data.
#' 
#' @param dat Data matrix with locations along the rows and measurements along 
#'  the columns. If only one set of measurements were made, this may be a 
#'  vector. Alternatively, a \code{"xifti"} object with subcortical data.
#' @param mask Volumetric binary mask. \code{TRUE} indicates voxels inside the
#'  mask. Required unless \code{dat} is a \code{"xifti"} object, in which case
#'  the mask in the metadata will be used.
#' @param fill The value for locations outside the mask. Default: \code{NA}.
#'
#' @return The 3D or 4D unflattened volume array
#'
#' @export
#' 
unmask_subcortex <- function(dat, mask=NULL, fill=NA) {

  if (is.xifti(dat, messages=FALSE) && (!is.null(dat$data$subcort))) {
    return(unmask_subcortex(dat$data$subcort, dat$meta$subcort$mask, fill=fill))
  } else {
    stopifnot(!is.null(mask))
  }

  # Check that dat is a vector or matrix.
  if (is.vector(dat) || is.factor(dat)) { dat <- matrix(dat, ncol=1) }
  stopifnot(length(dim(dat)) == 2)

  # Check that mask is numeric {0, 1} or logical, and is 3D.
  if (is.numeric(mask)) {
    mask_vals <- unique(as.vector(mask))
    stopifnot(length(mask_vals) <= 2)
    stopifnot(all(mask_vals %in% c(0,1)))
    mask <- array(as.logical(mask), dim=dim(mask))
  }
  stopifnot(length(dim(mask)) == 3)

  # Other checks.
  stopifnot(is.vector(fill) && length(fill)==1)
  stopifnot(sum(mask) == nrow(dat))

  # Make volume and fill.
  vol <- array(fill, dim=c(dim(mask), ncol(dat)))
  for (ii in seq_len(ncol(dat))) {
    vol[,,,ii][mask] <- dat[,ii]
  }
  if (ncol(dat)==1) { vol <- vol[,,,1] }

  vol
}

#' Pad a 3D Array
#'
#' Pad a 3D array by a certain amount in each direction, along each dimension.
#'  This effectively undoes a crop.
#'
#' @param x A 3D array, e.g. \code{unmask_subcortex(xifti$data$subcort, xifti$meta$subcort$mask)}.
#' @param padding A \eqn{d \times 2} matrix indicating the number of 
#'  slices to add at the beginning and end of each of the d dimensions, e.g.
#'  \code{xifti$meta$subcort$mask_padding}.
#' @param fill Values to pad with. Default: \code{NA}.
#'
#' @return The padded array
#'
#' @keywords internal
#' 
pad_vol <- function(x, padding, fill=NA){
  stopifnot(length(dim(x))==3)
  new_dim <- vector("numeric", 3)
  for (ii in seq_len(3)) {
    new_dim[ii] <- dim(x)[ii] + padding[ii,1] + padding[ii,2]
  }
  y <- array(fill, dim=new_dim)
  y[
    seq(padding[1,1]+1, padding[1,1]+dim(x)[1]),
    seq(padding[2,1]+1, padding[2,1]+dim(x)[2]),
    seq(padding[3,1]+1, padding[3,1]+dim(x)[3])
  ] <- x
  y
}

#' @rdname pad_vol
#' 
uncrop_vol <- function(x, padding, fill=NA){
  pad_vol(x, padding, fill)
}

#' Convert coordinate list to volume
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
#' @return The volumetric data
#'
#' @keywords internal
#' 
coordlist_to_vol <- function(coords, fill=FALSE){
  stopifnot(length(fill)==1)
  if (is.logical(fill)) {
    logical_vals <- TRUE
  } else if (is.numeric(fill)) {
    logical_vals <- FALSE
  } else { stop("Fill value must be logical or numeric.") }

  stopifnot(is.matrix(coords) || is.data.frame(coords))
  stopifnot(dim(coords)[2] %in% c(3,4))
  if (dim(coords)[2] == 3) {
    val <- ifelse(
      as.numeric(fill) != 1,
      ifelse(logical_vals, TRUE, 1),
      ifelse(logical_vals, FALSE, 0)
    )
    coords <- cbind(coords, val)
  } else {
    if (any(coords[,4] == fill, na.rm=TRUE)) { 
      warning("The fill value occurs in the data.") 
    }
  }

  vol <- array(fill, dim=apply(coords[,1:3], 2, max, na.rm=TRUE))
  vol[as.matrix(coords[,1:3])] <- coords[,4]
  vol
}

#' Crop a 3D array
#' 
#' Remove empty (zero-valued) edge slices from a 3D array.
#'
#' @param x The 3D array to crop.
#'
#' @keywords internal
#' 
crop_vol <- function(x) {
  d <- length(dim(x))

  if (all(unique(as.vector(x)) %in% c(NA, 0))) { stop("The array is empty.") }

  padding <- matrix(NA, nrow=d, ncol=2)
  rownames(padding) <- strsplit(rep("ijklmnopqrstuvwxyz", ceiling(d/15)), "")[[1]][1:d]
  empty_slice <- vector("list", d)
  for (ii in 1:d) {
    empty_slice[[ii]] <- apply(x, ii, sum, na.rm=TRUE) == 0
    first_slice <- min(which(!empty_slice[[ii]]))
    last_slice <- max(which(!empty_slice[[ii]]))
    padding[ii,1] <- ifelse(
      first_slice != 1, 
      first_slice - 1, 
      0
    )
    padding[ii,2] <- ifelse(
      last_slice != length(empty_slice[[ii]]), 
      length(empty_slice[[ii]]) - last_slice, 
      0
    )
  }
  x <- x[!empty_slice[[1]], !empty_slice[[2]], !empty_slice[[3]]]

  return(list(data=x, padding=padding))
}

#' Get spatial locations of each voxel
#' 
#' Use subcortical metadata (mask, transformation matrix and units) to get
#'  voxel locations in 3D space.
#' 
#' @param mask,trans_mat,trans_units The subcortical metadata
#' @return A list: \code{coords} and \code{units}
#' 
#' @keywords internal
#' 
vox_locations <- function(mask, trans_mat, trans_units=NULL){
  list(
    coords = (cbind(which(mask, arr.ind=TRUE), 1) %*% trans_mat)[,seq(3)],
    trans_units = NULL
  )
}

#' Write label table to text file
#' 
#' Write \code{xii$meta$cifti$labels[[idx]]} to a text file for use with the
#'  Connectome Workbench command \code{-volume-label-import}.
#' 
#' @param label_table The label table (one at a time!)
#' @param fname Where to write the label table text file
#' 
#' @keywords internal
write_label_table <- function(label_table, fname){

  ## Must be a matrix or data.frame
  stopifnot(is.matrix(label_table) || is.data.frame(label_table))

  ## Column names
  if (length(unique(colnames(label_table))) != length(colnames(label_table))) {
    stop("Label table column names must be unique.")
  }
  if (!all(colnames(label_table) %in% c("Key", "Red", "Green", "Blue", "Alpha"))) {
    stop("Label table columns must be among: `Key` (required), `Red`, `Green`, `Blue`, and `Alpha`.")
  }
  if (!("Key" %in% colnames(label_table))) { stop("`Key` column is required in the label table.") }

  # Color values must be integers from to 255
  label_table[,c("Red", "Green", "Blue", "Alpha")] <- round(label_table[,c("Red", "Green", "Blue", "Alpha")]*255)

  label_table <- apply(label_table, 1, paste0, collapse=" ")
  out <- c(rbind(names(label_table), label_table))
  writeLines(out, fname)
}