#' Apply function over locations in each parcel
#'
#' Apply a function across all locations in each parcel. By default, the mean
#' value in each parcel is calculated.
#'
#' @param xii The \code{"xifti"} data to apply the function over, within each
#'  parcel.
#' @param parc The \code{"xifti"} "dlabel" parcellation. Each parcel is defined
#'  by a unique key in the label table. If there are multiple columns, only the
#'  first column will be used. Alternatively, \code{parc} can just be a vector
#'  of keys whose length is the number of data locations in \code{"xii"}.
#' @param FUN A function that takes as input an \eqn{M \times N} matrix (\eqn{M}
#'  locations in a given parcel, and \eqn{N} measurements/columns in \code{xii})
#'  and outputs a constant-sized (\eqn{Q}) numeric vector.
#' @param mwall_value If there is a medial wall in \code{xii}, what should value
#'  should medial wall locations be replaced with prior to calculation?
#'  Default: \code{NA}.
#' @param ... Additional arguments to \code{FUN}.
#'
#' @return A \eqn{P \times Q} matrix, where \eqn{P} is the number of parcels and
#'  \eqn{Q} is the length of the output of \code{FUN}. (For \code{mean},
#'  \eqn{Q = 1}).
#'
#' @export
#'
parc_apply <- function(xii, parc, FUN=mean, mwall_value=NA, ...){
  # Arg checks
  stopifnot(is.xifti(xii))
  parc <- assure_parc(parc)

  # Replace medial wall and convert `xifti` to matrix.
  xii <- move_from_mwall(xii, value=mwall_value)
  if (nrow(xii) != nrow(parc)) {
    stop(
      "`xii` has ", nrow(xii), " locations (including any medial wall), but ",
      "`parc` has ", nrow(parc), " locations. They need to have the same resolution."
    )
  }
  xii <- as.matrix(xii)

  # Convert `parc` to vector.
  parc_names <- rownames(parc$meta$cifti$labels[[1]])
  parc_keys <- parc$meta$cifti$labels[[1]]$Key
  parc <- c(as.matrix(parc))

  nP <- length(parc_keys)
  nV <- nrow(xii)
  nT <- ncol(xii)

  # Compute function for each parcel.
  out <- vector("list", nP)
  names(out) <- parc_names
  for (ii in seq(length(parc_keys))) {
    out[ii] <- FUN(xii[parc==parc_keys[ii],], ...)
  }

  # Check that the output length is the same for each parcel.
  stopifnot(length(unique(lapply(out, dim)))==1)

  # Return.
  do.call(rbind, out)

  # [TO DO]: could consider using matrix multiplication, instead, for mean/avg.
  # Scrubbing paper should have code for this.
}
