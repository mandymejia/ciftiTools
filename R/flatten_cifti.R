#' Flatten a "cifti" object into a single matrix
#'
#' @description Flattens data in a "cifti" object into a single matrix. This 
#'  matrix is identical to that obtained by \code{\link{read_cifti_flat}}, 
#'  which uses the \code{-cifti-convert -to-gifti-ext} Workbench Command.
#'  
#' @aliases flattenCIfTI
#'
#' @param cif Object of class "cifti"
#' @param brainstructures "subcortical" for just the subcortical voxels 
#'  (default), or "everything" to include non-zero brainordinates from the left
#'  and right cortices.
#'
#' @return A T x B matrix, where T is the number of time points and B is the 
#'  number of brainordinates in the CIFTI.
#' @export
#'
#' @importFrom stats sd
#'
flatten_cifti <- function(cif, brainstructures=c("subcortical", "everything")) {

  if (!is_cifti(cif)) { stop("`cif` is not a valid CIFTI object.") }

  EPS <- ciftiTools.getOption("EPS")

  brainstructures <- match.arg(brainstructures, c("subcortical", "everything"))

  stopifnot(!is.null(cif$LABELS) && !is.null(cif$VOL))
  if (brainstructures == "everything") {
    stopifnot(!is.null(cif$CORTEX_LEFT) && !is.null(cif$CORTEX_RIGHT))
  }

  # SUBCORTICAL
  # Get the mask of non-zero labels, which should indicate the non-empty voxels.
  mask <- cif$LABELS != 0
  labs <- as.vector(cif$LABELS[mask])
  # Get the parcel ordering for the voxels.
  parcel_order <- as.vector(length(labs), mode="numeric")
  idx = 1
  for(p in sort(unique(labs))) {
    p_idx <- which(labs == p)
    parcel_order[idx:(idx+length(p_idx)-1)] = p_idx
    idx <- idx + length(p_idx)
  }
  # Mask the volume, and then order the voxels by parcel.
  t <- ifelse(length(dim(cif$VOL)) == 4, dim(cif$VOL)[4], 1)
  dat <- matrix(cif$VOL[mask], ncol=t)
  dat <- dat[parcel_order,]

  # CORTEX
  if (brainstructures=="everything") {
    dat <- rbind(
      cif$CORTEX_LEFT[apply(cif$CORTEX_LEFT, 1, stats::sd) > EPS,],
      cif$CORTEX_RIGHT[apply(cif$CORTEX_RIGHT, 1, stats::sd) > EPS,],
      dat
    )
  }

  dat
}

#' @rdname flatten_cifti
#' @export
flattenCIfTI <- flattencii <- function(
  cif, brainstructures=c("subcortical", "everything")){

  flatten_cifti(cif, brainstructures)
}