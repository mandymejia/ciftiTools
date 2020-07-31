#' Convert a "cifti_flat" to a "cifti_minimal"
#'
#' Convert a "cifti_flat" to a "cifti_minimal" by reordering the subcortical
#'  voxels alphabetically.
#'
#' @param cifti_flat The "cifti_flat" object
#'
#' @return The "cifti_minimal" object
#' @export
#'
minimize_cifti <- function(cifti_flat) {
  if (!(check_cifti(cifti_flat, flat=TRUE))) {
    if (check_cifti(cifti_flat, flat=FALSE)) {
      message("Input was a \"cifti\", so flattening first.")
      cifti_flat <- flatten_cifti(cifti_flat)
    } else {
      temp <- is.cifti(cifti_flat, flat=TRUE)
      stop("Not a \"cifti_flat\" (or a \"cifti\")")
    }
  }
  subcort_mask <- cifti_flat$LABELS$BRAINSTRUCTURE=="SUBCORT"
  dat_sc_mask <- subcort_mask[cifti_flat$LABELS$SUBSTRUCTURE!="Medial Wall"]
  back <- order(cifti_flat$LABELS[subcort_mask,"SUBSTRUCTURE"])
  cifti_flat$DAT[dat_sc_mask,] <- cifti_flat$DAT[dat_sc_mask,, drop=FALSE][back,, drop=FALSE]
  cifti_flat$DAT
}