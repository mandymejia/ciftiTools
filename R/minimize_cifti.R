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
  if (!(is.cifti(cifti_flat, flat=TRUE))) {
    if (is.cifti(cifti_flat, flat=FALSE)) {
      message("Input was a \"cifti\", so flattening first.")
      cifti_flat <- flatten_cifti(cifti_flat)
    } else {
      stop("Not a \"cifti_flat\" (or a \"cifti\")")
    }
  }
  subcort_mask <- cifti_flat$LABELS$BRAINSTRUCTURE=="SUBCORT"
  back = order(cifti_flat$LABELS[subcort_mask,"SUBSTRUCTURE"])
  cifti_flat$DAT[subcort_mask[cifti_flat$LABELS$SUBSTRUCTURE!="Medial Wall"],][back,]
}