#' Convert a "cifti" or "cifti_flat" to a "cifti_minimal"
#'
#' Convert a "cifti" or "cifti_flat" to a "cifti_minimal" by reordering the 
#'  subcortical voxels alphabetically.
#'
#' @param cifti The "cifti" object
#'
#' @return The "cifti_minimal" object
#' @export
#'
minimize_cifti <- function(cifti) {
  if (!(check_cifti(cifti, flat=TRUE))) {
    if (check_cifti(cifti, flat=FALSE)) {
      # Input was a "cifti", so flattening first.
      cifti <- flatten_cifti(cifti)
    } else {
      temp <- is.cifti(cifti, flat=TRUE)
      stop("Not a \"cifti_flat\" (or a \"cifti\")")
    }
  }
  subcort_mask <- cifti$LABELS$BRAINSTRUCTURE=="SUBCORT"
  dat_sc_mask <- subcort_mask[cifti$LABELS$SUBSTRUCTURE!="Medial Wall"]
  back <- order(cifti$LABELS[subcort_mask,"SUBSTRUCTURE"])
  cifti$DAT[dat_sc_mask,] <- cifti$DAT[dat_sc_mask,, drop=FALSE][back,, drop=FALSE]
  cifti$DAT
}