#' Unflatten a "cifti_flat" object.
#'
#' Yields a \code{"cifti"} object from a \code{"cifti_flat"} object.
#'
#' @param cifti_flat The flattened CIFTI.
#' @param fill Fill value for the medial wall in the cortical data.
#'
#' @return A \code{"cifti_flat"} object.
#' @export
#'
unflatten_cifti <- function(cifti_flat, fill=0) {
  if (!is.cifti(cifti_flat, flat=TRUE)) { 
    stop("Input is not a valid \"cifti_flat\" object.")
  }

  # Create the template.
  cifti <- c(
    list(
      CORTEX_LEFT = NULL,
      CORTEX_RIGHT = NULL,
      SUBCORT = NULL,
      LABELS = list(
        CORTEX_LEFT = NULL,
        CORTEX_RIGHT = NULL,
        SUBCORT = NULL
      )
    ),
    cifti_flat[c("SURF_LEFT", "SURF_RIGHT", "META")]
  )

  # Unflatten the data.
  mwall_mask <- cifti_flat$LABELS$SUBSTRUCTURE != "Medial Wall"
  for (bs in c("CORTEX_LEFT", "CORTEX_RIGHT", "SUBCORT")) {
    bs_mask <- cifti_flat$LABELS$BRAINSTRUCTURE == bs
    if (any(bs_mask)) {
      cifti[[bs]] <- matrix(fill, nrow=sum(mwall_mask), ncol=ncol(cifti_flat$DAT))
      cifti[[bs]] <- cifti_flat$DAT[bs_mask[mwall_mask],, drop=FALSE]
      cifti$LABELS[[bs]] <- cifti_flat$LABELS$SUBSTRUCTURE[bs_mask]
      class(cifti[[bs]]) <- "cifti_data"
    }
  }

  # Return cifti or error.
  if (!is.cifti(cifti)) {
    stop("The object could not be converted into a cifti object.")
  }
  class(cifti) <- "cifti"
  cifti
}

#' @rdname unflatten_cifti
#' @export
unflattenCIfTI <- unflattencii <- function(cifti_flat, fill=0){
  unflatten_cifti(cifti_flat)
}