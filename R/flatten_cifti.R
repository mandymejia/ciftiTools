#' Flatten a "cifti" object.
#' 
#' Yields a \code{"cifti_flat"} object from a \code{"cifti"} object.
#'
#' @inheritParams cifti_Param
#'
#' @return The "cifti_flat"
#' @export
#'
#' @importFrom stats sd
#'
flatten_cifti <- function(cifti) {
  if (!is.cifti(cifti)) { 
    stop("Input is not a valid \"cifti\" object.")
  }

  # Create the template.
  cifti_flat <- c(
    list(
      DAT = NULL,
      LABELS = NULL
    ),
    cifti[c("SURF_LEFT", "SURF_RIGHT", "META")]
  )

  # Flatten the data.
  to_dat <- vector(mode="list", length=3)
  to_lab <- vector(mode="list", length=3)
  mwall_mask <- cifti$LABELS$SUBSTRUCTURE != "Medial Wall"
  if (!is.null(cifti$CORTEX_LEFT)) { 
    to_dat[[1]] <- cifti$CORTEX_LEFT[cifti$LABELS$CORTEX_LEFT != "Medial Wall",, drop=FALSE]
    to_lab[[1]] <- data.frame(BRAINSTRUCTURE="CORTEX_LEFT", SUBSTRUCTURE=cifti$LABELS$CORTEX_LEFT)
  }
  if (!is.null(cifti$CORTEX_RIGHT)) { 
    to_dat[[2]] <- cifti$CORTEX_RIGHT[cifti$LABELS$CORTEX_RIGHT != "Medial Wall",, drop=FALSE]
    to_lab[[2]] <- data.frame(BRAINSTRUCTURE="CORTEX_RIGHT", SUBSTRUCTURE=cifti$LABELS$CORTEX_RIGHT)
  }
  if (!is.null(cifti$SUBCORT)) {
    to_dat[[3]] <- cifti$SUBCORT
    to_lab[[3]] <- data.frame(BRAINSTRUCTURE="SUBCORT", SUBSTRUCTURE=cifti$LABELS$SUBCORT)
  }
  cifti_flat$DAT <- do.call(rbind, to_dat)
  cifti_flat$LABELS <- do.call(rbind, to_lab)

  # Return cifti_flat or error.
  if (!is.cifti(cifti_flat, flat=TRUE)) {
    stop("The object could not be converted into a cifti_flat object.")
  }
  class(cifti_flat) <- "cifti_flat"
  cifti_flat
}

#' @rdname flatten_cifti
#' @export
flattenCIfTI <- flattencii <- function(cifti){
  flatten_cifti(cifti)
}