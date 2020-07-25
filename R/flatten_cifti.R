#' Flatten a "cifti" object into a single matrix
#'
#' @description Flattens data in a "cifti" object into a single matrix. This 
#'  matrix is identical to that obtained by \code{\link{read_cifti_flat}}, 
#'  which uses the \code{-cifti-convert -to-gifti-ext} Workbench Command.
#'  
#' @aliases flattenCIfTI
#'
#' @inheritParams cifti_Param
#' @inheritParams brainstructures_Param_all
#'
#' @return A T x B matrix, where T is the number of time points and B is the 
#'  number of brainordinates in the CIFTI.
#' @export
#'
#' @importFrom stats sd
#'
flatten_cifti <- function(cifti, 
  brainstructures=c("left","right", "subcortical")) {

  if (!is_cifti(cifti)) { stop("`cifti` is not a valid CIFTI object.") }

  EPS <- ciftiTools.getOption("EPS")

  brainstructures <- match_input(
    brainstructures, c("left","right","subcortical"),
    user_value_label="brainstructures"
  )

  to_flat <- vector(mode="list", length=0)

  # [TO DO]: Can't discard 0 values in dscalar/few-column CIFTIs. Fix this.
  # [TO DO]: ever NA values in the CIFTI?

  if ("left" %in% brainstructures) { 
    stopifnot(!is.null(cifti$CORTEX_LEFT))
    to_flat <- c(to_flat, 
      cifti$CORTEX_LEFT[rowSums(abs(cifti$CORTEX_LEFT)) > EPS,]
    )
  }

  if ("right" %in% brainstructures) { 
    stopifnot(!is.null(cifti$CORTEX_RIGHT))
    to_flat <- c(to_flat, 
      cifti$CORTEX_RIGHT[rowSums(abs(cifti$CORTEX_RIGHT)) > EPS,]
    )
  }

  if ("subcortical" %in% brainstructures) {

    # MAJOR [TO DO]: parcel ordering
    vol <- NA
    stop()

    to_flat <- c(to_flat, vol)    
  }

  # # SUBCORTICAL
  # # Get the mask of non-zero labels, which should indicate the non-empty voxels.
  # mask <- cifti$LABELS != 0
  # labs <- as.vector(cifti$LABELS[mask])
  # # Get the parcel ordering for the voxels.
  # parcel_order <- as.vector(length(labs), mode="numeric")
  # idx = 1
  # for(p in sort(unique(labs))) {
  #   p_idx <- which(labs == p)
  #   parcel_order[idx:(idx+length(p_idx)-1)] = p_idx
  #   idx <- idx + length(p_idx)
  # }
  # # Mask the volume, and then order the voxels by parcel.
  # t <- ifelse(length(dim(cifti$VOL)) == 4, dim(cifti$VOL)[4], 1)
  # dat <- matrix(cifti$VOL[mask], ncol=t)
  # dat <- dat[parcel_order,]

  # # CORTEX
  # if (brainstructures=="everything") {
  #   dat <- rbind(
  #     cifti$CORTEX_LEFT[apply(cifti$CORTEX_LEFT, 1, stats::sd) > EPS,],
  #     cifti$CORTEX_RIGHT[apply(cifti$CORTEX_RIGHT, 1, stats::sd) > EPS,],
  #     dat
  #   )
  # }

  do.call(rbind, to_flat)
}

#' @rdname flatten_cifti
#' @export
flattenCIfTI <- flattencii <- function(
  cifti, brainstructures=c("left","right", "subcortical")){

  flatten_cifti(cifti, brainstructures)
}