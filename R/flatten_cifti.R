#' Flatten a "cifti" object into a single matrix
#'
#' @description Flattens data in a "cifti" object into a single matrix. 
#'  Besides the row order for the subcortical brainordinates, this
#'  matrix is identical to that obtained by \code{\link{read_cifti_flat}},
#'  which uses the \code{-cifti-convert -to-gifti-ext} Workbench Command.
#'
#'  If \code{meta}, some information from the "cifti" will be retained
#'  and the result will be a "cifti_flat" object. Otherwise, only the data
#'  matrix will be returned.
#'
#' @inheritParams cifti_Param
#' @inheritParams brainstructures_Param_all
#' @param meta Should the metadata be obtained too? Default: 
#'  \code{TRUE}. If \code{FALSE}, return the data matrix only.
#'
#' @return If \code{meta}, a list containing the components "DAT" and 
#'  "META". "DAT" will be a T x B matrix, where T is the number of time points 
#'  and B is the number of non-empty brainordinates in the CIFTI (no medial wall
#'  from the cortical data). "META" will contain information about the row
#'  indices each brainstructure occupies, which medial wall values were masked
#'  (for the cortical data), and the labels and mask (for the subcortical data).
#' @export
#'
#' @importFrom stats sd
#'
flatten_cifti <- function(cifti, 
  brainstructures=c("left","right","subcortical"),
  meta=TRUE) {

  if (!is_cifti(cifti)) { stop("`cifti` is not a valid CIFTI object.") }

  EPS <- ciftiTools.getOption("EPS")

  brainstructures <- match_input(
    brainstructures, c("left","right","subcortical"),
    user_value_label="brainstructures"
  )

  # ----------------------------------------------------------------------------
  # Flatten each brainstructure. -----------------------------------------------
  # ----------------------------------------------------------------------------

  to_flat <- vector(mode="list", length=3)

  # [TO DO]: Can't discard 0 values in dscalar/few-column CIFTIs. Fix this.
  # [TO DO]: ever NA values in the CIFTI?

  if ("left" %in% brainstructures) { 
    stopifnot(!is.null(cifti$CORTEX_LEFT))
    cortexL_mask <- rowSums(abs(cifti$CORTEX_LEFT)) > EPS
    to_flat[[1]] <- cifti$CORTEX_LEFT[cortexL_mask,]
  }

  if ("right" %in% brainstructures) { 
    stopifnot(!is.null(cifti$CORTEX_RIGHT))
    cortexR_mask <- rowSums(abs(cifti$CORTEX_RIGHT)) > EPS
    to_flat[[2]] <- cifti$CORTEX_RIGHT[cortexR_mask,]
  }

  if ("subcortical" %in% brainstructures) {
    stopifnot(!is.null(cifti$SUBCORT))
    to_flat[[3]] <- cifti$SUBCORT$DAT
  }

  dat <- do.call(rbind, to_flat)

  if (!meta) return(dat)

  # ----------------------------------------------------------------------------
  # Format the output. ---------------------------------------------------------
  # ----------------------------------------------------------------------------

  cifti_flat <- list(DAT=dat, META=NULL)
  cifti_flat$META <- list()

  if ("left" %in% brainstructures) { 
    cifti_flat$META$CORTEX_LEFT <- list(
      rows = c(1, sum(cortexL_mask)), 
      NA_mask = cortexL_mask
    )
  }

  if ("right" %in% brainstructures) { 
    cifti_flat$META$CORTEX_RIGHT <- list(
      rows = c(1 + sum(cortexL_mask), sum(cortexL_mask) + sum(cortexR_mask)), 
      NA_mask = cortexR_mask
    )
  }

  if ("subcortical" %in% brainstructures) {
    cifti_flat$META$SUBCORT <- list(
      rows = c(1 + sum(cortexL_mask) + sum(cortexR_mask), nrow(dat)), 
      labels = cifti$SUBCORT$LABELS,
      mask = cifti$SUBCORT$MASK
    )
  }

  # Return cifti_flat or error.
  if (!is_cifti_flat(cifti_flat)) {
    stop("The object could not be converted into a cifti_flat object.")
  }
  class(cifti_flat) <- "cifti_flat"
  cifti_flat
}

#' @rdname flatten_cifti
#' @export
flattenCIfTI <- flattencii <- function(
  cifti, brainstructures=c("left","right","subcortical"), meta=TRUE){

  flatten_cifti(cifti, brainstructures, meta)
}