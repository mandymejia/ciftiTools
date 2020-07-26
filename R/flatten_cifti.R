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

  warning("This function doesn't work yet for ")

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
    cortexL_mask <- rowSums(abs(cifti$CORTEX_LEFT), na.rm=TRUE) > EPS
    to_flat[[1]] <- matrix(cifti$CORTEX_LEFT[cortexL_mask,], ncol=ncol(cifti$CORTEX_LEFT))
  }

  if ("right" %in% brainstructures) { 
    stopifnot(!is.null(cifti$CORTEX_RIGHT))
    cortexR_mask <- rowSums(abs(cifti$CORTEX_RIGHT), na.rm=TRUE) > EPS
    to_flat[[2]] <- matrix(cifti$CORTEX_RIGHT[cortexR_mask,], ncol=ncol(cifti$CORTEX_RIGHT))
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

  next_row <- 1

  if ("left" %in% brainstructures) { 
    bs_length <- sum(cortexL_mask)
    cifti_flat$META$CORTEX_LEFT <- list(
      rows = c(next_row, next_row + bs_length - 1), 
      mask = cortexL_mask
    )
    next_row <- next_row + bs_length
  }

  if ("right" %in% brainstructures) { 
    bs_length <- sum(cortexR_mask)
    cifti_flat$META$CORTEX_RIGHT <- list(
      rows = c(next_row, next_row + bs_length - 1), 
      mask = cortexR_mask
    )
    next_row <- next_row + bs_length
  }

  if ("subcortical" %in% brainstructures) {
    cifti_flat$META$SUBCORT <- list(
      rows = c(next_row, nrow(dat)), 
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

#' Use a mask to transform a flattened matrix to a volume. 
#'  They should both be numeric with compatible dimensions.
#'  ciftiTools uses \code{unflatten_cifti_vol} to unflatten the data in 
#'  \code{cifti$SUBCORT}, but this function should work for any matrix + mask
#'  pair.
#'
#' @param dat Data matrix for subcortical locations, with voxels along the rows 
#'  and measurements along the columns. If only one set of measurements were
#'  made, this may be a vector.
#' @param mask Volumetric brain mask for subcortical locations. See
#'  \code{\link{is_cifti_subcort_mask}}.
#'
#' @return The 3D or 4D unflattened volume array
#' @export
#'
unflatten_cifti_vol <- function(dat, mask) {
  # If dat is a vector, make it a matrix.
  if (is.vector(dat)) { dat <- matrix(dat, ncol=1) }
  
  # Check arguments.
  stopifnot(is_cifti_subcort_dat(dat))
  stopifnot(is_cifti_subcort_mask(mask))
  stopifnot(sum(mask) == nrow(dat))

  # Make volume and fill.
  vol <- array(NA, dim=c(dim(mask), ncol(dat)))
  for(ii in 1:ncol(dat)) {
    vol[,,,ii][mask] <- dat[,ii]
  }

  vol
}

#' Unflatten a flattened CIFTI
#'
#' Yields a "cifti" object from a "cifti_flat" object.
#'
#' @param cifti_flat The flattened CIFTI.
#'
#' @return A "cifti_flat" object.
#' @export
#'
unflatten_cifti <- function(cifti_flat) {
  stopifnot(is_cifti_flat(cifti_flat))

  cifti <- list(
    CORTEX_LEFT=NULL,
    CORTEX_RIGHT=NULL,
    SUBCORT=NULL,
    SURF_LEFT=NULL,
    SURF_RIGHT=NULL
  )

  if (!is.null(cifti_flat$META$CORTEX_LEFT)) {
    meta <- cifti_flat$META$CORTEX_LEFT
    cifti$CORTEX_LEFT <- matrix(
      NA, 
      nrow=length(meta$mask), 
      ncol=ncol(cifti_flat$DAT)
    )
    cifti$CORTEX_LEFT[meta$mask,] <- cifti_flat$DAT[meta$rows[1]:meta$rows[2],]
    class(cifti$CORTEX_LEFT) <- "cifti_cortex"
  }

  if (!is.null(cifti_flat$META$CORTEX_RIGHT)) {
    meta <- cifti_flat$META$CORTEX_RIGHT
    cifti$CORTEX_RIGHT <- matrix(
      NA, 
      nrow=length(meta$mask), 
      ncol=ncol(cifti_flat$DAT)
    )
    cifti$CORTEX_RIGHT[meta$mask,] <- cifti_flat$DAT[meta$rows[1]:meta$rows[2],]
    class(cifti$CORTEX_RIGHT) <- "cifti_cortex"
  }

  if (!is.null(cifti_flat$META$SUBCORT)) {
    meta <- cifti_flat$META$SUBCORT
    cifti$SUBCORT <- list(
      DAT = matrix(cifti_flat$DAT[meta$rows[1]:meta$rows[2],], ncol=ncol(cifti_flat$DAT)),
      LABELS = meta$labels,
      MASK = meta$mask
    )
    class(cifti$SUBCORT) <- "cifti_subcort"
  }

  # Return cifti or error.
  if (!is_cifti(cifti)) {
    stop("The object could not be converted into a cifti object.")
  }
  class(cifti) <- "cifti"
  cifti
}