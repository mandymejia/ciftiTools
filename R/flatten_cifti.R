#' Flatten a \code{"cifti"} object into a single matrix
#'
#' @description Flattens data in a \code{"cifti"} object into a single matrix. 
#'  Besides the row order for the subcortical brainordinates, this
#'  matrix is identical to that obtained by \code{\link{read_cifti_flat}},
#'  which uses the \code{-cifti-convert -to-gifti-ext} Workbench Command.
#'
#'  If \code{meta}, some information from the "cifti" will be retained
#'  and the result will be a \code{"cifti_flat"} object. Otherwise, only the data
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

  if (!is.cifti(cifti)) { stop("`cifti` is not a valid \"cifti\" object.") }

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
    to_flat[[1]] <- cifti$CORTEX_LEFT[cortexL_mask,, drop=FALSE]
  }

  if ("right" %in% brainstructures) { 
    stopifnot(!is.null(cifti$CORTEX_RIGHT))
    cortexR_mask <- rowSums(abs(cifti$CORTEX_RIGHT), na.rm=TRUE) > EPS
    to_flat[[2]] <- cifti$CORTEX_RIGHT[cortexR_mask,, drop=FALSE]
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

  # ##############################################################
  # } else if (method == "separate") {
  #   cifti_components <- read_cifti_separate(
  #     cifti_fname,
  #     surfL_fname=NULL, surfR_fname=NULL,
  #     map=cifti_map,
  #     brainstructures=brainstructures,
  #     wb_path=wb_path, ...
  #   )

  #   if (format == "regular") {
  #     cifti$CORTEX_LEFT <- cifti_components$CORTEX_LEFT
  #     cifti$CORTEX_RIGHT <- cifti_components$CORTEX_RIGHT
  #     cifti$SUBCORT <- cifti_components$SUBCORT

  #   } else if (format=="flat") {
  #     cifti$DAT <- rbind(
  #       cifti_components$CORTEX_LEFT,
  #       cifti_components$CORTEX_RIGHT,
  #       cifti_components$SUBCORT
  #     )[cifti$LABELS$SUBSTRUCTURE != "MEDIAL_WALL",]
  # ##############################################################

  # Return cifti_flat or error.
  if (!is.cifti(cifti_flat, flat=TRUE)) {
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

#' Undo a mask
#' 
#' Applies a mask to vectorized data to yield its volumetric representation.
#'  The mask and data should have compatible dimensions: the number of rows in
#'  \code{dat} should equal the number of locations within the \code{mask}.
#' 
#' @param dat Data matrix with locations along the rows and measurements along 
#'  the columns. If only one set of measurements were made, this may be a 
#'  vector.
#' @param mask Volumetric brain mask for subcortical locations.
#' @param fill The value for locations outside the mask. Default: \code{NA}.
#'
#' @return The 3D or 4D unflattened volume array
#'
unmask <- function(dat, mask, fill=NA) {

  # Check that dat is a vector or matrix.
  if (is.vector(dat)) { dat <- matrix(dat, ncol=1) }
  stopifnot(length(dim(dat)) == 2)

  # Check that mask is numeric {0, 1} or logical, and is 3D.
  if (is.numeric(mask)) {
    mask_vals <- unique(as.vector(mask))
    stopifnot(length(mask_vals) <= 2)
    stopifnot(all(mask_vals %in% c(0,1)))
  }
  mask <- as.logical(mask)
  stopifnot(length(dim(mask)) == 3)

  # Other checks.
  stopifnot(is.vector(fill) && length(fill)==1)
  stopifnot(sum(mask) == nrow(dat))

  # Make volume and fill.
  vol <- array(fill, dim=c(dim(mask), ncol(dat)))
  for(ii in 1:ncol(dat)) {
    vol[,,,ii][mask] <- dat[,ii]
  }

  vol
}