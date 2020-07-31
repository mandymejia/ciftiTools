#' Coerce a data matrix to a "cifti_data" object.
#' 
#' @param data What to coerce as a "cifti_data" object.
#' 
#' @return The "cifti_data" object.
#' @export 
#' 
make_cifti_data <- function(data) {
  # Return cifti_data or error.
  if (!is.cifti_data(data)) {
    stop("The object could not be converted into a cifti_data object.")
  }
  class(data) <- "cifti_data"
  data
}

#' Coerce a file path, GIFTI object, or "cifti_cortex" object to a
#'  "cifti_cortex" object.
#'
#' @param cortex What to coerce as a "cifti_cortex" object.
#'
#' @return The "cifti_cortex" object.
#' @export
#'
#' @importFrom gifti readgii is.gifti
make_cifti_cortex <- function(cortex) {
  # File --> GIFTI.
  if (is.fname(cortex)) {
    cortex <- readgii(cortex)
  }
  # GIFTI --> matrix.
  if (is.gifti(cortex)) {
    cortex <- do.call(cbind, cortex$data)
  }

  make_cifti_data(cortex)
}

#' Coerce a volume and labels into valid "cifti_subcort", their corresponding
#'  labels, "cifti_subcort_mask", and subcortical mask padding metadata. They 
#'  can both be file paths or arrays. Neither should be flattened/vectorized. 
#'  The volume and labels must be compatible.
#' 
#' @param vol File path or array for the volumetric subcortical data.
#'  It should be a 3D array if only one measurement was made per brainordinate,
#'  or a 4D array if the file represents a time series.
#' @param labels File path or array for the subcortical labels. The first three
#'  dimensions of \code{labels} should be the same as those of \code{vol}, and
#'  their non-empty voxels should be located in the same positions. 
#' @param mask The mask for \code{vol} and \code{labels}. This can be obtained
#'  with \code{map_cifti}. If \code{NULL} (default), the mask will instead be
#'  inferred from the zero-valued voxels of \code{labels}.
#' @param validate_mask Set this to \code{TRUE} to check that the mask only
#'  removes zero-valued voxels in \code{vol} and \code{labels}.  
#'  Default: \code{TRUE} (saves time).
#'
#' @return A list with components DAT, LABELS, MASK, and PADDING.
#' 
#'  The subcortical labels will be ordered spatially (not by alphabetical label).
#' 
#' @export
#'
#' @importFrom RNifti readNifti
make_cifti_subcort_fromvol <- function(
  vol, labels, mask=NULL, validate_mask=TRUE) {
  # Get vol.
  if (is.fname(vol)) {
    vol <- readNifti(vol)
  } else {
    if (!(is.array(vol) && (length(dim(vol)) > 2))) { 
      stop("The volume should be a 3D or 4D array. It shouldn't be vectorized.")
    }
  }

  # Get labels.
  if (is.fname(labels)) {
    labels <- readNifti(labels)
    labels[labels > 0] <- labels[labels > 0] + 2
  } else {
    if (!(is.array(labels) && (length(dim(labels)) != 3))) { 
      stop("The labels should be a 3D array. It shouldn't be vectorized.")
    }
  }

  # Get mask.
  mask <- labels > 0
  ## [TO DO]: Infer mask.
  #if (is.null(mask)) {
  #  mask <- labels > 0
  #} else {
  #  if (is.numeric(mask)) { 
  #    message("Numeric mask detected. Binarizing: values > 0 will be included.\n")
  #    mask <- mask > 0 
  #  }
  #}
  ## Validate mask.
  #if (validate_mask) {
  #  mask_vol <- apply(vol, c(1,2,3), sum, na.rm=TRUE) > ciftiTools.getOption("EPS")
  #  if (max(abs( mask - mask_vol )) > 0) {
  #    warning("The mask did not match the mask inferred from the volume (NA/0 values).")
  #  }
  #
  #  mask_labs <- apply(labels, c(1,2,3), sum, na.rm=TRUE) > ciftiTools.getOption("EPS")
  #  if (max(abs( mask - mask_labs )) > 0) {
  #    warning("The mask did not match the mask inferred from the labels (NA/0 values).")
  #  }
  #}

  # Crop mask.
  mask_crop <- crop_array(mask)

  # Use mask.
  substructure_levels <- substructure_table()$ciftiTools_Name
  labels <- factor(
    labels[mask], 
    levels=1:length(substructure_levels), 
    labels=substructure_levels
  )
  stopifnot(is.cifti_substructures(labels))
  vol <- matrix(vol[mask], nrow=sum(mask))

  list(
    DAT = make_cifti_data(vol),
    LABELS = labels,
    MASK = mask_crop$dat,
    PADDING = mask_crop$padding
  )

  # [TO DO]: Confirm:
  # all(dim(cifti$MASK) + sapply(cifti$PADDING, sum) == c(91, 109, 91))
}

#' Coerce a file path, GIFTI object, or "cifti_surface" object to a
#'  "cifti_surface" object.
#'
#' @param surf What to coerce as a "cifti_surface" object.
#'
#' @return The "cifti_surface" object.
#' @export
#'
#' @importFrom gifti readgii is.gifti
make_cifti_surface <- function(surf) {
  # File --> GIFTI.
  if (is.fname(surf)){ surf <- readgii(surf) }
  # GIFTI --> list of vertices and faces.
  gifti_to_surf <- function(gifti) {
    surf <- gifti$data
    verts <- surf$pointset
    faces <- surf$triangle
    if (min(faces)==0) faces <- faces + 1 # start indexing at 1 instead of 0
    surf <- list(vertices = verts, faces = faces)
  }
  if (is.gifti(surf)) { surf <- gifti_to_surf(surf) }

  # Return cifti_surface or error.
  if (!is.cifti_surface(surf)) {
    stop("The object could not be converted into a cifti_surface object.")
  }
  class(surf) <- "cifti_surface"
  surf
}