#' Make "xifti" Cortex Components
#' 
#' Coerce a file path, GIFTI object, or data matrix to a data matrix
#'  representing cortical data (and optionally a corresponding mask). That is,
#'  entries for \code{xifti$data$cortex_[left/right]} and 
#'  \code{xifti$meta$cortex}.
#'
#' @param cortex A file path, GIFTI object, or data matrix representing
#'  cortical data.
#' @param medial_wall_mask \code{FALSE} (default) to return the data matrix
#'  without inferring/applying a mask for the medial wall; \code{TRUE} to
#'  infer the medial wall mask from vertices with constant 0- or NA-values.
#'  Alternatively, this can be a mask which has been pre-determined: it should
#'  be boolean with \code{FALSE} values indicating vertices that make up the
#'  medial wall.
#'
#' @return A list with components "data" and "medial_wall_mask".
#' @export
#'
#' @importFrom gifti readgii is.gifti
make_xifti_cortex <- function(cortex, medial_wall_mask=FALSE) {
  # File --> GIFTI.
  if (is.fname(cortex)) {
    cortex <- readgii(cortex)
  }
  # GIFTI --> matrix.
  if (is.gifti(cortex)) {
    cortex <- do.call(cbind, cortex$data)
  }

  # Medial wall.
  if (identical(medial_wall_mask, TRUE)) {
    medial_wall_mask <- !apply(cortex==0 | is.na(cortex), 1, all)
  } else if (identical(medial_wall_mask, FALSE)) {
    medial_wall_mask <- NULL
  } 
  if (!is.null(medial_wall_mask)) {
    stopifnot(length(medial_wall_mask) == nrow(cortex))
    cortex <- cortex[medial_wall_mask,, drop=FALSE]
  }

  # Return value.
  list(
    data = cortex,
    medial_wall_mask = medial_wall_mask
  )
}

#' Make "xifti" Subcortical Components
#' 
#' Coerce a volume and labels into valid entries for \code{xifti$data$subcort}
#'  and \code{xifti$meta$subcort}. They can both be file paths or arrays. 
#'  Neither should be flattened/vectorized. They must be compatible.
#' 
#' @param vol File path or array for the volumetric subcortical data.
#'  It should be a 3D or 4D array where the first three dimensions are spatial,
#'  and the fourth dimension is the measurements per voxel (so if one measurement
#'  is made per voxel, it will be a 3D array).
#' @param labels File path or array for the subcortical labels. The first three
#'  dimensions of \code{labels} should be the same as those of \code{vol}, and
#'  the voxels should correspond to one another. Thus, zero-valued labels
#'  should correspond to voxels in \code{vol} that are constant 0 or NA.
#' @param mask A pre-computed mask for \code{vol} and \code{labels}, i.e. from
#'  \code{map_cifti}. If \code{NULL} (default), the mask will instead be
#'  inferred from the zero-valued voxels of \code{labels}. Not supported yet!
#' @param validate_mask Set this to \code{TRUE} to check that the mask only
#'  removes zero-valued voxels in \code{vol} and \code{labels}.  
#'  Default: \code{FALSE} (saves time).
#'
#' @return A list with components "data", "labels", "mask", and "mask_padding".
#' 
#'  The subcortical labels will be ordered spatially (not alphabetically
#'  according to the corresponding \code{\link{substructure_table}}).
#' 
#' @export
#'
#' @importFrom RNifti readNifti
make_xifti_subcort <- function(
  vol, labels, mask=NULL, validate_mask=FALSE) {
  stopifnot(is.null(mask))

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
  ## [TO DO]: Support providing a mask (would need to infer padding if from map).
  #if (is.null(mask)) {
  #  mask <- labels > 0
  #} else {
  #  if (is.numeric(mask)) { 
  #    message("Numeric mask detected. Binarizing: values > 0 will be included.\n")
  #    mask <- mask > 0 
  #  }
  #}
  
  # Validate mask.
  if (validate_mask) {
   mask_vol <- apply(vol, c(1,2,3), sum, na.rm=TRUE) > ciftiTools.getOption("EPS")
   if (max(abs( mask - mask_vol )) > 0) {
     warning("The mask did not match the mask inferred from the volume (NA/0 values)\n.")
   }
  
  #  mask_labs <- apply(labels, c(1,2,3), sum, na.rm=TRUE) > ciftiTools.getOption("EPS")
  #  if (max(abs( mask - mask_labs )) > 0) {
  #    warning("The mask did not match the mask inferred from the labels (NA/0 values).")
  #  }
  }

  # Get the cropped mask.
  mask_cropped <- crop_array(mask)
  names(dim(mask_cropped$data)) <- c("i", "j", "k")

  # Use mask on labels.
  substructure_levels <- substructure_table()$ciftiTools_Name
  labels <- factor(
    labels[mask], 
    levels=1:length(substructure_levels), 
    labels=substructure_levels
  )
  stopifnot(is.subcort_labels(labels))

  # Use mask on volume and return.
  list(
    data = matrix(vol[mask], nrow=sum(mask)),
    labels = labels,
    mask = mask_cropped$data,
    mask_padding = mask_cropped$padding
  )
}

#' Make "xifti" Surface Components
#' 
#' Coerce a file path, GIFTI object, or surface (list of vertices + faces)
#'  to a surface.
#'
#' @param surf What to coerce to a surface.
#'
#' @return The surface, a list of vertices (spatial locations) and faces
#'  (defined by three vertices).
#' @export
#'
#' @importFrom gifti readgii is.gifti
make_xifti_surface <- function(surf) {
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
  if (!is.xifti_surface(surf)) {
    stop("The object could not be converted into a surface.")
  }
  surf
}