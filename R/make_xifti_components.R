#' Make "xifti" Cortical Components
#' 
#' Coerce a file path, GIFTI object, or data matrix to a data matrix
#'  representing cortical data (and optionally a corresponding mask). That is,
#'  entries for \code{xifti$data$cortex_[left/right]} and 
#'  \code{xifti$meta$cortex}. 
#'
#' @param cortex A file path, GIFTI object, or data matrix representing
#'  cortical data.
#' @param side Left or right? (Character). Just used to print warnings.
#' @param medial_wall_mask A pre-determined mask for the medial wall: it should
#'  be logical with \code{FALSE} values indicating vertices that make up the
#'  medial wall. It will be verified, and overwritten/removed if inaccurate.
#'  Default: \code{NULL} will try to infer the medial wall mask, or leave it
#'  blank.
#'
#' @return A list with components "data" and "medial_wall_mask".
#'
#' @keywords internal
#' 
#' @importFrom gifti readgii is.gifti
make_xifti_cortex <- function(cortex, side=NULL, medial_wall_mask=NULL) {
  # File --> GIFTI.
  if (is.fname(cortex)) {
    cortex <- readgii(cortex)
  }
  # GIFTI --> matrix.
  if (is.gifti(cortex)) {
    cortex <- do.call(cbind, cortex$data)
  } else {
    stop("`cortex` was not an existing file (check file name?), nor was it an object made by `gifti::read_gifti()`.")
  }

  # Check if medial wall mask is valid.
  MIN_TO_INFER_MWALL <- 2
  enough_measurements = ncol(cortex) > MIN_TO_INFER_MWALL
  at_least_one_mwall_vert <- !all(medial_wall_mask)
  if (!is.null(medial_wall_mask)) {
    if (!at_least_one_mwall_vert) {
        warning(paste(
          "Metadata in CIFTI dense mapping did not indicate any medial wall",
          "vertices for", side, "cortex. Deleting this mask mask.\n"
        ))
        medial_wall_mask <- NULL
    } else {
      if (length(medial_wall_mask) == nrow(cortex)) {
        cortex <- cortex[medial_wall_mask,, drop=TRUE]
      } else {
        warning(paste(
          "The medial wall mask from the CIFTI metadata was not the same",
          "length as the", side, "cortex. Deleting.\n"
        ))
        medial_wall_mask <- NULL
      }
    }
  }
  if (is.null(medial_wall_mask)) {
    if (enough_measurements) {
      new_medial_wall_mask <- !apply(cortex==0 | is.na(cortex), 1, all)
      if (any(!new_medial_wall_mask)) {
        warning(paste(
          "Inferring medial wall from constant 0/NA columns in", side, "cortex.\n"
        ))
        medial_wall_mask <- new_medial_wall_mask
        cortex <- cortex[medial_wall_mask,, drop=TRUE]
      } else {
        warning(paste(
          "No medial wall vertices inferred from data. Leaving the medial",
          "wall mask field empty for", side, "cortex.\n"
        ))
      }
    } else {
      warning(paste(
        "Leaving medial wall mask field empty for", side, "cortex.\n"
      ))
    }
  }
  list(data = cortex, medial_wall_mask = medial_wall_mask)
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
#'  \code{info_cifti}. If \code{NULL} (default), the mask will instead be
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
    mask = mask
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
#'
#' @keywords internal
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
  if (is.gifti(surf)) { 
    surf <- gifti_to_surf(surf) 
  } else {
    stop("`surf` was not an existing file (check file name?), nor was it an object made by `gifti::read_gifti()`.")
  }

  # Return cifti_surface or error.
  if (!is.xifti_surface(surf)) {
    stop("The object could not be converted into a surface.")
  }
  surf
}