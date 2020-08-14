#' Make "xifti" Cortical Components
#' 
#' Coerce a file path, GIFTI object, or data matrix to a data matrix
#'  representing cortical data (and optionally a corresponding mask). That is,
#'  entries for \code{xifti$data$cortex_[left/right]} and 
#'  \code{xifti$meta$cortex}. 
#'
#' @param cortex A file path, GIFTI object, or data matrix representing
#'  cortical data.
#' @param mwall The medial wall mask: a logical vector with \code{FALSE} values 
#'  indicating vertices that make up the medial wall. It will be verified, and 
#'  overwritten or removed if inaccurate.
#' @param cortex_is_masked Has the cortex data been masked yet? \code{NULL}
#'  (default) indicates whether it has been masked or not unknown.
#' @param rm_blank_mwall If the medial wall mask is all \code{TRUE} 
#'  (indicating no medial wall vertices), should it be discarded? Default:
#'  \code{TRUE}. If \code{FALSE}, keep it.
#' @param rm_bad_mwall If the medial wall mask doesn't match up with the 
#'  data (e.g. the vertex count doesn't add up), should it be discarded?
#'  Default: \code{TRUE}. If \code{FALSE}, raise an error.
#' @param infer_mwall If the medial wall mask was not provided (or if it was
#'  discarded) should it be inferred from 0/NA values? Default: \code{TRUE}.
#' @param side "left" or "right"? Just used to print warnings.
#' @param mwall_source Character describing where the mwall came from. Just used
#'  to print warnings.
#'
#' @return A list with components "data" and "mwall".
#'
#' @keywords internal
#' 
#' @importFrom gifti readgii is.gifti
make_xifti_cortex <- function(
  cortex, mwall=NULL,
  cortex_is_masked=NULL,
  rm_blank_mwall=TRUE,
  rm_bad_mwall=TRUE,
  infer_mwall=TRUE,
  side=NULL,
  mwall_source=NULL) {

  if (is.null(side)) {side <- ""}
  if (is.null(mwall_source)) {mwall_source <- ""}

  # Cannot infer the medial wall if the cortex has been masked.
  if (cortex_is_masked) { infer_mwall <- FALSE }

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
  msg <- ""
  if (!is.null(mwall)) {
    if (all(mwall)) {
      msg <- paste0(msg,
        "The ",side," medial wall mask obtained from ",mwall_source,
        " was all TRUE (indicating no medial wall vertices). "
      )
      if (rm_blank_mwall) {
        msg <- paste0(msg,"It was deleted. ")
        mwall <- NULL
      }
    } else {
      same_len <- length(mwall) == nrow(cortex)
      same_pos <- sum(mwall) == nrow(cortex)
      if (!is.null(cortex_is_masked)) {
        if (cortex_is_masked) {
          if (!same_pos) {
            msg <- paste0(msg,
              "The ",side," medial wall mask obtained from ",mwall_source," has ",
              sum(mwall)," non-medial-wall locations,but there are ",nrow(cortex),
              " vertices in the masked data matrix."
            )
            if (rm_bad_mwall) {
              msg <- paste0(msg,"The ",side," medial wall mask was deleted. ")
              mwall <- NULL
            } else { stop(msg) }
          }
        } else if (!cortex_is_masked) {
          if (!same_len) {
            msg <- paste0(msg,
              "The ",side," medial wall mask obtained from ",mwall_source," has ",
              length(mwall)," total locations,but there are ",nrow(cortex),
              " total vertices in the data matrix."
            )
            if (rm_bad_mwall) {
              msg <- paste0(msg,"The ",side," medial wall mask was deleted. ")
              mwall <- NULL
            } else { stop(msg) }
          }
        }
      } else {
        # Only one of `same_len` and `same_pos` will be TRUE b/c !all(mwall) 
        cortex_is_masked <- same_pos
      }
    }
  }

  # If no valid medial wall mask, optionally infer it.
  if (is.null(mwall)) {
    if (infer_mwall) {
      new_mwall <- !apply(cortex==0 | is.na(cortex), 1, all)
      if (any(!new_mwall)) {
        msg <- paste0(
          msg,"A new medial wall mask for the ",side," cortex was inferred from ",
          "constant 0/NA columns."
        )
        mwall <- new_mwall
      } else {
        msg <- paste0(
          msg,"No medial wall vertices inferred from data. Leaving the medial ",
          "wall mask field empty for ", side, " cortex. "
        )
      }
    } else {
      msg <- paste0(
        msg,"Leaving medial wall mask field empty for ", side, " cortex. "
      )
    }
  }

  if (!(msg == "")) { ciftiTools_msg(msg) }

  # Apply a valid medial wall mask to an unmasked cortex.
  if (!is.null(mwall) && !cortex_is_masked) {
    cortex <- cortex[mwall,, drop=FALSE]
    cortex_is_masked <- TRUE
  }

  list(data = cortex, mwall = mwall)
}

#' Make "xifti" Subcortical Components
#' 
#' Coerce subcortical data into valid entries for \code{xifti$data$subcort}
#'  and \code{xifti$meta$subcort}. All data arguments can be matrices/arrays. 
#'  Additionally, the subcortical volume and labels can be paths to NIFTI files.
#'  If the mask is not provided, it will be inferred from the labels, or the 
#'  volume if the labels are already vectorized.
#' 
#' \code{subcortVol} is either a 3D/4D data array (i x j x k x T) or a 
#'  vectorized data matrix (V_S voxels x T measurements). If it's vectorized, 
#'  voxels should be in spatial order.
#' 
#'  \code{subcortLabs} is either a 3D data array (i x j x k) or a V_S-length 
#'  vector of subcortical brainstructure labels (as factors) or their indices 
#'  (as integers): see \code{\link{substructure_table}}. If it's vectorized,
#'  the labels should be in spatial order.
#'  
#'  \code{subcortMask} is a logical 3D data array (i x j x k) where \code{TRUE}
#'  values indicate voxels inside the brain mask. If it is not provided, the
#'  mask will be inferred from zero- and NA-valued voxels in \code{subcortLabs}
#'  (or \code{subcortVol} if \code{subcortLabs} is vectorized). If both 
#'  \code{subcortVol} and \code{subcortLabs} are vectorized and \code{subcortMask}
#'  is not provided, the mask cannot be inferred so an error occur.
#' 
#' @param vol represents the data values of the subcortical volume. It is either 
#'  a path to a NIFTI file, a 3D/4D data array (i x j x k x T), or a vectorized 
#'  data matrix (V_S voxels x T measurements). If it's vectorized, voxels should 
#'  be in spatial order.
#' @param labs represents the brainstructure labels of each voxel: see
#'  \code{\link{substructure_table}}. It is either a path to a NIFTI file, a 3D 
#'  data array (i x j x k) of brainstructure indices 3-21 with 0 representing 
#'  out-of-mask voxels; or, a V_S-length vector in spatial order with 
#'  brainstructure names as factors, or with brainstructure indices as integers.
#' @param mask is a logical 3D data array (i x j x k) where \code{TRUE}
#'  values indicate voxels inside the brain mask. If it is not provided, the
#'  mask will be inferred from zero- and NA-valued voxels in \code{subcortLabs}
#'  (or \code{subcortVol} if \code{subcortLabs} is vectorized). If both 
#'  \code{subcortVol} and \code{subcortLabs} are vectorized and \code{subcortMask}
#'  is not provided, the mask cannot be inferred so an error occur.
#' @param validate_mask If \code{mask} is provided, set this to \code{TRUE} to 
#'  check that the mask only removes NA- and zero-valued voxels in \code{vol} 
#'  and \code{labs}. Default: \code{FALSE} (saves time).
#'
#' @keywords internal
#' 
#' @return A list with components "data", "labels" and "mask". The first two
#'  will be vectorized and ordered spatially.
#' 
#'  The volume can be recovered using: 
#'    vol <- unmask(data, mask, fill=NA) 
#'    labs <- unmask(labels, mask, fill=0) 
#'
#' @importFrom RNifti readNifti
make_xifti_subcort <- function(
  vol, labs, mask=NULL, validate_mask=FALSE) {

  # Get vol.
  if (is.fname(vol)) {
    vol <- readNifti(vol)
  }
  vol_ndims <- length(dim(vol))
  if (vol_ndims == 1) { vol <- matrix(vol, ncol=1) }
  vol_is_vectorized <- vol_ndims < 3

  # Get labels.
  if (is.fname(labs)) {
    labs <- readNifti(labs)
    labs_vals <- order(unique(labs))
    if (!all(labs_vals %in% 0:19)) { stop("The labels read from a NIFTI file should be integers 0-19.") }
    labs[labs > 0] <- labs[labs > 0] + 2
  }
  if (!is.numeric(labs) || !is.factor(labs)) { 
    stop("The labels should be integers (or factor levels) 3-21 or 0. See `substructure_table`")
  }
  if (!all(as.integer(labs_vals) %in% c(0, 3:21))) { 
    stop("The labels should be integers (or factor levels) 3-21 or 0. See `substructure_table`")
  }
  labs_ndims <- length(dim(labs))
  labs_is_vectorized <- labs_ndims < 3

  # Infer mask if not provided.
  if (is.null(mask)) {
    if (!labs_is_vectorized) {
      mask <- labs > 0 | !is.na(labs)
      if (validate_mask) {
        mask_vol <- apply(vol!=0 | !is.na(vol), c(1,2,3), all)
        if !(all.equal(mask, mask_vol)) { 
          stop("The mask inferred from the labels did not match the mask inferred from the volume (NA/0 values).")
        }
      }
    } else if (!vol_is_vectorized) {
      mask <- apply(vol!=0 | !is.na(vol), c(1,2,3), all)
    } else {
      stop("The mask could not be inferred. At least one of the volume or labels must not be vectorized, if the mask is not provided.")
    }
  # Otherwise, validate it if requested.
  } else {
    if (is.numeric(mask)) { mask <- mask > 0 }
    stopifnot( is.logical(mask) )
    if (validate_mask) {
      stop_msg <- ""
      if (!labs_is_vectorized) {
        mask_labs <- labs > 0 | !is.na(labs)
        if !(all.equal(mask, mask_labs)) { 
          stop_msg <- paste0(stop_msg, "The input mask did not match the mask inferred from the labels (NA/0 values). ")
        }
      }
      if (!vol_is_vectorized) {
        mask_vol <- apply(vol!=0 | !is.na(vol), c(1,2,3), all)
        if !(all.equal(mask, mask_vol)) { 
          stop_msg <- paste0(stop_msg, "The input mask did not match the mask inferred from the volume (NA/0 values). ")
        }
      }
      if (stop_msg != "") { stop(stop_msg) }
    }
  }

  # Apply mask.
  substructure_levels <- substructure_table()$ciftiTools_Name
  labs <- factor(
    labs[mask], 
    levels=1:length(substructure_levels), 
    labs=substructure_levels
  )
  stopifnot(is.subcort_labs(labs))

  list(
    data = matrix(vol[mask], nrow=sum(mask)),
    labels = labels,
    mask = mask
  )
}

#' Make "xifti" Surface Components
#' 
#' Coerce a file path, GIFTI object (with entries "pointset" and "triangle"), 
#'  or surface (list of vertices + faces) to a surface.
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
  gifti_to_surf <- function(gii) {
    stopifnot(is.list(gii))
    if ("data" %in% names(gii)) { gii <- gii$data }
    stopifnot(all(c("pointset", "triangle") %in% names(gii)))
    verts <- gii$pointset
    faces <- gii$triangle
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