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
  # File path.
  if (is.fname(cortex)) {
    cortex <- readgii(cortex)
  }
  # GIFTI,
  if (is.gifti(cortex)) {
    cortex <- do.call(cbind, cortex$data)
  }

  # Return cifti_cortex or error.
  if (!is_cifti_cortex(cortex)) {
    stop("The object could not be converted into a cifti_cortex object.")
  }
  class(cortex) <- "cifti_cortex"
  cortex
}

#' Coerce a volume and labels into a valid "cifti_subcort"
#'  object. They can both be file paths or arrays. Neither should be
#'  flattened/vectorized. The volume and labels should be compatible.
#'
#' @param vol File path or array for the volumetric subcortical data.
#'  It should be a 3D array if only one measurement was made per brainordinate,
#'  or a 4D array if the file represents a time series.
#' @param labels File path or array for the subcortical labels. The first three
#'  dimensions of \code{labels} should be the same as those of \code{vol}, and
#'  their non-empty voxels should be located in the same positions. 
#' @param check_empty The labels are used to create the brain mask. Set this
#'  to \code{TRUE} to check that voxels with zero-valued labels are also
#'  zero-valued in \code{vol}. Default: \code{FALSE} (saves time).
#'
#' @return A "cifti_subcort" object with components "DAT", "LABELS", and "MASK".
#' @export
#'
#' @importFrom RNifti readNifti
make_cifti_subcort_fromvol <- function(vol, labels, check_empty=FALSE) {
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
  } else {
    if (!(is.array(labels) && (length(dim(labels)) != 3))) { 
      stop("The labels should be a 3D array. It shouldn't be vectorized.")
    }

    # [TO DO]: Use if statement, or always do?
    if ((1 %in% labels) | (2 %in% labels)) {
      labels[labels > 0] <- labels[labels > 0] + 2
    }
  }

  # Mask and vectorize.
  mask <- labels > 0
  labels <- as.numeric(labels[mask])
  if (check_empty) {
    mask_vol <- apply(vol, c(1,2,3), sum, na.rm=TRUE) > ciftiTools.getOption("EPS")
    if (max(abs( mask - mask_vol )) > 0) {
      warning("The mask made from the labels (0 values) did not match the mask inferred from the voxels (NA/0 values).")
    }
  }
  vol <- matrix(vol[mask], nrow=sum(mask))
  vol <- matrix(vol[order(labels),], nrow=sum(mask))

  # Return them or raise an error.
  subcort <- list(DAT=vol, LABELS=labels, MASK=mask)

  if (!is_cifti_subcort(subcort)) {
    stop(paste(
      "The volume and label pair could not be converted into ",
      "a \"cifti_subcort\" object."
    ))
  }
  class(subcort) <- "cifti_subcort"
  subcort
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
  # file path
  if (is.fname(surf)){ surf <- readgii(surf) }
  # GIFTI
  gifti_to_surf <- function(gifti) {
    surf <- gifti$data
    verts <- surf$pointset
    faces <- surf$triangle
    if (min(faces)==0) faces <- faces + 1 #start vertex indexing at 1 instead of 0
    surf <- list(vertices = verts, faces = faces)
  }
  if (is.gifti(surf)) { surf <- gifti_to_surf(surf) }

  # Return cifti_surface or error.
  if (!is_cifti_surface(surf)) {
    stop("The object could not be converted into a cifti_surface object.")
  }
  class(surf) <- "cifti_surface"
  surf
}

#' Get CIFTI data from separate GIfTI and NIfTI files
#'
#' @description Make a "cifti" object from the separated left and right cortical
#'  GIfTI data and surface geometry files (\code{\link[gifti]{readgii}}),
#'  as well as the subcortical NIfTI file (\code{\link[RNifti]{readNifti}}).
#'  Objects can be provided as file names, GIFTI/NIFTI objects, or "cifti_[...]"
#'  objects.
#'
#' @param cortexL,cortexR (Optional) [Left/right] cortical data. Can be a file
#'  path for GIFTI data, an object from \code{\link[gifti]{readgii}}, or an
#'  object of class "ciftiTools_cortex".
#' @param subcortVol (Optional) Volumetric subcortical data. Can
#'  be a file path for NIFTI data, an object from
#'  \code{\link[RNifti]{readNifti}}, or more generally a 3D/4D array. If it is 
#'  provided then \code{subcortDat} and \code{subcortMask} should not be, and 
#'  vice versa. Requires \code{subcortLab}.
#' @param subcortDat,subcortMask (Optional) Alternatively to \code{subcortVol},
#'  the vectorized subcortical data can be provided with a volumetric mask. If
#'  these are provided then \code{subcortVol} should not be, and vice versa.
#'  Requires \code{subcortLab}. \code{subcortDat} should be a data matrix
#'  with voxels in rows (see \code{is_cifti_subcort_dat}), and 
#'  \code{subcortMask} should be a volumetric brain mask for subcortical
#'  locations (see \code{is_cifti_subcort_mask}).
#' @param subcortLab (Required if \code{subcortVol} or [\code{subcortDat} and
#'  \code{subcortMask}] are provided) Labels for subcortical ROIs. Can
#'  be a file path for NIFTI data or an object from
#'  \code{\link[RNifti]{readNifti}}. 
#'  
#'  If [\code{subcortDat} and \code{subcortMask}] are provided, this may also be
#'  a numeric vector, since the mask should have the spatial information. 
#'  But if \code{subcortVol} is provided, it must represent 3D data because it 
#'  is used to make the 3D mask.
#' @param surfL,surfR (Optional) [Left/right] brain surface model. Can be a file
#'  path for GIFTI data, an object from \code{\link[gifti]{readgii}}, or an
#'  object of class "cifti_surface".
#' @param read_dir (Optional) Append a directory to all file names in the
#'  arguments. If \code{NULL} (default), do not modify file names.
#'
#' @return An object of type "cifti", a list containing these five elements:
#'  CORTEX_LEFT, CORTX_RIGHT, SUBCORT, SURF_LEFT and SURF_RIGHT. 
#'
#' @export
#'
make_cifti <- function(
  cortexL=NULL, cortexR=NULL, 
  subcortVol=NULL, subcortDat=NULL, subcortMask=NULL, subcortLab=NULL, 
  surfL=NULL, surfR=NULL, 
  read_dir=NULL) {

  if (is.fname(cortexL)) { cortexL <- format_path(cortexL, read_dir, mode=4) }
  if (is.fname(cortexR)) { cortexR <- format_path(cortexR, read_dir, mode=4) }
  if (is.fname(subcortVol)) { subcortVol <- format_path(subcortVol, read_dir, mode=4) }
  if (is.fname(subcortLab)) { subcortLab <- format_path(subcortLab, read_dir, mode=4) }
  if (is.fname(surfL)) { surfL <- format_path(surfL, read_dir, mode=4) }
  if (is.fname(surfR)) { surfR <- format_path(surfR, read_dir, mode=4) }

  # Cortical data.
  if (!is.null(cortexL)) {
    cortexL <- make_cifti_cortex(cortexL)
  }
  if (!is.null(cortexR)) {
    cortexR <- make_cifti_cortex(cortexR)
  }

  # Subcortical data.
  if (is.null(subcortLab)) {
    if(!is.null(subcortVol)){
      stop("subcortVol must be accompanied by subcortLab.")
    }
    if(!is.null(subcortDat) | !is.null(subcortMask)){
      stop("subcortDat and subcortMask must be accompanied by subcortLab.")
    }
    subcort <- NULL
  } else {
    if (is.null(subcortDat) != is.null(subcortMask)) {
      stop("Either both or none of subcortDat and subcortMask should be provided.")
    }
    if (!xor(is.null(subcortVol), is.null(subcortDat))) {
      stop("Exactly one of subcortVol and (subcortDat/Mask) should be provided.")
    }

    if (!is.null(subcortVol)) { # Infer that MAT and MASK must both be present.
      subcort <- make_cifti_subcort_fromvol(subcortVol, subcortLab)
    } else {
      if (is.numeric(subcortMask)) { 
        cat("Numeric mask detected. Binarizing: values > 0 will be included.\n")
        subcortMask <- subcortMask > 0 
      }
      subcort <- list(DAT=subcortDat, LABELS=subcortLab, MASK=subcortMask)
    }
  }

  # Surfaces
  if (!is.null(surfL)) {
    surfL <- make_cifti_surface(surfL)
  }
  if (!is.null(surfR)) {
    surfR <- make_cifti_surface(surfR)
  }

  cifti <- list(
    CORTEX_LEFT = cortexL,
    CORTEX_RIGHT = cortexR,
    SUBCORT = subcort,
    SURF_LEFT = surfL,
    SURF_RIGHT = surfR
  )

  # Return cifti or error.
  if (!is_cifti(cifti)) {
    stop("The object could not be converted into a cifti object.")
  }

  class(cifti) <- 'cifti'
  cifti
}

#' @rdname make_cifti
#' @export
makeCIfTI <- makecii <- function(
  cortexL=NULL, cortexR=NULL,
  subcortVol=NULL, subcortDat=NULL, subcortMask=NULL, subcortLab=NULL,
  surfL=NULL, surfR=NULL,
  read_dir=NULL) {

  make_cifti(
    cortexL, cortexR, 
    subcortVol, subcortDat, subcortMask, subcortLab, 
    surfL, surfR, 
    read_dir
  )
}