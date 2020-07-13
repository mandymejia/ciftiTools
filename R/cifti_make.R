#' Coerce a file path, GIFTI object, or "cifti_cortex" object to a 
#'  "cifti_cortex" object.
#'
#' @param cortex What to coerce as a "cifti_cortex" object.
#'
#' @return The "cifti_cortex" object.
#' @export
#'
#' @importFrom gifti readGIfTI is.gifti
cifti_make_cortex <- function(cortex) {
  # File path.
  if (is.fname(cortex)) {
    cortex <- readGIfTI(cortex)
  }
  # GIFTI,
  if (is.gifti(cortex)) { 
    cortex <- do.call(cbind, cortex$data)
  }

  # Return cifti_cortex or error.
  if (!is.cifti_cortex(cortex)) { 
    stop("The object could not be converted into a cifti_cortex object.")
  } 
  class(cortex) <- "cifti_cortex"
  cortex
}

#' Coerce a volume and label into valid "cifti_volume" and "cifti_label" 
#'  objects. They can both be file paths, NIFTI objects, or "cifti_volume"
#'  / "cifti_labels" objects. They should correspond to one another.
#'
#' @param vol What to coerce as a "cifti_volume" object.
#' @param labels What to coerce as a "cifti_labels" object.
#'
#' @return The "cifti_volume" and "cifti_label"  objects.
#' @export
#'
#' @importFrom RNifti readNifti
cifti_make_subcortical <- function(vol, labels) {
  # vol 
  if (is.fname(vol)) {
    vol <- readNifti(vol)
  }
  # [TO DO]: check if it is from RNifti::readNifti.
  # https://github.com/jonclayden/RNifti/blob/master/NAMESPACE

  # labels
  if (is.fname(labels)) {
    labels <- readNifti(labels)
  }
  labels[labels > 0] <- labels[labels > 0] + 2
  # [TO DO]: check if it is from RNifti::readNifti.

  # Return them or raise an error.
  if (!is.cifti_subcortical(vol, labels)) { 
    stop(paste0(
      "The objects could not be converted into",
      "\"cifti_volume\" and \"cifti_label\" objects."
    ))
  } 
  subcortical <- list(vol=vol, labels=labels)
  class(subcortical) <- "cifti_subcortical"
  subcortical
}

#' Coerce a matrix and mask into valid "cifti_volume" and "cifti_label" 
#'  objects. They should both be numeric data with compatible dimensions.
#'
#' @param mat Data matrix for subcortical locations, with voxels in rows
#' @param mask Volumetric brain mask for subcortical locations
#'
#' @return The "cifti_volume" and "cifti_label"  objects.
#' @export
#'
cifti_make_vol <- function(mat, mask) {
  if (!is.cifti_matmask_pair(mat, mask)) {
    stop(paste0(
      "The objects are not a compatible matrix and mask pair."
    ))
  }
  vol <- array(NA, dim=c(dim(mask), ncol(mat)))
  for(ii in 1:ncol(mat)) { 
    vol[,,,ii][mask==1] <- mat[,ii] 
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
#' @importFrom gifti readGIfTI is.gifti
cifti_make_surface <- function(surf) {
  # file path
  if (is.fname(surf)){ surf <- readGIfTI(surf) }
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
  if (!is.cifti_surface(surf)) { 
    stop("The object could not be converted into a cifti_surface object.")
  } 
  class(surf) <- "cifti_surface"
  surf
}

#' Get CIFTI data from separate GIfTI and NIfTI files
#'
#' @description Make a "cifti" object from the separated left and right cortical 
#'  GIfTI data and surface geometry files (\code{\link[gifti]{readGIfTI}}), 
#'  as well as the subcortical NIfTI file (\code{\link[RNifti]{readNifti}}).
#'  Objects can be provided as file names, GIFTI/NIFTI objects, or "cifti_[...]"
#'  objects. 
#'
#' @param cortexL,cortexR (Optional) [Left/right] cortical data. Can be a file
#'  path for GIFTI data, an object from \code{\link[gifti]{readGIfTI}}, or an 
#'  object of class "ciftiTools_cortex".
#' @param subcortVol (Optional) Volumetric subcortical data. Can 
#'  be a file path for NIFTI data, an object from 
#'  \code{\link[RNifti]{readNifti}}, or the "vol" component of a 
#'  ciftiTools_subcortical" object. If it is provided then \code{subcortMat} and 
#'  \code{subcortMask} should not be, and vice versa. Requires \code{subcortLab}.
#' @param subcortMat,subcortMask (Optional) Alternatively to \code{subcortVol},
#'  the vectorized subcortical data can be provided with a volumetric mask. If
#'  these are provided then \code{subcortVol} should not be, and vice versa. 
#'  Requires \code{subcortLab}. The \code{subcortMat} should be a data matrix 
#'  for subcortical locations, with voxels in rows, and \code{subcortMask}
#'  should be a volumetric brain mask for subcortical locations.
#' @param subcortLab (Required if \code{subcortVol} or [\code{subcortMat} and 
#'  \code{subcortMask}] are present) Labels for subcortical ROIs. Can 
#'  be a file path for NIFTI data, an object from 
#'  \code{\link[RNifti]{readNifti}}, or the "labels" component of a 
#'  ciftiTools_subcortical" object.
#' @param surfL,surfR (Optional) [Left/right] brain surface model. Can be a file
#'  path for GIFTI data, an object from \code{\link[gifti]{readGIfTI}}, or an 
#'  object of class "cifti_surface". 
#' @param read_dir (Optional) Append a directory to all file names. If \code{NULL}
#'  (default), do not modify file names. 
#' 
#' @return An object of type "cifti", a list containing at least 4 elements: 
#'  CORTEX_LEFT, CORTX_RIGHT, VOL and LABELS. LABELS contains the brain 
#'  structure labels (usually 3-21) of the subcortical elements. If surface 
#'  geometry files were provided in the arguments, the list will also contain 
#'  SURF_LEFT and/or SURF_RIGHT.
#'
#' @export
#'
cifti_make_from_separate <- function(
  cortexL=NULL, cortexR=NULL, 
  subcortVol=NULL, subcortMat=NULL, subcortMask=NULL, subcortLab=NULL, 
  surfL=NULL, surfR=NULL, 
  read_dir=NULL) {

  cif <- list(
    CORTEX_LEFT=cortexL, CORTEX_RIGHT=cortexR, 
    VOL=subcortVol, MAT=subcortMat, MASK=subcortMask, 
    LABELS=subcortLab, 
    SURF_LEFT=surfL, SURF_RIGHT=surfR
  )

  cif <- lapply(cif, function(x){ 
    if (is.fname(x)) { format_path(x, read_dir, mode=4) } else { x } 
  })

  # Cortical data.
  if (!is.null(cif$CORTEX_LEFT)) {
    cif$CORTEX_LEFT <- cifti_make_cortex(cif$CORTEX_LEFT)
  }
  if (!is.null(cif$CORTEX_RIGHT)) {
    cif$CORTEX_RIGHT <- cifti_make_cortex(cif$CORTEX_RIGHT)
  }

  # Subcortical data.
  if (is.null(cif$LABELS)) { 
    if(!is.null(cif$VOL)){
      stop("subcortVol must be accompanied by subcortLab.")
    }
    if(!is.null(cif$MAT) | !is.null(cif$MASK)){
      stop("subcortMat and subcortMask must be accompanied by subcortLab.")
    }
  } else {
    if (is.null(cif$MAT) != is.null(cif$MASK)) {
      stop("Either both or none of subcortMat and subcortMask should be provided.")
    }
    if (!xor(is.null(cif$VOL), is.null(cif$MAT))) {
      stop("Exactly one of subcortVol and (subcortMat/Mask) should be provided.")
    }
    if (is.null(cif$VOL)) { # Infer that MAT and MASK must both be present.
      cif$VOL <- cifti_make_vol(cif$MAT, cif$MASK)
    }
    subcort <- cifti_make_subcortical(cif$VOL, cif$LABELS)
    cif$VOL <- subcort$vol; cif$LABELS <- subcort$labels
  }

  # Surfaces
  if (!is.null(cif$SURF_LEFT)) { 
    cif$SURF_LEFT <- cifti_make_surface(cif$SURF_LEFT) 
  }
  if (!is.null(cif$SURF_RIGHT)) { 
    cif$SURF_RIGHT <- cifti_make_surface(cif$SURF_RIGHT) 
  }

  cif$MAT <- cif$MASK <- NULL

  class(cif) <- 'cifti'

  # Return cifti or error.
  if (!is.cifti(cif)) { 
    stop("The object could not be converted into a cifti object.")
  } 
  cif
}