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

#' Coerce a volume and label into a valid "cifti_subcort"
#'  object. They can both be file paths or arrays. Neither should be
#'  flattened/vectorized already. They should be compatible.
#'
#' @param vol File path or array for the volumetric subcortical data.
#'  It should be a 3D array if only one measurement was made per brainordinate,
#'  or a 4D array if the file represents a time series.
#' @param labels File path or array for the subcortical labels. It should be a
#'  3D array.
#'
#' @return The "cifti_subcort" object with components "DAT", "LABELS", and "MASK".
#' @export
#'
#' @importFrom RNifti readNifti
make_cifti_subcortical <- function(vol, labels) {
  # vol
  if (is.fname(vol)) {
    vol <- readNifti(vol)
  } else {
    if (dim(vol) > 2) { stop("The volume should be a 3D or 4D array. It shouldn't be vectorized.") }
  }
  # [TO DO]: check if it is from RNifti::readNifti.
  # https://github.com/jonclayden/RNifti/blob/master/NAMESPACE

  # labels
  if (is.fname(labels)) {
    labels <- readNifti(labels)
  } else {
    if (!is.array(labels)) { stop("The labels should be a 3D array. It shouldn't be vectorized.") }
  }

  # mask
  mask <- labels > 0
  labels <- as.numeric(labels)
  vol <- matrix(as.numeric(vol[mask]), ncol=dim(vol)[length(dim(vol))])

  # [TO DO]: Use if statement, or always do?
  #if ((1 %in% labels) | (2 %in% labels))
  labels[labels > 0] <- labels[labels > 0] + 2

  # Return them or raise an error.
  if (!is_cifti_subcortical(vol, labels)) {
    stop(paste(
      "The objects could not be converted into",
      "\"cifti_volume\" and \"cifti_label\" objects."
    ))
  }
  subcort <- list(DAT=vol, LABELS=labels, MASK=mask)
  if (!is_cifti_subcortical(subcort)) { stop('Could not convert to "cifti_subcort".') }

  class(subcort) <- "cifti_subcort"
  subcort
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
unflatten_cifti_vol <- function(mat, mask) {
  if (!is_cifti_matmask_pair(mat, mask)) {
    stop(paste(
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
#'  path for GIFTI data, an object from \code{\link[gifti]{readgii}}, or an
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
make_cifti <- function(
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
    cif$CORTEX_LEFT <- make_cifti_cortex(cif$CORTEX_LEFT)
  }
  if (!is.null(cif$CORTEX_RIGHT)) {
    cif$CORTEX_RIGHT <- make_cifti_cortex(cif$CORTEX_RIGHT)
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
      cif$VOL <- unflatten_cifti_vol(cif$MAT, cif$MASK)
    }
    cif$SUBCORT <- make_cifti_subcortical(cif$VOL, cif$LABELS)
  }

  # Surfaces
  if (!is.null(cif$SURF_LEFT)) {
    cif$SURF_LEFT <- make_cifti_surface(cif$SURF_LEFT)
  }
  if (!is.null(cif$SURF_RIGHT)) {
    cif$SURF_RIGHT <- make_cifti_surface(cif$SURF_RIGHT)
  }

  cif$MAT <- cif$MASK <- NULL

  class(cif) <- 'cifti'

  # Return cifti or error.
  if (!is_cifti(cif)) {
    stop("The object could not be converted into a cifti object.")
  }
  cif
}

#' @rdname make_cifti
#' @export
makeCIfTI <- makecii <- make_cifti <- function(
  cortexL=NULL, cortexR=NULL,
  subcortVol=NULL, subcortMat=NULL, subcortMask=NULL, subcortLab=NULL,
  surfL=NULL, surfR=NULL,
  read_dir=NULL) {

  make_cifti(
    cortexL, cortexR, 
    subcortVol, subcortMat, subcortMask, subcortLab, 
    surfL, surfR, 
    read_dir
  )
}
