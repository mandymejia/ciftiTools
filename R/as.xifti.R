#' Make a \code{"xifti"} object
#' 
#' Merge separate data objects into a \code{"xifti"} object.
#' 
#' Takes in cortical data (medial wall masks optional but recommended if 
#'  available), subcortical data, and surfaces. Each entry is optional. If all 
#'  entries are \code{NULL} then the template \code{"xifti"} will be returned.
#' 
#'  If cortical data are provided without a corresponding medial wall mask, then
#'  it will try to infer the medial wall mask. ...
#' 
#'  The total number of brainordinates will be $V = V_L + V_R + V_S$: $V_L$ left
#'  vertices, $V_R$ right vertices and $V_S$ subcortical voxels. $T$, the total
#'  number of measurements (columns of data), must be the same for each
#'  brainstructure.
#' 
#' @param cortexL,cortexL_mwall \code{cortexL} is a data matrix ($V_L$ vertices x 
#'  $T$ measurements) representing the left cortex. If it's provided, \code{cortexL_mwall}
#'  can be provided too. $V_L$ is equal to either the length of \code{cortexL_mwall} 
#'  (if the left cortex data is unmasked) or the sum of \code{TRUE} values 
#'  (if the left cortex data is masked).
#' 
#'  If \code{cortexL_mwall} is not provided, ...
#' 
#'  Since the unmasked cortices must have the same number of vertices,
#'  \code{cortexL_mwall} and \code{cortexR_mwall} should be of equal length.
#' @param cortexR,cortexR_mwall \code{cortexR} is a data matrix (V_R vertices x 
#'  T measurements) representing the right cortex. If it's provided, \code{cortexR_mwall}
#'  can be provided too. V_R is equal to either the length of \code{cortexR_mwall} 
#'  (if the right cortex data is unmasked) or the sum of \code{TRUE} values 
#'  (if the right cortex data is masked).
#' 
#'  If \code{cortexR_mwall} is not provided, ...
#' 
#'  Since the unmasked cortices must have the same number of vertices,
#'  \code{cortexL_mwall} and \code{cortexR_mwall} should be of equal length.
#' @param subcortVol,subcortLabs,subcortMask \code{subcortVol} represents the
#'  data values of the subcortical volume. It is either a 3D/4D data array
#'  (i x j x k x T) or a vectorized data matrix (V_S voxels x T measurements). 
#'  If it's vectorized, voxels should be in spatial order.
#' 
#'  \code{subcortLabs} represents the brainstructure labels of each voxel: see
#'  \code{\link{substructure_table}}. It is either a 3D data array (i x j x k)
#'  of brainstructure indices 3-21 with 0 representing out-of-mask voxels; or,
#'  a V_S-length vector in spatial order with brainstructure names as factors, 
#'  or with  brainstructure indices as integers.
#'  
#'  \code{subcortMask} is a logical 3D data array (i x j x k) where \code{TRUE}
#'  values indicate voxels inside the brain mask. If it is not provided, the
#'  mask will be inferred from zero- and NA-valued voxels in \code{subcortLabs}
#'  (or \code{subcortVol} if \code{subcortLabs} is vectorized). If both 
#'  \code{subcortVol} and \code{subcortLabs} are vectorized and \code{subcortMask}
#'  is not provided, the mask cannot be inferred so an error occur.
#' @param surfL,surfR (Optional) Surface geometries for the left or right cortex. 
#'  Can a GIFTI representing surface geometry which has been read in or a list 
#'  with components "verts" (V x 3 data matrix indicating spatial locations of 
#'  each vertex) and "faces" (F x 3 data matrix indicating the indices of the 
#'  three vertices defining each triangular face).
#'
#' @return A "xifti" object.
#' @export
#'
as.xifti <- function(
  cortexL=NULL, cortexL_mwall=NULL,
  cortexR=NULL, cortexR_mwall=NULL,
  subcortVol=NULL, subcortLabs=NULL, subcortMask=NULL,
  surfL=NULL, surfR=NULL){

  if (!is.null(cortexL)) { stopifnot(is.numeric(cortexL)) }
  if (!is.null(cortexR)) { stopifnot(is.numeric(cortexR)) }
  if (!is.null(subcortVol)) { stopifnot(is.numeric(subcortVol)) }
  if (!is.null(subcortLabs)) { stopifnot(is.factor(subcortLabs) | is.numeric(subcortLabs)) }
  if (!is.null(subcortMask)) { stopifnot(is.logical(subcortMask) | is.numeric(subcortMask)) }
  if (!is.null(surfL)) { stopifnot(is.list(surfL)) }
  if (!is.null(surfR)) { stopifnot(is.list(surfR)) }

  make_xifti(
    cortexL=cortexL, cortexL_mwall=cortexL_mwall,
    cortexR=cortexR, cortexR_mwall=cortexR_mwall,
    subcortVol=subcortVol, subcortLabs=subcortLabs, subcortMask=subcortMask,
    surfL=surfL, surfR=surfR
  )
}

#' @rdname as.xifti
#' @export
as_xifti <- as.cifti <- as_cifti <- function(
  cortexL=NULL, cortexL_mwall=NULL,
  cortexR=NULL, cortexR_mwall=NULL,
  subcortVol=NULL, subcortLabs=NULL, subcortMask=NULL,
  surfL=NULL, surfR=NULL){

  as.xifti(
    cortexL=cortexL, cortexL_mwall=cortexL_mwall,
    cortexR=cortexR, cortexR_mwall=cortexR_mwall,
    subcortVol=subcortVol, subcortLabs=subcortLabs, subcortMask=subcortMask,
    surfL=surfL, surfR=surfR
  )
}