#' Assemble a \code{"xifti"} object from data
#' 
#' Assembles cortical data, subcortical data, and/or surface geometry to form a 
#'  "xifti". The inputs must be data objects (matrices or arrays). 
#'  \code{as.xifti} can be used to combine the files written by 
#'  \code{\link{separate_cifti}}, or read individual components independent of 
#'  any CIFTI file. 
#' 
#' @inherit make_xifti details
#' 
#' @param cortexL,cortexL_mwall \code{cortexL} represents the left cortex data.
#'  Each must be a data matrix or vector.
#' 
#'  If \code{cortexL_mwall} is not provided, \code{cortexL} should have data for
#'  all vertices on the left cortical surface ($V_L$ x $T$ data matrix). There 
#'  will not be a mask for the medial wall. Not providing the medial wall mask 
#'  is appropriate for ".dlabels.nii" files where the medial wall has its own 
#'  label and should not be treated as missing data.
#' 
#'  If \code{cortexL_mwall} is provided, \code{cortexL} should either have data
#'  for all vertices on the left cortical surface ($V_L$ x $T$ data matrix, with
#'  filler values e.g. \code{0} or \code{NaN} for medial wall vertices), or for
#'  only non-medial wall vertices ($(V_L - mwall_L)$ x $T$ data matrix). The
#'  medial wall mask will be the \code{0} values in \code{cortexL_mwall}. 
#'  The medial wall mask should be provided for ".dscalar.nii" and ".dtseries.nii"
#'  files where the medial wall data is not present. 
#' 
#'  Since the unmasked cortices must have the same number of vertices,
#'  \code{cortexL_mwall} and \code{cortexR_mwall} should be of equal length.
#' @param cortexR,cortexR_mwall \code{cortexR} represents the right cortex data.
#'  Each must be a data matrix or vector.
#' 
#'  If \code{cortexR_mwall} is not provided, \code{cortexR} should have data for
#'  all vertices on the right cortical surface ($V_R$ x $T$ data matrix). There 
#'  will not be a mask for the medial wall. Not providing the medial wall mask 
#'  is appropriate for ".dlabels.nii" files where the medial wall has its own 
#'  label and should not be treated as missing data.
#' 
#'  If \code{cortexR_mwall} is provided, \code{cortexR} should either have data
#'  for all vertices on the right cortical surface ($V_R$ x $T$ data matrix, with
#'  filler values e.g. \code{0} or \code{NaN} for medial wall vertices), or for
#'  only non-medial wall vertices ($(V_R - mwall_R)$ x $T$ data matrix). The
#'  medial wall mask will be the \code{0} values in \code{cortexR_mwall}. 
#'  The medial wall mask should be provided for ".dscalar.nii" and ".dtseries.nii"
#'  files where the medial wall data is not present. 
#' 
#'  Since the unmasked cortices must have the same number of vertices,
#'  \code{cortexL_mwall} and \code{cortexR_mwall} should be of equal length.
#' @param subcortVol,subcortLabs,subcortMask \code{subcortVol} represents the
#'  data values of the subcortex. It is either a 3D/4D data array
#'  ($i$ x $j$ x $k$ x $T$), or a vectorized data matrix ($V_S$ voxels x $T$ 
#'  measurements). If it's vectorized, the voxels should be in spatial order.
#' 
#'  \code{subcortLabs} represents the brainstructure labels of each voxel: see
#'  \code{\link{substructure_table}}. It is either a 3D data array 
#'  ($i$ x $j$ x $k$) of numeric brainstructure indices, or a $V_S$ length
#'  vector in spatial order with brainstructure names as factors or integer
#'  indices. The indices should be 3-21 (2 and 3 correspond to left and right
#'  cortex, respectively) or 1-19 (cortex labels omitted), with 0 representing
#'  out-of-mask voxels.
#'  
#'  \code{subcortMask} is logical 3D data array (i x j x k) where \code{TRUE}
#'  values indicate subcortical voxels (in-mask). If it is not provided, the
#'  mask will be inferred from voxels with labels \code{0} or \code{NA} in 
#'  \code{subcortLabs}. If \code{subcortLabs} are vectorized and \code{subcortMask}
#'  is not provided, the mask cannot be inferred so an error will occur.
#' @param surfL,surfR (Optional) Surface geometries for the left or right cortex. 
#'  Can be a surface GIFTI file path or "surface" object; see 
#'  \code{\link{make_surf}} for a full description of valid inputs.
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