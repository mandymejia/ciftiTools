#' Assemble a \code{"xifti"} from data
#' 
#' Assembles cortical data, subcortical data, and/or surface geometry to form a 
#'  \code{"xifti"}. The inputs must be data objects (vectors, matrices or 
#'  arrays, depending on the argument).
#' 
#' @inherit make_xifti details
#' 
#' @param cortexL,cortexL_mwall Left cortex data and ROI. Each must be a data 
#'  matrix or vector.
#' 
#'  If \code{cortexL_mwall} is not provided, \code{cortexL} should have data for
#'  all vertices on the left cortical surface (\eqn{V_L x T} data matrix). There 
#'  will not be a mask for the medial wall. Not providing the medial wall mask 
#'  is appropriate for ".dlabels.nii" files where the medial wall may have its 
#'  own label and therefore should not be treated as missing data.
#' 
#'  If \code{cortexL_mwall} is provided, \code{cortexL} should either have data
#'  for all vertices on the left cortical surface (\eqn{V_L x T} data matrix, with
#'  filler values e.g. \code{0} or \code{NaN} for medial wall vertices), or have data 
#'  only for non-medial wall vertices (\eqn{(V_L - mwall_L) x T} data matrix).
#'  The medial wall mask will be the \code{0} values in \code{cortexL_mwall}. 
#'  The medial wall mask should be provided whenever the medial wall should be
#'  treated as missing data. 
#' 
#'  Since the unmasked cortices must have the same number of vertices,
#'  \code{V_L} should match \code{V_R}.
#' @param cortexR,cortexR_mwall Right cortex data and ROI. Each must be a data
#'  matrix or vector.
#' 
#'  If \code{cortexR_mwall} is not provided, \code{cortexR} should have data for
#'  all vertices on the right cortical surface (\eqn{V_R x T} data mre 
#'  will not be a mask for the medial wall. Not providing the medial wall mask 
#'  is appropriate for ".dlabels.nii" files where the medial wall may have its 
#'  own label and therefore should not be treated as missing data.
#' 
#'  If \code{cortexR_mwall} is provided, \code{cortexR} should either have data
#'  for all vertices on the right cortical surface (\eqn{V_R x T} data matrix, with
#'  filler values e.g. \code{0} or \code{NaN} for medial wall vertices), or have data 
#'  only for non-medial wall vertices (\eqn{(V_R - mwall_R) x T} data matrix).
#'  The medial wall mask will be the \code{0} values in \code{cortexR_mwall}. 
#'  The medial wall mask should be provided whenever the medial wall should be
#'  treated as missing data. 
#' 
#'  Since the unmasked cortices must have the same number of vertices,
#'  \code{V_L} should match \code{V_R}.
#' @param subcortVol,subcortLabs,subcortMask \code{subcortVol} represents the
#'  data values of the subcortex. It is either a 3D/4D numeric array
#'  (\eqn{i x j x k x T}), or a vectorized matrix (\eqn{V_S} voxels by \eqn{T} 
#'  measurements). If it's vectorized, the voxels should be in spatial order 
#'  (\eqn{i} index increasing fastest, then \eqn{j}, then \eqn{k}).
#' 
#'  \code{subcortLabs} represents the brainstructure labels of each voxel: see
#'  \code{\link{substructure_table}}. It is either a 3D data array 
#'  (\eqn{i x j x k}) of integer brainstructure indices, or a \eqn{V_S} length
#'  vector in spatial order with brainstructure names as factors or integer
#'  indices. The indices should be 3-21 (1 and 2 correspond to left and right
#'  cortex, respectively) or 1-19 (cortex labels omitted), with 0 representing
#'  out-of-mask voxels.
#'  
#'  \code{subcortMask} is logical 3D data array (\eqn{i x j x k}) where \code{TRUE}
#'  values indicate subcortical voxels (in-mask). If it is not provided, the
#'  mask will be inferred from voxels with labels \code{0}, \code{NA}, or 
#'  \code{NaN} in \code{subcortLabs}. If \code{subcortLabs} are vectorized and 
#'  \code{subcortMask} is not provided, the mask cannot be inferred so an error 
#'  will occur.
#' @param mwall_values If \code{cortex[L/R]_mwall} was not provided, or if it
#'  was invalid (i.e. bad length or all \code{TRUE}), the medial wall mask will
#'  be inferred from rows in \code{cortex[L/R]} that are constantly one of these
#'  values. Default: \code{c(NA, NaN)}. If \code{NULL}, do not attempt to infer
#'  the medial wall from the data values. \code{NULL} should be used if \code{NA}
#'  or \code{NaN} are legitimate values that non-medial wall vertices might
#'  take on.
#' @param surfL,surfR (Optional) Surface geometries for the left or right cortex. 
#'  Can be a surface GIFTI file path or \code{"surf"} object; see 
#'  \code{\link{make_surf}} for a full description of valid inputs.
#' @param col_names Names of each measurement/column in the data.
#' @param HCP_32k_auto_mwall If left and/or right cortex data is provided, and
#'  the number of vertices matches that of the HCP 32k mesh (29696 on left, and
#'  29716 on right), should the medial wall masks be added to the \code{"xifti"}
#'  if not provided? Default: \code{TRUE}.
#' @param validate Validate that the result is a \code{"xifti"}? Default:
#'  \code{TRUE}. If \code{FALSE}, the result may not be properly formatted
#'  if the inputs were invalid.
#' @return A \code{"xifti"}
#' 
#' @family reading
#' @export
#'
as.xifti <- function(
  cortexL=NULL, cortexL_mwall=NULL,
  cortexR=NULL, cortexR_mwall=NULL,
  mwall_values=c(NA, NaN),
  subcortVol=NULL, subcortLabs=NULL, subcortMask=NULL,
  surfL=NULL, surfR=NULL,
  col_names=NULL, HCP_32k_auto_mwall=TRUE, validate=TRUE){

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
    mwall_values=mwall_values,
    subcortVol=subcortVol, subcortLabs=subcortLabs, subcortMask=subcortMask,
    surfL=surfL, surfR=surfR, col_names=col_names, 
    HCP_32k_auto_mwall=HCP_32k_auto_mwall,
    validate=validate
  )
}

#' @rdname as.xifti
#' @export
as_xifti <- function(
  cortexL=NULL, cortexL_mwall=NULL,
  cortexR=NULL, cortexR_mwall=NULL,
  mwall_values=c(NA, NaN),
  subcortVol=NULL, subcortLabs=NULL, subcortMask=NULL,
  surfL=NULL, surfR=NULL){

  as.xifti(
    cortexL=cortexL, cortexL_mwall=cortexL_mwall,
    cortexR=cortexR, cortexR_mwall=cortexR_mwall,
    mwall_values=mwall_values,
    subcortVol=subcortVol, subcortLabs=subcortLabs, subcortMask=subcortMask,
    surfL=surfL, surfR=surfR
  )
}

#' @rdname as.xifti
#' @export
as.cifti <- function(
  cortexL=NULL, cortexL_mwall=NULL,
  cortexR=NULL, cortexR_mwall=NULL,
  mwall_values=c(NA, NaN),
  subcortVol=NULL, subcortLabs=NULL, subcortMask=NULL,
  surfL=NULL, surfR=NULL){

  ciftiTools_warn("as.cifti() is an alias for as.xifti().\n")

  as.xifti(
    cortexL=cortexL, cortexL_mwall=cortexL_mwall,
    cortexR=cortexR, cortexR_mwall=cortexR_mwall,
    mwall_values=mwall_values,
    subcortVol=subcortVol, subcortLabs=subcortLabs, subcortMask=subcortMask,
    surfL=surfL, surfR=surfR
  )
}

#' @rdname as.xifti
#' @export
as_cifti <- function(
  cortexL=NULL, cortexL_mwall=NULL,
  cortexR=NULL, cortexR_mwall=NULL,
  mwall_values=c(NA, NaN),
  subcortVol=NULL, subcortLabs=NULL, subcortMask=NULL,
  surfL=NULL, surfR=NULL){

  ciftiTools_warn("as_cifti() is an alias for as.xifti().\n")

  as.xifti(
    cortexL=cortexL, cortexL_mwall=cortexL_mwall,
    cortexR=cortexR, cortexR_mwall=cortexR_mwall,
    mwall_values=mwall_values,
    subcortVol=subcortVol, subcortLabs=subcortLabs, subcortMask=subcortMask,
    surfL=surfL, surfR=surfR
  )
}