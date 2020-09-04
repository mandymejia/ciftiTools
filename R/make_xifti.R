#' Assemble a \code{"xifti"} object
#'
#' Assembles cortical data, subcortical data, and/or surface geometry to form a 
#'  "xifti". The inputs can be file paths, GIFTI or NIFTI files which have been
#'  read in, or objects formatted as they are in \code{ciftiTools}. See 
#'  \code{as.xifti} for a user-function wrapper that only works with data 
#'  objects. \code{make_xifti} can be used to combine the files written by 
#'  \code{\link{separate_cifti}}, or read individual components independent of 
#'  any CIFTI file. 
#' 
#' Each data or surface component is optional. A metadata component will be 
#'  ignored if its corresponding data component is not provided. If no data or
#'  surface components are provided, then the \code{\link{template_xifti}} will 
#'  be returned. 
#' 
#'  If cortical data are provided without a corresponding medial wall mask, or
#'  if the provided mask is invalid or empty, then the medial wall metadata
#'  entry will be \code{NULL}. (It does not attempt to infer the medial wall
#'  mask from data values.)
#'  
#'  The total number of brainordinates will be $V = V_L + V_R + V_S$: $V_L$ left
#'  vertices, $V_R$ right vertices and $V_S$ subcortical voxels. $T$, the total
#'  number of measurements (columns of data), must be the same for each
#'  brainstructure.
#' 
#' @param cortexL,cortexL_mwall \code{cortexL} represents the left cortex data.
#'  Each can be a file path to a metric GIFTI, a \code{"gifti"} object, or a 
#'  data matrix or vector.
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
#'  Each can be a file path to a metric GIFTI, a \code{"gifti"} object, or a 
#'  data matrix or vector.
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
#'  data values of the subcortex. It is either a NIFTI file path, 3D/4D data array
#'  ($i$ x $j$ x $k$ x $T$), or a vectorized data matrix ($V_S$ voxels x $T$ 
#'  measurements). If it's vectorized, the voxels should be in spatial order.
#' 
#'  \code{subcortLabs} represents the brainstructure labels of each voxel: see
#'  \code{\link{substructure_table}}. It is either a NIFTI file path, a 3D data array 
#'  ($i$ x $j$ x $k$) of numeric brainstructure indices, or a $V_S$ length
#'  vector in spatial order with brainstructure names as factors or integer
#'  indices. The indices should be 3-21 (2 and 3 correspond to left and right
#'  cortex, respectively) or 1-19 (cortex labels omitted), with 0 representing
#'  out-of-mask voxels.
#'  
#'  \code{subcortMask} is a NIFTI file path or logical 3D data array (i x j x k) where \code{TRUE}
#'  values indicate subcortical voxels (in-mask). If it is not provided, the
#'  mask will be inferred from voxels with labels \code{0} or \code{NA} in 
#'  \code{subcortLabs}. If \code{subcortLabs} are vectorized and \code{subcortMask}
#'  is not provided, the mask cannot be inferred so an error will occur.
#' @param cifti_info (Optional) The result of \code{\link{info_cifti}}. If 
#'  GIFTI and/or NIFTI components from a CIFTI are being provided, 
#'  providing \code{cifti_info} gives metadata information that would otherwise
#'  have to be inferred. 
#' 
#'  This argument is probably not necessary for end users: reading a CIFTI
#'  should be done by providing \code{cifti_fname}, and for reading separate
#'  GIFTI/NIFTI components \code{cifti_info} is not applicable.
#' @param surfL,surfR (Optional) Surface geometries for the left or right cortex. 
#'  Can be a surface GIFTI file path or "surface" object; see 
#'  \code{\link{make_surf}} for a full description of valid inputs.
#' @param read_dir (Optional) Append a directory to all file names in the
#'  arguments. If \code{NULL} (default), do not modify file names.
#'
#' @return A "xifti" object; see \code{\link{template_xifti}}. 
#'
#' @inheritSection labels_Description Label Levels
#' 
#' @keywords internal
#'
make_xifti <- function(
  cortexL=NULL, cortexL_mwall=NULL,
  cortexR=NULL, cortexR_mwall=NULL,
  subcortVol=NULL, subcortLabs=NULL, subcortMask=NULL,
  cifti_info=NULL,
  surfL=NULL, surfR=NULL, 
  read_dir=NULL) {
  
  # # Use `read_cifti` if `cifti_fname` was provided.
  # if (!is.null(cifti_fname)) {
  #   if (!all(sapply(list(cortexL, cortexR, subcortVol, subcortLabs, cifti_info), is.null))) {
  #     ciftiTools_warn("`cifti_fname` was provided, so it will be read. separate GIFTI/NIFTI data and `cifti_info` will be ignored.")
  #   }
  #   return( read_cifti(cifti_fname, brainstructures=cifti_brainstructures, ...) )
  # }
  
  # Add `read_dir` and check file paths.
  if (is.fname(cortexL)) { cortexL <- format_path(cortexL, read_dir, mode=4) }
  if (is.fname(cortexR)) { cortexR <- format_path(cortexR, read_dir, mode=4) }
  if (is.fname(subcortVol)) { subcortVol <- format_path(subcortVol, read_dir, mode=4) }
  if (is.fname(subcortLabs)) { subcortLabs <- format_path(subcortLabs, read_dir, mode=4) }
  if (is.fname(surfL)) { surfL <- format_path(surfL, read_dir, mode=4) }
  if (is.fname(surfR)) { surfR <- format_path(surfR, read_dir, mode=4) }

  # Template.
  xifti <- template_xifti()

  # Cortex data.
  # TO DO: see `read_cifti_convert` on using intent to change mwall infer behavior
  if (!is.null(cortexL)) {
    x <- make_cortex(
      cortexL, cortexL_mwall,
      side="left", mwall_source="the input `cortexL_mwall`"
    )
    xifti$data$cortex_left <- x$data
    xifti$meta$cortex$medial_wall_mask["left"] <- list(x$mwall)
  }
  if (!is.null(cortexR)) {
    x <- make_cortex(
      cortexR, cortexR_mwall,
      side="right", mwall_source="the input `cortexR_mwall`"
    )
    xifti$data$cortex_right <- x$data
    xifti$meta$cortex$medial_wall_mask["right"] <- list(x$mwall)
  }

  # Subcortical data. 
  if (xor(is.null(subcortVol), is.null(subcortLabs))) {
    stop("subcortVol and subcortLabs must be provided together.")
  }
  if (!is.null(subcortVol)) {
    x <- make_subcort(subcortVol, subcortLabs, subcortMask, validate_mask=FALSE)
    xifti$data$subcort <- x$data
    xifti$meta$subcort$labels <- x$labels
    xifti$meta$subcort$mask <- x$mask
  }

  # CIFTI metadata.
  if (!is.null(cifti_info)) { xifti$meta$cifti <- cifti_info$cifti }

  # Surfaces.
  if (!is.null(surfL)) { xifti$surf$cortex_left <- make_surf(surfL) }
  if (!is.null(surfR)) { xifti$surf$cortex_right <- make_surf(surfR) }

  if (!is.xifti(xifti)) { stop("Could not make a valid \"xifti\" object.") }
  structure(xifti, class="xifti")
}