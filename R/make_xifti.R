#' Assemble a CIFTI or GIFTI/NIFTI Components into a "xifti"
#'
#' @description Puts cortical and/or subcortical data together into a "xifti".
#'  This can be used to read a CIFTI, combine the files written by 
#'  \code{\link{separate_cifti}}, or read in individual GIFTIs and NIFTIs
#'  independent of any CIFTI file.
#'
#' @param cifti_fname (Optional) Path to a CIFTI file. 
#' 
#'  If provided, none of \code{cortexL}, \code{cortexR}, \code{subcortVol},
#'  \code{subcortLabs}, and \code{cifti_info} should not be provided, since the 
#'  data will come from the CIFTI file only.
#' @param cifti_brainstructures If \code{cifti_fname} is provided, use this
#'  argument to select only certain brain structures to read in. This should be
#'  a character vector indicating the brain structure(s): 
#'  \code{"left"} (left cortical surface), \code{"right"} (right 
#'  cortical surface) and/or \code{"subcortical"} (subcortical and cerebellar
#'  gray matter). Can also be \code{"all"} (obtain all three brain structures). 
#'  Default: \code{c("left","right")} (cortical surface only).
#' 
#'  Ignored it \code{cifti_fname} is not provided.
#' @param cortexL,cortexL_mwall \code{cortexL} is a data matrix (V_L vertices x 
#'  T measurements) representing the left cortex. If it's provided, \code{cortexL_mwall}
#'  can be provided too. V_L is equal to either the length of \code{cortexL_mwall} 
#'  (if the left cortex data is unmasked) or the sum of \code{TRUE} values 
#'  (if the left cortex data is masked).
#' 
#'  If \code{cortexL_mwall} is not provided, ...
#' 
#'  Since the unmasked cortices must have the same number of vertices,
#'  \code{cortexL_mwall} and \code{cortexR_mwall} should be of equal length.
#' 
#'  The data for each cortex should only be provided by one argument, so if 
#'  \code{cortexL} is provided, \code{cifti_fname} should not be (and same for
#'  the right cortex).
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
#' 
#'  The data for each cortex should only be provided by one argument, so if 
#'  \code{cortexL} is provided, \code{cifti_fname} should not be (and same for
#'  the right cortex).
#' @param subcortVol,subcortLabs,subcortMask \code{subcortVol} represents the
#'  data values of the subcortical volume. It is either a 3D/4D data array
#'  (i x j x k x T) or a vectorized data matrix (V_S voxels x T measurements). 
#' If it's vectorized, voxels should be in spatial order.
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
#' @param cifti_info (Optional) The result of \code{\link{info_cifti}}. If 
#'  GIFTI and/or NIFTI components from a CIFTI are being provided, 
#'  providing \code{cifti_info} gives metadata information that would otherwise
#'  have to be inferred. 
#' 
#'  This argument is probably not necessary for end users: reading a CIFTI
#'  should be done by providing \code{cifti_fname}, and for reading separate
#'  GIFTI/NIFTI components \code{cifti_info} is not applicable.
#' @param surfL,surfR (Optional) Surface geometries for the left or right cortex. 
#'  Can be a file path for GIFTI data, a GIFTI representing surface geometry 
#'  which has been read in, or a list with components "verts" (V x 3 data matrix 
#'  indicating spatial locations of each vertex) and "faces" (F x 3 data matrix 
#'  indicating the indices of the three vertices defining each triangular face).
#' @param read_dir (Optional) Append a directory to all file names in the
#'  arguments. If \code{NULL} (default), do not modify file names.
#' @param ... Additional arguments to \code{\link{read_cifti}} if 
#'  \code{cifti_fname} was provided.
#'
#' @return A "xifti" object; see \code{\link{template_xifti}}. 
#'
#' @export
#'
make_xifti <- function(
  cifti_fname=NULL,
  cifti_brainstructures=c("left", "right"),
  cortexL=NULL, cortexL_mwall=NULL,
  cortexR=NULL, cortexR_mwall=NULL,
  subcortVol=NULL, subcortLabs=NULL, subcortMask=NULL,
  cifti_info=NULL,
  surfL=NULL, surfR=NULL, 
  read_dir=NULL, ...) {
  
  # Use `read_cifti` if `cifti_fname` was provided.
  if (!is.null(cifti_fname)) {
    if (!all(sapply(list(cortexL, cortexR, subcortVol, subcortLabs, cifti_info), is.null))) {
      warning("`cifti_fname` was provided, so it will be read. separate GIFTI/NIFTI data and `cifti_info` will be ignored.")
    }
    return( read_cifti(cifti_fname, brainstructures=cifti_brainstructures, ...) )
  }
  
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
  if (!is.null(cortexL)) {
    x <- make_xifti_cortex(
      cortexL, cortexL_mwall,
      side="left", mwall_source="the input `cortexL_mwall`"
    )
    xii$data$cortex_left <- x$data
    xii$meta$cortex$medial_wall_mask["left"] <- list(x$mwall)
  }
  if (!is.null(cortexR)) {
    x <- make_xifti_cortex(
      cortexR, cortexR_mwall,
      side="right", mwall_source="the input `cortexR_mwall`"
    )
    xii$data$cortex_right <- x$data
    xii$meta$cortex$medial_wall_mask["right"] <- list(x$mwall)
  }

  # Subcortical data. 
  if (xor(is.null(subcortVol), is.null(subcortLabs))) {
    stop("subcortVol and subcortLabs must be provided together.")
  }
  if (!is.null(subcortVol)) {
    x <- make_xifti_subcort(subcortVol, subcortLabs, subcortMask, validate_mask=FALSE)
    xifti$data$subcort <- x$data
    xifti$meta$subcort$labels <- x$labels
    xifti$meta$subcort$mask <- x$mask
    if (!is.null(cifti_info)) {
      xifti$meta$subcort$trans_mat <- cifti_info$trans_mat
    }
  }

  # CIFTI metadata.
  if (!is.null(cifti_info)) { xifti$meta$cifti <- cifti_info$cifti }

  # Surfaces.
  if (!is.null(surfL)) { xifti$surf$cortex_left <- make_xifti_surface(surfL) }
  if (!is.null(surfR)) { xifti$surf$cortex_right <- make_xifti_surface(surfR) }

  if (!is.xifti(xifti)) { stop("Could not make a valid \"xifti\" object.") }
  structure(xifti, class="xifti")
}