#' Assemble CIFTI components together.
#'
#' @description Assembles the separated left and right cortical
#'  GIfTI data and surface geometry files (\code{\link[gifti]{readgii}}),
#'  as well as the subcortical NIfTI file (\code{\link[RNifti]{readNifti}}).
#'  Objects can be provided as file names, GIFTI/NIFTI objects, or "cifti_[...]"
#'  objects. This is a helper function for \code{\link{make_cifti}}.
#'
#' @param cortexL,cortexR (Optional) [Left/right] cortical data. Can be a file
#'  path for GIFTI data or an object from \code{\link[gifti]{readgii}}.
#' @param subcortVol (Optional) Volumetric subcortical data. Can
#'  be a file path for NIFTI data or an object from
#'  \code{\link[RNifti]{readNifti}}.
#' @param subcortLab (Required if \code{subcortVol} is present) Labels for 
#'  subcortical ROIs. Can be a file path for NIFTI data or an object from
#'  \code{\link[RNifti]{readNifti}}.
#' @param subcortMask (Optional) the volumetric mask for the subcortical data.
#'  If not provided, the mask will be inferred from \code{subcortLab}.
#' @param surfL,surfR (Optional) [Left/right] brain surface model. Can be a file
#'  path for GIFTI data or an object from \code{\link[gifti]{readgii}}.
#' @param read_dir (Optional) Append a directory to all file names in the
#'  arguments. If \code{NULL} (default), do not modify file names.
#'
#' @return A list containing these eight elements:
#'  CORTEX_LEFT, CORTEX_RIGHT, SUBCORT, SUBCORT_LABELS, SUBCORT_MASK, 
#'  SUBCORT_MASK_PADDING, SURF_LEFT and SURF_RIGHT. 
#' 
#'  The subcortical labels will be ordered spatially (not by alphabetical label).
#'
#' @export
#'
make_cifti <- function(
  cortexL=NULL, cortexR=NULL, 
  subcortVol=NULL, subcortLab=NULL, subcortMask=NULL,
  surfL=NULL, surfR=NULL, 
  read_dir=NULL) {
  
  # Add `read_dir` and check file paths.
  if (is.fname(cortexL)) { cortexL <- format_path(cortexL, read_dir, mode=4) }
  if (is.fname(cortexR)) { cortexR <- format_path(cortexR, read_dir, mode=4) }
  if (is.fname(subcortVol)) { subcortVol <- format_path(subcortVol, read_dir, mode=4) }
  if (is.fname(subcortLab)) { subcortLab <- format_path(subcortLab, read_dir, mode=4) }
  if (is.fname(surfL)) { surfL <- format_path(surfL, read_dir, mode=4) }
  if (is.fname(surfR)) { surfR <- format_path(surfR, read_dir, mode=4) }

  # Cortical data.
  if (!is.null(cortexL)) { cortexL <- make_cifti_cortex(cortexL) }
  if (!is.null(cortexR)) { cortexR <- make_cifti_cortex(cortexR) }

  # Subcortical data.
  if (xor(is.null(subcortVol), is.null(subcortLab))) {
    stop("subcortVol and subcortLab must be provided together.")
  }
  if (!is.null(subcortVol)) {
    subcort <- make_cifti_subcort_fromvol(subcortVol, subcortLab, subcortMask)
  } else { 
    subcort <- list(DAT = NULL, LABELS = NULL, MASK = NULL, PADDING = NULL) 
  }

  # Surfaces.
  if (!is.null(surfL)) { surfL <- make_cifti_surface(surfL) }
  if (!is.null(surfR)) { surfR <- make_cifti_surface(surfR) }

  list(
    CORTEX_LEFT = cortexL,
    CORTEX_RIGHT = cortexR,
    SUBCORT = subcort$DAT,
    SUBCORT_LABELS = subcort$LABELS,
    SUBCORT_MASK = subcort$MASK,
    SUBCORT_MASK_PADDING = subcort$MASK_PADDING,
    SURF_LEFT = surfL,
    SURF_RIGHT = surfR
  )
}


#' @rdname make_cifti
#' @export
makeCIfTI <- makecii <- function(
  cortexL=NULL, cortexR=NULL,
  subcortVol=NULL,subcortMask=NULL, subcortLab=NULL,
  surfL=NULL, surfR=NULL,
  read_dir=NULL) {

  make_cifti(
    cortexL, cortexR, 
    subcortVol, subcortMask, subcortLab, 
    surfL, surfR, 
    read_dir
  )
}