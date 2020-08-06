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
#'  \code{subcortLab}, and \code{cifti_map} should not be provided, since the 
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
#' @param cortexL,cortexR (Optional) [Left/right] cortical data. Can be a file
#'  path to a GIFTI file, or an object from \code{\link[gifti]{readgii}}.
#' 
#'  The data for each cortex should only be provided by one argument, so if 
#'  \code{cortexL} is provided, \code{cifti_fname} should not be (and same for
#'  the right cortex).
#' @param subcortVol (Optional) Volumetric subcortical data. Can
#'  be a file path for NIFTI data or an object from
#'  \code{\link[RNifti]{readNifti}}.
#' 
#'  The subcortical data should only be provided by one argument, so if 
#'  \code{subcortVol} is provided, \code{cifti_fname} should not be.
#' @param subcortLab (Required if \code{subcortVol} is present) Labels for 
#'  subcortical ROIs. Can be a file path for NIFTI data or an object from
#'  \code{\link[RNifti]{readNifti}}.
#' @param cifti_map (Optional) The result of \code{\link{map_cifti}}. If 
#'  GIFTI and/or NIFTI components from a CIFTI are being provided, 
#'  providing \code{cifti_map} gives metadata information that would otherwise
#'  have to be inferred. 
#' 
#'  This argument is probably not necessary for end users: reading a CIFTI
#'  should be done by providing \code{cifti_fname}, and for reading separate
#'  GIFTI/NIFTI components \code{cifti_map} is not applicable.
#' @param surfL,surfR (Optional) [Left/right] brain surface model. Can be a file
#'  path for GIFTI data or an object from \code{\link[gifti]{readgii}}.
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
  cortexL=NULL, cortexR=NULL, 
  subcortVol=NULL, subcortLab=NULL,
  cifti_map=NULL,
  surfL=NULL, surfR=NULL, 
  read_dir=NULL, ...) {
  
  # Use `read_cifti` if `cifti_fname` was provided.
  if (!is.null(cifti_fname)) {
    if (!all(sapply(is.null, list(cortexL, cortexR, subcortVol, subcortLab, cifti_map)))) {
      warning("`cifti_fname` was provided, so separate GIFTI/NIFTI data and `cifti_map` will be ignored.")
    }
    return( read_cifti(cifti_fname, brainstructures=cifti_brainstructures, ...) )
  }
  
  # Add `read_dir` and check file paths.
  if (is.fname(cortexL)) { cortexL <- format_path(cortexL, read_dir, mode=4) }
  if (is.fname(cortexR)) { cortexR <- format_path(cortexR, read_dir, mode=4) }
  if (is.fname(subcortVol)) { subcortVol <- format_path(subcortVol, read_dir, mode=4) }
  if (is.fname(subcortLab)) { subcortLab <- format_path(subcortLab, read_dir, mode=4) }
  if (is.fname(surfL)) { surfL <- format_path(surfL, read_dir, mode=4) }
  if (is.fname(surfR)) { surfR <- format_path(surfR, read_dir, mode=4) }

  # Template.
  xifti <- template_xifti()

  # Cortical data.
  if (!is.null(cortexL)) { 
    if (!is.null(cifti_map)) {
      cortexL <- make_xifti_cortex(cortex, cifti_map$cortex$medial_wall_mask$left)
    } else {
      cortexL <- make_xifti_cortex(cortex, TRUE)
    }
    xifti$data$cortex_left <- cortexL$data
    xifti$meta$cortex$medial_wall_mask$left <- cortexL$medial_wall_mask
  }
  if (!is.null(cortexR)) { 
    if (!is.null(cifti_map)) {
      cortexR <- make_xifti_cortex(cortex, cifti_map$cortex$medial_wall_mask$right)
    } else {
      cortexR <- make_xifti_cortex(cortex, TRUE)
    }
    xifti$data$cortex_right <- cortexR$data
    xifti$meta$cortex$medial_wall_mask$right <- cortexR$medial_wall_mask
  }

  # Subcortical data. 
  if (xor(is.null(subcortVol), is.null(subcortLab))) {
    stop("subcortVol and subcortLab must be provided together.")
  }
  if (!is.null(subcortVol)) {
    if (!is.null(cifti_map)) {
      # [TO DO]: support for cifti_map$subcort$mask
      subcort <- make_xifti_subcort(
        subcortVol, subcortLab, validate_mask=TRUE
      )
    } else {
      subcort <- make_xifti_subcort(
        subcortVol, subcortLab, validate_mask=TRUE
      )
    }
    xifti$data$subcort <- subcort$data
    xifti$meta$subcort$labels <- subcort$labels
    xifti$meta$subcort$mask <- subcort$mask
    xifti$meta$subcort$mask_padding <- subcort$mask_padding
  }

  # Surfaces.
  if (!is.null(surfL)) { xifti$surf$left_cortex <- make_xifti_surface(surfL) }
  if (!is.null(surfR)) { xifti$surf$right_cortex <- make_xifti_surface(surfR) }

  if (!check_xifti(xifti)) { stop("Could not make a valid \"xifti\" object.") }
  structure(xifti, class="xifti")
}


#' @rdname make_xifti
#' @export
makeXIfTI <- makexii <- function(
  cifti_fname=NULL,
  cifti_brainstructures=c("left", "right"),
  cortexL=NULL, cortexR=NULL, 
  subcortVol=NULL, subcortLab=NULL,
  cifti_map=NULL,
  surfL=NULL, surfR=NULL, 
  read_dir=NULL, ...) {

  make_xifti(
    cifti_fname=NULL,
    cifti_brainstructures=c("left", "right"),
    cortexL=NULL, cortexR=NULL, 
    subcortVol=NULL, subcortLab=NULL,
    cifti_map=NULL,
    surfL=NULL, surfR=NULL, 
    read_dir=NULL, ...
  )
}