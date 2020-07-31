#' Assemble CIFTI components together.
#'
#' @description Assembles the separated left and right cortical
#'  GIfTI data and surface geometry files (\code{\link[gifti]{readgii}}),
#'  as well as the subcortical NIfTI file (\code{\link[RNifti]{readNifti}}).
#'  Objects can be provided as file names, GIFTI/NIFTI objects, or "cifti_[...]"
#'  objects. This is a helper function for \code{\link{make_cifti}}.
#'
#' @param cifti_map The CIFTI mapping: \code{map_cifti(cifti_fname)}.
#' @param cortexL,cortexR (Optional) [Left/right] cortical data. Can be a file
#'  path for GIFTI data or an object from \code{\link[gifti]{readgii}}.
#' @param subcortVol (Optional) Volumetric subcortical data. Can
#'  be a file path for NIFTI data or an object from
#'  \code{\link[RNifti]{readNifti}}.
#' @param subcortLab (Required if \code{subcortVol} is present) Labels for 
#'  subcortical ROIs. Can be a file path for NIFTI data or an object from
#'  \code{\link[RNifti]{readNifti}}.
#' @param surfL,surfR (Optional) [Left/right] brain surface model. Can be a file
#'  path for GIFTI data or an object from \code{\link[gifti]{readgii}}.
#' @param flat Flatten the CIFTI?
#' @param read_dir (Optional) Append a directory to all file names in the
#'  arguments. If \code{NULL} (default), do not modify file names.
#'
#' @return The output will be a "cifti" object if \code{!flat} and a 
#'  "cifti_flat" object if \code{flat}. 
#' 
#'  If \code{cifti_map} was provided the medial wall locations and subcortical 
#'  mask will be inferred from constant 0/NA values and 0-valued labels, 
#'  respectively. This should work but is prone to error, especially if there
#'  is only one or a few observations per brainordinate.
#'
#' @export
#'
make_cifti <- function(
  cifti_map=NULL,
  cortexL=NULL, cortexR=NULL, 
  subcortVol=NULL, subcortLab=NULL,
  surfL=NULL, surfR=NULL, 
  flat=FALSE,
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
    if (!is.null(cifti_map)) {
      subcort <- make_cifti_subcort_fromvol(
        subcortVol, subcortLab, 
        cifti_map$SUBCORT_MASK, validate_mask=FALSE
      )
    } else {
      subcort <- make_cifti_subcort_fromvol(
        subcortVol, subcortLab, 
        validate_mask=TRUE
      )
    }
  } else { 
    subcort <- list(DAT = NULL, LABELS = NULL, MASK = NULL, PADDING = NULL) 
    subcort$PADDING <- list(
      i = c(NA, NA), 
      j = c(NA, NA), 
      k = c(NA, NA)
    )
  }

  # Surfaces.
  if (!is.null(surfL)) { surfL <- make_cifti_surface(surfL) }
  if (!is.null(surfR)) { surfR <- make_cifti_surface(surfR) }

  # Labels.
  labs <- list(
    CORTEX_LEFT=NULL,
    CORTEX_RIGHT=NULL,
    SUBCORT=NULL
  )
  bs_names <- c("CORTEX_LEFT", "CORTEX_RIGHT", "SUBCORT")
  bs_list <- list(cortexL, cortexR, subcort$DAT)
  names(bs_list) <- bs_names
  if (!is.null(cifti_map)) {
    for (bs_name in bs_names) {
      if(!is.null(bs_list[[bs_name]])){ 
        labs[[bs_name]] <- cifti_map$LABELS[cifti_map$LABELS$BRAINSTRUCTURE==bs_name,"SUBSTRUCTURE"]
      }
    }
    
    # Set empty to NULL and check match.
    if (!is.null(subcortVol)) {
      stopifnot(all.equal(as.numeric(labs$SUBCORT), as.numeric(subcort$LABELS)))
    }

  } else {
    warning("No cifti map was provided, so inferring the medial wall.")
    labs <- list(
      CORTEX_LEFT = factor(
        ifelse(apply(cortexL==0 | is.na(cortexL), 1, all), "Medial Wall", "Cortex-L"),
        levels=substructure_table()$ciftiTools_Name
      ),
      CORTEX_RIGHT = factor(
        ifelse(apply(cortexR==0 | is.na(cortexR), 1, all), "Medial Wall", "Cortex-R"),
        levels=substructure_table()$ciftiTools_Name
      ),
      SUBCORT = subcort$LABELS
    )
  }

  # Apply label mask to cortex data.
  cortexL <- cortexL[labs$CORTEX_LEFT != "Medial Wall",, drop=FALSE]
  cortexR <- cortexR[labs$CORTEX_RIGHT != "Medial Wall",, drop=FALSE]

  cifti <- list(
    CORTEX_LEFT = cortexL,
    CORTEX_RIGHT = cortexR,
    SUBCORT = subcort$DAT,
    LABELS = labs,
    SURF_LEFT = surfL,
    SURF_RIGHT = surfR,
    META = list(
      CORTEX_RESOLUTION = NULL,
      SUBCORT_MASK = subcort$MASK,
      SUBCORT_MASK_PADDING = subcort$PADDING
    )
  )

  #if (!is.cifti(cifti)) { stop("Could not make a valid \"cifti\" object.") }
  class(cifti) <- "cifti"
  cifti
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