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

  # Template.
  cifti <- list(
    CORTEX_LEFT = NULL,
    CORTEX_RIGHT = NULL,
    SUBCORT = NULL
  )

  # Cortical data.
  if (!is.null(cortexL)) { cifti$CORTEX_LEFT <- make_cifti_cortex(cortexL) }
  if (!is.null(cortexR)) { cifti$CORTEX_RIGHT <- make_cifti_cortex(cortexR) }

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
    cifti$SUBCORT <- subcort$DAT
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
  bs_names <- names(cifti)
  # For each present brainstructure...
  for (bs_name in bs_names) {
    if(!is.null(cifti[[bs_name]])){ 
      # Use the cifti_map if possible.
      if (!is.null(cifti_map)) {
        bs_count <- sum(cifti_map$LABELS$BRAINSTRUCTURE==bs_name)
        if (bs_count == nrow(cifti[[bs_name]])) {
          labs[[bs_name]] <- cifti_map$LABELS[cifti_map$LABELS$BRAINSTRUCTURE==bs_name,"SUBSTRUCTURE"]
        } else {
          # Otherwise, inform the user about a discrepency 
          # between the cifti_map and the GIFTI/NIFTI component
          # if the cifti_map was provided...
          warning(paste0(
            "The brainordinate count for ", bs_name, " from the GIFTI/NIFTI (",
            nrow(cifti[[bs_name]]), ") did not match that which was inferred ",
            "from the CIFTI mapping metadata (", bs_count, ").\n",
            "The medial wall will have to be inferred, and the results may not ",
            "match if read_cifti_flat() was used.\n"
          ))
        }
      }
      if (is.null(labs[[bs_name]])) {
        # And infer the medial wall.
        if (bs_name=="CORTEX_LEFT") {
          bs_labs <- ifelse(
            apply(cifti[[bs_name]]==0 | is.na(cifti[[bs_name]]), 1, all), 
            "Medial Wall", "Cortex-L"
          )
        } else if(bs_name=="CORTEX_RIGHT") {
          bs_labs <- ifelse(
            apply(cifti[[bs_name]]==0 | is.na(cifti[[bs_name]]), 1, all), 
            "Medial Wall", "Cortex-R"
          )
        } else {
          bs_labs <- subcort$LABELS
        }
        labs[[bs_name]] <- factor(bs_labs, levels=substructure_table()$ciftiTools_Name)
      }
      cifti[[bs_name]] <- cifti[[bs_name]][labs[[bs_name]] != "Medial Wall",, drop=FALSE]
    }
  }

  cifti <- c(
    cifti,
    list(
      LABELS = labs,
      SURF_LEFT = surfL,
      SURF_RIGHT = surfR,
      META = list(
        CORTEX_RESOLUTION = NULL,
        SUBCORT_MASK = subcort$MASK,
        SUBCORT_MASK_PADDING = subcort$PADDING
      )
    )
  )

  #if (!is.cifti(cifti)) { stop("Could not make a valid \"cifti\" object.") }
  class(cifti) <- "cifti"
  cifti
}


#' @rdname make_cifti
#' @export
makeCIfTI <- makecii <- function(
  cifti_map=NULL,
  cortexL=NULL, cortexR=NULL,
  subcortVol=NULL, subcortLab=NULL,
  surfL=NULL, surfR=NULL,
  read_dir=NULL) {

  make_cifti(
    cifti_map,
    cortexL, cortexR, 
    subcortVol, subcortLab, 
    surfL, surfR, 
    read_dir
  )
}