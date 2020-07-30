#' Read in CIFTI data
#'
#' @description Read a CIFTI file, either by exporting it as a single GIFTI 
#'  using the \code{-cifti-convert -to-gifti-ext} Workbench Command, or by 
#'  separating it into multiple GIFTIs (cortical surfaces) and a NIFTI 
#'  (subcortical data) using the \code{-cifti-separate} Workbench Command. The 
#'  former method is much faster and is the default, but the latter method is 
#'  necessary for resampling, obtaining the NIFTI volume without cropping, or 
#'  working with the ROIs.
#' 
#'  Metadata is obtained with \code{-cifti-export-dense-mapping}; see
#'  \code{\link{map_cifti}}.
#'
#' @inheritParams cifti_fname_Param
#' @param format One of \code{"regular"}, \code{"flat"}, or \code{"minimal"}.
#' 
#'  If \code{"regular"}, the result will be a "cifti" object. See 
#'  \code{\link{is.cifti}} with \code{flat==FALSE}.
#' 
#'  If \code{"flat"}, the result will be a "cifti_flat" object. See 
#'  \code{\link{is.cifti}} with \code{flat==TRUE}. If 
#'  \code{brainstructures=="all"}, the "DAT" component will be identical to the
#'  data matrix created by \code{-cifti-convert -to-gifti-ext}.
#' 
#'  If \code{"minimal"}, the result will be a T x B matrix (T measurements, B 
#'  non-empty brainordinates). The \code{method} and \code{brainstructures} 
#'  arguments will be ignored, because the \code{"convert"} method is required
#'  and all brainstructures will be obtained. This is the fastest way to read 
#'  in CIFTI data. 
#'  The brainordinates will be ordered by left cortex, right cortex, and then 
#'  subcortical, but where each brainstructure begins, as well as which 
#'  substructure each brainordinate represents, cannot be determined. The
#'  medial wall vertices and subcortical brain mask are also not included. The 
#'  data matrix will be identical to that created by 
#'  \code{-cifti-convert -to-gifti-ext}.
#' 
#' @inheritParams surfL_fname_Param
#' @inheritParams surfR_fname_Param
#' @inheritParams brainstructures_Param_LR
#' @param method Either \code{"convert"} (default) or \code{"separate"}. Only 
#'  applies if \code{format=="regular"} or \code{format=="flat"}.
#' 
#'  If \code{"convert"}, the CIFTI will be read in using the 
#'  \code{-cifti-convert -to-gifti-ext} Workbench Command. If \code{"separate"},
#'  it will be read in using the \code{-cifti-separate} Workbench Command. 
#'  The former is much faster, but the latter is necessary for resampling, 
#'  obtaining the full NIFTI volume (without cropping empty edge slices), or 
#'  working with the ROIs. Aside from those limitations the results are 
#'  identical, so the more efficient method \code{"convert"} is the default.
#' @inheritParams wb_path_Param
#' @param ... Additional arguments to \code{read_cifti_minimal} if 
#'  \code{method=="convert"}, or \code{read_cifti_separate} if
#'  \code{method=="separate"}.
#'
#' @return If \code{format=="regular"}, a \code{"cifti"} object. If 
#'  \code{format=="flat"}, a \code{"cifti_flat"} object. If 
#'  \code{format=="minimal"}, a T x B matrix (T measurements, B non-empty 
#'  brainordinates). 
#' 
#'  See \code{\link{is.cifti}} for details about the \code{"cifti"} and
#'  \code{"cifti_flat"} objects.
#' @export
#'
#' @details This function uses a system wrapper for the "wb_command"
#'  executable. The user must first download and install the Connectome
#'  Workbench, available from
#'  \url{https://www.humanconnectome.org/software/get-connectome-workbench}.
#'  The \code{wb_path} argument is the path to the Connectime Workbench folder
#'  or executable.
#'
read_cifti <- function(
  cifti_fname, format=c("regular", "flat", "minimal"),
  surfL_fname=NULL, surfR_fname=NULL,
  brainstructures=c("left","right"), 
  method=c("convert", "separate"),
  wb_path=NULL, ...){

  stop("Doesn't work.")

  format <- match.arg(format, c("regular", "flat", "minimal"))
  if (format=="minimal") { method="convert" }
  method <- match.arg(method, c("convert", "separate"))

  brainstructures <- match_input(
    brainstructures, c("left","right","subcortical","all"),
    user_value_label="brainstructures"
  )
  if ("all" %in% brainstructures) { 
    brainstructures <- c("left","right","subcortical")
  }
  do_left <- "left" %in% brainstructures
  do_right <- "right" %in% brainstructures
  do_sub <- "subcortical" %in% brainstructures

  dots <- list(...)
  if ("resamp_res" %in% dots && method == "convert") {
    stop( "To resample, set `method= \"separate\"`." )
  }

  # ----------------------------------------------------------------------------
  # If `format=="minimal"`, just get the data and finish. ----------------------
  # Otherwise, create the template. --------------------------------------------
  # ----------------------------------------------------------------------------

  if (format=="minimal") {
    return( read_cifti_minimal(cifti_fname, wb_path=wb_path) )
  } else if (format=="regular") {
    stop("Doesn't work.")
  } else if (format=="flat") {
    return(read_cifti_flat(
      cifti_fname, 
      surfL_fname=surfL_fname, surfR_fname=surfR_fname, 
      brainstructures=brainstructures, 
      wb_path=wb_path
    ))
  } else { stop() }


  #   if (format == "regular") {
  #     flat_idx <- cifti$LABELS$SUBSTRUCTURE != "MEDIAL_WALL"
  #     left_idx <- flat_idx & cifti$LABELS$BRAINSTRUCTURE == "CORTEX_LEFT"
  #     right_idx <- flat_idx & cifti$LABELS$BRAINSTRUCTURE == "CORTEX_RIGHT"
  #     sub_idx <- flat_idx & cifti$LABELS$BRAINSTRUCTURE == "SUBCORT"
  #     if (do_left) {
  #       cifti$CORTEX_LEFT <- cifti_dat[left_idx,]
  #     }
  #     if (do_right) {
  #       cifti$CORTEX_RIGHT <- cifti_dat[right_idx,]
  #     }
  #     if (do_subcort) {
  #       cifti$SUBCORT <- cifti_dat[sub_idx,]
  #     }

  #   } else if (format == "flat") {
  #     cifti$DAT <- cifti_dat[cifti$LABELS$BRAINSTRUCTURE %in% brainstructures,]
      
  #   } else { stop() }

  #   if (!is.null(surfL)) { cifti$SURF_LEFT <- make_cifti_surface(surfL) }
  #   if (!is.null(surfR)) { cifti$SURF_RIGHT <- make_cifti_surface(surfR) }

  # } else if (method == "separate") {
  #   cifti_components <- read_cifti_separate(
  #     cifti_fname,
  #     surfL_fname=NULL, surfR_fname=NULL,
  #     map=cifti_map,
  #     brainstructures=brainstructures,
  #     wb_path=wb_path, ...
  #   )

  #   if (format == "regular") {
  #     cifti$CORTEX_LEFT <- cifti_components$CORTEX_LEFT
  #     cifti$CORTEX_RIGHT <- cifti_components$CORTEX_RIGHT
  #     cifti$SUBCORT <- cifti_components$SUBCORT

  #   } else if (format=="flat") {
  #     cifti$DAT <- rbind(
  #       cifti_components$CORTEX_LEFT,
  #       cifti_components$CORTEX_RIGHT,
  #       cifti_components$SUBCORT
  #     )[cifti$LABELS$SUBSTRUCTURE != "MEDIAL_WALL",]

  #   } else { stop() }

  #   stopifnot(identical(cifti$LABELS$SUBCORT, cifti_components$SUBCORT_LABELS))
  #   stopifnot(identical(cifti$META$SUBCORT_MASK, cifti_components$SUBCORT_MASK))
  #   cifti$META$SUBCORT_MASK_PADDING <- cifti_components$SUBCORT_MASK_PADDING
  #   cifti$SURF_LEFT <- cifti_components$SURF_LEFT
  #   cifti$SURF_RIGHT <- cifti_components$SURF_RIGHT

  # } else { stop() }

  # cifti
}

# #' @rdname read_cifti
# #' @export
# readCIfTI <- readcii <- function(
#   cifti_fname, format=c("regular", "flat", "minimal"),
#   surfL_fname=NULL, surfR_fname=NULL,
#   brainstructures=c("left","right"), 
#   method=c("convert", "separate"),
#   wb_path=NULL, ...){

#   read_cifti(
#     cifti_fname, format,
#     surfL_fname, surfR_fname,
#     brainstructures, 
#     method,
#     wb_path, ...
#   )
# }