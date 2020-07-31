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
#'  If \code{"regular"}, the result will be a \code{"cifti"} object. See 
#'  \code{\link{is.cifti}} with \code{flat==FALSE}. 
#' 
#'  If \code{"flat"}, the result will be a \code{"cifti_flat"} object. See 
#'  \code{\link{is.cifti}} with \code{flat==TRUE}. If 
#'  \code{brainstructures=="all"}, the "DAT" component will be identical to the
#'  data matrix created by \code{-cifti-convert -to-gifti-ext}, except the 
#'  order of the subcortical voxels will be spatial instead of alphabetical.
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
#'  The former is faster and memory-efficient if the subcortical data are
#'  requested, but the latter is necessary for resampling, 
#'  obtaining the full NIFTI volume (without cropping empty edge slices), or 
#'  working with the ROIs. Aside from those limitations the results are 
#'  identical, so the more efficient method \code{"convert"} is the default.
#' @param resamp_res Resolution to resample the cortical data and surface to.
#'  Resampling is only possible if \code{method=="separate"}. Spheres in the
#'  original resolution are required (\code{sphereL_fname} and 
#'  \code{sphereR_fname}).
#' @inheritParams sphereL_fname_Param
#' @inheritParams sphereR_fname_Param
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
  resamp_res=NULL, sphereL_fname=NULL, sphereR_fname=NULL,
  wb_path=NULL, 
  ...){

  # ----------------------------------------------------------------------------
  # Check arguments. -----------------------------------------------------------
  # ----------------------------------------------------------------------------

  format <- match.arg(format, c("regular", "flat", "minimal"))
  if (format == "minimal") { method = "convert" }
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
  if (!is.null(resamp_res) && method == "convert") {
    stop( "To resample, set `method= \"separate\"`." )
  }

  # ----------------------------------------------------------------------------
  # Read. ----------------------------------------------------------------------
  # ----------------------------------------------------------------------------

  if (format=="minimal") {
    cifti = read_cifti_minimal(
      cifti_fname, wb_path=wb_path
    )

  } else if (method=="convert") {
    cifti = read_cifti_separate(
      cifti_fname,
      surfL_fname=surfL_fname, surfR_fname=surfR_fname,
      brainstructures=brainstructures,
      resamp_res=resamp_res, 
      sphereL_fname=sphereL_fname, sphereR_fname=sphereR_fname,
      wb_path=wb_path,
      ...
    )
    if (format=="regular") {
      cifti <- unflatten_cifti(cifti)
    }

  } else if (method=="separate") {
    cifti = read_cifti_flat(
      cifti_fname, 
      surfL_fname=surfL_fname, surfR_fname=surfR_fname, 
      brainstructures=brainstructures, 
      wb_path=wb_path,
      ...
    )
    if (format=="minimal") {
      cifti <- flatten_cifti(cifti)
    }

  } else { stop() }

  cifti
}

#' @rdname read_cifti
#' @export
readCIfTI <- readcii <- function(
  cifti_fname, format=c("regular", "flat", "minimal"),
  surfL_fname=NULL, surfR_fname=NULL,
  brainstructures=c("left","right"), 
  method=c("convert", "separate"),
  resamp_res=NULL, sphereL_fname=NULL, sphereR_fname=NULL,
  wb_path=NULL, ...){

  read_cifti(
    cifti_fname, format,
    surfL_fname, surfR_fname,
    brainstructures, 
    method,
    wb_path, ...
  )
}