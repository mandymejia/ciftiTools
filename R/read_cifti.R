#' Read a CIFTI file
#' 
#' Read a CIFTI file as a \code{"xifti"} object.
#' 
#' First, metadata is obtained with \code{\link{info_cifti}}. Then, if no 
#'  resampling is requested, the \code{-cifti-convert -to-gifti-ext} Workbench 
#'  Command is used to "flatten" the data and save it as a metric GIFTI file, 
#'  which is read in and separated by brainstructure according to the metadata 
#'  (\code{\link{read_cifti_convert}}). Otherwise, if sampling is requested, 
#'  then the CIFTI is separated into its GIFTI and NIFTI components, resampled,
#'  and then re-assembled (\code{\link{read_cifti_separate}}). The former is
#'  much faster for large CIFTI files, so the latter is only used when necessary
#'  for resampling.
#' 
#' If \code{cifti_fname} is not provided, then only the surfaces are read in.
#' 
#' @inheritSection Connectome_Workbench_Description Connectome Workbench Requirement
#' @inheritSection labels_Description Label Levels
#' 
#' @inheritParams cifti_fname_Param
#' @param flat Should the result be flattened into a single matrix? 
#' 
#'  If \code{FALSE} (default), the result will be a \code{"xifti"} object.
#' 
#'  If \code{TRUE}, the result will be a \eqn{T x G} matrix (\eqn{T} 
#'  measurements, \eqn{G} greyordinates not including the medial wall if it's 
#'  excluded from the ROI). All below arguments will be ignored because the 
#'  brain structures cannot be identified. Surfaces will not be appended. 
#'  Resampling is also not possible. \code{flat==TRUE} is the fastest way to 
#'  read in just the CIFTI data. 
#' 
#'  If \code{TRUE}, the greyordinates will be ordered by left cortex, right 
#'  cortex, and then subcortex. Subcortical voxels will be ordered by alphabetical 
#'  label. However, where each brainstructure (and subcortical structure) begins
#'  and ends cannot be determined. The medial wall locations and subcortical 
#'  brain mask are also not included. The data matrix will be identical to that 
#'  created by \code{-cifti-convert -to-gifti-ext}.
#' @inheritParams surfL_fname_Param
#' @inheritParams surfR_fname_Param
#' @inheritParams brainstructures_Param_LR 
#' @param resamp_res Resolution to resample the cortical data and surface to.
#'  Default: \code{NULL} (do not resample). If not \code{NULL}, the data will 
#'  have to be read in with \code{-cifti-separate}, which is slower than 
#'  \code{-cifti-convert -to-gifti-ext}.
#' @param mwall_values If the medial wall locations are not indicated in the
#'  CIFTI, use these values to infer the medial wall mask. Default: 
#'  \code{c(NA, NaN)}. If \code{NULL}, do not attempt to infer the medial wall.
#' @inheritParams verbose_Param_FALSE
#' @param ... Additional arguments to \code{\link{read_cifti_convert}} or 
#'  \code{\link{read_cifti_separate}}.
#'
#' @return If \code{!flat}, a \code{"xifti"} object. Otherwise, a \eqn{T x G} 
#'  matrix (\eqn{T} measurements, \eqn{G} greyordinates). 
#' 
#' @export
#'
read_cifti <- function(
  cifti_fname=NULL, flat=FALSE,
  surfL_fname=NULL, surfR_fname=NULL,
  brainstructures=c("left","right"), 
  resamp_res=NULL,
  mwall_values=c(NA, NaN), verbose=FALSE, ...){

  if (is.null(cifti_fname)) {
    if (is.null(surfL_fname) && is.null(surfR_fname)) {
      warning("`cifti_fname`, `surfL_fname` and `surfR_fname` were all NULL: Nothing to read!\n")
      return(template_xifti())
    } else {
      xifti <- template_xifti()
      xifti <- add_surf(xifti, surfL=surfL_fname, surfR=surfR_fname)
      return(xifti)
    }
  }

  # ----------------------------------------------------------------------------
  # Handle flat method. --------------------------------------------------------
  # ----------------------------------------------------------------------------

  if (flat) {
    if (!is.null(resamp_res)) {
      ciftiTools_warn(paste(
        "Resampling is not possible with the flat method.",
        "Ignoring `resamp_res`.\n"
      ))
    }
    return( read_cifti_flat(cifti_fname) )
  }

  # ----------------------------------------------------------------------------
  # Check arguments. -----------------------------------------------------------
  # ----------------------------------------------------------------------------

  require_separate_method <- !is.null(resamp_res)

  brainstructures <- match_input(
    brainstructures, c("left","right","subcortical","all"),
    user_value_label="brainstructures"
  )
  if ("all" %in% brainstructures) { 
    brainstructures <- c("left","right","subcortical")
  }

  # ----------------------------------------------------------------------------
  # Read. ----------------------------------------------------------------------
  # ----------------------------------------------------------------------------

  if (!require_separate_method) {
    return(read_cifti_convert(
      cifti_fname,
      surfL_fname=surfL_fname, surfR_fname=surfR_fname,
      brainstructures=brainstructures, 
      mwall_values=mwall_values, verbose=verbose,
      ...
    ))

  } else {
    return(read_cifti_separate(
      cifti_fname,
      surfL_fname=surfL_fname, surfR_fname=surfR_fname,
      brainstructures=brainstructures, 
      resamp_res=resamp_res, 
      mwall_values=mwall_values, verbose=verbose,
      ...
    ))
  }
}

#' @rdname read_cifti
#' @export
readCIfTI <- function(
  cifti_fname, flat=FALSE,
  surfL_fname=NULL, surfR_fname=NULL,
  brainstructures=c("left","right"), 
  resamp_res=NULL,
  mwall_values=c(NA, NaN), verbose=FALSE, ...){

  read_cifti(
    cifti_fname=cifti_fname, flat=flat,
    surfL_fname=surfL_fname, surfR_fname=surfR_fname,
    brainstructures=brainstructures, 
    resamp_res=resamp_res, mwall_values=mwall_values,
    verbose=verbose, ...
  )
}

#' @rdname read_cifti
#' @export
readcii <- function(
  cifti_fname, flat=FALSE,
  surfL_fname=NULL, surfR_fname=NULL,
  brainstructures=c("left","right"), 
  resamp_res=NULL,
  mwall_values=c(NA, NaN), verbose=FALSE, ...){

  read_cifti(
    cifti_fname=cifti_fname, flat=flat,
    surfL_fname=surfL_fname, surfR_fname=surfR_fname,
    brainstructures=brainstructures, 
    resamp_res=resamp_res, mwall_values=mwall_values,
    verbose=verbose, ...
  )
}

#' @rdname read_cifti
#' @export
read_xifti <- function(
  cifti_fname, flat=FALSE,
  surfL_fname=NULL, surfR_fname=NULL,
  brainstructures=c("left","right"), 
  resamp_res=NULL,
  mwall_values=c(NA, NaN),
  verbose=FALSE, ...){

  read_cifti(
    cifti_fname=cifti_fname, flat=flat,
    surfL_fname=surfL_fname, surfR_fname=surfR_fname,
    brainstructures=brainstructures, 
    resamp_res=resamp_res, mwall_values=mwall_values,
    verbose=verbose, ...
  )
}