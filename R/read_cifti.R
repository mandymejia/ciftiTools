#' Read a CIFTI file
#' 
#' Read a CIFTI file as a "xifti" object (see \code{\link{is.xifti}}).
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
#' @inheritParams cifti_fname_Param
#' @param flat Should the result be flattened into a single matrix? If \code{TRUE},
#'  the result will be a $T$ x $B$ matrix ($T$ measurements, $B$ brainordinates 
#'  not including the medial wall). All below arguments will be ignored, because 
#'  the brain  structures cannot be identified and surfaces will not be appended. 
#'  Resampling is also not possible.
#' 
#'  This is the fastest way to read in just the CIFTI data. 
#' 
#'  The brainordinates will be ordered by left cortex, right cortex, and then 
#'  subcortical, but where each brainstructure begins, as well as which 
#'  structures each brainordinate represents, cannot be determined. The
#'  medial wall vertices and subcortical brain mask are also not included. The 
#'  data matrix will be identical to that created by 
#'  \code{-cifti-convert -to-gifti-ext}.
#' 
#'  If \code{FALSE}, the result will be a "xifti" object.
#' @inheritParams surfL_fname_Param
#' @inheritParams surfR_fname_Param
#' @inheritParams brainstructures_Param_LR 
#' @param resamp_res Resolution to resample the cortical data and surface to.
#'  Default: \code{NULL} (do not resample). If not \code{NULL}, the data will 
#'  have to be read in with \code{-cifti-separate}, which is slower than 
#'  \code{-cifti-convert -to-gifti-ext}.
#' @inheritParams wb_path_Param
#' @inheritParams verbose_Param_FALSE
#' @param ... Additional arguments to \code{\link{read_cifti_convert}} or 
#'  \code{\link{read_cifti_separate}}.
#'
#' @return If \code{!flat}, a \code{"xifti"} object. Otherwise, a $T$ x $B$ 
#'  matrix ($T$ measurements, $B$ brainordinates). 
#' 
#' @inheritSection Connectome_Workbench_Description Connectome Workbench Requirement
#' @inheritSection labels_Description Label Levels
#' 
#' @export
#'
read_cifti <- function(
  cifti_fname, flat=FALSE,
  surfL_fname=NULL, surfR_fname=NULL,
  brainstructures=c("left","right"), 
  resamp_res=NULL,
  wb_path=NULL, verbose=FALSE, ...){

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
    return( read_cifti_flat(cifti_fname, wb_path=wb_path) )
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
      wb_path=wb_path, verbose=verbose,
      ...
    ))

  } else {
    return(read_cifti_separate(
      cifti_fname,
      surfL_fname=surfL_fname, surfR_fname=surfR_fname,
      brainstructures=brainstructures, 
      resamp_res=resamp_res,
      wb_path=wb_path, verbose=verbose,
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
  wb_path=NULL, verbose=FALSE, ...){

  read_cifti(
    cifti_fname, flat,
    surfL_fname, surfR_fname,
    brainstructures, 
    resamp_res,
    wb_path, verbose, ...
  )
}

#' @rdname read_cifti
#' @export
readcii <- function(
  cifti_fname, flat=FALSE,
  surfL_fname=NULL, surfR_fname=NULL,
  brainstructures=c("left","right"), 
  resamp_res=NULL,
  wb_path=NULL, verbose=FALSE, ...){

  read_cifti(
    cifti_fname, flat,
    surfL_fname, surfR_fname,
    brainstructures, 
    resamp_res,
    wb_path, verbose, ...
  )
}

#' @rdname read_cifti
#' @export
read_xifti <- function(
  cifti_fname, flat=FALSE,
  surfL_fname=NULL, surfR_fname=NULL,
  brainstructures=c("left","right"), 
  resamp_res=NULL,
  wb_path=NULL, verbose=FALSE, ...){

  read_cifti(
    cifti_fname, flat,
    surfL_fname, surfR_fname,
    brainstructures, 
    resamp_res,
    wb_path, verbose, ...
  )
}