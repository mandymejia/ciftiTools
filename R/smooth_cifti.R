#' Smooth a CIFTI 
#'
#' Smooth CIFTI data. This uses the \code{-cifti-smoothing} command 
#'  from Connectome Workbench.
#' 
#' If the CIFTI is a ".dlabel" file (intent 3007), then it will be converted
#'  to a ".dscalar" file because the values will no longer be integer indices.
#'  Unless the label values were ordinal, this is probably not desired so a
#'  warning will be printed.
#' 
#' @inheritSection Connectome_Workbench_Description Connectome Workbench Requirement
#' 
#' @param cifti_original_fname The CIFTI file to smooth.
#' @param cifti_target_fname The file name to save the smoothed CIFTI.
#' @param surface_sigma The sigma for the gaussian surface smoothing kernel, in mm
#' @param volume_sigma The sigma for the gaussian volume smoothing kernel, in mm
#' @param surfL_fname,surfR_fname (Required if the 
#'  corresponding cortex is present) Surface GIFTI files for the left and right
#'  cortical surface
#' @param cerebellum_fname (Optional) Surface GIFTI file for the 
#'  cerebellar surface
#' @param subcortical_zeroes_as_NA,cortical_zeroes_as_NA Should zero-values in 
#'  the subcortical volume or cortex be treated as NA? Default: \code{FALSE}.
#' @param subcortical_merged Smooth across subcortical structure boundaries?
#'  Default: \code{FALSE}.
#' @inheritParams wb_path_Param
#'
#' @return The \code{cifti_target_fname}, invisibly
#' 
#' @export
#'
smooth_cifti <- function(
  cifti_original_fname, cifti_target_fname,
  surface_sigma, volume_sigma,
  surfL_fname=NULL, surfR_fname=NULL, cerebellum_fname=NULL,
  subcortical_zeroes_as_NA=FALSE, cortical_zeroes_as_NA=FALSE,
  subcortical_merged=FALSE,
  wb_path=NULL){

  stopifnot(file.exists(cifti_original_fname))
  cifti_info <- info_cifti(cifti_original_fname)
  fix_dlabel <- FALSE
  if (!is.null(cifti_info$cifti$intent)) {
    if (cifti_info$cifti$intent == 3007) {
      warning(paste(
        "Smoothing a label file will convert the labels to their numeric",
        "indices. Coercing `cifti_target_fname` to a \".dscalar\" file.\n"
      ))
      fix_dlabel <- TRUE
    }
  }

  # Build the Connectome Workbench command. 
  cmd <- paste(
    "-cifti-smoothing", 
    sys_path(cifti_original_fname), 
    surface_sigma,
    volume_sigma,
    "COLUMN",
    sys_path(cifti_target_fname)
  )

  if (!is.null(surfL_fname)) { cmd <- paste(cmd, "-left-surface", sys_path(surfL_fname)) }  
  if (!is.null(surfR_fname)) { cmd <- paste(cmd, "-right-surface", sys_path(surfR_fname)) }  
  if (!is.null(cerebellum_fname)) { cmd <- paste(cmd, "-cerebellum-surface", sys_path(cerebellum_fname)) }  

  if (subcortical_zeroes_as_NA) { cmd <- paste(cmd, "-fix-zeros-volume") }
  if (cortical_zeroes_as_NA) { cmd <- paste(cmd, "-fix-zeros-surface") }

  if (subcortical_merged) { cmd <- paste(cmd, "-merged-volume") }

  run_wb_cmd(cmd, wb_path)

  if (fix_dlabel) {
    old_target_fname <- cifti_target_fname
    cifti_target_fname <- gsub("dlabel", "dscalar", old_target_fname)
    names_fname <- tempfile()
    cat(names(cifti_info$cifti$labels), file = names_fname, sep = "\n")
    run_wb_cmd(
      paste(
        "-cifti-change-mapping", old_target_fname, 
        "ROW", cifti_target_fname,
        "-scalar", "-name-file", names_fname
      ),
      wb_path
    )
  }
  
  invisible(cifti_target_fname)
}

#' @rdname smooth_cifti
#' @export
smoothCIfTI <- function(
  cifti_original_fname, cifti_target_fname,
  surface_sigma, volume_sigma,
  surfL_fname=NULL, surfR_fname=NULL, cerebellum_fname=NULL,
  subcortical_zeroes_as_NA=FALSE, cortical_zeroes_as_NA=FALSE,
  subcortical_merged=FALSE,
  wb_path=NULL){

  smooth_cifti(
    cifti_original_fname=cifti_original_fname, cifti_target_fname=cifti_target_fname,
    surface_sigma=surface_sigma, volume_sigma=volume_sigma,
    surfL_fname=surfL_fname, surfR_fname=surfR_fname, cerebellum_fname=cerebellum_fname,
    subcortical_zeroes_as_NA=subcortical_zeroes_as_NA, cortical_zeroes_as_NA=cortical_zeroes_as_NA,
    subcortical_merged=subcortical_merged,
    wb_path=wb_path
  )
}

#' @rdname smooth_cifti
#' @export
smoothcii <- function(
  cifti_original_fname, cifti_target_fname,
  surface_sigma, volume_sigma,
  surfL_fname=NULL, surfR_fname=NULL, cerebellum_fname=NULL,
  subcortical_zeroes_as_NA=FALSE, cortical_zeroes_as_NA=FALSE,
  subcortical_merged=FALSE,
  wb_path=NULL){

  smooth_cifti(
    cifti_original_fname=cifti_original_fname, cifti_target_fname=cifti_target_fname,
    surface_sigma=surface_sigma, volume_sigma=volume_sigma,
    surfL_fname=surfL_fname, surfR_fname=surfR_fname, cerebellum_fname=cerebellum_fname,
    subcortical_zeroes_as_NA=subcortical_zeroes_as_NA, cortical_zeroes_as_NA=cortical_zeroes_as_NA,
    subcortical_merged=subcortical_merged,
    wb_path=wb_path
  )
}