#' Smooth a CIFTI 
#'
#' @description Smooth CIFTI data. This uses the \code{-cifti-smoothing} command 
#'  from Connectome Workbench.
#'
#' @param cifti_original_fname A CIFTI file to smooth.
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
#' @return Whether the CIFTI was successfully smoothed
#'
#' @export
#'
#' @details This function uses a system wrapper for the "wb_command"
#'  executable. The user must first download and install the Connectome 
#'  Workbench, available from 
#'  \url{https://www.humanconnectome.org/software/get-connectome-workbench}. 
#'  The \code{wb_path} argument is the path to the Connectime Workbench folder or
#'  executable.
#'
smooth_cifti <- function(
  cifti_original_fname, cifti_target_fname,
  surface_sigma, volume_sigma,
  surfL_fname=NULL, surfR_fname=NULL, cerebellum_fname=NULL,
  subcortical_zeroes_as_NA=FALSE, cortical_zeroes_as_NA=FALSE,
  subcortical_merged=FALSE,
  wb_path=NULL){

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
}