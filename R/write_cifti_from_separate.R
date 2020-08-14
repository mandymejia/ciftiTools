#' Make CIFTI file
#'
#' Make a CIFTI file from components
#'
#' @param cifti_fname Path to the CIFTI to write.
#' @param cortexL_fname The left cortex file.
#' @param cortexR_fname The right cortex file.
#' @param ROIcortexL_fname The left cortex ROI file.
#' @param ROIcortexR_fname The right cortex ROI file.
#' @param subcortVol_fname The subcortical data file.
#' @param subcortLabs_fname The subcortical labels file.
#' @param timestep If a dense time series (dtseries.nii) file is being written,
#'  this is the time between measurements. If \code{NULL}, use the Connectome
#'  Workbench default (1.0).
#' @param timestart If a dense time series (dtseries.nii) file is being written,
#'  this is starting time. If \code{NULL}, use the Connectome Workbench default 
#'  (0.0).
#' @inheritParams wb_path_Param
#'
#' @keywords internal
#' 
write_cifti_from_separate <- function(
  cifti_fname, 
  cortexL_fname, cortexR_fname,
  ROIcortexL_fname=NULL, ROIcortexR_fname=NULL,
  subcortVol_fname, subcortLabs_fname,
  timestep=NULL, timestart=NULL,
  wb_path=NULL){

  cifti_info <- info_cifti(cifti_fname, wb_path)

  # Determine what kind of CIFTI is being written.
  # Must be one of the following after the check in `cifti_info`
  create_cmd <- switch(as.character(cifti_info$cifti%intent),
    `3002` = "-cifti-create-dense-timeseries",
    `3006` = "-cifti-create-dense-scalar",
    `3007` = "-cifti-create-label"
  )
  if (is.null(create_cmd))
    stop(paste(
      "NIFTI intent code", cifti_info$cifti%intent, "is not supported."
    ))
  }
  # TO-DO: adjust GIFTI/NIFTI written files accordingly?

  # [TO DO]: Resolve the warning about orientation metadata, 
  # qform_code/sform_code:
  # https://brainder.org/2012/09/23/the-nifti-file-format/
  
  # Volume
  cmd <- paste(
    create_cmd, sys_path(cifti_fname), 
    "-volume", sys_path(subcortVol_fname), sys_path(subcortLabs_fname), 
  )

  # Left
  cmd <- paste(cmd, "-left-metric", sys_path(cortexL_fname))
  if (!is.null(ROIcortexL_fname)) {
    cmd <- paste(cmd, "-roi-left", sys_path(ROIcortexL_fname))
  }

  # Right
  cmd <- paste(cmd, "-right-metric", sys_path(cortexR_fname) )
  if (!is.null(ROIcortexR_fname)) {
    cmd <- paste(cmd, "-roi-right", sys_path(ROIcortexR_fname))
  }

  # Metadata
  if (create_cmd == "-cifti-create-dense-timeseries") {
    if (!is.null(timestep)) { cmd <- paste(cmd, "-timestep", timestep) }
    if (!is.null(timestart)) { cmd <- paste(cmd, "-timestart", timestart) }
  }

  run_wb_cmd(cmd, wb_path)
}