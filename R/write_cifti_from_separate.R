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
  cortexL_fname=NULL, cortexR_fname=NULL,
  ROIcortexL_fname=NULL, ROIcortexR_fname=NULL,
  subcortVol_fname=NULL, subcortLabs_fname=NULL,
  timestep=NULL, timestart=NULL,
  wb_path=NULL){

  # Determine what kind of CIFTI is being written.
  # Must be one of the following after the check in `cifti_info`
  cifti_extn <- get_cifti_extn(cifti_fname)
  if (cifti_extn=="dlabel.nii") { stop("Writing label files is not yet implemented.") }
  create_cmd <- switch(cifti_extn,
    "dtseries.nii" = "-cifti-create-dense-timeseries",
    "dscalar.nii" = "-cifti-create-dense-scalar",
    "dlabel.nii" = "-cifti-create-label"
  )
  what <- switch(cifti_extn,
    "dtseries.nii" = "metric",
    "dscalar.nii" = "metric",
    "dlabel.nii" = "label"
  )
  if (is.null(create_cmd)) {
    stop(paste(
      "CIFTI extension", cifti_extn, "is not yet supported by ciftiTools."
    ))
  }
  # TO-DO: adjust GIFTI/NIFTI written files accordingly?

  # [TO DO]: Resolve the warning about orientation metadata, 
  # qform_code/sform_code:
  # https://brainder.org/2012/09/23/the-nifti-file-format/

  cmd <- paste(
    create_cmd, sys_path(cifti_fname)
  )

  # Volume
  if (!is.null(subcortVol_fname)){
    cmd <- paste(
      cmd, "-volume", sys_path(subcortVol_fname), sys_path(subcortLabs_fname)
    )
  }

  # Left
  if (!is.null(cortexL_fname)) {
    cmd <- paste(cmd, paste0("-left-", what), sys_path(cortexL_fname))
    if (!is.null(ROIcortexL_fname)) {
      cmd <- paste(cmd, "-roi-left", sys_path(ROIcortexL_fname))
    }
  }

  # Right
  if (!is.null(cortexR_fname)) {
    cmd <- paste(cmd, paste0("-right-", what), sys_path(cortexR_fname))
    if (!is.null(ROIcortexR_fname)) {
      cmd <- paste(cmd, "-roi-right", sys_path(ROIcortexR_fname))
    }
  }

  # Metadata
  if (create_cmd == "-cifti-create-dense-timeseries") {
    if (!is.null(timestep)) { cmd <- paste(cmd, "-timestep", timestep) }
    if (!is.null(timestart)) { cmd <- paste(cmd, "-timestart", timestart) }
  }

  run_wb_cmd(cmd, wb_path)

  invisible(cifti_fname)
}