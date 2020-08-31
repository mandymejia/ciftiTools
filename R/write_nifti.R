
#' Write subcortical NIFTI
#'
#' Write subcortical NIFTI from vectorized data and labels, and the 
#'  subcortical mask.
#'
#' To write out NIFTIs from the volumetric data and labels, use \code{writeNifti}
#'  directly.
#'
#' @param subcortVol V voxels by T measurements data matrix
#' @param subcortLabs Numeric or factor vector corresponding to subcortical
#'  structure labels
#' @param subcortMask The volumetric mask.
#' @param subcortVol_fname What to name the data file
#' @param subcortLabs_fname What to name the labels file
#' @param fill Out-of-mask values. Default: \code{0}.
#' @inheritParams wb_path_Param
write_subcort_nifti <- function(
  subcortVol, subcortLabs, subcortMask, 
  subcortVol_fname, subcortLabs_fname, fill=0, wb_path=NULL){

  # Data.
  writeNifti(unmask_vol(subcortVol, subcortMask, fill=fill), subcortVol_fname)

  # Labels...
  stopifnot(is.subcort_labs(subcortLabs))
  subcortLabs <- as.numeric(subcortLabs) #- 2
  writeNifti(unmask_vol(subcortLabs, subcortMask, fill=fill), subcortLabs_fname)
  
  # ...Add back subcortical label information.
  # https://www.humanconnectome.org/software/workbench-command/-volume-help
  subcort_lab_list <- system.file("subcort_label_list.txt", package="ciftiTools")
  cmd <- paste(
    "-volume-label-import", 
    sys_path(subcortLabs_fname), 
    sys_path(subcort_lab_list), 
    sys_path(subcortLabs_fname)
  )
  run_wb_cmd(cmd, wb_path)
}