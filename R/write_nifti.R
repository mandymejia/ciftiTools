
#' Write subcortical data to NIFTI files
#'
#' Write subcortical data to NIFTI files representing the data values,
#'  subcortical structure labels, and volumetric mask. The input formats of
#'  \code{subcortVol}, \code{subcortLabs}, and \code{subcortMask}
#'  correspond to the data structures of \code{xifti$data$subcort},
#'  \code{xifti$meta$subcort$labels} , and \code{xifti$meta$subcort$mask}
#'  respectively. \code{subcortVol} and \code{subcortLabs} should be vectorized,
#'  so if they are volumes consider using \code{oro.nifti::writeNIfTI}.
#' 
#' 
#' All file path arguments are required except \code{ROIsubcortVol_fname}. If
#'  not provided, the volumetric mask will not be written. (It's redundant with
#'  the 0 values in \code{subcortLabs_fname} because valid labels have positive
#'  indexes.)
#' @param subcortVol A vectorized data matrix: V voxels by T measurements
#' @param subcortLabs Numeric (0 and 3-21) or factor vector corresponding to 
#'  subcortical structure labels. See \code{\link{substructure_table}}.
#' @param subcortMask Logical volumetric mask. Values of 0 represent out-of-mask
#'  voxels (not subcortical), and values of 1 represent in-mask voxels
#'  (subcortical),
#' @param subcortVol_fname,subcortLabs_fname,ROIsubcortVol_fname File path to
#'  a NIFTI to save the corresponding data. \code{ROIsubcortVol_fname} is
#'  optional but the rest is required.
#' @param fill Values to use for out-of-mask voxels. Default: \code{0}.
#' @inheritParams wb_path_Param
#' 
#' @return Named character vector with the \code{"subcortVol"}, 
#'  \code{"subcortLabs"}, and \code{"ROIsubcortVol"} file names (if written).
#' 
#' @export
write_subcort_nifti <- function(
  subcortVol, subcortLabs, subcortMask, 
  subcortVol_fname, subcortLabs_fname, ROIsubcortVol_fname=NULL,
  fill=0, wb_path=NULL){

  # Data.
  writeNifti(unmask_vol(subcortVol, subcortMask, fill=fill), subcortVol_fname)

  # Labels...
  stopifnot(is.subcort_labs(subcortLabs))
  subcortLabs <- as.numeric(subcortLabs) #- 2
  writeNifti(unmask_vol(subcortLabs, subcortMask, fill=fill), subcortLabs_fname)

  # ...Add back subcortical label information.
  # https://www.humanconnectome.org/software/workbench-command/-volume-help
  subcort_lab_list <- system.file("extdata", "subcort_label_list.txt", package="ciftiTools")
  cmd <- paste(
    "-volume-label-import", 
    sys_path(subcortLabs_fname), 
    sys_path(subcort_lab_list), 
    sys_path(subcortLabs_fname)
  )
  run_wb_cmd(cmd, wb_path)

  # Mask (as numeric).
  if (!is.null(ROIsubcortVol_fname)) {
    writeNifti(subcortMask + 0, ROIsubcortVol_fname)
  }
  out <- unlist(list(
    subcortVol=subcortVol_fname,
    subcortLabs=subcortLabs_fname,
    ROIsubcortVol=ROIsubcortVol_fname
  ))
}