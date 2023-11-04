#' Remap GIFTI metric or label data
#' 
#' Remap GIFTI metric or label data between two different spaces, such as
#'  between FreeSurfer fsaverage group data and fs_LR group data. This function
#'  is a wrapper to \code{\link{resample_gifti}}.
#' 
#' @param original_fname The GIFTI file to remap.
#' @param target_fname Where to save the remapped file.
#' @param hemisphere \code{"left"} (default) or \code{"right"}. An error will
#'  occur if the hemisphere indicated in the GIFTI metadata does not match.
#' @param remap_method \code{"adaptive"} (default) or \code{"adaptive"}
#'  resampling. These options correspond to the Workbench command options
#'  \code{"BARYCENTRIC"} and \code{"ADAP_BARY_AREA"}, respectively.
#' 
#'  For remapping between fs_LR group data and FreeSurfer fsaverage group data, 
#'  adaptive resampling should be used.
#' @param area_original_fname,area_target_fname File paths to the surfaces to
#'  use for vertex area correction during adaptive resampling. Required if
#'  \code{remap_method} is \code{"adaptive"}.
#' @param ROIcortex_original_fname,ROIcortex_target_fname 
#'  \code{ROIcortex_original_fname} is the name of the ROI file corresponding to
#'  \code{original_fname}. Leave as \code{NULL} (default) if not applicable. If
#'  provided, then also provide \code{ROIcortex_target_fname} to say where to
#'  write the remapped ROI file. 
#' @param sphere_original_fname,sphere_target_fname File paths to the sphere
#'  surfaces in the original and target spaces.
#' 
#' @return The remapped GIFTI file name, invisibly
#' 
#' @export
#' 
#' @section Connectome Workbench:
#' This function interfaces with the \code{"-metric-resample"}, \code{"-label-resample"},
#'  and/or \code{"-surface-resample"} Workbench commands, depending on the input.
#' 
remap_gifti <- function(
  original_fname, target_fname,
  hemisphere=c("left", "right"),
  remap_method=c("adaptive", "barycentric"),
  area_original_fname, area_target_fname,
  ROIcortex_original_fname, ROIcortex_target_fname,
  sphere_original_fname, sphere_target_fname
){

  resample_gifti(
    original_fname=original_fname,
    target_fname=target_fname,
    hemisphere=hemisphere,
    resamp_method=remap_method,
    area_original_fname=area_original_fname,
    area_target_fname=area_target_fname,
    ROIcortex_original_fname=ROIcortex_original_fname,
    ROIcortex_target_fname=ROIcortex_target_fname,
    sphere_original_fname=sphere_original_fname,
    sphere_target_fname=sphere_target_fname
  )
}