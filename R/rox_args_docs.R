#' brainstructures
#'
#' @param brainstructures Character vector indicating which brain structure(s) 
#'  to obtain: \code{"left"} (left cortical surface), \code{"right"} (right 
#'  cortical surface), and/or \code{"subcortical"} (subcortical and cerebellar 
#'  gray matter). Default: \code{c("left","right")} (brain surface only).
#' @name brainstructures_Param
NULL

#' cifti_fname
#'
#' @param cifti_fname File path of CIFTI-format data (ending in .d*.nii) to read 
#'  in.
#' @name cifti_fname_Param
NULL

#' resamp_keep
#'
#' @param resamp_keep If files are created by 
#'  \code{\link{cifti_resample_separate}}, should they be deleted at the end of 
#'  this function call? (If \code{resamp_kwargs$overwrite == FALSE} and the 
#'  resampled files already exist, they will not be deleted even if 
#'  \code{resamp_keep == FALSE}.) Default: \code{FALSE} (delete new files).
#' @name resamp_keep_Param
NULL

#' resamp_kwargs
#'
#' @param resamp_kwargs (Optional) Additional arguments to 
#'  \code{\link{cifti_resample_separate}} in the form of a list, e.g. 
#'  \code{list(overwrite=FALSE, write_dir="resampled_files")}. Do not specify
#'  \code{resamp_res}, \code{sphereL_fname}, or \code{sphereR_fname} in 
#'  \code{resamp_kwargs}; instead, use the immediate arguments. Do not 
#'  specify the names of the files to resample, e.g. 
#'  \code{cortexL_original_fname}, because these are determined by the
#'  preceeding \code{\link{cifti_separate}} call.
#' @name resamp_kwargs_Param
NULL

#' resamp_res
#'
#' @param resamp_res (Optional) Target resolution for resampling (number of 
#'  cortical surface vertices per hemisphere). If \code{NULL} (default) or 
#'  \code{FALSE}, do not perform resampling.
#' @name resamp_res_Param
NULL

#' ROI_brainstructures
#'
#' @param ROI_brainstructures Character vector indicating which ROIs should be 
#'  obtained. \code{NULL} (default) to not get any ROIs. Otherwise, this should 
#'  be a subset of the \code{brainstructures} argument. Any elements in 
#'  \code{ROI_brainstructures} but not in \code{brainstructures} will be 
#'  ignored.
#' @name ROI_brainstructures_Param
NULL

#' sep_keep
#'
#' @param sep_keep If files are created by \code{\link{cifti_separate}}, should 
#'  they be deleted at the end of this function call? (If 
#'  \code{sep_kwargs$overwrite == FALSE} and the separated files already exist,
#'  they will not be deleted even if \code{sep_keep == FALSE}.) Default:
#'  \code{FALSE} (delete new files).
#' @name sep_keep_Param
NULL

#' sep_kwargs
#'
#' @param sep_kwargs (Optional) Additional arguments to 
#'  \code{\link{cifti_separate}} in the form of a list, e.g. 
#'  \code{list(overwrite=FALSE, write_dir="separated_files", 
#'  cortexL_fname="my_cortexL.nii")}. Do not specify \code{cifti_fname} in 
#'  \code{sep_kwargs}; instead, use the \code{cifti_fname} argument directly.
#' @name sep_kwargs_Param
NULL

#' sphereL_fname
#'
#' @param sphereL_fname The left GIFTI sphere file in the same resolution
#'  as the CIFTI data. It is only required for resampling. 
#' @name sphereL_fname_Param
NULL

#' sphereR_fname
#'
#' @param sphereR_fname The right GIFTI sphere file in the same resolution
#'  as the CIFTI data. It is only required for resampling. 
#' @name sphereR_fname_Param
NULL

#' surfL_fname
#'
#' @param surfL_fname (Optional) File path of GIFTI surface geometry
#'  file representing the left cortex.
#' @name surfL_fname_Param
NULL

#' surfR_fname
#'
#' @param surfR_fname (Optional) File path of GIFTI surface geometry
#'  file representing the left/right cortex. One or both can be provided.
#' @name surfR_fname_Param
NULL

#' verbose
#'
#' @param verbose Should occasional updates be printed? Default: \code{TRUE}.
#' @name verbose_Param
NULL

#' wb_path
#'
#' @param wb_path (Optional) Path to Connectome Workbench folder or executable. 
#'  If not provided, should be set with 
#'  \code{ciftiTools.setOption("wb_path", "path/to/workbench")}.
#' @name wb_path_Param
NULL