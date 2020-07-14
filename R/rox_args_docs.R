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

#' original_fnames: for resampling
#'
#' @param original_fnames The original files to resample. This is a named list 
#'  where each element's name is a file type label, and each element's value
#'  is a file name. Labels must be one of the following: "cortexL", "cortexR", 
#'  "ROIcortexL", "ROIcortexR", "sphereL", "sphereR", "surfL", or "surfR".
#'  Both "sphereL" and "sphereR" are required; all others are optional. If 
#'  \code{read_dir} is not \code{NULL}, then all these file names should be
#'  relative to \code{read_dir}. 
#' @name original_fnames_Param_resample
NULL

#' resamp_fnames: for resampling
#'
#' @param resamp_fnames Where to write the resampled files. This is a named list 
#'  where each element's name is a file type label, and each element's value
#'  is a file name. Labels must be one of the following: "cortexL", "cortexR", 
#'  "ROIcortexL", "ROIcortexR", "validROIcortexL", "validROIcortexR", 
#'  "sphereL", "sphereR", "surfL", or "surfR". All except "validROIcortex[L/R]"
#'  must be in \code{original_fnames}: if "validROIcortex[L/R]" is present, 
#'  "cortex[L/R]" and "ROIcortex[L/R]" must be in \code{original_fnames}. 
#'  File names can be \code{NULL}, in which case a default file name based on the
#'  original file name will be used: see \code{\link{cifti_resample_default_fname}}.
#'  If \code{write_dir} is not \code{NULL}, then all these file names should be
#'  relative to \code{write_dir}.
#' @name resamp_fnames_Param_resample
NULL

#' resamp_keep
#'
#' @param resamp_keep If resampled files are created, will they be kept or 
#'  deleted at the end of this function call? Default: \code{FALSE} (delete).
#' @name resamp_keep_Param
NULL

#' resamp_fnames
#'
#' @param resamp_fnames Where to write the resampled files. This is a named list 
#'  where each entry's name is a file type label, and each entry's value
#'  is a file name indicating where to write the corresponding resampled file. 
#'  The recognized file type labels are: "cortexL", "cortexR", 
#'  "ROIcortexL", "ROIcortexR", "validROIcortexL", and "validROIcortexR".
#'  
#'  Entry values can be \code{NULL}, in which case a default file name will be 
#'  used: see \code{\link{cifti_resample_default_fname}}. Default file names
#'  will also be used for files that need to be resampled/written but without a
#'  corresponding entry in \code{resamp_fnames}.
#'  
#'  Entries in \code{resamp_fnames} will be ignored if they are not needed
#'  based on \code{[ROI_]brainstructures}. For example, if
#'  \code{brainstructures="left"}, then \code{resamp_fnames$cortexR} will be 
#'  ignored if specified. 
#'
#'  The \code{write_dir} argument can be used to place each resampled file in
#'  the same directory. 
#' @name resamp_fnames_Param
NULL

#' resamp_res: required
#'
#' @param resamp_res Target resolution for resampling (number of 
#'  cortical surface vertices per hemisphere).  
#' @name resamp_res_Param_required
NULL

#' resamp_res: optional
#'
#' @param resamp_res (Optional) Target resolution for resampling (number of 
#'  cortical surface vertices per hemisphere). If \code{NULL} (default) or 
#'  \code{FALSE}, do not perform resampling.
#' @name resamp_res_Param_optional
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
#' @param sep_keep If separated files are created, will they be kept or 
#'  deleted at the end of this function call? Default: \code{FALSE} (delete).
#' @name sep_keep_Param
NULL

#' sep_fnames
#'
#' @param sep_fnames Where to write the separated files. This is a named list 
#'  where each entry's name is a file type label, and each entry's value
#'  is a file name indicating where to write the corresponding separated file. 
#'  The recognized file type labels are: "cortexL", "cortexR", 
#'  "ROIcortexL", "ROIcortexR", "subcortVol", and "subcortLab".
#'  
#'  Entry values can be \code{NULL}, in which case a default file name will be 
#'  used: see \code{\link{cifti_separate_default_suffix}}. Default file names
#'  will also be used for files that need to be separated/written but without a
#'  corresponding entry in \code{sep_fnames}.
#'  
#'  Entries in \code{sep_fnames} will be ignored if they are not needed
#'  based on \code{[ROI_]brainstructures}. For example, if
#'  \code{brainstructures="left"}, then \code{sep_fnames$cortexR} will be 
#'  ignored if specified. 
#'
#'  The \code{write_dir} argument can be used to place each separated file in
#'  the same directory. 
#' @name sep_fnames_Param
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

#'  write_dir: intermediate separated/resampled files
#'  
#' @param write_dir Where should the separated (and resampled) intermediate
#'  files be placed? \code{NULL} (default) will write the separated files to
#'  the current working directory if \code{sep_keep}, and to a temporary
#'  directory if not \code{sep_keep}. Likewise, it will write the resampled
#'  files to the current working directory if \code{resamp_keep}, and to a 
#'  temporary directory if not \code{resamp_keep}. Otherwise, both the
#'  separated and resampled files will be written to the same directory,
#'  \code{write_dir}. (Different subfolders can be used by modifying the
#'  individual file names in \code{sep_fnames} and \code{resamp_fnames}.)
#' 
#'  \code{write_dir} must already exist, or an error will be raised.
#' @name write_dir_Param_intermediate
NULL

#'  write_dir: resampled files
#'  
#' @param write_dir Where should the resampled
#'  files be placed? \code{NULL} (default) will write the files to
#'  the current working directory. 
#' 
#'  \code{write_dir} must already exist, or an error will be raised.
#' @name write_dir_Param_resampled
NULL

#'  write_dir: separated files
#'  
#' @param write_dir Where should the separated
#'  files be placed? \code{NULL} (default) will write the files to
#'  the current working directory. 
#' 
#'  \code{write_dir} must already exist, or an error will be raised.
#' @name write_dir_Param_separated
NULL