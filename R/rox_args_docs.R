#' labels
#' 
#' @section Label Levels:
#'  \code{xifti$meta$subcort$labels} is a factor with the following levels:
#' 
#'  \describe{
#'    \item{1}{Cortex-L}
#'    \item{2}{Cortex-R}
#'    \item{3}{Accumbens-L}
#'    \item{4}{Accumbens-R}
#'    \item{5}{Amygdala-L}
#'    \item{6}{Amygdala-R}
#'    \item{7}{Brain Stem}
#'    \item{8}{Caudate-L}
#'    \item{9}{Caudate-R}
#'    \item{10}{Cerebellum-L}
#'    \item{11}{Cerebellum-R}
#'    \item{12}{Diencephalon-L}
#'    \item{13}{Diencephalon-R}
#'    \item{14}{Hippocampus-L}
#'    \item{15}{Hippocampus-R}
#'    \item{16}{Pallidum-L}
#'    \item{17}{Pallidum-R}
#'    \item{18}{Putamen-L}
#'    \item{19}{Putamen-R}
#'    \item{20}{Thalamus-L}
#'    \item{21}{Thalamus-R}
#'  }
#' 
#'  Levels 1-21 correspond to the same structures as given by 
#'  \code{ft_read_cifti} in the \code{cifti-matlab} MATLAB toolbox. 
#' @name labels_Description
NULL

#' brainstructures
#'
#' @param brainstructures Character vector indicating which brain structure(s) 
#'  to obtain: \code{"left"} (left cortical surface), \code{"right"} (right 
#'  cortical surface) and/or \code{"subcortical"} (subcortical and cerebellar
#'  gray matter). Can also be \code{"all"} (obtain all three brain structures). 
#'  Default: \code{"all"}. 
#' @name brainstructures_Param_all
NULL

#' brainstructures
#'
#' @param brainstructures Character vector indicating which brain structure(s) 
#'  to obtain: \code{"left"} (left cortical surface), \code{"right"} (right 
#'  cortical surface) and/or \code{"subcortical"} (subcortical and cerebellar
#'  gray matter). Can also be \code{"all"} (obtain all three brain structures). 
#'  Default: \code{c("left","right")} (cortical surface only).
#' @name brainstructures_Param_LR
NULL

#' cifti_fname
#'
#' @param cifti_fname File path of CIFTI-format data (ending in .d*.nii).
#' @name cifti_fname_Param
NULL

#' original_fnames: for resampling
#'
#' @param original_fnames The files to resample. This is a named list 
#'  where each element's name is a file type label, and each element's value
#'  is a file name. Labels must be one of the following: "cortexL", "cortexR", 
#'  "ROIcortexL", "ROIcortexR", "sphereL", "sphereR", "surfL", or "surfR".
#'  Both "sphereL" and "sphereR" are required; all others are optional. If 
#'  \code{read_dir} is not \code{NULL}, then all these file names should be
#'  relative to \code{read_dir}. 
#' @name original_fnames_Param_resampled
NULL

#'  read_dir: separated files
#'  
#' @param read_dir Directory to append to the path of every original file name,
#'  e.g. \code{cortexL_original_fname}. If \code{NULL} (default), do not append
#'  any directory to the path.
#' 
#'  \code{read_dir} must already exist, or an error will be raised.
#' @name read_dir_Param_separated
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
#'  used: see \code{\link{resample_cifti_default_fname}}. Default file names
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

#' resamp_keep
#'
#' @param resamp_keep If resampled files are created, will they be kept or 
#'  deleted at the end of this function call? Default: \code{FALSE} (delete).
#'  Keeping the resampled files may help speed up certain tasks, for example
#'  when repeatedly iterating over CIFTI files--resampling will only be done
#'  once instead of every new iteration.
#' @name resamp_keep_Param
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
#'  be a subset of the \code{brainstructures} argument. 
#'  
#'  NOTE: ROIs are currently
#'  not fully supported by ciftiTools, since \code{"cifti"} objects will not contain
#'  the ROIs. A workaround would be to keep the separated/resampled files
#'  with \code{sep_keep}/\code{resamp_keep} and then read those in with
#'  \code{make_xifti}. 
#' @name ROI_brainstructures_Param_LR
NULL

#' sep_keep
#'
#' @param sep_keep If separated files are created, should they be kept or 
#'  deleted at the end of this function call? Default: \code{FALSE} (delete).
#'  Keeping the separated files may help speed up certain tasks, for example
#'  when repeatedly iterating over subjects--the CIFTI will only be separated
#'  once instead of at each iteration.
#' @name sep_keep_Param
NULL

#' sep_fnames
#'
#' @param sep_fnames (Optional) Where to write the separated files (override
#'  their default file names). This is a named list 
#'  where each entry's name is a file type label, and each entry's value
#'  is a file name indicating where to write the corresponding separated file. 
#'  The recognized file type labels are: "cortexL", "cortexR", 
#'  "ROIcortexL", "ROIcortexR", "subcortVol", and "subcortLab".
#'  
#'  Entry values can be \code{NULL}, in which case a default file name will be 
#'  used: see \code{\link{cifti_component_suffix}}. Default file names
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
#'  as the CIFTI data. It is required for resampling. 
#' @name sphereL_fname_Param
NULL

#' sphereR_fname
#'
#' @param sphereR_fname The right GIFTI sphere file in the same resolution
#'  as the CIFTI data. It is required for resampling. 
#' @name sphereR_fname_Param
NULL

#' surfL_fname
#'
#' @param surfL_fname (Optional) File path of GIFTI surface geometry
#'  file representing the left cortex.
#' @name surfL_fname_Param
NULL

#' surfL_original_fname
#'
#' @param surfL_original_fname (Optional) File path of GIFTI surface geometry
#'  file representing the left cortex.
#' @name surfL_original_fname_Param
NULL

#' surfL_target_fname
#'
#' @param surfL_target_fname (Optional) File path to save the resampled GIFTI 
#'  surface geometry file representing the left cortex at.
#' @name surfL_target_fname_Param
NULL

#' surfR_fname
#'
#' @param surfR_fname (Optional) File path of GIFTI surface geometry
#'  file representing the right cortex.
#' @name surfR_fname_Param
NULL

#' surfR_original_fname
#'
#' @param surfR_original_fname (Optional) File path of GIFTI surface geometry
#'  file representing the right cortex.
#' @name surfR_original_fname_Param
NULL

#' surfR_target_fname
#'
#' @param surfR_target_fname (Optional) File path to save the resampled GIFTI 
#'  surface geometry file representing the right cortex at.
#' @name surfR_target_fname_Param
NULL

#' verbose: FALSE
#'
#' @param verbose Should occasional updates be printed? Default: \code{FALSE}.
#' @name verbose_Param_FALSE
NULL

#' verbose: TRUE
#'
#' @param verbose Should occasional updates be printed? Default: \code{TRUE}.
#' @name verbose_Param_TRUE
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
#' @param write_dir Where should any output files be written? \code{NULL}
#'  (default) will write them to the current working directory.
#'
#'  Files flagged for deletion will be written to a temporary directory, and
#'  thus are not affected by this argument. So if \code{sep_keep} is 
#'  \code{TRUE}, the separated files will be written to \code{write_dir}, but if
#'  \code{sep_keep} is \code{FALSE}, they will be written to \code{tempdir()} 
#'  and later deleted. \code{resamp_keep} works similarly. 
#'
#'  For \code{read_cifti_separate}, the surface files (\code{surfL} or \code{surfR})
#'  are deleted if \code{resamp_keep} is \code{FALSE}, so in this case they will
#'  be written to \code{tempdir()}. But for \code{resample_cifti}, the
#'  surface files are kept even if \code{resamp_keep} is \code{FALSE}, so they 
#'  will always be written to \code{write_dir}. 
#' 
#'  Different subfolders for the separated, resampled, and final output files
#'  cannot be specified by \code{write_dir}. Instead, modify the individual file
#'  names in \code{sep_fnames} and \code{resamp_fnames}.
#' 
#'  \code{write_dir} must already exist, or an error will occur.
#' @name write_dir_Param_intermediate
NULL

#'  write_dir: resampled files
#'  
#' @param write_dir Where should the resampled
#'  files be placed? \code{NULL} (default) will write them to
#'  the current working directory if \code{keep}, and a temporary directory
#'  if \code{!keep}. 
#' 
#'  \code{write_dir} must already exist, or an error will occur.
#' @name write_dir_Param_resampled
NULL

#'  write_dir: separated files
#'  
#' @param write_dir Where should the separated
#'  files be placed? \code{NULL} (default) will write them to
#'  the current working directory if \code{keep}, and a temporary directory
#'  if \code{!keep}. 
#' 
#'  \code{write_dir} must already exist, or an error will occur.
#' @name write_dir_Param_separated
NULL

#' xifti
#' 
#' @param xifti Object of class "xifti". 
#'  See \code{\link{is.xifti}} and \code{\link{make_xifti}}.
#' @name xifti_Param
NULL

#' x: xifti
#' 
#' @param x Object of class "xifti". 
#'  See \code{\link{is.xifti}} and \code{\link{make_xifti}}.
#' @name x_Param_xifti
NULL