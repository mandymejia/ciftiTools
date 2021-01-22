#' \code{separate_cifti} wrapper
#'
#' Calls \code{separate_cifti} using the file names listed in the
#'  argument \code{sep_fnames}. 
#'
#' Currently used by \code{read_cifti} and \code{resample_cifti}.
#' 
#' @inheritParams cifti_fname_Param
#' @inheritParams brainstructures_Param_LR
#' @inheritParams ROI_brainstructures_Param_LR
#' @inheritParams sep_fnames_Param
#' @inheritParams write_dir_Param_generic
#'
#' @return The return value of the \code{separate_cifti} call
#'
#' @keywords internal
#' 
separate_cifti_wrapper <- function(
  cifti_fname, brainstructures=NULL, ROI_brainstructures=NULL,
  sep_fnames=NULL, write_dir=NULL) {

  # Get kwargs.
  sep_kwargs <- list(
    cifti_fname=cifti_fname,
    brainstructures=brainstructures, ROI_brainstructures=ROI_brainstructures,
    write_dir=write_dir
  )

  # Get expected file names.
  expected_labs <- get_kwargs(ciftiTools::separate_cifti)
  expected_labs <- expected_labs[grepl("fname", expected_labs, fixed=TRUE)]
  expected_labs <- expected_labs[expected_labs != "cifti_fname"]
  # Check file names.
  if (!is.null(sep_fnames)) {
    match_input(names(sep_fnames), gsub("_.*", "", expected_labs), 
      user_value_label="sep_fnames")
    sep_kwargs[names(sep_fnames)] <- sep_fnames
  }
  # Do separate_cifti.
  sep_kwargs[vapply(sep_kwargs, is.null, FALSE)] <- NULL
  do.call(separate_cifti, sep_kwargs)
}

#' \code{resample_cifti} wrapper
#'
#' Calls \code{resample_cifti} using the original file names 
#'  listed in the \code{original_fnames} argument and the target file names
#'  listed in the \code{resamp_fnames} argument. 
#'
#' Currently used by read_cifti and resample_cifti.
#' 
#' @inheritParams original_fnames_Param_resampled
#' @inheritParams resamp_fnames_Param
#' @param original_res The original resolution of the CIFTI cortical surface(s).
#' @inheritParams resamp_res_Param_required
#' @inheritParams surfL_fname_Param
#' @inheritParams surfR_fname_Param
#' @param surfL_target_fname,surfR_target_fname (Optional) File path for
#'  the resampled GIFTI surface geometry file representing the left/right 
#'  cortex. If NULL (default),
#' @inheritParams read_dir_Param_separated
#' @inheritParams write_dir_Param_generic
#'
#' @return The return value of the \code{resample_cifti} call
#' 
#' @keywords internal
#' 
resample_cifti_wrapper <- function(
  original_fnames, resamp_fnames,
  original_res, resamp_res,
  surfL_fname=NULL, surfR_fname=NULL, 
  surfL_target_fname=NULL, surfR_target_fname=NULL, 
  read_dir=NULL, write_dir=NULL) {

  # [TO DO]: surfL_fname --> surfL_original_fname? (and same for right?)
  
  # Get kwargs.
  resamp_kwargs <- list(
    original_res=original_res, resamp_res=resamp_res, 
    surfL_original_fname=surfL_fname, surfR_original_fname=surfR_fname,
    surfL_target_fname=surfL_target_fname, 
    surfR_target_fname=surfR_target_fname, 
    read_dir=read_dir, write_dir=write_dir
  )

  # Get expected file names.
  expected_labs <- get_kwargs(resample_cifti_components)
  expected_labs <- expected_labs[grepl("fname", expected_labs, fixed=TRUE)]

  # Check and add original file names to the kwargs.
  if (!is.null(original_fnames)) {
    match_input(names(original_fnames), gsub("_.*", "", expected_labs), 
      user_value_label="original_fnames")
    resamp_kwargs[paste0(names(original_fnames), "_original_fname")] <- original_fnames
  }
  # Check and add resampled/target file names to the kwargs.
  if (!is.null(resamp_fnames)) {
    match_input(names(resamp_fnames), gsub("_.*", "", expected_labs), 
      user_value_label="resamp_fnames")
    resamp_kwargs[paste0(names(resamp_fnames), "_target_fname")] <- resamp_fnames
  }

  # Do resample_cifti_components.
  resamp_kwargs[vapply(resamp_kwargs, is.null, FALSE)] <- NULL
  do.call(resample_cifti_components, resamp_kwargs)
}