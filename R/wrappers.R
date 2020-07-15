#' separate_cifti with collapsed file name argument
#'
#' @description Calls \code{separate_cifti} using the file names listed in the
#'  argument \code{sep_fnames}. 
#'
#' @inheritParams cifti_fname_Param
#' @inheritParams brainstructures_Param
#' @inheritParams ROI_brainstructures_Param
#' @inheritParams sep_fnames_Param
#' @inheritParams write_dir_Param_separated
#' @inheritParams wb_path_Param
#'
#' @return The return value of the separate_cifti call.
#'
#' @details Currently used by read_cifti and resample_cifti.
separate_cifti_wrapper <- function(
  cifti_fname, brainstructures=NULL, ROI_brainstructures=NULL,
  sep_fnames=NULL, write_dir=NULL, wb_path=NULL) {

  # Get kwargs.
  sep_kwargs <- list(
    cifti_fname=cifti_fname,
    brainstructures=brainstructures, ROI_brainstructures=ROI_brainstructures,
    write_dir=write_dir, wb_path=wb_path
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
  sep_kwargs[sapply(sep_kwargs, is.null)] <- NULL
  do.call(separate_cifti, sep_kwargs)
}

#' resample_cifti with collapsed file name argument
#'
#' @description Calls \code{resample_cifti} using the original file names 
#'  listed in the \code{original_fnames} argument and the target file names
#'  listed in the \code{resamp_fnames} argument. 
#'
#' @inheritParams original_fnames_Param_resampled
#' @inheritParams resamp_fnames_Param_resampled
#' @inheritParams resamp_res_Param_required
#' @inheritParams sphereL_fname_Param
#' @inheritParams sphereR_fname_Param
#' @inheritParams surfL_fname_Param
#' @inheritParams surfR_fname_Param
#' @param surfL_target_fname,surfR_target_fname (Optional) File path for
#'  the resampled GIFTI surface geometry file representing the left/right 
#'  cortex. If NULL (default),
#' @inheritParams read_dir_Param_separated
#' @inheritParams write_dir_Param_resampled
#' @inheritParams wb_path_Param
#'
#' @return The return value of the resample_cifti call.
#'
#' @details Currently used by read_cifti and resample_cifti.
resample_cifti_wrapper <- function(
  resamp_res,
  original_fnames, resamp_fnames,
  sphereL_fname, sphereR_fname, 
  surfL_fname=NULL, surfR_fname=NULL, 
  surfL_target_fname=NULL, surfR_target_fname=NULL, 
  read_dir=NULL, write_dir=NULL, wb_path=NULL) {

  # [TO DO]: Decide: add "original"/"target" to sphere and surf?
  
  # Get kwargs.
  resamp_kwargs <- list(
    resamp_res=resamp_res, 
    sphereL_fname=sphereL_fname, sphereR_fname=sphereR_fname,
    surfL_original_fname=surfL_fname, surfR_original_fname=surfR_fname,
    surfL_target_fname=surfL_target_fname, 
    surfR_target_fname=surfR_target_fname, 
    read_dir=read_dir, write_dir=write_dir, wb_path=wb_path
  )

  # Get expected file names.
  expected_labs <- get_kwargs(ciftiTools::resample_cifti_separate)
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

  # Do resample_cifti_separate.
  resamp_kwargs[sapply(resamp_kwargs, is.null)] <- NULL
  do.call(resample_cifti_separate, resamp_kwargs)
}