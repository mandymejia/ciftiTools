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
  # Check file names.
  for (ii in 1:length(sep_fnames)) {
    lab <- names(sep_fnames)[[ii]]
    if (!(paste0(lab, "_fname") %in% expected_labs)) {
      stop(paste0(
        "An entry in `sep_fnames` was not recognized. The entry name was ",
        lab, " whereas the expected labels for `separate_cifti` are:\n\t",
        paste(gsub("_fname", "", expected_labs), collapse="\n\t"), ".\n",
      ))
    }
    sep_kwargs[paste0(lab, "_fname")] <- lab
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
#' @inheritParams read_dir_Param_separated
#' @inheritParams write_dir_Param_resampled
#' @inheritParams wb_path_Param
#'
#' @return The return value of the resample_cifti call.
#'
#' @details Currently used by read_cifti and resample_cifti.
resample_cifti_wrapper <- function(
  original_fnames, resamp_fnames,
  resamp_res, sphereL_fname, sphereR_fname, 
  surfL_fname=NULL, surfR_fname=NULL, 
  read_dir=NULL, write_dir=NULL, wb_path=NULL) {
  
  # Get kwargs.
  resamp_kwargs <- list(
    resamp_res=resamp_res, 
    sphereL_fname=sphereL_fname, sphereR_fname=sphereR_fname,
    surfL_original_fname=surfL_fname, surfR_original_fname=surfR_fname,
    read_dir=read_dir, write_dir=write_dir, wb_path=wb_path
  )

  # Get expected file names.
  expected_labs <- get_kwargs(ciftiTools::resample_cifti_separate)
  expected_labs <- expected_labs[grepl("fname", expected_labs, fixed=TRUE)]
  # Check original file names.
  for (ii in 1:length(original_fnames)){
    lab <- names(original_fnames)[[ii]]
    if (!(paste0(lab, "_original_fname") %in% expected_labs)) {
      stop(paste0(
        "An entry in `original_fnames` was not recognized. The entry name was ",
        lab, " whereas the expected labels for original files ",
        "for `resample_cifti_separate` are:\n\t",
        paste(gsub("_fname", "", 
          expected_labs[grepl("original", expected_labs)]), collapse="\n\t"), 
        ".\n",
      ))
    }
    resamp_kwargs[paste0(lab, "_original_fname")] <- lab
  }
  # Check resampled/target file names.
  for (ii in 1:length(resamp_fnames)){
    lab <- names(resamp_fnames)[[ii]]
    if (!(paste0(lab, "_target_fname") %in% expected_labs)) {
      stop(paste0(
        "An entry in `resamp_fnames` was not recognized. The entry name was ",
        lab, " whereas the expected labels for target files ",
        "for `resample_cifti_separate` are:\n\t",
        paste(gsub("_fname", "", 
          expected_labs[grepl("resamp", expected_labs)]), collapse="\n\t"), 
        ".\n",
      ))
    }
    resamp_kwargs[paste0(lab, "_target_fname")] <- lab
  }

  # Do resample_cifti_separate.
  resamp_kwargs[sapply(resamp_kwargs, is.null)] <- NULL
  do.call(resample_cifti_separate, resamp_kwargs)
}