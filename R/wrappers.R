#' Calls cifti_separate after validating and modifying a few arguments.
#'
#' @description Calls \code{cifti_separate} after validating and modifying a few 
#'  arguments. This function is used by other functions in \code{ciftiTools}
#'  and should not be used directly by users. Also, note that each argument
#'  is required despite the documentation mentioning defaults for certain
#'  arguments. (The parameter documentation is copied over from the function
#'  it wraps, cifti_separate.)
#'
#' @inheritParams cifti_fname_Param
#' @inheritParams brainstructures_Param
#' @inheritParams ROI_brainstructures_Param
#' @inheritParams sep_kwargs_Param
#' @inheritParams sep_keep_Param
#' @inheritParams wb_path_Param
#'
#' @return The return value of the cifti_separate call.
#'
#' @details Currently used by cifti_read and cifti_resample.
cifti_separate_wrapper <- function(
  cifti_fname, brainstructures, ROI_brainstructures,
  sep_kwargs, sep_keep, wb_path) {

  wb_cmd <- get_wb_cmd_path(wb_path)

  # Match arguments to cifti_separate.
  if (!is.null(sep_kwargs)) {
    names(sep_kwargs) <- match_input(
      names(sep_kwargs), 
      get_kwargs(ciftiTools::cifti_separate)
    )
  } 
  if (!is.null(sep_kwargs$cifti_fname)) {
    # [TO DO]: Below warning works for cifti_read and cifti_separate but is not general for this wrapper.
    #   But then again, this wrapper isn't meant to be used generally.
    warning(paste(
      "`sep_kwargs$cifti_fname` should be `NULL`, because `cifti_fname`",
      "should be a complete file path. Ignoring."
    ))
    sep_kwargs$cifti_fname <- NULL
  }

  # If sep_keep==FALSE and the writing directory hasn't been set, use a temporary directory.
  if (is.null(sep_kwargs$write_dir) & !sep_keep) { 
    sep_kwargs$write_dir <- tempdir()
  } 

  # Check brainstructures and ROI_brainstructures. 
  brainstructures <- match_input(
    brainstructures, 
    c("left","right","subcortical")
  )
  if (!is.null(ROI_brainstructures)) {
    ROI_brainstructures <- match_input(ROI_brainstructures, brainstructures)
  }
  
  # Add brainstructures, ROI_brainstructures, and cifti_fname to the kwargs. 
  #   Raise an error if they were also provided in sep_kwargs but do not match.
  sep_kwargs <- merge_kwargs(list(
    cifti_fname=cifti_fname,
    brainstructures=brainstructures, 
    ROI_brainstructures=ROI_brainstructures
  ), sep_kwargs, labelA="immediate arguments", labelB="sep_kwargs")
  
  # Do cifti_separate.
  sep_kwargs[sapply(sep_kwargs, is.null)] <- NULL
  sep_result <- do.call(cifti_separate, sep_kwargs)
  # data.frame with column "label", "fname", and "existed"
  return(sep_result)
}

#' Calls cifti_resample_separate after validating and modifying a few arguments.
#'
#' @description Calls \code{cifti_resample_separate} after validating and 
#'  modifying a few arguments. This function is used by other functions in 
#'  \code{ciftiTools} and should not be used directly by users. Also, note that 
#'  each argument is required despite the documentation mentioning defaults for 
#'  certain arguments. (The parameter documentation is copied over from the 
#'  function it wraps, cifti_resample_separate.)
#'
#' @inheritParams resamp_res_Param
#' @param to_resample_fnames The files from \code{cifti_resample_separate} to 
#'  resample. Should be a named character vector with names as the file type label. 
#' @inheritParams resamp_kwargs_Param
#' @inheritParams resamp_keep_Param
#' @inheritParams surfL_fname_Param
#' @inheritParams surfR_fname_Param
#' @inheritParams sphereL_fname_Param
#' @inheritParams sphereR_fname_Param
#' @inheritParams wb_path_Param

#'
#' @return The return value of the cifti_resample call.
#'
#' @details Currently used by cifti_read and cifti_resample.
cifti_resample_wrapper <- function(resamp_res,
  to_resample_fnames, resamp_kwargs, resamp_keep, 
  surfL_fname, surfR_fname, sphereL_fname, sphereR_fname, 
  wb_path) {

  wb_cmd <- get_wb_cmd_path(wb_path)

  # Match arguments to cifti_resample_separate.
  if (!is.null(resamp_kwargs)) {
    names(resamp_kwargs) <- match_input(names(resamp_kwargs), get_kwargs(ciftiTools::cifti_resample_separate))
  } else {
    resamp_kwargs <- vector("list", 0)
  }
  if (!is.null(resamp_kwargs$read_dir)) {
    # [TO DO]: Below warning works for cifti_read and cifti_separate but is not general for this wrapper.
    #   But then again, this wrapper isn't meant to be used generally.
    warning(paste(
      "`resamp_kwargs$read_dir` should be `NULL`, because `to_resample_fnames`",
      "should be complete file paths. Ignoring."
    ))
    resamp_kwargs$read_dir <- NULL
  }
  if ("original_fnames" %in% names(resamp_kwargs)) {
    # [TO DO]: Below warning works for cifti_read and cifti_separate but is not general for this wrapper.
    #   But then again, this wrapper isn't meant to be used generally.
    warning(paste(
      "`resamp_kwargs` should not specify the original files names, because",
      "these should all be in the",
      "`to_resample_fnames` argument. They will be ignored."
    ))
  }

  # Format kwargs.
  resamp_kwargs <- merge_kwargs(list(resamp_res=resamp_res), resamp_kwargs,
    labelA="immediate arguments", labelB="resamp_kwargs")
  resamp_kwargs$original_fnames <- c(
    to_resample_fnames, 
    list(surfL=surfL_fname, surfR=surfR_fname, sphereL=sphereL_fname, sphereR=sphereR_fname)
  )
  resamp_kwargs$original_fnames <- resamp_kwargs$original_fnames[!sapply(resamp_kwargs$original_fnames, is.null)]
  resamp_kwargs$original_fnames <- resamp_kwargs$original_fnames[!sapply(resamp_kwargs$original_fnames, is.null)]
  
  # `cifti_resample_separate` already has extensive checks to validate
  #   the original files and target files. So, don't perform any here.

  # If resamp_keep==FALSE and the writing directory hasn't been set, use a temporary directory.
  if (!resamp_keep & is.null(resamp_kwargs$write_dir)) {
    resamp_kwargs$write_dir <- tempdir()
  }

  print(resamp_kwargs)

  # Do `cifti_resample_separate`.
  resamp_kwargs[sapply(resamp_kwargs, is.null)] <- NULL
  resamp_result <- do.call(cifti_resample_separate, resamp_kwargs)

  return(resamp_result)

}