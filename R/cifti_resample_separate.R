#' Resample CIFTI data from separate GIfTI and NIfTI files
#'
#' @description Performs spatial resampling of various CIFTI file components on the cortical surface.
#'
#' @param original_fnames The original files to resample. This is a named list 
#'  where each element's name is a file type label, and each element's value
#'  is a file name. Labels must be one of the following: "cortexL", "cortexR", 
#'  "ROIcortexL", "ROIcortexR", "sphereL", "sphereR", "surfL", or "surfR".
#'  Both "sphereL" and "sphereR" are required; all others are optional. If 
#'  \code{read_dir} is not \code{NULL}, then all these file names should be
#'  relative to \code{read_dir}. 
#' @param target_fnames Where to write the resampled files. This is a named list 
#'  where each element's name is a file type label, and each element's value
#'  is a file name. Labels must be one of the following: "cortexL", "cortexR", 
#'  "ROIcortexL", "ROIcortexR", "validROIcortexL", "validROIcortexR", 
#'  "sphereL", "sphereR", "surfL", or "surfR". All except "validROIcortex[L/R]"
#'  must be in \code{original_fnames}: if "validROIcortex[L/R]" is present, 
#'  "cortex[L/R]" and "ROIcortex[L/R]" must be in \code{original_fnames}. 
#'  File names can be \code{NULL}, in which case a default file name based on the
#'  original file name will be used: see \code{\link{original_to_target_fname}}.
#'  If \code{write_dir} is not \code{NULL}, then all these file names should be
#'  relative to \code{write_dir}.
#'
#'  Only the files in \code{target_fnames} will be created, even if others
#'  can be made from the \code{original_fnames}. If \code{NULL} (default), 
#'  all possible resampled files will be created.
#'
#' @inheritParams resamp_res_Param
#' @param sphere_target_keep Should the sphere target files be kept or deleted
#'  at the end of this function call, if they were created? Default: 
#'  \code{FALSE} (keep them). NOT IMPLEMENTED YET.
#' @param sphere_target_overwrite Logical indicating whether 
#'  \code{sphere[L/R]_target_fname} should be overwritten if it already exists. 
#'  Default: \code{TRUE}.
#' @param overwrite Logical indicating whether each target file should be 
#'  overwritten if it already exists. Default: \code{TRUE}.
#' @param read_dir Directory to append to the path of every file name in
#'  \code{original_fnames}. If \code{NULL} 
#'  (default), do not append any directory to the path. 
#' @param write_dir Directory to append to the path of every file name in
#'  \code{target_fnames}. If \code{NULL} 
#'  (default), do not append any directory to the path. 
#' @param sphere_target_dir Directory to append to the path of the sphere
#'  target files. Default is \code{"helper_files_resampling"}. 
#' @inheritParams wb_path_Param
#'
#' @return Logical indicating whether resampled CIFTI file was created.
#' @export
#'
#' @details Performs resampling of CIFTI files using Connectome Workbench tools.  
#"  Several helper files must be created:
#'  Step 1: Generate spheres in the target resolution (if not already existing and provided)
#'  Step 2: Use -metric-resample to resample surface/cortex files into target resolution
#'  Step 3: Use -surface-resample to resample the gifti files (if provided) into target resolution
cifti_resample_separate <- function(
  original_fnames, target_fnames=NULL, resamp_res, 
  sphere_target_keep=FALSE, sphere_target_overwrite=TRUE, sphere_target_dir=NULL,
  overwrite=TRUE, read_dir=NULL, write_dir=NULL, wb_path=NULL) {

  wb_cmd <- get_wb_cmd_path(wb_path)

  all_labels <- c(
    "cortexL", "cortexR", "ROIcortexL", "ROIcortexR",
    "sphereL", "sphereR", "surfL", "surfR"
  )
  all_labels_with_vROI <- c(all_labels, "validROIcortexL", "validROIcortexR")

  # Check original files.
  stopifnot(is.list(original_fnames))
  stopifnot(length(original_fnames) > 0)
  stopifnot(!any(sapply(original_fnames, is.null)))
  stopifnot(all(names(original_fnames) %in% all_labels))
  stopifnot(all(c("sphereL", "sphereR") %in% names(original_fnames)))
  stopifnot(length(original_fnames) - 
    sum(c("sphereL", "sphereR") %in% names(original_fnames)) > 0)
  original_fnames <- lapply(original_fnames, format_path, read_dir, mode=4)
  if (!all(sapply(original_fnames, file.exists))) {
    stop(paste("This file(s) to resample does not exist:\n\n",
      paste(unique(as.character(original_fnames)[!sapply(original_fnames, file.exists)]), collapse="\n")
    ))
  }
  # Check target files.
  if (is.null(target_fnames)) {
    target_fnames <- vector("list", length(original_fnames))
    names(target_fnames) <- names(original_fnames)
    if (all(c("cortexL", "ROIcortexL") %in% names(original_fnames))) {
      target_fnames$validROIcortexL <- NULL
    }
    if (all(c("cortexR", "ROIcortexR") %in% names(original_fnames))) {
      target_fnames$validROIcortexR <- NULL
    }
  }
  stopifnot(is.list(target_fnames))
  stopifnot(length(target_fnames) > 0)
  stopifnot(all(names(target_fnames) %in% all_labels_with_vROI))
  missing_original <- !(names(target_fnames) %in% c(
    c("validROIcortexL", "validROIcortexR"), names(original_fnames)))
  if (sum(missing_original) > 0) {
    warning(paste0(
      "Ignoring these resampling targets because their original files were not provided:\n", 
      paste(names(target_fnames)[!sapply(target_fnames, is.null)][missing_original], collapse="\n"),
      "\n"
    ))
  }
  stopifnot(!(
    "validROIcortexL" %in% target_fnames & !all(
      c("cortexL", "ROIcortexL") %in% original_fnames)
  ))
  stopifnot(!(
    "validROIcortexR" %in% target_fnames & !all(
      c("cortexR", "ROIcortexR") %in% original_fnames)
  ))

  # Use default file names for targets without a specified file name.
  for(ii in 1:length(target_fnames)) {
    lab <- names(target_fnames)[ii]
    if (is.null(target_fnames[[lab]])) {
      if (grepl("validROI", lab)) {
        # [TO DO]: use cifti_separate_default_suffix
        target_fnames[[lab]] <- paste0(
          "validROI_", original_to_target_fname(
            original_fnames[[gsub("validROI", "", lab)]], resamp_res))
      } else {
        target_fnames[[lab]] <- original_to_target_fname(
          original_fnames[[lab]], resamp_res)
      }
    }
    if (grepl("sphere", lab)) {
      target_fnames[[lab]] <- format_path(target_fnames[[lab]], sphere_target_dir, mode=2)
    } else {
      target_fnames[[lab]] <- format_path(target_fnames[[lab]], write_dir, mode=2)
    }
  }

  for(ii in 1:length(target_fnames)) {
    original_fnames[[ii]] <- format_path(original_fnames[[ii]], read_dir, mode=4)
  }

  # Collect the paths to each file in a data.frame to return later. 
  # Also record whether each existed before the Workbook command.
  resamp_files <- data.frame(
    label = names(target_fnames), 
    fname = as.character(target_fnames),
    stringsAsFactors=FALSE
  )
  resamp_files$existed <- file.exists(resamp_files$fname)

  # Step 1: Generate spheres in the target resolution (if not already existing and provided)
  sphere_target_exists <- file.exists(target_fnames$sphereL, target_fnames$sphereR)
  if (sum(sphere_target_exists) == 1) { 
    warning(paste(
      "One sphere target file exists but not the other.",
      "Overwriting the existing file."
    ))
  }
  if (sphere_target_overwrite | !all(sphere_target_exists)) {
    make_helper_spheres(target_fnames$sphereL, target_fnames$sphereR, resamp_res)
  }

  # Step 2: Use -metric-resample or -surface-rsample to resample 
  #   cortex, ROI, and surface files into target resolution.
  gifti_resample_kwargs_common <- list(resamp_res=resamp_res, 
    overwrite=overwrite, wb_path=wb_path,
    #   Since we already appended read/write/sphere_target directories,
    #     set them to NULL.
    read_dir=NULL, write_dir=NULL, sphere_target_dir=NULL)
  for(ii in 1:length(original_fnames)) {
    lab <- names(original_fnames)[ii]
    # Check if this file should be skipped.
    if (grepl("sphere", lab)) { next }    # Already done.
    if (grepl("validROI", lab)) { next }  # Obtained with cortex[L/R].
    if (!overwrite & resamp_files$existed[resamp_files$label==lab]) { next }
    # Get additional kwargs.
    is_left <- substr(lab, nchar(lab), nchar(lab)) == "L" # last char: L or R.
    resample_kwargs <- c(gifti_resample_kwargs_common, list(
      original_fname=original_fnames[[lab]], target_fname=target_fnames[[lab]],
      sphere_original_fname=ifelse(is_left, 
        original_fnames$sphereL, original_fnames$sphereR),
      sphere_target_fname=ifelse(is_left, 
        target_fnames$sphereL, target_fnames$sphereR) 
    ))
    # Do resampling.
    if (lab %in% c("cortexL", "cortexR")) {
      # Get ROI kwargs if applicable. 
      validROI_lab <- ifelse(is_left, "validROIcortexL", "validROIcortexR")
      if (validROI_lab %in% names(target_fnames)) {
        resample_kwargs <- c(
          resample_kwargs, 
          list(ROIcortex_original_fname=original_fnames[[ifelse(is_left, "ROIcortexL", "ROIcortexR")]], 
               validROIcortex_target_fname=target_fnames[[validROI_lab]]
          )
        )
      }

      do.call(metric_resample, c(resample_kwargs))
    } else if (lab %in% c("ROIcortexL", "ROIcortexR")) {
      do.call(metric_resample, c(resample_kwargs))
    } else if (lab %in% c("surfL", "surfR")) {
      do.call(surface_resample, c(resample_kwargs))
    }
  }
  
  # [TO DO]: sphere_target_keep

  invisible(resamp_files)
}