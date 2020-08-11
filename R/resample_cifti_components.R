#' Resample a series of GIFTIs related to a CIFTI file
#'
#' @description Performs spatial resampling of various CIFTI file components on 
#'  the cortical surface. (The subcortical data is not resampled here.) 
#'  GIFTI surface geometry files can additionally be included: see 
#' \code{surfL_original_fname} and \code{surfR_original_fname}.
#'
#' @inheritParams resamp_res_Param_required
#' @inheritParams sphereL_fname_Param
#' @inheritParams sphereR_fname_Param
#' @param cortexL_original_fname,cortexR_original_fname (Optional) File path of 
#'  GIFTI data for [left/right] cortex to resample.
#' @param cortexL_target_fname,cortexR_target_fname (Optional) File path to 
#'  save the resampled GIFTI data for [left/right] cortex as.
#'  If NULL (default) and \code{cortex[L/R]_original_fname} was provided, it 
#'  will be named by \code{\link{cifti_component_suffix}}.
#' @param ROIcortexL_original_fname,ROIcortexR_original_fname (Optional) File 
#'  path of GIFTI ROI corresponding to \code{cortex[L/R]_original_fname} to 
#'  resample.
#' @param ROIcortexL_target_fname,ROIcortexR_target_fname (Optional) File path 
#'  of to save the resampled GIFTI ROI corresponding to 
#'  \code{cortex[L/R]_target_fname} as.
#'  If NULL (default) and \code{cortex[L/R]_original_fname} was provided, it 
#'  will be named by \code{\link{cifti_component_suffix}}.
#' @param validROIcortexL_target_fname,validROIcortexR_target_fname (Optional) 
#'  Where to save the valid ROI from resampling \code{cortex[L/R]_original_fname}.
#'  If NULL (default) and \code{cortex[L/R]_original_fname} was provided, it 
#'  will be named by \code{\link{cifti_component_suffix}}.
#' @inheritParams surfL_original_fname_Param
#' @inheritParams surfR_original_fname_Param
#' @inheritParams surfL_target_fname_Param
#' @inheritParams surfR_target_fname_Param
#' @inheritParams read_dir_Param_separated
#' @inheritParams write_dir_Param_generic
#' @inheritParams wb_path_Param
#'
#' @return A data frame with column names "label" and "fname", and 
#'  rows corresponding to each resampled file.
#'
#' @details Performs resampling of CIFTI files using Connectome Workbench tools.  
#'  Step 1: Generate spheres in the target resolution
#'  Step 2: Use -metric-resample to resample surface/cortex files 
#'  Step 3: Use -surface-resample to resample gifti files
#'
#' @keywords internal
#' 
#' 
resample_cifti_components <- function(
  resamp_res, 
  sphereL_fname, sphereR_fname, 
  cortexL_original_fname=NULL, cortexR_original_fname=NULL, 
  cortexL_target_fname=NULL, cortexR_target_fname=NULL, 
  ROIcortexL_original_fname=NULL, ROIcortexR_original_fname=NULL, 
  ROIcortexL_target_fname=NULL, ROIcortexR_target_fname=NULL,
  validROIcortexL_target_fname=NULL, validROIcortexR_target_fname=NULL, 
  surfL_original_fname=NULL, surfR_original_fname=NULL, 
  surfL_target_fname=NULL, surfR_target_fname=NULL,
  read_dir=NULL, write_dir=NULL, wb_path=NULL) {

  wb_cmd <- get_wb_cmd_path(wb_path)

  original_fnames <- list(
    cortexL=cortexL_original_fname, cortexR=cortexR_original_fname, 
    ROIcortexL=ROIcortexL_original_fname, ROIcortexR=ROIcortexR_original_fname,
    surfL=surfL_original_fname, surfR=surfR_original_fname
  )
  original_fnames <- original_fnames[!sapply(original_fnames, is.null)]

  target_fnames <- list(
    cortexL=cortexL_target_fname, cortexR=cortexR_target_fname, 
    ROIcortexL=ROIcortexL_target_fname, ROIcortexR=ROIcortexR_target_fname,
    validROIcortexL=validROIcortexL_target_fname, 
    validROIcortexR=validROIcortexR_target_fname,
    surfL=surfL_target_fname, surfR=surfR_target_fname
  )
  target_fnames <- target_fnames[!sapply(target_fnames, is.null)]

  all_labels <- c(
    "cortexL", "cortexR", "ROIcortexL", "ROIcortexR",
    "surfL", "surfR"
  )
  all_labels_with_vROI <- c(all_labels, "validROIcortexL", "validROIcortexR")

  # Check original files.
  stopifnot(is.list(original_fnames))
  stopifnot(length(original_fnames) > 0)
  stopifnot(all(names(original_fnames) %in% all_labels))
  original_fnames <- lapply(original_fnames, format_path, read_dir, mode=4)
  if (!all(sapply(original_fnames, file.exists))) {
    stop(paste("This file(s) to resample does not exist:\n\n",
      paste(unique(as.character(original_fnames)[
        !sapply(original_fnames, file.exists)]), collapse="\n")
    ))
  }
  # Check target files.
  if (is.null(target_fnames)) {
    target_fnames <- vector("list", length(original_fnames))
    names(target_fnames) <- names(original_fnames)
    # Add valid ROI if cortex and ROIcortex are present.
    if (all(c("cortexL", "ROIcortexL") %in% names(original_fnames))) {
      target_fnames$validROIcortexL <- NULL
    }
    if (all(c("cortexR", "ROIcortexR") %in% names(original_fnames))) {
      target_fnames$validROIcortexR <- NULL
    }
  }
  stopifnot(is.list(target_fnames))
  if (length(target_fnames) > 0) {
    stopifnot(all(names(target_fnames) %in% all_labels_with_vROI))
    missing_original <- !(names(target_fnames) %in% c(
      c("validROIcortexL", "validROIcortexR"), names(original_fnames)
    ))
    if (sum(missing_original) > 0) {
      # [TO DO]: print file names too.
      warning(paste0(
        "Ignoring these resampling targets because",
        " their original files were not provided:\n", 
        paste(names(target_fnames)[
          !sapply(target_fnames, is.null)][missing_original], collapse="\n"),
        "\n"
      ))
    }
  }
  stopifnot(!(
    "validROIcortexL" %in% target_fnames && !all(
      c("cortexL", "ROIcortexL") %in% original_fnames)
  ))
  stopifnot(!(
    "validROIcortexR" %in% target_fnames && !all(
      c("cortexR", "ROIcortexR") %in% original_fnames)
  ))
  if (length(unique(as.character(target_fnames))) != length(target_fnames)) {
    print(target_fnames)
    stop(paste0(
      "The file paths for the resampled components are printed above. ",
      "Some file paths were identical, but ",
      "the same path cannot be used to write out different components. ",
      "Check if identical file names were specified, or if any provided ",
      "file name overlapped with a default file name.\n\n"
    ))
  }

  # Use default file names for targets without a specified file name.
  for(ii in 1:length(original_fnames)) {
    lab <- names(original_fnames)[ii]
    if (is.null(target_fnames[[lab]])) {
      if (grepl("validROI", lab)) {
        # [TO DO]: check if this works. use cifti_component_suffix?
        target_fnames[[lab]] <- paste0(
          "validROI_", resample_cifti_default_fname(
            original_fnames[[gsub("validROI", "", lab)]], resamp_res)
        )
      } else {
        target_fnames[[lab]] <- resample_cifti_default_fname(
          original_fnames[[lab]], resamp_res)
      }
    }
    target_fnames[[lab]] <- format_path(target_fnames[[lab]], write_dir, mode=2)
  }

  original_fnames[names(original_fnames)] <- format_path(
    as.character(original_fnames), read_dir, mode=4)
  # [TO DO]: error if a file name is absolute?

  # Step 1: Generate spheres in the target resolution (if not already existing and provided)
  if(is.null(sphereL_fname) | is.null(sphereR_fname)){
    stop("Both sphere files names must be provided (cannot be NULL).")
  }
  sphere_dir <- tempdir()
  target_fnames$sphereL <- resample_cifti_default_fname(sphereL_fname, resamp_res)
  target_fnames$sphereL <- format_path(target_fnames$sphereL, sphere_dir, mode=2)
  target_fnames$sphereR <- resample_cifti_default_fname(sphereR_fname, resamp_res)
  target_fnames$sphereR <- format_path(target_fnames$sphereR, sphere_dir, mode=2)
  make_helper_spheres(target_fnames$sphereL, target_fnames$sphereR, resamp_res)

  # Step 2 and 3: Use -metric-resample or -surface-rsample to resample 
  #   cortex, ROI, and surface files into target resolution.

  # Collect the paths to each file in a data.frame to return later. 
  resamp_files <- data.frame(
    label = names(target_fnames), 
    fname = as.character(target_fnames),
    stringsAsFactors=FALSE
  )

  resample_gifti_kwargs_common <- list(
    resamp_res=resamp_res, wb_path=wb_path,
    #   Since we already appended read/write/sphere_target directories,
    #     set them to NULL.
    read_dir=NULL, write_dir=NULL)
  for(ii in 1:length(original_fnames)) {
    lab <- names(original_fnames)[ii]
    # Check if this file should be skipped.
    if (grepl("validROI", lab)) { next }  # Obtained with cortex[L/R].
    # Get additional kwargs.
    is_left <- substr(lab, nchar(lab), nchar(lab)) == "L" # last char: L or R.
    resample_kwargs <- c(resample_gifti_kwargs_common, list(
      original_fname=original_fnames[[lab]], target_fname=target_fnames[[lab]],
      sphere_original_fname=ifelse(is_left, 
        sphereL_fname, sphereR_fname),
      sphere_target_fname=ifelse(is_left, 
        target_fnames$sphereL, target_fnames$sphereR) 
    ))
    
    # Do resampling.
    if (lab %in% c("cortexL", "cortexR")) {
      # Get ROI kwargs if applicable. 
      ROI_lab <- ifelse(is_left, "ROIcortexL", "ROIcortexR")
      validROI_lab <- ifelse(is_left, "validROIcortexL", "validROIcortexR")
      if (validROI_lab %in% names(target_fnames)) {
        resample_kwargs <- c(
          resample_kwargs, list(
            ROIcortex_original_fname=original_fnames[[ROI_lab]], 
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
  
  invisible(resamp_files)
}