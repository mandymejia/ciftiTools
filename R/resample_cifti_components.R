#' Resample a series of GIFTIs related to a CIFTI file
#'
#' Performs spatial resampling of various CIFTI file components on 
#'  the cortical surface. (The subcortical data is not resampled here.) 
#'  GIFTI surface geometry files can additionally be included: see 
#'  \code{surfL_original_fname} and \code{surfR_original_fname}.
#'
#' Step 1: Generate spheres in the original and target resolutions
#'  Step 2: Use -metric-resample to resample surface/cortex files 
#'  Step 3: Use -surface-resample to resample gifti files
#' 
#' @param original_res The original resolution of the CIFTI cortical surface(s).
#' @inheritParams resamp_res_Param_required
#' @param cortexL_original_fname,cortexR_original_fname (Optional) File path of 
#'  GIFTI data for \[left/right\] cortex to resample.
#' @param cortexL_target_fname,cortexR_target_fname (Optional) File path to 
#'  save the resampled GIFTI data for \[left/right\] cortex as.
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
#' @inheritParams surfL_original_fname_Param
#' @inheritParams surfR_original_fname_Param
#' @inheritParams surfL_target_fname_Param
#' @inheritParams surfR_target_fname_Param
#' @inheritParams read_dir_Param_separated
#' @inheritParams write_dir_Param_generic
#'
#' @return A named character vector of file paths to each resampled file
#'
#' @keywords internal
#' 
resample_cifti_components <- function(
  original_res, resamp_res, 
  cortexL_original_fname=NULL, cortexR_original_fname=NULL, 
  cortexL_target_fname=NULL, cortexR_target_fname=NULL, 
  ROIcortexL_original_fname=NULL, ROIcortexR_original_fname=NULL, 
  ROIcortexL_target_fname=NULL, ROIcortexR_target_fname=NULL,
  surfL_original_fname=NULL, surfR_original_fname=NULL, 
  surfL_target_fname=NULL, surfR_target_fname=NULL,
  read_dir=NULL, write_dir=NULL) {

  original_fnames <- list(
    cortexL=cortexL_original_fname, cortexR=cortexR_original_fname, 
    ROIcortexL=ROIcortexL_original_fname, ROIcortexR=ROIcortexR_original_fname,
    surfL=surfL_original_fname, surfR=surfR_original_fname
  )
  original_fnames <- original_fnames[!vapply(original_fnames, is.null, FALSE)]

  target_fnames <- list(
    cortexL=cortexL_target_fname, cortexR=cortexR_target_fname, 
    ROIcortexL=ROIcortexL_target_fname, ROIcortexR=ROIcortexR_target_fname,
    surfL=surfL_target_fname, surfR=surfR_target_fname
  )
  target_fnames <- target_fnames[!vapply(target_fnames, is.null, FALSE)]

  all_labels <- c(
    "cortexL", "cortexR", "ROIcortexL", "ROIcortexR", "surfL", "surfR"
  )

  # Check original files.
  stopifnot(is.list(original_fnames))
  stopifnot(length(original_fnames) > 0)
  stopifnot(all(names(original_fnames) %in% all_labels))
  original_fnames <- lapply(original_fnames, format_path, read_dir, mode=4)
  if (!all(vapply(original_fnames, file.exists, FALSE))) {
    stop(paste("This file(s) to resample does not exist:\n\n",
      paste(unique(as.character(original_fnames)[
        !vapply(original_fnames, file.exists, FALSE)]), collapse="\n")
    ))
  }
  # Use default names for target files if none provided.
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

  # Check target files.
  stopifnot(is.list(target_fnames))
  if (length(target_fnames) > 0) {
    stopifnot(all(names(target_fnames) %in% all_labels))
    missing_original <- !(names(target_fnames) %in% c(
      c("validROIcortexL", "validROIcortexR"), names(original_fnames)
    ))
    if (sum(missing_original) > 0) {
      ciftiTools_warn(paste0(
        "Ignoring these resampling targets because",
        " their original files were not provided:\n", 
        paste(names(target_fnames)[
          !vapply(target_fnames, is.null, FALSE)][missing_original], collapse="\n"),
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
  for (ii in seq_len(length(original_fnames))) {
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

  # Step 2 and 3: Use -metric-resample or -surface-rsample to resample 
  #   cortex, ROI, and surface files into target resolution.

  resample_gifti_kwargs_common <- list(
    original_res=original_res, resamp_res=resamp_res,
    #   Since we already appended read/write/sphere_target directories,
    #     set them to NULL.
    read_dir=NULL, write_dir=NULL
  )
  for (ii in seq_len(length(original_fnames))) {
    lab <- names(original_fnames)[ii]
    # Check if this file should be skipped.
    if (grepl("ROI", lab)) { next }  # Obtained with cortex[L/R].
    # Get additional kwargs.
    is_left <- substr(lab, nchar(lab), nchar(lab)) == "L" # last char: L or R.
    resample_kwargs <- c(resample_gifti_kwargs_common, list(
      original_fname=original_fnames[[lab]], target_fname=target_fnames[[lab]],
      hemisphere=ifelse(is_left, "left", "right")
    ))
    
    # Resample cortical data.
    if (lab %in% c("cortexL", "cortexR")) {
      # Use ROI if provided.
      ROI_lab <- ifelse(is_left, "ROIcortexL", "ROIcortexR")
      if (ROI_lab %in% names(target_fnames)) {
        resample_kwargs <- c(
          resample_kwargs, list(
            ROIcortex_original_fname=original_fnames[[ROI_lab]], 
            ROIcortex_target_fname=target_fnames[[ROI_lab]]
          )
        )
      }
      file_type <- ifelse(grepl(".label.gii", original_fnames[[lab]], fixed=TRUE), "label", "metric")
      do.call(resample_gifti, c(resample_kwargs, list(file_type=file_type)))
    
    # Resample surfaces.
    } else if (lab %in% c("surfL", "surfR")) {
      do.call(resample_gifti, c(resample_kwargs, list(file_type="surface")))
    }
  }
  
  invisible(unlist(target_fnames))
}