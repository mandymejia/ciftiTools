#' Resample CIFTI data from separate GIfTI and NIfTI files
#'
#' @description Performs spatial resampling of various CIFTI file components on the cortical surface.
#'
#' @param cortexL_original_fname,cortexR_original_fname (Optional) File path of 
#'  GIFTI data for [left/right] cortex to resample.
#' @param cortexL_target_fname,cortexR_target_fname (Optional) File path to 
#'  save the resampled GIFTI data for [left/right] cortex as.
#'  If NULL (default) and \code{cortex[L/R]_original_fname} was provided, it 
#'  will be named by \code{\link{cifti_separate_default_suffix}}.
#' @param ROIcortexL_original_fname,ROIcortexR_original_fname (Optional) File 
#'  path of GIFTI ROI corresponding to \code{cortex[L/R]_original_fname} to 
#'  resample.
#' @param ROIcortexL_target_fname,ROIcortexR_target_fname (Optional) File path 
#'  of to save the resampled GIFTI ROI corresponding to 
#'  \code{cortex[L/R]_target_fname} as.
#'  If NULL (default) and \code{cortex[L/R]_original_fname} was provided, it 
#'  will be named by \code{\link{cifti_separate_default_suffix}}.
#' @param validROIcortexL_target_fname,validROIcortexR_target_fname (Optional) 
#'  Where to save the valid ROI from resampling \code{cortex[L/R]_original_fname}.
#'  If NULL (default) and \code{cortex[L/R]_original_fname} was provided, it 
#'  will be named by \code{\link{cifti_separate_default_suffix}}.
#' @param surfL_original_fname,surfR_original_fname (Optional) File path, or 
#'  vector of multiple file paths, of GIFTI surface geometry file 
#'  representing left/right cortex to resample too.
#' @param surfL_target_fname,surfR_target_fname (Optional) File path, or vector 
#'  of multiple file paths, to save the resampled data at. 
#'  Each should correspond to \code{surf[L/R]_original_fname}.
#' @inheritParams resamp_res_Param
#' @param sphereL_original_fname,sphereR_original_fname File path of 
#'  [left/right]-hemisphere spherical GIFTI files in original resolution 
#'  (compatible with original files e.g. \code{cortex[L/R]_original_fname} and 
#'  \code{surf[L/R]_original_fname}) .
#' @param sphereL_target_fname,sphereR_target_fname File path of 
#'  [left/right]-hemisphere spherical GIFTI files in targetinal resolution 
#'  (compatible with target files) . \code{NULL} (default) will be named as 
#'  "Resampled_[resamp_res]_[basename(sphere[L/R]_original_fname)]"
#' @param sphere_target_keep Should helper files be deleted at the end of this 
#'  function call, if they were created? Default: \code{FALSE}.
#' @param sphere_target_overwrite Logical indicating whether 
#'  \code{sphere[L/R]_target_fname} should be overwritten if it already exists. 
#'  Default: \code{TRUE}.
#' @param overwrite Logical indicating whether each target file should be 
#'  overwritten if it already exists. Default: \code{TRUE}.
#' @param read_dir Directory to append to the path of every file name in
#'  \code{original_fname} and \code{ROIcortex_original_fname}. If \code{NULL} 
#'  (default), do not append any directory to the path. 
#' @param write_dir Directory to append to the path of every file name in
#'  \code{target_fname} and \code{validROIcortex_target_fname}. If \code{NULL} 
#'  (default), do not append any directory to the path. 
#' @param sphere_target_dir Directory to append to the path of 
#'  \code{sphere_target_fname}. If \code{NULL} (default), it will be a folder
#'  titled \code{"helper_files_resampling"} within \code{write_dir} (or the 
#"  current working directory if \code{write_dir} is \code{NULL}.
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
  cortexL_original_fname=NULL, cortexR_original_fname=NULL, cortexL_target_fname=NULL, cortexR_target_fname=NULL, 
  ROIcortexL_original_fname=NULL, ROIcortexR_original_fname=NULL, ROIcortexL_target_fname=NULL, ROIcortexR_target_fname=NULL,
  validROIcortexL_target_fname=NULL, validROIcortexR_target_fname=NULL, 
  surfL_original_fname=NULL, surfR_original_fname=NULL, surfL_target_fname=NULL, surfR_target_fname=NULL,
  resamp_res, 
  sphereL_original_fname, sphereR_original_fname, sphereL_target_fname=NULL, sphereR_target_fname=NULL, 
  sphere_target_keep=FALSE, sphere_target_overwrite=TRUE,
  overwrite=TRUE, read_dir=NULL, write_dir=NULL, sphere_target_dir=NULL, wb_path=NULL) {

  wb_cmd <- get_wb_cmd_path(wb_path)

  ## original files
  original_fnames <- list(
    cortexL=cortexL_original_fname, cortexR=cortexR_original_fname, 
    ROIcortexL=ROIcortexL_original_fname, ROIcortexR=ROIcortexR_original_fname,
    surfL=surfL_original_fname, surfR=surfR_original_fname
  )
  ##b/c cortex[L/R] needed for validROI
  if (!is.null(validROIcortexL_target_fname)) { original_fnames$validROIcortexL <- cortexL_original_fname }
  if (!is.null(validROIcortexR_target_fname)) { original_fnames$validROIcortexR <- cortexR_original_fname }
  if (all(sapply(original_fnames, is.null))) { stop("No original files provided--nothing to resample!") }
  original_fnames <- original_fnames[!sapply(original_fnames, is.null)]
  original_fnames <- lapply(original_fnames, format_path, read_dir, mode=4)
  if (!all(sapply(original_fnames, file.exists))) {
    stop(paste("This file(s) to resample does not exist:\n\n",
               paste(unique(as.character(original_fnames)[!sapply(original_fnames, file.exists)]), collapse="\n")))
  }
  ## target files
  target_fnames <- list(
    cortexL=cortexL_target_fname, cortexR=cortexR_target_fname, 
    ROIcortexL=ROIcortexL_target_fname, ROIcortexR=ROIcortexR_target_fname,
    validROIcortexL=validROIcortexL_target_fname, validROIcortexR=validROIcortexR_target_fname,
    surfL=surfL_target_fname, surfR=surfR_target_fname
  )
  missing_original <- !(names(target_fnames[!sapply(target_fnames, is.null)]) %in% names(original_fnames))
  if (sum(missing_original) > 0) {
    warning(paste0("Ignoring these resampling targets because their original files were not provided:\n", 
                   paste(names(target_fnames)[!sapply(target_fnames, is.null)][missing_original], collapse="\n")
    ))
  }
  target_fnames <- target_fnames[names(original_fnames)]
  original_to_target_fname <- function(original_fname, resamp_res) {
    bname <- basename(original_fname)
    paste("resampled", resamp_res, bname, sep="_")
  }
  for(i in 1:length(original_fnames)) {
    lab <- names(original_fnames)[i]
    if (is.null(target_fnames[[lab]])) { 
      target_fnames[lab] <- original_to_target_fname(original_fnames[[lab]], resamp_res)
    }
    target_fnames[lab] <- format_path(target_fnames[[lab]], write_dir, mode=2) # already in gifti_resample
  }

  # other args
  if (is.null(sphere_target_dir)) {
    if (is.null(write_dir)) {
      sphere_target_dir <- format_path("helper_files_resampling", mode=NA)
    } else {
      sphere_target_dir <- format_path(file.path(write_dir, "helper_files_resampling"), mode=NA)
    }
  }
  if (!dir.exists(sphere_target_dir)) { dir.create(sphere_target_dir) }
  stopifnot(all(file.exists(c(sphereL_original_fname, sphereR_original_fname))))
  if (is.null(sphereL_target_fname)) { 
    sphereL_target_fname <- format_path(original_to_target_fname(sphereL_original_fname, resamp_res), sphere_target_dir, mode=4) 
  }
  if (is.null(sphereR_target_fname)) { 
    sphereR_target_fname <- format_path(original_to_target_fname(sphereR_original_fname, resamp_res), sphere_target_dir, mode=4)
  }
  stopifnot(is.logical(sphere_target_keep))
  stopifnot(is.logical(sphere_target_overwrite))

  target_fnames$sphereL <- sphereL_target_fname
  target_fnames$sphereR <- sphereR_target_fname

  # Collect the absolute paths to each file in a data.frame to return later. Also record whether each existed before the
  # workbook command.
  resamp_files <- data.frame(
    label = names(target_fnames), 
    fname = as.character(target_fnames),
    stringsAsFactors=FALSE
  )
  resamp_files$existed <- file.exists(resamp_files$fname)

  # Step 1: Generate spheres in the target resolution (if not already existing and provided)
  sphere_target_exists <- file.exists(sphereL_target_fname, sphereR_target_fname)
  if (sum(sphere_target_exists) == 1) { warning("One sphere target file exists but not the other. Overwriting the existing file.") }
  if (sphere_target_overwrite | !all(sphere_target_exists)) {
    make_helper_spheres(sphereL_target_fname, sphereR_target_fname, resamp_res)
  }

  # Step 2: Use -metric-resample or -surface-rsample to resample cortex, ROI, and surface files into target resolution
  gifti_resample_kwargs_common <- list(resamp_res=resamp_res, 
    overwrite=overwrite, read_dir=NULL, write_dir=NULL, sphere_target_dir=NULL, wb_path=wb_path)
  for(i in 1:length(original_fnames)) {
    lab <- names(original_fnames)[i]
    fname <- original_fnames[[lab]]
    if (!overwrite & resamp_files$existed[resamp_files$label==lab]) { next }
    if (grepl("validROI", lab)) { next } # obtained in conjunction with cortex[L/R] resampling
    is_left <- substr(lab, nchar(lab), nchar(lab)) == "L" # last character. should be R otherwise
    resample_kwargs <- c(gifti_resample_kwargs_common, list(
      original_fname=original_fnames[[lab]], target_fname=target_fnames[[lab]],
      sphere_original_fname=ifelse(is_left, sphereL_original_fname, sphereR_original_fname),
      sphere_target_fname=ifelse(is_left, sphereL_target_fname, sphereR_target_fname) 
    ))

    if (lab %in% c("cortexL", "cortexR")) {
      validROI_lab <- ifelse(is_left, "validROIcortexL", "validROIcortexR")
      if (validROI_lab %in% names(original_fnames)) {
        resample_kwargs <- c(
          resample_kwargs, 
          list(ROIcortex_original_fname=original_fnames[[ifelse(is_left, "ROIcortexL", "ROIcortexR")]], 
               validROIcortex_target_fname=target_fnames[[validROI_lab]]))
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