#' Resample an individual GIFTI file (with its ROI)
#'
#' @description Performs spatial resampling of NIfTI/GIfTI data on the cortical surface
#'
#' @param original_fname The file to resample.
#' @param target_fname Where to save the resampled file.
#' @param ROIcortex_original_fname The name of the ROI file corresponding to \code{original_fname}. Leave as NULL (default) if this
#'  doesn't exist or shouldn't be resampled.
#' @param validROIcortex_target_fname The name of the resampled ROI file. Only applicable if \code{ROIcortex_original_fname} is provided.
#' @param file_type "metric" or "surface", or NULL (default) to infer from \code{original_fname}.
#' @param res_target Target resolution (number of cortical surface vertices per hemisphere)
#' @param sphere_original_fname File path of [left/right]-hemisphere spherical GIFTI files in original resolution 
#'  The hemisphere side should match \code{original_fname}.
#' @param sphere_target_fname File path of [left/right]-hemisphere spherical GIFTI files in targetinal resolution 
#'  The hemisphere side should match \code{target_fname}. See \code{make_helper_spheres}.
#' @param overwrite Logical indicating whether original_fname should be overwritten if it already exists.
#' @param read_dir If the file names of \code{original_fname} and \code{ROIcortex_original_fname} are relative, this is the directory to 
#'  look for them in. If NULL (default), use the current working directory. \code{read_dir} will not affect files specified with absolute paths.
#' @param write_dir If the file names of \code{target_fname} and \code{validROIcortex_target_fname} are relative, this is the directory to 
#'  look for them in. If NULL (default), use the current working directory. \code{write_dir} will not affect files specified with absolute paths.
#' @param sphere_target_dir If \code{sphere_target_keep} and the file names of \code{c_original_fnames} are relative, this is the directory to 
#'  write and look for them in. The default is "./helper_files_resampling".
#' @param wb_path (Optional) Path to Connectome Workbench folder. If not provided, should be set with 
#'  \code{ciftiTools.setOption('wb_path', 'path/to/workbench')}.
#'
#' @return Logical indicating whether resampled file was created.
#' @export
#'
gifti_resample <- function(original_fname, target_fname, 
  ROIcortex_original_fname=NULL, validROIcortex_target_fname=NULL, file_type=NULL, res_target,
  sphere_original_fname, sphere_target_fname, overwrite=TRUE,
  read_dir=NULL, write_dir=NULL, sphere_target_dir=NULL, wb_path=NULL){

  wb_cmd <- get_wb_cmd_path(wb_path)

  # Check arguments.
  read_dir <- check_dir(read_dir)
  original_fname <- make_abs_path(original_fname, read_dir)
  stopifnot(file.exists(original_fname))
  do_ROI <- !is.null(ROIcortex_original_fname)
  if(do_ROI){
    ROIcortex_original_fname <- make_abs_path(ROIcortex_original_fname, read_dir)
    stopifnot(file.exists(ROIcortex_original_fname))
  }
  write_dir <- check_dir(write_dir, make=TRUE)
  target_fname <- make_abs_path(target_fname, write_dir)
  if(do_ROI){
    validROIcortex_target_fname <- make_abs_path(validROIcortex_target_fname, read_dir)
  }
  if(is.null(file_type)){
    if(grepl("func.gii", original_fname, fixed=TRUE)){ file_type <- "metric" }
    else if(grepl("surf.gii", original_fname, fixed=TRUE)){ file_type <- "surface" }
    else{ stop(paste("Could not infer file type of ", original_fname, ". Please set the file_type argument.")) }
  }
  file_type <- match.arg(file_type, c("metric", "surface"))
  stopifnot(is.numeric(res_target))
  stopifnot(res_target > 0)
  sphere_original_fname <- make_abs_path(sphere_original_fname)
  stopifnot(file.exists(sphere_original_fname))
  sphere_target_dir <- check_dir(sphere_target_dir, "helper_files_resampling", make=TRUE)
  sphere_target_fname <- make_abs_path(sphere_target_fname, sphere_target_dir)
  stopifnot(file.exists(sphere_target_fname))
  stopifnot(is.logical(overwrite))
  if(do_ROI & file_type=="surface"){ stop("do_ROI AND file_type=='surface', but surface files do not use ROI.") }

  # Run the command if overwrite==TRUE, or if any desired file does not exist.
  run_cmd <- overwrite | !all(c(file.exists(target_fname, ifelse(do_ROI, file.exists(validROIcortex_target_fname), TRUE))))
  if(!run_cmd){
    cmd_code <- NA
  } else {
    cmd_name <- switch(file_type,
      metric="-metric-resample",
      surface="-surface-resample"
    )

    cmd <- paste(wb_cmd, cmd_name, original_fname, sphere_original_fname, sphere_target_fname, "BARYCENTRIC", target_fname)
    if(do_ROI){
      cmd <- paste(cmd, "-current-roi", ROIcortex_original_fname, "-valid-roi-out", validROIcortex_target_fname)
    }
    # Run it! Raise an error if it fails.
    cmd_code <- system(cmd)
    if(cmd_code != 0){
      stop(paste0("The Connectome Workbench command failed with code ", cmd_code, 
        ". The command was:\n", cmd))
    }
  }

  invisible(NULL)
}

#' Resample a metric GIFTI file (ends with "func.gii")
#'
#' @param ... Arguments to gifti_resample. All except \code{file_type} (which is "metric") can be provided.
#'
#' @return Logical indicating whether resampled file was created.
#' @export
#'
metric_resample <- function(...){
  # Check that the arguments are valid.
  kwargs_allowed <- names(as.list(args(ciftiTools::gifti_resample)))
  kwargs_allowed <- kwargs_allowed[1:(length(kwargs_allowed)-1)] # last is empty
  kwargs <- names(list(...))
  if("file_type" %in% kwargs){ stop("file_type==\"metric\" for metric_resample and therefore should not be provided as an argument.") }
  stopifnot(all(kwargs %in% kwargs_allowed))

  gifti_resample(file_type="metric", ...)
}

#' Resample a surface GIFTI file (ends with "func.gii")
#'
#' @param ... Arguments to gifti_resample. All except \code{file_type} (which is "surface") can be provided.
#'
#' @return Logical indicating whether resampled file was created.
#' @export
#'
surface_resample <- function(...){
  # Check that the arguments are valid.
  kwargs_allowed <- names(as.list(args(ciftiTools::gifti_resample)))
  kwargs_allowed <- kwargs_allowed[1:(length(kwargs_allowed)-1)] # last is empty
  kwargs <- names(list(...))
  if("file_type" %in% kwargs){ stop("file_type==\"surface\" for surface_resample and therefore should not be provided as an argument.") }
  stopifnot(all(kwargs %in% kwargs_allowed))

  gifti_resample(file_type="surface", ...)
}

#' Generates GIFTI sphere surface files
#'
#' @description Generates GIFTI spherical surface files in a target resolution. These are required for resampling CIFTI
#' and GIFTI files.  This function generates a pair of vertex-matched left and right spheres in the target resolution.
#'
#' @param sphereL_fname File path to left-hemisphere spherical GIFTI to be created
#' @param sphereR_fname File path to right-hemisphere spherical GIFTI to be created
#' @param resamp_res Target resolution of spherical GIFTIs to be created (approximate number of vertices per hemisphere)
#' @param write_dir If a file name is relative, what directory should it be saved to? Defaults to the current working directory.
#' @param wb_path (Optional) Path to Connectome Workbench folder. If not provided, should be set with 
#'  \code{ciftiTools.setOption('wb_path', 'path/to/workbench')}.
#'
#' @return Logical indicating whether output files exist
#' @export
#'
make_helper_spheres <- function(sphereL_fname, sphereR_fname, resamp_res, write_dir=NULL, wb_path=NULL){

  wb_cmd <- get_wb_cmd_path(wb_path)

  # Check that write_dir is valid. Use the current working directory if no write_dir is given.
  write_dir <- check_dir(write_dir)

  sphereL_fname <- make_abs_path(sphereL_fname, write_dir)
  sphereR_fname <- make_abs_path(sphereR_fname, write_dir)

  system(paste(wb_cmd, "-surface-create-sphere", resamp_res, sphereL_fname, sep=" "))
  system(paste(wb_cmd, "-surface-flip-lr", sphereL_fname, sphereR_fname, sep=" "))
  system(paste(wb_cmd, "-set-structure", sphereL_fname, "CORTEX_LEFT", sep=" "))
  system(paste(wb_cmd, "-set-structure", sphereR_fname, "CORTEX_RIGHT", sep=" "))

  invisible(file.exists(sphereL_fname) & file.exists(sphereR_fname))
}