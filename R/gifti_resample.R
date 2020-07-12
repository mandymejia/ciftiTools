#' Resample an individual GIFTI file (with its ROI)
#'
#' @description Performs spatial resampling of NIfTI/GIfTI data on the cortical 
#'  surface.
#'
#' @param original_fname The GIFTI file to resample.
#' @param target_fname Where to save the resampled file.
#' @param ROIcortex_original_fname The name of the ROI file corresponding to 
#'  \code{original_fname}. Leave as \code{NULL} (default) if this doesn't exist
#'  or shouldn't be resampled.
#' @param validROIcortex_target_fname The name of the resampled ROI file. Only 
#'  applicable if \code{ROIcortex_original_fname} is provided.
#' @param file_type \code{"metric"} or \code{"surface"}, or \code{NULL} 
#'  (default) to infer from \code{original_fname}.
#' @inheritParams resamp_res_Param
#' @param sphere_original_fname File path of [left/right]-hemisphere spherical 
#'  GIFTI files in original resolution. The hemisphere side should match that of
#'  \code{original_fname}.
#' @param sphere_target_fname File path of [left/right]-hemisphere spherical 
#'  GIFTI files in targetinal resolution. The hemisphere side should match 
#'  that of \code{target_fname}. See \code{\link{make_helper_spheres}}.
#' @param overwrite Logical indicating whether original_fname should be 
#'  overwritten if it already exists. Default is \code{TRUE}.
#' @param read_dir Directory to append to the path of every file name in
#'  \code{original_fname} and \code{ROIcortex_original_fname}. If \code{NULL} 
#'  (default), do not append any directory to the path. 
#' @param write_dir Directory to append to the path of every file name in
#'  \code{target_fname} and \code{validROIcortex_target_fname}. If \code{NULL} 
#'  (default), do not append any directory to the path. 
#' @param sphere_target_dir Directory to append to the path of 
#'  \code{sphere_target_fname}. If \code{NULL} (default), do not append any 
#'  directory to the path. 
#' @inheritParams wb_path_Param
#'
#' @return Logical indicating whether resampled file was created.
#' @export
#'
gifti_resample <- function(original_fname, target_fname, 
  ROIcortex_original_fname=NULL, validROIcortex_target_fname=NULL, file_type=NULL, 
  resamp_res, sphere_original_fname, sphere_target_fname, 
  overwrite=TRUE, read_dir=NULL, write_dir=NULL, 
  sphere_target_dir=NULL, wb_path=NULL) {

  wb_cmd <- get_wb_cmd_path(wb_path)

  # Check arguments.
  original_fname <- format_path(original_fname, read_dir, mode=4)
  stopifnot(file.exists(original_fname))
  do_ROI <- !is.null(ROIcortex_original_fname)
  if (do_ROI) {
    ROIcortex_original_fname <- format_path(
      ROIcortex_original_fname, read_dir, mode=4)
    stopifnot(file.exists(ROIcortex_original_fname))
  }
  target_fname <- format_path(target_fname, write_dir)
  if (do_ROI) {
    validROIcortex_target_fname <- format_path(
      validROIcortex_target_fname, read_dir, mode=4)
  }
  if (is.null(file_type)) {
    if (grepl("func.gii", original_fname, fixed=TRUE)) { 
      file_type <- "metric" 
    } else if (grepl("surf.gii", original_fname, fixed=TRUE)) { 
      file_type <- "surface" 
    } else { 
      stop(paste(
        "Could not infer file type of ", original_fname, 
        ". Please set the file_type argument."
      )) 
    }
  }
  file_type <- match.arg(file_type, c("metric", "surface"))
  stopifnot(is.numeric(resamp_res))
  stopifnot(resamp_res > 0)
  sphere_original_fname <- format_path(sphere_original_fname)
  stopifnot(file.exists(sphere_original_fname))
  if (!is.null(sphere_target_dir)) {
    if (!dir.exists(sphere_target_dir)) {dir.create(sphere_target_dir)}
    sphere_target_fname <- format_path(
      sphere_target_fname, sphere_target_dir, mode=4)
  }
  stopifnot(file.exists(sphere_target_fname))
  stopifnot(is.logical(overwrite))
  if (do_ROI & file_type=="surface") { 
    stop("do_ROI AND file_type=='surface', but surface files do not use ROI.") 
  }

  # Run the command if overwrite==TRUE, or if any desired file does not exist.
  resampled_files_exist <- all(c(file.exists(
    target_fname, 
    ifelse(do_ROI, file.exists(validROIcortex_target_fname), TRUE)
  )))
  run_cmd <- overwrite | !resampled_files_exist
  if (!run_cmd) {
    cmd_code <- NA
  } else {
    cmd_name <- switch(file_type,
      metric="-metric-resample",
      surface="-surface-resample"
    )

    cmd <- paste(
      sys_path(wb_cmd), cmd_name, 
      sys_path(original_fname), sys_path(sphere_original_fname), 
      sys_path(sphere_target_fname), "BARYCENTRIC", sys_path(target_fname)
    )
    if (do_ROI) {
      cmd <- paste(
        cmd, "-current-roi", sys_path(ROIcortex_original_fname), 
        "-valid-roi-out", sys_path(validROIcortex_target_fname)
      )
    }
    # Run it! Raise an error if it fails.
    cmd_code <- system(cmd)
    if (cmd_code != 0) {
      stop(paste0(
        "The Connectome Workbench command failed with code ", cmd_code, 
        ". The command was:\n", cmd
      ))
    }
  }

  invisible(cmd_code)
}

#' Resample a metric GIFTI file (ends with "func.gii")
#'
#' @param ... Arguments to \code{\link{gifti_resample}}. All except 
#'  \code{file_type} (which is "metric") can be provided.
#'
#' @return Logical indicating whether resampled file was created.
#' @export
#'
metric_resample <- function(...) {
  # Check that the arguments are valid.
  kwargs_allowed <- c("", get_kwargs(ciftiTools::gifti_resample))
  kwargs <- names(list(...))
  if ("file_type" %in% kwargs) { 
    stop(paste(
      "file_type==\"metric\" for metric_resample and therefore",
      "should not be provided as an argument."
    ))
  }
  stopifnot(all(kwargs %in% kwargs_allowed))

  gifti_resample(..., file_type="metric")
}

#' Resample a surface GIFTI file
#'
#' @param ... Arguments to \code{\link{gifti_resample}}. All except 
#'  \code{file_type} (which is "surface") can be provided.
#'
#' @return Logical indicating whether resampled file was created.
#' @export
#'
surface_resample <- function(...) {
  # Check that the arguments are valid.
  kwargs_allowed <- c("", get_kwargs(ciftiTools::gifti_resample))
  kwargs <- names(list(...))
  if ("file_type" %in% kwargs) { 
    stop(paste(
      "file_type==\"surface\" for surface_resample and therefore",
      "should not be provided as an argument."
    ))
  }
  stopifnot(all(kwargs %in% kwargs_allowed))

  gifti_resample(..., file_type="surface")
}

#' Generate GIFTI sphere surface files
#'
#' @description This function generates a pair of GIFTI vertex-matched left and 
#'  right spheres in the target resolution. These are required for resampling 
#'  CIFTI and GIFTI files. 
#'
#' @param sphereL_fname File path to left-hemisphere spherical GIFTI to be 
#'  created
#' @param sphereR_fname File path to right-hemisphere spherical GIFTI to be 
#'  created
#' @inheritParams resamp_res_Param
#' @param write_dir (Optional) directory to place the sphere files in. If
#'  \code{NULL} (default), do not append any directory to the sphere file paths.
#' @param overwrite If both spheres already exist, should they be written over? 
#'  Default: \code{FALSE} (skip execution if both exist).
#' @inheritParams wb_path_Param
#'
#' @return Logical indicating whether output files exist. Or, \code{NA} if the 
#'  command was skipped because both already existed and \code{overwrite==FALSE}.
#' @export
#'
make_helper_spheres <- function(
  sphereL_fname, sphereR_fname, resamp_res, write_dir=NULL, 
  overwrite=FALSE, wb_path=NULL) {

  wb_cmd <- get_wb_cmd_path(wb_path)

  sphereL_fname <- format_path(sphereL_fname, write_dir, mode=2)
  sphereR_fname <- format_path(sphereR_fname, write_dir, mode=2)

  if (file.exists(sphereL_fname) & file.exists(sphereR_fname) & !overwrite) { 
    return(invisible(NA)) 
  }

  system(paste(
    sys_path(wb_cmd), "-surface-create-sphere", 
    resamp_res, sys_path(sphereL_fname), 
  sep=" "))
  system(paste(
    sys_path(wb_cmd), "-surface-flip-lr", 
    sys_path(sphereL_fname), sys_path(sphereR_fname), 
  sep=" "))
  system(paste(
    sys_path(wb_cmd), "-set-structure", 
    sys_path(sphereL_fname), "CORTEX_LEFT", 
  sep=" "))
  system(paste(
    sys_path(wb_cmd), "-set-structure", 
    sys_path(sphereR_fname), "CORTEX_RIGHT", 
  sep=" "))

  invisible(file.exists(sphereL_fname) & file.exists(sphereR_fname))
}