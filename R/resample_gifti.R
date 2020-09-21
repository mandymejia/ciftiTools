#' Resample a GIFTI file (with its ROI)
#'
#' Performs spatial resampling of GIFTI data on the cortical surface.
#'
#' @param original_fname The GIFTI file to resample.
#' @param target_fname Where to save the resampled file.
#' @param hemisphere "left" (default) or "right". An error will be raised if
#'  the hemisphere indicated in the GIFTI metadata does not match.
#' @param file_type \code{"metric"}, \code{"label"}, \code{"surface"}, or \code{NULL} 
#'  (default) to infer from \code{original_fname}.
#' @param original_res The resolution of the original file. If \code{NULL}
#'  (default), infer from the file.
#' @inheritParams resamp_res_Param_required
#' @param ROIcortex_original_fname The name of the ROI file corresponding to 
#'  \code{original_fname}. Leave as \code{NULL} (default) if this doesn't exist
#'  or shouldn't be resampled.
#' @param ROIcortex_target_fname The name of the resampled ROI file. Only 
#'  applicable if \code{ROIcortex_original_fname} is provided.
#' @param read_dir Directory to append to the path of every file name in
#'  \code{original_fname} and \code{ROIcortex_original_fname}. If \code{NULL} 
#'  (default), do not append any directory to the path. 
#' @param write_dir Directory to append to the path of every file name in
#'  \code{target_fname} and \code{ROIcortex_target_fname}. If \code{NULL} 
#'  (default), do not append any directory to the path. 
#' @inheritParams wb_path_Param
#'
#' @return The resampled GIFTI file name, invisibly
#'
#' @importFrom gifti readgii
#'
#' @export
#'
resample_gifti <- function(
  original_fname, target_fname, hemisphere=c("left", "right"),
  file_type=NULL, original_res=NULL, resamp_res,
  ROIcortex_original_fname=NULL, ROIcortex_target_fname=NULL,
  read_dir=NULL, write_dir=NULL, wb_path=NULL) {

  # ----------------------------------------------------------------------------
  # Check arguments. -----------------------------------------------------------
  # ----------------------------------------------------------------------------

  # File type
  if (is.null(file_type)) {
    if (grepl("func.gii", original_fname, fixed=TRUE)) { 
      file_type <- "metric" 
    } else if (grepl("label.gii", original_fname, fixed=TRUE)) {
      file_type <- "label"
    } else if (grepl("surf.gii", original_fname, fixed=TRUE)) { 
      file_type <- "surface" 
    } else { 
      stop(paste(
        "Could not infer file type of ", original_fname, 
        ". Please set the file_type argument."
      )) 
    }
  }
  file_type <- match.arg(file_type, c("metric", "label", "surface"))

  # Original & target file names
  original_fname <- format_path(original_fname, read_dir, mode=4)
  stopifnot(file.exists(original_fname))
  target_fname <- format_path(target_fname, write_dir, mode=2)

  # Hemisphere
  hemisphere <- match.arg(hemisphere, c("left", "right")) 
  if (file_type == "surface") {
    surf <- make_surf(original_fname, hemisphere)
  }

  # Original ROI & target ROI file names
  do_ROI <- !is.null(ROIcortex_original_fname)
  if (do_ROI) {
    ROIcortex_original_fname <- format_path(
      ROIcortex_original_fname, read_dir, mode=4
    )
    stopifnot(file.exists(ROIcortex_original_fname))
    if (is.null(ROIcortex_target_fname)) { 
      ROIcortex_target_fname <- cifti_component_suffix(
        paste0("ROIcortex", switch(hemisphere, left="L", right="R")), 
        switch(file_type, metric="func", label="func", surface="surf")
      ) 
    }
    ROIcortex_target_fname <- format_path(
      ROIcortex_target_fname, write_dir, mode=2
    )
  }

  # Resolution
  if (is.null(original_res)) {
    gii <- readgii(original_fname)
    original_res <- switch(file_type,
      metric = length(gii$data),
      label = length(gii$data),
      surface = nrow(gii$data$pointset)
    )
  } else {
    stopifnot(is.numeric(original_res) && original_res > 0)
  }
  stopifnot(is.numeric(resamp_res) && resamp_res > 0)

  # Compatible args
  if (do_ROI & file_type=="surface") { 
    stop("do_ROI AND file_type=='surface', but surface files do not use ROI.") 
  }

  # ----------------------------------------------------------------------------
  # Spheres. -------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  tdir <- tempdir()

  sphereL_original_fname <- format_path(paste0("sphereL_", original_res, ".surf.gii"), tdir, mode=2)
  sphereR_original_fname <- format_path(paste0("sphereR_", original_res, ".surf.gii"), tdir, mode=2)
  write_spheres(sphereL_original_fname, sphereR_original_fname, original_res)

  sphereL_target_fname <- format_path(paste0("sphereL_", resamp_res, ".surf.gii"), tdir, mode=2)
  sphereR_target_fname <- format_path(paste0("sphereR_", resamp_res, ".surf.gii"), tdir, mode=2)
  write_spheres(sphereL_target_fname, sphereR_target_fname, resamp_res)

  sphere_original_fname <- switch(hemisphere,
    left = sphereL_original_fname,
    right = sphereR_original_fname
  )
  sphere_target_fname <- switch(hemisphere,
    left = sphereL_target_fname, 
    right = sphereR_target_fname
  )
  # ----------------------------------------------------------------------------
  # Make and run command. ------------------------------------------------------
  # ----------------------------------------------------------------------------

  cmd_name <- switch(file_type,
    metric="-metric-resample",
    label="-label-resample",
    surface="-surface-resample"
  )

  cmd <- paste(
    cmd_name, 
    sys_path(original_fname), sys_path(sphere_original_fname), 
    sys_path(sphere_target_fname), "BARYCENTRIC", sys_path(target_fname)
  )
  if (do_ROI) {
    cmd <- paste(
      cmd, 
      "-current-roi", sys_path(ROIcortex_original_fname), 
      "-valid-roi-out", sys_path(ROIcortex_target_fname)
    )
  }
  run_wb_cmd(cmd, wb_path)

  if (do_ROI) {
    out <- c(target_fname, ROIcortex_target_fname)
    names(out) <- c(file_type, "ROI")
  } else {
    out <- target_fname
    names(out) <- file_type
  }
  invisible(out)
}

#' Generate GIFTI sphere surface files
#'
#' This function generates a pair of GIFTI vertex-matched left and right spheres
#'  in the target resolution. These are required for resampling CIFTI and GIFTI 
#'  files. 
#'
#' @param sphereL_fname File path to left-hemisphere spherical GIFTI to be 
#'  created
#' @param sphereR_fname File path to right-hemisphere spherical GIFTI to be 
#'  created
#' @inheritParams resamp_res_Param_required
#' @param write_dir (Optional) directory to place the sphere files in. If
#'  \code{NULL} (default), do not append any directory to the sphere file paths.
#' @inheritParams wb_path_Param
#'
#' @return The names of the written sphere files, invisibly
#' @keywords internal
#'
write_spheres <- function(
  sphereL_fname, sphereR_fname, resamp_res, 
  write_dir=NULL, wb_path=NULL) {

  sphereL_fname <- format_path(sphereL_fname, write_dir, mode=2)
  sphereR_fname <- format_path(sphereR_fname, write_dir, mode=2)

  run_wb_cmd(
    paste("-surface-create-sphere", resamp_res, sys_path(sphereL_fname)), 
    wb_path
  )
  run_wb_cmd(
    paste("-surface-flip-lr", sys_path(sphereL_fname), sys_path(sphereR_fname)), 
    wb_path
  )
  run_wb_cmd(
    paste("-set-structure", sys_path(sphereL_fname), "CORTEX_LEFT"), 
    wb_path
  )
  run_wb_cmd(
    paste("-set-structure", sys_path(sphereR_fname), "CORTEX_RIGHT"), 
    wb_path
  )

  invisible(list(sphereL_fname=sphereL_fname, sphereR_fname=sphereR_fname))
}

#' @rdname resample_gifti
#' @export
resampleGIfTI <- function(
  original_fname, target_fname, hemisphere,
  file_type=NULL, original_res=NULL, resamp_res,
  ROIcortex_original_fname=NULL, ROIcortex_target_fname=NULL,
  read_dir=NULL, write_dir=NULL, wb_path=NULL){

  resample_gifti(
    original_fname, target_fname, hemisphere,
    file_type=NULL, original_res=NULL, resamp_res,
    ROIcortex_original_fname=NULL, ROIcortex_target_fname=NULL,
    read_dir=NULL, write_dir=NULL, wb_path=NULL
  )
}

#' @rdname resample_gifti
#' @export
resamplegii <- function(
  original_fname, target_fname, hemisphere,
  file_type=NULL, original_res=NULL, resamp_res,
  ROIcortex_original_fname=NULL, ROIcortex_target_fname=NULL,
  read_dir=NULL, write_dir=NULL, wb_path=NULL){

  resample_gifti(
    original_fname, target_fname, hemisphere,
    file_type=NULL, original_res=NULL, resamp_res,
    ROIcortex_original_fname=NULL, ROIcortex_target_fname=NULL,
    read_dir=NULL, write_dir=NULL, wb_path=NULL
  )
}