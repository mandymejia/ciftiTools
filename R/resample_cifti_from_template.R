#' Resample a CIFTI from a template
#'
#' @description Resample a CIFTI from a template. This uses the
#'  \code{-cifti-resample} command from Connectome Workbench.
#'
#' @param cifti_original_fname A CIFTI file to resample.
#' @param cifti_template_fname A CIFTI file to use as the template.
#' @param cifti_target_fname The file name to save the resampled CIFTI.
#' @inheritParams wb_path_Param
#'
#' @return Whether the CIFTI was successfully resampled
#'
#' @export
#'
#' @details This function uses a system wrapper for the "wb_command"
#'  executable. The user must first download and install the Connectome 
#'  Workbench, available from 
#'  \url{https://www.humanconnectome.org/software/get-connectome-workbench}. 
#'  The \code{wb_path} argument is the path to the Connectime Workbench folder or
#'  executable.
#'
resample_cifti_from_template <- function(
  cifti_original_fname, cifti_template_fname, cifti_target_fname,
  wb_path=NULL){
  
  # Check brainstructures.
  original_info <- info_cifti(cifti_original_fname, wb_path)
  brainstructures <- original_info$cifti$brainstructures
  template_info <- info_cifti(cifti_template_fname, wb_path)
  template_brainstructures <- template_info$cifti$brainstructures  
  for (b in brainstructures) {
    if (!(b %in% template_brainstructures)) {
      stop(paste("The", b, "brainstructure is in the original CIFTI but not in the template."))
    }
  }

  # Build the Connectome Workbench command. 
  cmd <- paste(
    "-cifti-resample", 
    sys_path(cifti_original_fname),
    "COLUMN",
    sys_path(cifti_template_fname),
    "COLUMN", 
    "BARYCENTRIC",
    "CUBIC",
    sys_path(cifti_target_fname)
  )

  # Cortical spheres.
  if ("left" %in% brainstructures || "right" %in% brainstructures) {
    if (!("left" %in% brainstructures)) {
      original_res <- length(original_info$cortex$medial_wall_mask$left)
    } else {
      original_res <- length(original_info$cortex$medial_wall_mask$right)
    }

    if (!("left" %in% template_brainstructures)) {
      resamp_res <- length(template_info$cortex$medial_wall_mask$left)
    } else {
      resamp_res <- length(template_info$cortex$medial_wall_mask$right)
    }

    tdir <- tempdir()
    # Create original sphere.
    sphereL_original_fname <- format_path(paste0("sphereL_", original_res, ".surf.gii"), tdir, mode=2)
    sphereR_original_fname <- format_path(paste0("sphereR_", original_res, ".surf.gii"), tdir, mode=2)
    make_helper_spheres(sphereL_original_fname, sphereR_original_fname, original_res)
    # Create target sphere.
    sphereL_target_fname <- format_path(paste0("sphereL_", resamp_res, ".surf.gii"), tdir, mode=2)
    sphereR_target_fname <- format_path(paste0("sphereR_", resamp_res, ".surf.gii"), tdir, mode=2)
    make_helper_spheres(sphereL_target_fname, sphereR_target_fname, resamp_res)

    if ("left" %in% brainstructures) {
      cmd <- paste(cmd, "-left-spheres", sphereL_original_fname, sphereL_target_fname)
    }
    if ("right" %in% brainstructures) {
      cmd <- paste(cmd, "-right-spheres", sphereR_original_fname, sphereR_target_fname)
    }
  }

  # Run command.
  run_wb_cmd(cmd, wb_path)
}