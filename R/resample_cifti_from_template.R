#' Resample a CIFTI from a template
#'
#' Resample a CIFTI from a template CIFTI using the \code{-cifti-resample} 
#'  Connectome Workbench command.
#' 
#' @inheritSection Connectome_Workbench_Description Connectome Workbench Requirement
#' 
#' @param original_fname A CIFTI file to resample.
#' @param template_fname A CIFTI file to use as the template.
#' @param target_fname The file name to save the resampled CIFTI.
#'
#' @return The \code{target_fname}, invisibly
#' 
#' @export
#'
resample_cifti_from_template <- function(
  original_fname, template_fname, target_fname){
  
  # Check brainstructures.
  original_info <- info_cifti(original_fname)
  brainstructures <- original_info$cifti$brainstructures
  template_info <- info_cifti(template_fname)
  template_brainstructures <- template_info$cifti$brainstructures  
  for (b in brainstructures) {
    if (!(b %in% template_brainstructures)) {
      stop(paste(
        "The", b, 
        "brainstructure is in the original CIFTI but not in the template."
      ))
    }
  }

  # Build the Connectome Workbench command. 
  cmd <- paste(
    "-cifti-resample", 
    sys_path(original_fname),
    "COLUMN",
    sys_path(template_fname),
    "COLUMN", 
    "BARYCENTRIC",
    "CUBIC",
    sys_path(target_fname)
  )

  # Cortical spheres.
  if ("left" %in% brainstructures || "right" %in% brainstructures) {
    if (!("left" %in% brainstructures)) {
      original_res <- length(original_info$cortex$medial_wall_mask$left)
    } else {
      original_res <- length(original_info$cortex$medial_wall_mask$right)
    }
    # [TO DO]: Handle this case i.e. dlabels
    if (original_res == 0) { 
      stop("Could not infer original CIFTI's resolution from its metadata.")
    }

    if (!("left" %in% template_brainstructures)) {
      resamp_res <- length(template_info$cortex$medial_wall_mask$left)
    } else {
      resamp_res <- length(template_info$cortex$medial_wall_mask$right)
    }
    # [TO DO]: Handle this case i.e. dlabels
    if (original_res == 0) { 
      stop("Could not infer template CIFTI's resolution from its metadata.")
    }

    tdir <- tempdir()
    # Create original sphere.
    sphereL_original_fname <- format_path(paste0("sphereL_", original_res, ".surf.gii"), tdir, mode=2)
    sphereR_original_fname <- format_path(paste0("sphereR_", original_res, ".surf.gii"), tdir, mode=2)
    write_spheres(sphereL_original_fname, sphereR_original_fname, original_res)
    # Create target sphere.
    sphereL_target_fname <- format_path(paste0("sphereL_", resamp_res, ".surf.gii"), tdir, mode=2)
    sphereR_target_fname <- format_path(paste0("sphereR_", resamp_res, ".surf.gii"), tdir, mode=2)
    write_spheres(sphereL_target_fname, sphereR_target_fname, resamp_res)

    if ("left" %in% brainstructures) {
      cmd <- paste(cmd, "-left-spheres", sphereL_original_fname, sphereL_target_fname)
    }
    if ("right" %in% brainstructures) {
      cmd <- paste(cmd, "-right-spheres", sphereR_original_fname, sphereR_target_fname)
    }
  }

  # Run command.
  run_wb_cmd(cmd)

  invisible(target_fname)
}