#' Resample CIFTI data
#'
#' @description Performs spatial resampling of CIFTI data on the cortical surface
#'
#' @param cifti_original_fname A CIFTI file to resample.
#' @param cifti_target_fname The file name to save the resampled CIFTI.
#' @param surfL_original_fname,surfR_original_fname (Optional) File path of 
#'  existing GIFTI surface geometry file representing the left/right cortex. 
#'  One or both can be provided.
#' @param surfL_target_fname,surfR_target_fname (Optional) File path for
#'  the resampled GIFTI surface geometry file representing the left/right 
#'  cortex. If NULL (default),
#' @inheritParams resamp_res_Param_required
#' @inheritParams sphereL_fname_Param
#' @inheritParams sphereR_fname_Param
#' @inheritParams sep_fnames_Param
#' @inheritParams sep_keep_Param
#' @inheritParams resamp_fnames_Param
#' @inheritParams resamp_keep_Param
#' @inheritParams write_dir_Param_intermediate
#' @inheritParams verbose_Param
#' @inheritParams wb_path_Param
#'
#' @return The code returned by the Connectome Workbench command
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
resample_cifti <- function(
  cifti_original_fname, cifti_target_fname, 
  surfL_original_fname=NULL, surfR_original_fname=NULL,
  surfL_target_fname=NULL, surfR_target_fname=NULL,
  resamp_res, sphereL_fname, sphereR_fname,
  sep_fnames=NULL, sep_keep=FALSE, #separate_cifti
  resamp_fnames=NULL, resamp_keep=FALSE, # resample_cifti
  write_dir=NULL, verbose=TRUE, wb_path=NULL) {

  wb_cmd <- get_wb_cmd_path(wb_path)

  # ----------------------------------------------------------------------------
  # Setup ----------------------------------------------------------------------
  # ----------------------------------------------------------------------------

  brainstructures <- ROI_brainstructures <- c("left","right","subcortical")

  # [TO DO]: consider auto-generating cifti_target_fname & make it optional
  cifti_target_fname <- format_path(cifti_target_fname, write_dir, mode=2)

  if (is.null(write_dir)) {
    write_dir_sep <- ifelse(sep_keep, getwd(), tempdir())
    write_dir_resamp <- ifelse(resamp_keep, getwd(), tempdir())
  }

  if (verbose) { exec_time <- Sys.time() }

  # ----------------------------------------------------------------------------
  # separate_cifti() -----------------------------------------------------------
  # ----------------------------------------------------------------------------

  if (verbose) { cat("Separating CIFTI file.\n") }

  sep_result <- separate_cifti_wrapper(
    cifti_fname=cifti_original_fname, 
    brainstructures=brainstructures, ROI_brainstructures=ROI_brainstructures,
    sep_fnames=sep_fnames, wb_path=wb_path
  )

  to_cif <- sep_result$fname
  names(to_cif) <- sep_result$label

  if (verbose) { 
    print(Sys.time() - exec_time)
    exec_time <- Sys.time()
  }

  # ----------------------------------------------------------------------------
  # resample_cifti_separate() --------------------------------------------------
  # ----------------------------------------------------------------------------

  if (verbose) { cat("Resampling CIFTI file.\n") }

  # Do not resample the subcortical data.
  to_resample <- to_cif[!grepl("subcort", names(to_cif))]

    # Do resample_cifti_separate.
    resamp_result <- resample_cifti_wrapper(
      resamp_res, to_resample, resamp_fnames, resamp_keep, 
      surfL_fname, surfR_fname,
      sphereL_fname, sphereR_fname, 
      wb_path
    )
  # Replace resampled files.
  to_cif_resampled <- names(to_read)[names(to_read) %in% resamp_result$label]
  to_read[to_cif_resampled] <- resamp_result$fname[
    resamp_result$label %in% to_cif_resampled]

  if (!is.null(surfL_fname)) { 
    surfL_fname <- resamp_result$fname[resamp_result$label == "surfL"] 
  }
  if (!is.null(surfR_fname)) { 
    surfR_fname <- resamp_result$fname[resamp_result$label == "surfR"] 
  }

  if (verbose) { 
    print(Sys.time() - exec_time)
    exec_time <- Sys.time()
  }

  # ----------------------------------------------------------------------------
  # Resample with template -----------------------------------------------------
  # ----------------------------------------------------------------------------

  # Create a template CIFTI dense timeseries.
  if (verbose) cat("Creating template CIFTI file in target resolution... \n")
  cifti_template_fname <- format_path(
    paste0("template_", basename(cifti_original_fname)), tempdir(), mode=4)
  cifti_extn <- get_cifti_extn(cifti_original_fname)
  if (grepl("dtseries", cifti_extn)) create_cmd <- "-cifti-create-dense-timeseries"
  else if (grepl("dscalar", cifti_extn)) create_cmd <- "-cifti-create-dense-scalar"
  else if (grepl("dlabel", cifti_extn)) create_cmd <- "-cifti-create-label"
  else {
    stop(paste(
      "The data type of cifti_original_fname", cifti_original_fname, 
      "could not be determined. The file name should end in e.g. \
      \".dtseries.nii\""
    ))
  }
  system(paste(
    sys_path(wb_cmd), create_cmd, sys_path(cifti_template_fname), 
    "-volume", sys_path(to_cif["subcortVol"]), sys_path(to_cif["subcortLab"]), 
    "-left-metric", sys_path(to_cif["cortexL"]), 
      ifelse("ROIcortexL" %in% names(to_cif), 
        paste("-roi-left", sys_path(to_cif["ROIcortexL"])), 
        NULL
      ), 
    "-right-metric", sys_path(to_cif["cortexR"]), 
      ifelse("ROIcortexR" %in% names(to_cif), 
        paste("-roi-right", sys_path(to_cif["ROIcortexR"])), 
        NULL
      )
  ))

  if (verbose) { 
    print(Sys.time() - exec_time)
    exec_time <- Sys.time()
  }

  # Use the template to resample.
  if (verbose) cat("Resampling cifti_original_fname to target resolution... \n")
  
  sphereL_target_fname <- resamp_result$fname[resamp_result$label=="sphereL"]
  sphereR_target_fname <- resamp_result$fname[resamp_result$label=="sphereR"]
  stopifnot(all(file.exists(c(sphereL_target_fname, sphereR_target_fname))))
  cmd = paste(
    sys_path(wb_cmd), "-cifti-resample", sys_path(cifti_original_fname), 
    "COLUMN", sys_path(cifti_template_fname), 
    "COLUMN BARYCENTRIC CUBIC", sys_path(cifti_target_fname), 
    "-left-spheres", sys_path(sphereL_fname), sys_path(sphereL_target_fname), 
    "-right-spheres", sys_path(sphereR_fname), sys_path(sphereR_target_fname))
  cmd_code <- system(cmd)
  if (cmd_code != 0) {
    stop(paste0("The Connectome Workbench command failed with code ", cmd_code, 
      ". The command was:\n", cmd))
  }

  if (verbose) { 
    print(Sys.time() - exec_time)
    exec_time <- Sys.time()
  }

  file.remove(cifti_template_fname)

  # ----------------------------------------------------------------------------
  # Finish ---------------------------------------------------------------------
  # ----------------------------------------------------------------------------

  # Delete the separated files, unless otherwise requested. 
  #   Do not delete files that existed before.
  if (!sep_keep) {
    for(f in sep_result$fname[!(sep_result$existed)]) {
      file.remove(f)
      if (file.exists(paste0(f, ".data"))) {
        file.remove(paste0(f, ".data"))
      }
    }
  }

  # Same for resampled files.
  if (!resamp_keep) {
    for(f in resamp_result$fname[!(resamp_result$existed)]) {
      file.remove(f)
      if (file.exists(paste0(f, ".data"))) {
        file.remove(paste0(f, ".data"))
      }
    }
  }

  out <- list(cifti=cifti_target_fname, surfL=surfL_target_fname, surfR=surfR_target_fname)
  out[!sapply(out, is.null)]
}
