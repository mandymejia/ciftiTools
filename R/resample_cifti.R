#' Resample CIFTI Data
#'
#' @description Performs spatial resampling of CIFTI data on the cortical surface
#'  by separating it into GIfTI and NIfTI files 
#'  (\code{\link{separate_cifti}}), optionally resampling them 
#'  (\code{\link{resample_cifti_components}}), and then using the 
#'  \code{-cifti-resample} Workbench Command with a template.
#'
#' @param cifti_original_fname A CIFTI file to resample.
#' @param cifti_target_fname The file name to save the resampled CIFTI.
#' @param surfL_original_fname,surfR_original_fname (Optional) File path of 
#'  existing GIFTI surface geometry file representing the left/right cortex. 
#'  One or both can be provided.
#' @param surfL_target_fname,surfR_target_fname (Optional) File path for
#'  the resampled GIFTI surface geometry file representing the left/right 
#'  cortex. If NULL (default), will use default names: see 
#'  \code{resample_cifti_default_fname}.
#' @inheritParams resamp_res_Param_required
#' @inheritParams sphereL_fname_Param
#' @inheritParams sphereR_fname_Param
#' @inheritParams sep_fnames_Param
#' @inheritParams sep_keep_Param
#' @inheritParams resamp_fnames_Param
#' @inheritParams resamp_keep_Param
#' @inheritParams write_dir_Param_intermediate
#' @inheritParams verbose_Param_TRUE
#' @inheritParams wb_path_Param
#'
#' @return A list of output files written. The elements are "cifti" and
#'  potentially "surfL" (if \code{surfL_original_fname} was provided) and 
#'  "surfR" (if \code{surfR_original_fname} was provided).
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
  sep_keep=FALSE, sep_fnames=NULL, #separate_cifti
  resamp_keep=FALSE, resamp_fnames=NULL, # resample_cifti
  write_dir=NULL, verbose=TRUE, wb_path=NULL) {

  # ----------------------------------------------------------------------------
  # Setup ----------------------------------------------------------------------
  # ----------------------------------------------------------------------------

  wb_cmd <- get_wb_cmd_path(wb_path)

  # [TO DO]: more extensive preliminary check.
  if (!is.null(resamp_res)) {
    if (is.null(sphereL_fname) | is.null(sphereR_fname)) {
      stop("`sphereL_fname` and `sphereR_fname` are required for resampling.")
    }
  }

  if (sep_keep) { 
    write_dir_sep <- write_dir 
  } else { 
    write_dir_sep <- tempdir()
  }
  if (resamp_keep) { 
    write_dir_resamp <- write_dir 
  } else { 
    write_dir_resamp <- tempdir() 
  }


  brainstructures <- ROI_brainstructures <- c("left","right","subcortical")

  # [TO DO]: consider auto-generating cifti_target_fname & make it optional
  cifti_target_fname <- format_path(cifti_target_fname, write_dir, mode=2)

  if (verbose) { exec_time <- Sys.time() }

  # ----------------------------------------------------------------------------
  # separate_cifti() -----------------------------------------------------------
  # ----------------------------------------------------------------------------

  if (verbose) { cat("Separating CIFTI file.\n") }

  sep_result <- separate_cifti_wrapper(
    cifti_fname=cifti_original_fname, 
    brainstructures=brainstructures, ROI_brainstructures=ROI_brainstructures,
    sep_fnames=sep_fnames, write_dir=write_dir_sep, wb_path=wb_path
  )

  to_cif <- sep_result$fname
  names(to_cif) <- sep_result$label

  if (verbose) { 
    print(Sys.time() - exec_time)
    exec_time <- Sys.time()
  }

  # ----------------------------------------------------------------------------
  # resample_cifti_components() --------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Do not resample the subcortical data.
  to_resample <- to_cif[!grepl("subcort", names(to_cif))]
  if (verbose) { cat("Resampling CIFTI file.\n") }

  # Do resample_cifti_components.
  resamp_result <- resample_cifti_wrapper(
    resamp_res=resamp_res, original_fnames=to_resample, 
    resamp_fnames=resamp_fnames, 
    sphereL_fname=sphereL_fname, sphereR_fname=sphereR_fname, 
    surfL_fname=surfL_original_fname, surfR_fname=surfR_original_fname,
    surfL_target_fname=surfL_target_fname, 
    surfR_target_fname=surfR_target_fname,
    read_dir=NULL, write_dir=write_dir_resamp, wb_path=wb_path
  )

  # Replace resampled files.
  to_cif_resampled <- names(to_cif)[names(to_cif) %in% resamp_result$label]
  to_cif[to_cif_resampled] <- resamp_result$fname[
    resamp_result$label %in% to_cif_resampled]

  # [TO DO]: is it safe to copy these files when write_dir=tempdir()?
  if (!is.null(surfL_original_fname)) { 
    surfL_target_fname_old <- resamp_result$fname[resamp_result$label == "surfL"]
    surfL_target_fname <- format_path(basename(surfL_target_fname_old), write_dir, mode=2)
    file.copy(surfL_target_fname_old, surfL_target_fname)
  }
  if (!is.null(surfR_original_fname)) { 
    surfR_target_fname_old <- resamp_result$fname[resamp_result$label == "surfR"]
    surfR_target_fname <- format_path(basename(surfR_target_fname_old), write_dir, mode=2)
    file.copy(surfR_target_fname_old, surfR_target_fname)
  }

  # [TO DO]: is it safe to recycle the target spheres? in future, just make new.
  sphereL_target_fname <- resamp_result$fname[resamp_result$label == "sphereL"]
  sphereR_target_fname <- resamp_result$fname[resamp_result$label == "sphereR"]

  if (verbose) { 
    print(Sys.time() - exec_time)
    exec_time <- Sys.time()
  }

  # ----------------------------------------------------------------------------
  # Resample with template -----------------------------------------------------
  # ----------------------------------------------------------------------------

  # [TO DO]: move to separate function

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

  cmd <- paste(
    sys_path(wb_cmd), create_cmd, sys_path(cifti_template_fname), 
    "-volume", sys_path(to_cif["subcortVol"]), sys_path(to_cif["subcortLab"]), 
    "-left-metric", sys_path(to_cif["cortexL"])
  )
  if ("ROIcortexL" %in% names(to_cif)) {
    cmd <- paste(cmd, "-roi-left", sys_path(to_cif["ROIcortexL"]))
  }
  cmd <- paste(cmd, "-right-metric", sys_path(to_cif["cortexR"]) )
  if ("ROIcortexR" %in% names(to_cif)) {
    cmd <- paste(cmd, "-roi-right", sys_path(to_cif["ROIcortexR"]))
  }
  cmd_code <- system(cmd)
  if (cmd_code != 0) {
    stop(paste0(
      "The Connectome Workbench command failed with code ", cmd_code, 
      ". The command was:\n", cmd
    ))
  }

  if (verbose) { 
    print(Sys.time() - exec_time)
    exec_time <- Sys.time()
  }

  # Use the template to resample.
  if (verbose) cat("Resampling cifti_original_fname to target resolution... \n")
  
  stopifnot(file.exists(cifti_template_fname))
  stopifnot(all(file.exists(c(sphereL_fname, sphereR_fname))))
  cmd = paste(
    sys_path(wb_cmd), "-cifti-resample", sys_path(cifti_original_fname), 
    "COLUMN", sys_path(cifti_template_fname), 
    "COLUMN BARYCENTRIC CUBIC", sys_path(cifti_target_fname), 
    "-left-spheres", sys_path(sphereL_fname), 
    sys_path(sphereL_target_fname), 
    "-right-spheres", sys_path(sphereR_fname), 
    sys_path(sphereL_target_fname)
  )
  cmd_code <- system(cmd)
  if (cmd_code != 0) {
    stop(paste0("The Connectome Workbench command failed with code ", cmd_code, 
      ". The command was:\n", cmd))
  }

  if (verbose) { 
    print(Sys.time() - exec_time)
    exec_time <- Sys.time()
  }

  # [TO DO]: need to delete?
  # file.remove(cifti_template_fname)

  # ----------------------------------------------------------------------------
  # Finish ---------------------------------------------------------------------
  # ----------------------------------------------------------------------------

  # [TO DO] Is this necessary? how do temporary directories work?
  #file.remove(sphereL_target_fname)
  #file.remove(sphereR_target_fname)

  # # Delete the separated files, unless otherwise requested. 
  # if (!sep_keep) {
  #   for(f in sep_result$fname) {
  #     file.remove(f)
  #     if (file.exists(paste0(f, ".data"))) {
  #       file.remove(paste0(f, ".data"))
  #     }
  #   }
  # }

  # # Same for resampled files.
  # if (!resamp_keep) {
  #   for(f in resamp_result$fname) {
  #     file.remove(f)
  #     if (file.exists(paste0(f, ".data"))) {
  #       file.remove(paste0(f, ".data"))
  #     }
  #   }
  # }

  out <- list(
    cifti=cifti_target_fname, 
    surfL=surfL_target_fname, surfR=surfR_target_fname
  )
  out[!sapply(out, is.null)]
}

#' @rdname resample_cifti
#' @export
resampleCIfTI <- resamplecii <- function(
  cifti_original_fname, cifti_target_fname, 
  surfL_original_fname=NULL, surfR_original_fname=NULL,
  surfL_target_fname=NULL, surfR_target_fname=NULL,
  resamp_res, sphereL_fname, sphereR_fname,
  sep_keep=FALSE, sep_fnames=NULL, #separate_cifti
  resamp_keep=FALSE, resamp_fnames=NULL, # resample_cifti
  write_dir=NULL, verbose=TRUE, wb_path=NULL) {

  resample_cifti(
    cifti_original_fname, cifti_target_fname, 
    surfL_original_fname, surfR_original_fname,
    surfL_target_fname, surfR_target_fname,
    resamp_res, sphereL_fname, sphereR_fname,
    sep_keep, sep_fnames, #separate_cifti
    resamp_keep, resamp_fnames, # resample_cifti
    write_dir, verbose, wb_path
  ) 
}