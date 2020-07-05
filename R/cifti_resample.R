
#' Resample CIFTI data
#'
#' @description Performs spatial resampling of CIFTI data on the cortical surface
#'
#' @param cifti_original_fname A CIFTI file to resample.
#' @param cifti_target_fname Where to save the resampled CIFTI file.
#' @param surfL_original_fname,surfR_original_fname (Optional) File path, or vector of multiple file paths, of GIFTI surface geometry file 
#'  representing left/right cortex to resample.
#' @param surfL_target_fname,surfR_target_fname (Optional) File path, or vector of multiple file paths, of GIFTI surface geometry file 
#'  representing left/right cortex to save the resampled data as. Each should correspond to \code{surf[L/R]_original_fname}.
#' @param res_target Target resolution (number of cortical surface vertices per hemisphere)
#' @param sphereL_original_fname,sphereR_original_fname File path of [left/right]-hemisphere spherical GIFTI files in original resolution 
#'  (compatible with cifti_orig) .
#' @param sphereL_target_fname,sphereR_target_fname File path of [left/right]-hemisphere spherical GIFTI files in targetinal resolution 
#'  (compatible with cifti_Target) . NULL (default) will be named as "Resampled_[res_target]_[basename(sphere[L/R]_original_fname)]"
#' @param sphere_target_keep Should helper files be deleted at the end of this function call, if they were created? Default is FALSE.
#' @param sphere_target_overwrite Logical indicating whether sphere[L/R]_target_fname should be overwritten if it already exists. Default is TRUE.
#' @param overwrite Logical indicating whether each target file should be overwritten if it already exists.
#' @param read_dir If the file names of \code{cifti_original_fname}, \code{surfL_original_fname}, or \code{surfR_original_fname} are relative, this is the 
#'  directory to look for them in. If NULL (default), use the current working directory. \code{read_dir} will not affect files specified 
#   with absolute paths.
#' @param write_dir If the file names of \code{cifti_target_fname} or surf[L/R]_original_fname are relative, this is the directory to look for them in.
#'  Defaults to the current working directory.
#' @param sphere_target_dir If \code{sphere_target_keep} and the file names of \code{c_original_fnames} are relative, this is the directory to 
#'  write and look for them in. The default is "./helper_files_resampling".
#' @param wb_path (Optional) Path to Connectome Workbench folder. If not provided, should be set with 
#'  \code{ciftiTools.setOption('wb_path', 'path/to/workbench')}.
#'
#' @return Logical indicating whether resampled CIFTI file was created.
#' @export
#'
#' @details Performs resampling of CIFTI files using Connectome Workbench tools.  Several helper files must be created:
#'
#' Step 1: Generate spheres in the target resolution (if not already existing and provided)
#' Step 2: Use -metric-resample to resample surface/cortex files into target resolution
#' Step 3: Use -surface-resample to resample the gifti files (if provided) into target resolution
cifti_resample_separate <- function(cifti_original_fname, cifti_target_fname=NULL, 
  surfL_original_fname=NULL, surfR_original_fname=NULL, surfL_target_fname=NULL, surfR_target_fname=NULL,
  res_target, 
  sphereL_original_fname, sphereR_original_fname, 
  sphereL_target_fname=NULL, sphereR_target_fname=NULL, 
  sphere_target_keep=FALSE, sphere_target_overwrite=TRUE,
  overwrite=TRUE, read_dir=NULL, write_dir=NULL, sphere_target_dir=NULL, wb_path=NULL){

  wb_cmd <- get_wb_cmd_path(wb_path)

  # Check Arguments
  ## c_original_fnames
  read_dir <- check_dir(read_dir)
  cifti_original_fname <- make_abs_path(cifti_original_fname, read_dir)
  stopifnot(file.exists(cifti_original_fname))
  ## c_target_fnames
  original_to_target_fname <- function(original_fname, res_target){
    bname <- basename(original_fname)
    paste("resampled", res_target, bname, sep="_")
  }
  write_dir <- check_dir(write_dir)
  if(is.null(c_target_fnames)){
    c_target_fnames <- lapply(c_original_fnames, original_to_target_fname, res_target)
  } else if(length(c_target_fnames)==1 & grepl("\\.d.*.nii$", c_target_fnames)){ # .d*.nii or .d*.nii where * is tseries or scalar
    c_target_fnames <- lapply(c_original_fnames, original_to_target_fname, res_target)
  } else {
    names(c_target_fnames) <- match.arg(names(c_target_fnames), possible_file_labels, several.ok=TRUE)
    stopifnot(length(unique(names(c_target_fnames))) == length(c_target_fnames))
    stopifnot(length(c_target_fnames) > 0)
  }
  for(i in 1:length(c_original_fnames)){
    lab_i <- names(c_original_fnames)[i]
    if(identical(c_target_fnames[[lab_i]], NULL)){ 
        c_target_fnames[[lab_i]] <- original_to_target_fname(c_original_fnames[[lab_i]], res_target)
    }
    c_target_fnames[[lab_i]] <- make_abs_path(c_target_fnames[[lab_i]], write_dir)
  }
  if(length(c_target_fnames) > length(c_original_fnames)){
    missing_original <- names(c_target_fnames)[!(names(c_target_fnames) %in% names(c_original_fnames))]
    warning(paste("Ignoring these resampling targets because their original files were not provided:", missing_original))
  }
  c_target_fnames <- c_target_fnames[names(c_original_fnames)]
  ## surfL
  if(!is.null(surfL_original_fname)){
    surfL_original_fname <- make_abs_path(surfL_original_fname, read_dir)
    if(!all(file.exists(surfL_original_fname))){
      stop(paste(c("This file(s) to resample does not exist:", surfL_original_fname[!file.exists(surfL_original_fname)]), collapse=" "))
    }
    if(is.null(surfL_target_fname)){
      surfL_target_fname <- original_to_target_fname(surfL_original_fname, res_target)
    }
    surfL_target_fname <- make_abs_path(surfL_target_fname, write_dir)
    stopifnot(length(surfL_original_fname) == length(surfL_target_fname))
  } else { surfL_target_fname <- "" }
  ## surfR
  if(!is.null(surfR_original_fname)){
    surfR_original_fname <- make_abs_path(surfR_original_fname, read_dir)
    if(!all(file.exists(surfR_original_fname))){
      stop(paste(c("This file(s) to resample does not exist:", surfR_original_fname[!file.exists(surfR_original_fname)]), collapse=" "))
    }
    if(is.null(surfR_target_fname)){
      surfR_target_fname <- original_to_target_fname(surfR_original_fname, res_target)
    }
    surfR_target_fname <- make_abs_path(surfR_target_fname, write_dir)
    stopifnot(length(surfR_original_fname) == length(surfR_target_fname))
  } else { surfR_target_fname <- "" }
  # other args
  sphere_target_dir <- check_dir(sphere_target_dir, "helper_files_resampling", make=TRUE)
  sphereL_original_fname <- make_abs_path(sphereL_original_fname)
  sphereR_original_fname <- make_abs_path(sphereR_original_fname)
  stopifnot(all(file.exists(c(sphereL_original_fname, sphereR_original_fname))))
  if(is.null(sphereL_target_fname)){ sphereL_target_fname <- original_to_target_fname(sphereL_original_fname, res_target) }
  if(is.null(sphereR_target_fname)){ sphereR_target_fname <- original_to_target_fname(sphereR_original_fname, res_target) }
  sphereL_target_fname <- make_abs_path(sphereL_target_fname, write_dir)
  sphereR_target_fname <- make_abs_path(sphereR_target_fname, write_dir)
  stopifnot(is.logical(sphere_target_keep))
  stopifnot(is.logical(sphere_target_overwrite))

  # Collect the absolute paths to each file in a data.frame to return later. Also record whether each existed before the
  # workbook command.
  resamp_files <- data.frame(
    label = c(names(c_target_fnames), 
      paste0("surfL_", 1:length(surfL_target_fname)), 
      paste0("surfR_", 1:length(surfR_target_fname))),
    fname = c(as.character(c_target_fnames), 
      surfL_target_fname, 
      surfR_target_fname),
    stringsAsFactors=FALSE
  )
  resamp_files <- resamp_files[resamp_files$fname != "",]
  resamp_files$existed <- file.exists(resamp_files$fname)

  # Step 1: Generate spheres in the target resolution (if not already existing and provided)
  sphere_target_exists <- file.exists(sphereL_target_fname, sphereR_target_fname)
  if(sum(sphere_target_exists) == 1){ warning("One sphere target file exists but not the other. Overwriting the existing file.") }
  if(sphere_target_overwrite | !all(sphere_target_exists)){
    make_helper_spheres(sphereL_target_fname, sphereR_target_fname, res_target, sphere_target_dir, wb_cmd)
  }

  # Step 2: Use -metric-resample to resample surface/cortex files into target resolution
  resample_individual_kwargs_common <- list(res_target=res_target, 
    overwrite=overwrite, read_dir=read_dir, write_dir=write_dir, sphere_target_dir=sphere_target_dir, wb_path=wb_path)
  for(lab in c("cortexL", "cortexR")){
    if(lab %in% names(c_original_fnames)){
      if(overwrite | !resamp_files$existed[resamp_files$label==lab]){
        is_left <- lab == "cortexL"
        additional_kwargs <- list(
          original_fname=c_original_fnames[[lab]], target_fname=c_target_fnames[[lab]], file_type="metric",
          sphere_original_fname=ifelse(is_left, sphereL_original_fname, sphereR_original_fname),
          sphere_target_fname=ifelse(is_left, sphereL_target_fname, sphereR_target_fname) )
        roi_lab <- paste0(gsub("subcortVol", "subcort", lab), "_ROI")
        if(roi_lab %in% names(c_original_fnames)){
          additional_kwargs <- c(additional_kwargs, 
            list(original_ROI_fname=c_original_fnames[[roi_lab]], target_ROI_fname=c_target_fnames[[roi_lab]]))
        }
        do.call(resample_individual, c(resample_individual_kwargs_common, additional_kwargs))
      }
    }
  }

  # Step 3: Use -surface-resample to resample surface/cortex files into target resolution
  for(i in 1:length(surfL_original_fname)){
    if(overwrite | !resamp_files$existed[resamp_files$label==paste0("surfL_", i)]){
      additional_kwargs <- list(
        original_fname=surfL_original_fname[i], target_fname=surfL_target_fname[i], file_type="surface",
        sphere_original_fname=sphereL_original_fname, sphere_target_fname=sphereL_target_fname)
      do.call(resample_individual, c(resample_individual_kwargs_common, additional_kwargs))
    }
  }
  for(i in 1:length(surfR_original_fname)){
    if(overwrite | !resamp_files$existed[resamp_files$label==paste0("surfR_", i)]){
      additional_kwargs <- list(
        original_fname=surfR_original_fname[i], target_fname=surfR_target_fname[i], file_type="surface",
        sphere_original_fname=sphereR_original_fname, sphere_target_fname=sphereR_target_fname)
      do.call(resample_individual, c(resample_individual_kwargs_common, additional_kwargs))
    }
  }

  invisible(resamp_files)
}

#' Resample an individual file (with its ROI)
#'
#' @description Performs spatial resampling of NIfTI/GIfTI data on the cortical surface
#'
#' @param original_fname The file to resample.
#' @param target_fname Where to save the resampled file.
#' @param original_ROI_fname The name of the ROI file corresponding to \code{original_fname}. Leave as NULL (default) if this
#'  doesn't exist or shouldn't be resampled.
#' @param target_ROI_fname The name of the resampled ROI file. Only applicable if \code{original_ROI_fname} is provided.
#' @param file_type "metric" or "surface", or NULL (default) to infer from \code{original_fname}.
#' @param res_target Target resolution (number of cortical surface vertices per hemisphere)
#' @param sphere_original_fname File path of [left/right]-hemisphere spherical GIFTI files in original resolution 
#'  The hemisphere side should match \code{original_fname}.
#' @param sphere_target_fname File path of [left/right]-hemisphere spherical GIFTI files in targetinal resolution 
#'  The hemisphere side should match \code{target_fname}. See \code{make_helper_spheres}.
#' @param overwrite Logical indicating whether original_fname should be overwritten if it already exists.
#' @param read_dir If the file names of \code{original_fname} and \code{original_ROI_fname} are relative, this is the directory to 
#'  look for them in. If NULL (default), use the current working directory. \code{read_dir} will not affect files specified with absolute paths.
#' @param write_dir If the file names of \code{target_fname} and \code{target_ROI_fname} are relative, this is the directory to 
#'  look for them in. If NULL (default), use the current working directory. \code{write_dir} will not affect files specified with absolute paths.
#' @param sphere_target_dir If \code{sphere_target_keep} and the file names of \code{c_original_fnames} are relative, this is the directory to 
#'  write and look for them in. The default is "./helper_files_resampling".
#' @param wb_path (Optional) Path to Connectome Workbench folder. If not provided, should be set with 
#'  \code{ciftiTools.setOption('wb_path', 'path/to/workbench')}.
#'
#' @return Logical indicating whether resampled file was created.
#' @export
#'
resample_individual <- function(original_fname, target_fname, 
  original_ROI_fname=NULL, target_ROI_fname=NULL, file_type=NULL, res_target,
  sphere_original_fname, sphere_target_fname, overwrite=TRUE,
  read_dir=NULL, write_dir=NULL, sphere_target_dir=NULL, wb_path=NULL){

  wb_cmd <- get_wb_cmd_path(wb_path)

  # Check arguments.
  read_dir <- check_dir(read_dir)
  original_fname <- make_abs_path(original_fname, read_dir)
  stopifnot(file.exists(original_fname))
  do_ROI <- !is.null(original_ROI_fname)
  if(do_ROI){
    original_ROI_fname <- make_abs_path(original_ROI_fname, read_dir)
    stopifnot(file.exists(original_ROI_fname))
  }
  write_dir <- check_dir(write_dir, make=TRUE)
  target_fname <- make_abs_path(target_fname, write_dir)
  if(do_ROI){
    target_ROI_fname <- make_abs_path(target_ROI_fname, read_dir)
    stopifnot(file.exists(target_ROI_fname))
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
  run_cmd <- overwrite | all(c(file.exists(target_fname, ifelse(do_ROI, file.exists(target_ROI_fname), TRUE))))
  if(!run_cmd){
    cmd_code <- NA
  } else {
    cmd_name <- switch(file_type,
      metric="-metric-resample",
      surface="-surface-resample"
    )

    cmd <- paste(wb_cmd, cmd_name, original_fname, sphere_original_fname, sphere_target_fname, "BARYCENTRIC", target_fname)
    if(do_ROI){
      cmd <- paste(cmd, "-current-roi", original_ROI_fname, "-valid-roi-out", target_ROI_fname)
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

  system(paste(wb_cmd,'-surface-create-sphere', resamp_res, sphereL_fname, sep=' '))
  system(paste(wb_cmd, '-surface-flip-lr', sphereL_fname, sphereR_fname, sep=' '))
  system(paste(wb_cmd, '-set-structure', sphereL_fname, 'CORTEX_LEFT', sep=' '))
  system(paste(wb_cmd, '-set-structure', sphereR_fname, 'CORTEX_RIGHT', sep=' '))

  invisible(file.exists(sphereL_fname) & file.exists(sphereR_fname))
}