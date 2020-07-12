#' Read in CIFTI data
#'
#' @description Read a CIFTI file by separating it into GIfTI and NIfTI files 
#'  with \code{\link{cifti_separate}}, optionally resampling them with 
#'  \code{\link{cifti_resample_separate}}, and then reading each separated 
#'  component into R with \code{\link{cifti_read_from_separate}}.
#'
#' @inheritParams cifti_fname_Param
#' @inheritParams brainstructures_Param
#' @inheritParams ROI_brainstructures_Param
#' @inheritParams sep_kwargs_Param
#' @inheritParams sep_keep_Param
#' @inheritParams resamp_res_Param
#' @inheritParams sphereL_fname_Param
#' @inheritParams sphereR_fname_Param
#' @inheritParams resamp_kwargs_Param
#' @inheritParams resamp_keep_Param
#' @param surfL_fname,surfR_fname (Optional) File path of GIFTI surface geometry
#'  file representing the left/right cortex. One or both can be provided.
#' @inheritParams verbose_Param
#' @inheritParams wb_path_Param
#'
#' @return An object of type \code{cifti}, a list containing at least 4 elements: 
#'  \code{CORTEX_LEFT}, \code{CORTEX_RIGHT}, \code{VOL} and \code{LABELS}.
#'  \code{LABELS} contains the brain structure labels (usually 3-21) of the 
#'  subcortical elements. If a surface geometry file(s) was provided in the 
#'  arguments, the list will also contain \code{SURF_LEFT}/\code{SURF_RIGHT}.
#' @export
#'
#' @details This function uses a system wrapper for the "wb_command"
#'  executable. The user must first download and install the Connectome 
#'  Workbench, available from 
#'  \url{https://www.humanconnectome.org/software/get-connectome-workbench}. 
#'  The \code{wb_path} argument is the path to the Connectime Workbench folder or
#'  executable.
#'
#' The subcortical brain structure labels (LABELS element of returned list) take 
#'  values 3-21 and represent:
#'  \describe{
#'    \item{3}{Accumbens-L}
#'    \item{4}{Accumbens-R}
#'    \item{5}{Amygdala-L}
#'    \item{6}{Amygdala-R}
#'    \item{7}{Brain Stem}
#'    \item{8}{Caudate-L}
#'    \item{9}{Caudate-R}
#'    \item{10}{Cerebellum-L}
#'    \item{11}{Cerebellum-R}
#'    \item{12}{Diencephalon-L}
#'    \item{13}{Diencephalon-R}
#'    \item{14}{Hippocampus-L}
#'    \item{15}{Hippocampus-R}
#'    \item{16}{Pallidum-L}
#'    \item{17}{Pallidum-R}
#'    \item{18}{Putamen-L}
#'    \item{19}{Putamen-R}
#'    \item{20}{Thalamus-L}
#'    \item{21}{Thalamus-R}
#'  }
#'
cifti_read <- function(
  cifti_fname, brainstructures=c("left","right"), ROI_brainstructures=NULL,
  sep_kwargs=NULL, sep_keep=FALSE, # cifti_separate
  resamp_res=NULL, sphereL_fname=NULL, sphereR_fname=NULL, # cifti_resample
  resamp_kwargs=NULL, resamp_keep=FALSE, # cifti_resample
  surfL_fname=NULL, surfR_fname=NULL, # cifti_read_from_separate
  verbose=TRUE, wb_path=NULL) {

  # ----------------------------------------------------------------------------
  # Setup ----------------------------------------------------------------------
  # ----------------------------------------------------------------------------

  brainstructures <- match_input(brainstructures, c("left","right","subcortical"))
  if (!is.null(ROI_brainstructures)) {
    ROI_brainstructures <- match_input(ROI_brainstructures, brainstructures)
  }

  if (verbose) { exec_time <- Sys.time() }

  # ----------------------------------------------------------------------------
  # cifti_separate() -----------------------------------------------------------
  # ----------------------------------------------------------------------------

  if (verbose) { cat("Separating CIFTI file.\n") }

  sep_result <- cifti_separate_wrapper(
    cifti_fname=cifti_fname, 
    brainstructures=brainstructures, ROI_brainstructures=ROI_brainstructures,
    sep_kwargs=sep_kwargs, sep_keep=sep_keep, wb_path=wb_path
  )
  to_read <- sep_result$fname
  names(to_read) <- sep_result$label

  if (verbose) { 
    print(Sys.time() - exec_time)
    exec_time <- Sys.time()
  }

  # ----------------------------------------------------------------------------
  # cifti_resample_separate() --------------------------------------------------
  # ----------------------------------------------------------------------------

  do_resamp <- !identical(resamp_res, NULL) & !identical(resamp_res, FALSE)
  if (do_resamp) {
    if (verbose) { cat("Resampling CIFTI file.\n") }

    # Do not resample the subcortical data.
    to_resample <- to_read[!grepl("subcort", names(to_read))]
    
    # Do cifti_resample_separate.
    resamp_result <- cifti_resample_wrapper(
      resamp_res, to_resample, resamp_kwargs, resamp_keep, 
      surfL_fname, surfR_fname, sphereL_fname, sphereR_fname, 
      wb_path
    )
    # Replace resampled files.
    to_read_resampled <- names(to_read)[names(to_read) %in% resamp_result$label]
    to_read[to_read_resampled] <- resamp_result$fname[resamp_result$label %in% to_read_resampled]
    if (length(surfL_fname) > 0) { 
      surfL_fname <- resamp_result$fname[grepl("surfL", resamp_result$label)] 
    }
    if (length(surfR_fname) > 0) { 
      surfR_fname <- resamp_result$fname[grepl("surfR", resamp_result$label)] 
    }

    if (verbose) { 
      print(Sys.time() - exec_time)
      exec_time <- Sys.time()
    }
  }

  # ----------------------------------------------------------------------------  
  # cifti_read_from_separate() -------------------------------------------------
  # ----------------------------------------------------------------------------

  # ROI not supported yet.
  is_ROI <- grepl("ROI", names(to_read))
  if(any(is_ROI)){ 
    warning(paste(
      "ROIs are not supported by ciftiTools yet.", 
      "The separated ROI file(s) has been created but will not be read in.\n"
    ))
  }
  to_read <- to_read[!is_ROI]

  # Read the CIFTI file from the separated files.
  if (verbose) { cat("Reading GIfTI and NIfTI files to form the CIFTI.\n") }

  # if ("read_dir" %in% read_from_separate_kwargs) {
  #   # [TO DO]: Below warning works for cifti_read and cifti_separate but is not general for this wrapper.
  #   #   But then again, this wrapper isn't meant to be used generally.
  #   warning("read_from_separate_kwargs$read_dir should not be set, because the file paths are determined by cifti_separate. Ignoring.")
  #   read_from_separate_kwargs$read_dir <- NULL
  # }
  # read_from_separate_kwargs <- merge_kwargs(
  #   to_read, 
  #   read_from_separate_kwargs, 
  #   "files listed for reading", "read_from_separate_kwargs"
  # )
  # read_from_separate_kwargs <- merge_kwargs(
  #   list(surfL_fname=surfL_fname, surfR_fname=surfR_fname, read_dir=NULL, surf_label=surf_label, wb_path=wb_path),
  #   read_from_separate_kwargs,
  #   "file names from immediate argments", "read_from_separate_kwargs"
  # )
  # read_from_separate_kwargs[sapply(read_from_separate_kwargs, is.null)] <- NULL
  read_from_separate_kwargs <- c(to_read, list(
    surfL_fname=surfL_fname, surfR_fname=surfR_fname 
  ))
  result <- do.call(cifti_read_from_separate, read_from_separate_kwargs)

  if (verbose) { 
    print(Sys.time() - exec_time)
    exec_time <- Sys.time()
  }
  
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

    if (do_resamp) {
      for(f in resamp_result$fname[!(resamp_result$existed)]) {
        file.remove(f)
        if (file.exists(paste0(f, ".data"))) {
          file.remove(paste0(f, ".data"))
        }
      }
    }
  }

  result
}
