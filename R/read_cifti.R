#' Read in CIFTI data
#'
#' @description Read a CIFTI file by separating it into GIfTI and NIfTI files
#'  (\code{\link{separate_cifti}}), optionally resampling them
#'  (\code{\link{resample_cifti_separate}}), and then reading each separated
#'  component into R  (\code{\link{make_cifti_from_separate}}).
#'
#' @inheritParams cifti_fname_Param
#' @param flat Should the cortical and subcortical data be obtained as one T x B
#'  matrix (T measurements, B brainordinates)? This will be much faster but the
#'  output will not include any spatial information, including whether a
#'  brainordinate corresponds to cortical or subcortical data. All arguments
#'  below except \code{wb_path} will be ignored.
#' @inheritParams surfL_fname_Param
#' @inheritParams surfR_fname_Param
#' @inheritParams brainstructures_Param
#' @inheritParams ROI_brainstructures_Param
#' @inheritParams resamp_res_Param_optional
#' @inheritParams sphereL_fname_Param
#' @inheritParams sphereR_fname_Param
#' @inheritParams sep_fnames_Param
#' @inheritParams sep_keep_Param
#' @inheritParams resamp_keep_Param
#' @inheritParams resamp_fnames_Param
#' @inheritParams write_dir_Param_intermediate
#' @inheritParams verbose_Param
#' @inheritParams wb_path_Param
#'
#' @return If \code{flat}, an object of type \code{cifti_flat}: a T x B matrix
#'  with T measurements and B brainordinates. If not \code{flat},
#'  an object of type \code{cifti}, a list containing 6 elements:
#'  \code{CORTEX_LEFT}, \code{CORTEX_RIGHT}, \code{VOL} \code{LABELS},
#'  \code{SURF_LEFT} and \code{SURF_RIGHT}.
#'
#' @export
#'
#' @details This function uses a system wrapper for the "wb_command"
#'  executable. The user must first download and install the Connectome
#'  Workbench, available from
#'  \url{https://www.humanconnectome.org/software/get-connectome-workbench}.
#'  The \code{wb_path} argument is the path to the Connectime Workbench folder
#'  or executable.
#'
#' The subcortical brain structure labels (LABELS element of returned list) take
#'  on integer values 3-21 and represent:
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
read_cifti <- function(
  cifti_fname, flat=FALSE,
  surfL_fname=NULL, surfR_fname=NULL,
  brainstructures=c("left","right"), ROI_brainstructures=NULL,
  resamp_res=NULL, sphereL_fname=NULL, sphereR_fname=NULL,
  sep_keep=FALSE, sep_fnames=NULL,
  resamp_keep=FALSE, resamp_fnames=NULL,
  write_dir=NULL, verbose=TRUE, wb_path=NULL) {

  if (flat) { return(read_cifti_flat(cifti_fname, wb_path=wb_path)) }

  # ----------------------------------------------------------------------------
  # Setup ----------------------------------------------------------------------
  # ----------------------------------------------------------------------------

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

  brainstructures <- match_input(
    brainstructures, c("left","right","subcortical"),
    user_value_label="brainstructures"
  )
  if (!is.null(ROI_brainstructures)) {
    ROI_brainstructures <- match_input(ROI_brainstructures, brainstructures,
      user_value_label="ROI_brainstructures")
  }

  if (verbose) { exec_time <- Sys.time() }

  # ----------------------------------------------------------------------------
  # separate_cifti() -----------------------------------------------------------
  # ----------------------------------------------------------------------------

  if (verbose) { cat("Separating CIFTI file.\n") }

  sep_result <- separate_cifti_wrapper(
    cifti_fname=cifti_fname,
    brainstructures=brainstructures, ROI_brainstructures=ROI_brainstructures,
    sep_fnames=sep_fnames, write_dir=write_dir_sep, wb_path=wb_path
  )

  to_read <- sep_result$fname
  names(to_read) <- sep_result$label

  if (verbose) {
    print(Sys.time() - exec_time)
    exec_time <- Sys.time()
  }

  # ----------------------------------------------------------------------------
  # resample_cifti_separate() --------------------------------------------------
  # ----------------------------------------------------------------------------

  do_resamp <- !is.null(resamp_res) && !identical(resamp_res, FALSE)
  # Do not resample the subcortical data.
  to_resample <- to_read[!grepl("subcort", names(to_read))]
  do_resamp <- do_resamp & length(to_resample) > 0
  if (do_resamp) {
    if (verbose) { cat("Resampling CIFTI file.\n") }

    # Do resample_cifti_separate.
    resamp_result <- resample_cifti_wrapper(
      resamp_res=resamp_res, original_fnames=to_resample,
      resamp_fnames=resamp_fnames,
      sphereL_fname=sphereL_fname, sphereR_fname=sphereR_fname,
      surfL_fname=surfL_fname, surfR_fname=surfR_fname,
      read_dir=NULL, write_dir=write_dir_resamp, wb_path=wb_path
    )

    # Replace resampled files.
    to_read_resampled <- names(to_read)[names(to_read) %in% resamp_result$label]
    to_read[to_read_resampled] <- resamp_result$fname[
      resamp_result$label %in% to_read_resampled]

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
  }

  # ----------------------------------------------------------------------------
  # make_cifti_from_separate() -------------------------------------------------
  # ----------------------------------------------------------------------------

  # ROIs are not supported yet.
  is_ROI <- grepl("ROI", names(to_read))
  if(any(is_ROI)){
    warning(paste(
      "ROIs are not supported by ciftiTools yet.",
      "The separated ROI file(s) has been created but will not be read in.\n"
    ))
  }
  to_read <- to_read[!is_ROI]
  to_read <- as.list(to_read)

  if (!is.null(surfL_fname)) { to_read$surfL <- surfL_fname }
  if (!is.null(surfR_fname)) { to_read$surfR <- surfR_fname }

  # Read the CIFTI file from the separated files.
  if (verbose) { cat("Reading GIfTI and NIfTI files to form the CIFTI.\n") }
  result <- do.call(make_cifti_from_separate, to_read)

  if (verbose) {
    print(Sys.time() - exec_time)
    exec_time <- Sys.time()
  }

  # ----------------------------------------------------------------------------
  # Finish ---------------------------------------------------------------------
  # ----------------------------------------------------------------------------

  ## [TO DO]: The files are in tempdir(), so no need to manually delete?
  # Delete the separated files, unless otherwise requested.
  # if (!sep_keep) {
  #   for(f in sep_result$fname) {
  #     file.remove(f)
  #     if (file.exists(paste0(f, ".data"))) {
  #       file.remove(paste0(f, ".data"))
  #     }
  #   }
  # }
  # # Same for resampled files.
  # if (do_resamp && !resamp_keep) {
  #   for(f in resamp_result$fname) {
  #     file.remove(f)
  #     if (file.exists(paste0(f, ".data"))) {
  #       file.remove(paste0(f, ".data"))
  #     }
  #   }
  # }

  result
}

#' @rdname read_cifti
#' @export
readCIfTI <- readcii <- function(
  cifti_fname, flat=FALSE,
  surfL_fname=NULL, surfR_fname=NULL,
  brainstructures=c("left","right"), ROI_brainstructures=NULL,
  resamp_res=NULL, sphereL_fname=NULL, sphereR_fname=NULL,
  sep_keep=FALSE, sep_fnames=NULL,
  resamp_keep=FALSE, resamp_fnames=NULL,
  write_dir=NULL, verbose=TRUE, wb_path=NULL){

  read_cifti(
    cifti_fname, flat,
    surfL_fname, surfR_fname,
    brainstructures, ROI_brainstructures,
    resamp_res, sphereL_fname, sphereR_fname,
    sep_keep, sep_fnames,
    resamp_keep, resamp_fnames,
    write_dir, verbose, wb_path
  )
}
