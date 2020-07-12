#' Read in CIFTI data from separate GIfTI and NIfTI files
#'
#' @description Reads in CIFTI data from the separated left and right cortical GIfTI files, the subcortical NIfTI file, and optionally any surface geometry GIfTI files.
#'
#' @param cortexL_fname,cortexR_fname (Optional) File path of GIfTI data for [left/right] cortex.
#' @param subcortVol_fname (Optional) File path of NIfTI volume data for subcortical structures.
#' @param subcortLab_fname (Optional) File path of the NIfTI labels for subcortical structures.
#' @param surfL_fname,surfR_fname (Optional) File path, or vector of multiple file paths, of GIFTI surface geometry file 
#'  representing [left/right] cortex.
#' @param surf_label (Optional) Character vector containing descriptive names of each GIFTI surface geometry provided 
#'  (e.g. midthickness, inflated, etc.). Should match the length of surfL_fname and/or surfL_fname if they are 
#'  provided. Otherwise, ignored.
#' @param read_dir If any of the file names are relative, this is the directory to look for them in. If NULL (default),
#'  use the current working directory. \code{read_dir} will not affect files specified with absolute paths.
#' 
#' @return An object of type 'cifti', a list containing at least 4 elements: CORTEX_LEFT, CORTX_RIGHT, VOL and LABELS.
#'  LABELS contains the brain structure labels (usually 3-21) of the subcortical elements. If surface geometry files
#'  were provided in the arguments, the list will also contain SURF_LEFT and SURF_RIGHT.
#' @export
#' @importFrom gifti readGIfTI
#' @importFrom RNifti readNifti
#'
#' @details This function uses a system wrapper for the 'wb_command' executable. The user must first download and 
#'  install the Connectome Workbench, available from https://www.humanconnectome.org/software/get-connectome-workbench. 
#'  The 'wb_path' argument is the full file path to the Connectome Workbench folder. (The full file path to the 'wb_cmd' 
#'  executable also works.)
#'
cifti_read_from_separate <- function(cortexL_fname=NULL, cortexR_fname=NULL, subcortVol_fname=NULL, subcortLab_fname=NULL, 
  surfL_fname=NULL, surfR_fname=NULL, surf_label="surface",
  read_dir=NULL) {

  # [TO DO]: Integrate with `cifti_make`

  result <- vector("list", length=6)
  names(result) <- c("CORTEX_LEFT", "CORTEX_RIGHT", "VOL", "LABELS", "SURF_LEFT", "SURF_RIGHT")

  # Read in GIfTI files for left and right cortex.
  if (!is.null(cortexL_fname)) {
    cortexL_fname <- format_path(cortexL_fname, read_dir, mode=4)
    result$CORTEX_LEFT <- do.call(cbind, readGIfTI(cortexL_fname)$data)
  }
  if (!is.null(cortexR_fname)) {
    cortexR_fname <- format_path(cortexR_fname, read_dir, mode=4)
    result$CORTEX_RIGHT <- do.call(cbind, readGIfTI(cortexR_fname)$data)  }

  # Read in NIfTI files for subcortical data.
  if (!is.null(subcortVol_fname)) {
    subcortVol_fname <- format_path(subcortVol_fname, read_dir, mode=4)
    result$VOL <- readNifti(subcortVol_fname)
  }
  if (!is.null(subcortLab_fname)) {
    subcortLab_fname <- format_path(subcortLab_fname, read_dir, mode=4)
    result$LABELS <- readNifti(subcortLab_fname)
    result$LABELS[result$LABELS > 0] <- result$LABELS[result$LABELS > 0] + 2 
  }

  # Read in GIfTI surface geometry files.
  num_surf <- length(surf_label) #number of surface types provided
  if (!is.null(surfL_fname)) {
    result$SURF_LEFT <- vector('list', num_surf)
    names(result$SURF_LEFT) <- surf_label
    for(ii in 1:num_surf) {
      surfL_fname[ii] <- format_path(surfL_fname[ii], read_dir, mode=4)
      result$SURF_LEFT[[ii]] <- make_cifti_surface(surfL_fname[ii])
    }
  }
  if (!is.null(surfR_fname)) {
    result$SURF_RIGHT <- vector('list', num_surf)
    names(result$SURF_RIGHT) <- surf_label
    for(ii in 1:num_surf) {
      surfR_fname[ii] <- format_path(surfR_fname[ii], read_dir, mode=4)
      result$SURF_RIGHT[[ii]] <- make_cifti_surface(surfR_fname[ii])
    }
  }

  class(result) <- 'cifti'
  result
}
