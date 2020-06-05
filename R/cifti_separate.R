#' Separates and saves CIFTI data to NIfTI and GIfTI files.
#'
#' @description Separates a CIFTI file into GIfTIs for the cortical (left and right) structures, and NIfTIs for the 
#'  subcortical structures. This uses the -cifti-separate command from Connectome Workbench.
#'
#' @param fname_cifti File path of CIFTI-format data (ending in .d*.nii).
#' @param brainstructures A vector indicating which brain structure(s) to obtain: \code{"left"} (left cortical surface), 
#'  \code{"right"} (right cortical surface), and/or \code{"subcortical"} (subcortical and cerebellar gray matter). The
#'  default is \code{c('left','right','subcortical')} (all brain structures).
#' @param fnames_cortexL,fnames_cortexR (Optional) where to save the left and right cortex GIfTIs. If not provided, 
#'  defaults to \code{"*[L/R].func.gii"}, where * is the same directory and file name component of \code{fname_cifti}.
#' @param fname_subcortVol,fname_subcortLab (Optional) where to save the subcortical volume and label NIfTIs. If not 
#'  provided, defaults to \code{"[/.labels].nii.gz"}, where * is the same directory and file name component of 
#'  \code{fname_cifti}.
#' @param wb_dir (Optional) Path to Connectome Workbench folder. If not provided, should be set by option ...
#' @param overwrite If a NIfTI or GIfTI file already exists, should it be overwritten? Default is FALSE.
#'
#' @return Nothing
#' @export
#'
#' @details This function uses a system wrapper for the 'wb_command' executable. The user must first download and 
#'  install the Connectome Workbench, available from https://www.humanconnectome.org/software/get-connectome-workbench. 
#'  The 'wb_dir' argument is the path to the Connectome Workbench folder.
#'
#' The subcortical brain structure labels (LABELS element of returned list) take values 3-21 and represent:
#' 3 Accumbens-L
#' 4 Accumbens-R
#' 5 Amygdala-L
#' 6 Amygdala-R
#' 7 Brain Stem
#' 8 Caudate-L
#' 9 Caudate-R
#' 10 Cerebellum-L
#' 11 Cerebellum-R
#' 12 Diencephalon-L
#' 13 Diencephalon-R
#' 14 Hippocampus-L
#' 15 Hippocampus-R
#' 16 Pallidum-L
#' 17 Pallidum-R
#' 18 Putamen-L
#' 19 Putamen-R
#' 20 Thalamus-L
#' 21 Thalamus-R
#'
cifti_separate <- function(fname_cifti, brainstructures=c("left","right","subcortical"), fname_cortexL=NULL, 
  fname_cortexR=NULL, fname_subcortVol=NULL, fname_subcortLab=NULL, dir=NULL, wb_dir=NULL, overwrite=FALSE){

  wb_dir <- check_wb_dir(wb_dir)

  # Separate the CIfTI file path into directory, file name, and extension components.
  dir_cifti <- dirname(fname_cifti) 
  fname_cifti <- basename(fname_cifti) 
  extn_cifti <- get_cifti_extn(fname_cifti)  # "dtseries.nii" or "dscalar.nii"
  all_files <- list.files(dir_cifti)
  if(!(fname_cifti %in% all_files)) stop("fname_cifti does not exist")

  if(is.null(dir)){ dir <- "."}

  # Determine which brainstructures to obtain.
  for(i in 1:length(brainstructures)){
    brainstructures[i] <- match.arg(brainstructures[i], c("left","right","subcortical"))
  }

  # Build the Connectome Workbench command. Plan to run it only if at least one file is needed.
  cmd <- paste(wb_dir, "-cifti-separate", file.path(dir_cifti, fname_cifti), "COLUMN", sep=" ")
  run_cmd <- FALSE
  # Define the default names.
  fnames_sep_files_defaults <- list(cortexL="L.func.gii", cortexR="R.func.gii", subcortVol="nii", 
    subcortLab="labels.nii")
  for(i in 1:length(fnames_sep_files_defaults)){
    fnames_sep_files_defaults[[i]] <- gsub(extn_cifti, fnames_sep_files_defaults[[i]], fname_cifti, fixed=TRUE)
  }
  # Get the name and command string for each structure to obtain.
  if("left" %in% brainstructures){
    if(is.null(fname_cortexL)){ fname_cortexL <- fnames_sep_files_defaults$cortexL }
    if(overwrite | (!(fname_cortexL %in% all_files))){
      cmd <- paste(cmd, '-metric CORTEX_LEFT', file.path(dir, fname_cortexL), sep=' ')
      run_cmd <- TRUE
    }
  }
  if("right" %in% brainstructures){
    if(is.null(fname_cortexR)){ fname_cortexR <- fnames_sep_files_defaults$cortexR }
    if(overwrite | (!(fname_cortexR %in% all_files))){
      cmd <- paste(cmd, '-metric CORTEX_RIGHT', file.path(dir, fname_cortexR), sep=' ')
      run_cmd <- TRUE
    }
  }    
  if("subcortical" %in% brainstructures) {
    if(is.null(fname_subcortVol)){ fname_subcortVol <- fnames_sep_files_defaults$subcortVol }
    if(is.null(fname_subcortLab)){ fname_subcortLab <- fnames_sep_files_defaults$subcortLab }
    if(overwrite | (!(fname_subcortVol %in% all_files & fname_subcortLab %in% all_files))){
      cmd <- paste(cmd, '-volume-all', file.path(dir, fname_subcortVol), sep=' ')
      cmd <- paste(cmd, '-label', file.path(dir, fname_subcortLab), sep=' ')
      run_cmd <- TRUE
    }
  }

  # Run Connectome Workbench command.
  if(run_cmd){
    out <- system(cmd)
    if(out != 0){
      stop(paste0("The Connectome Workbench command failed with code ", out, ". The command was:\n", cmd))
    }
  } else {
    out <- NA
  }
  invisible(out)
}