#' Reads in CIFTI data
#'
#' @description Reads CIfTI data as a single large matrix. This uses the -cifti-convert -to-gifti-ext Connectome
#'  Workbench command.
#'
#' @param cifti_fname File path of CIFTI-format data (ending in .d*.nii).
#' @param dir_gifti The directory to save the GIfTI into.
#' @param fname_gifti The GIfTI file name to save as.
#' @param keep This function works by saving the CIfTI file as a GIfTI file, and then reading it in. If a new GIfTI was
#'  created by this function call, should it be kept or deleted? Default is FALSE (deletes the new file).
#' @param overwrite This function works by saving the CIfTI file as a GIfTI file, and then reading it in. Should the 
#'  GIfTI file be overwritten if it already exists? Default is FALSE. If FALSE, the existing file is read in. 
#' @param wb_dir (Optional) Path to Connectome Workbench folder. If not provided, should be set by option ...
#'
#' @importFrom gifti readGIfTI
#'
#' @return A T x B matrix, where T is the number of time points and B is the number of brainordinates in the CIfTI file.
#' @export
#'
#' @details This function uses a system wrapper for the 'wb_command' executable. The user must first download and 
#'  install the Connectome Workbench, available from https://www.humanconnectome.org/software/get-connectome-workbench. 
#'  The 'wb_dir' argument is the full file path to the 'wb_command' executable file.
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
cifti_read_flat <- function(cifti_fname, dir_gifti=NULL, fname_gifti=NULL, keep=FALSE, overwrite=FALSE, wb_dir=NULL){

  wb_dir <- check_wb_dir(wb_dir)

  # Separate the CIfTI file path into directory, file name, and extension components.
  dir_cifti <- dirname(cifti_fname) 
  bname_cifti <- basename(cifti_fname) 
  extn_cifti <- get_cifti_extn(bname_cifti)  # "dtseries.nii" or "dscalar.nii"
  all_files <- list.files(dir_cifti)
  if(!(bname_cifti %in% all_files)) stop("cifti_fname does not exist")

  if(identical(dir_gifti, NULL)){ dir_gifti <- "." }

  if(identical(fname_gifti, NULL)){
    fname_gifti <- gsub(extn_cifti, "flat.gii", bname_cifti, fixed=TRUE)
  }
  fname_gifti <- file.path(dir_gifti, fname_gifti)
  gifti_existed <- file.exists(fname_gifti)
  if(overwrite | !(gifti_existed)){
    cmd <- paste(wb_dir, "-cifti-convert -to-gifti-ext", cifti_fname, fname_gifti)
    cmd_result <- system(cmd)
    if(cmd_result != 0){
      stop(paste0("The Connectome Workbench command failed with code ", cmd_result, 
                  ". The command was:\n", cmd))
    }
  }

  result <- readGIfTI(fname_gifti)
  result <- result$data$normal

  # Delete the GIfTI, unless otherwise requested. Do not delete files that existed before.
  if(!keep & !gifti_existed){ 
    file.remove(fname_gifti) 
    if(file.exists(paste0(fname_gifti, ".data"))){
      file.remove(paste0(fname_gifti, ".data"))
    }
  }

  return(result)
}
