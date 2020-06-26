#' Reads in CIFTI data
#'
#' @description Reads CIfTI data as a single large matrix. This uses the -cifti-convert -to-gifti-ext Connectome
#'  Workbench command.
#'
#' @param cifti_fname File path of CIFTI-format data (ending in .d*.nii).
#' @param gifti_fname The GIfTI file name to save as.
#' @param keep This function works by saving the CIfTI file as a GIfTI file, and then reading it in. If a new GIfTI was
#'  created by this function call, should it be kept or deleted? Default is FALSE (deletes the new file).
#' @param overwrite This function works by saving the CIfTI file as a GIfTI file, and then reading it in. Should the 
#'  GIfTI file be overwritten if it already exists? Default is FALSE. If FALSE, the existing file is read in. 
#' @param write_dir The directory to save the GIfTI into.
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
cifti_read_flat <- function(cifti_fname, gifti_fname=NULL, 
  keep=FALSE, overwrite=TRUE, write_dir=NULL, wb_dir=NULL){

  wb_cmd <- get_wb_cmd_path(wb_dir)

  cifti_fname <- make_abs_path(cifti_fname)
  if(!file.exists(cifti_fname)) stop('cifti_fname does not exist.')

  # Separate the CIfTI file path into directory, file name, and extension components.
  bname_cifti <- basename(cifti_fname) 
  extn_cifti <- get_cifti_extn(bname_cifti)  # "dtseries.nii" or "dscalar.nii"

  if(is.null(write_dir)){ 
    write_dir <- getwd()
  } else {
    if(!dir.exists(write_dir)){ stop("write_dir does not exist, check and try again.") }
    # TO DO: dir.create?
  }
  #TO DO: Check that the user has write permissions in outdir

  if(identical(gifti_fname, NULL)){
    gifti_fname <- gsub(extn_cifti, "flat.gii", bname_cifti, fixed=TRUE)
  }
  gifti_fname <- make_abs_path(gifti_fname, write_dir)
  gifti_existed <- file.exists(gifti_fname)
  if(overwrite | !(gifti_existed)){
    cmd <- paste(wb_cmd, "-cifti-convert -to-gifti-ext", cifti_fname, gifti_fname)
    cmd_code <- system(cmd)
    if(cmd_code != 0){
      stop(paste0("The Connectome Workbench command failed with code ", cmd_code, 
                  ". The command was:\n", cmd))
    }
  }

  result <- readGIfTI(gifti_fname)
  result <- result$data$normal

  # Delete the GIfTI, unless otherwise requested. Do not delete files that existed before.
  if(!keep & !gifti_existed){ 
    file.remove(gifti_fname) 
    if(file.exists(paste0(gifti_fname, ".data"))){
      file.remove(paste0(gifti_fname, ".data"))
    }
  }

  return(result)
}
