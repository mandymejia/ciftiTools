#' Reads in CIFTI data
#'
#' @description Reads CIfTI data as a single large matrix. This uses the -cifti-convert -to-gifti-ext Connectome
#'  Workbench command.
#'
#' @param cifti_fname File path of CIfTI-format data (ending in .d*.nii) to read in.
#' @param keep \code{cifti_read_flat} works by saving the CIfTI as a GIfTI file, and then reading it in. If 
#'  a new GIfTI file was made by this function call, should it be deleted once it is read in? Default is FALSE (delete it). 
#'  If \code{overwrite==FALSE} and the GIfTI already exists, it will not be deleted even if \code{keep==FALSE}.
#' @param gifti_fname File path of GIfTI-format data to save the CIfTI as.
#' @param overwrite \code{cifti_read_flat} works by saving the CIfTI as a GIfTI file, and then reading it in. Should the 
#'  GIfTI file be overwritten if it already exists? Default is FALSE. If \code{overwrite==TRUE} and the GIfTI file exists,
#'  the existing file is used.
#' @param write_dir The directory in which to save the GIfTI (or look for it if \code{overwrite==FALSE}). If NULL, 
#'  defaults to the current working directory.
#' @param wb_dir (Optional) Path to Connectome Workbench folder. If not provided, should be set with 
#'  \code{ciftiTools.setOption('wb_path', 'path/to/workbench')}.
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
cifti_read_flat <- function(cifti_fname, keep=FALSE, gifti_fname=NULL, 
  overwrite=TRUE, write_dir=NULL, wb_dir=NULL){

  wb_cmd <- get_wb_cmd_path(wb_dir)

  cifti_fname <- make_abs_path(cifti_fname)
  if(!file.exists(cifti_fname)) stop('cifti_fname does not exist.')
  # Get the components of the CIfTI file path.
  bname_cifti <- basename(cifti_fname) 
  extn_cifti <- get_cifti_extn(bname_cifti)  # "dtseries.nii" or "dscalar.nii"

  # Check that write_dir is valid. Use the current working directory if no write_dir is given.
  write_dir <- check_dir(write_dir, default=getwd())

  # If gifti_fname is not provided, use the CIfTI_fname but replace the extension with "flat.gii".
  if(identical(gifti_fname, NULL)){
    gifti_fname <- gsub(extn_cifti, "flat.gii", bname_cifti, fixed=TRUE)
  }
  gifti_fname <- make_abs_path(gifti_fname, write_dir)
  # Write the file and read it in. If overwrite=FALSE, do not write it and read in the existing file.
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

  # Delete the GIfTI only if it is new and keep==FALSE. Also delete the ".data" file (Note: I don't know what it is?)
  if(!keep & !gifti_existed){ 
    file.remove(gifti_fname) 
    if(file.exists(paste0(gifti_fname, ".data"))){
      file.remove(paste0(gifti_fname, ".data"))
    }
  }

  return(result)
}
