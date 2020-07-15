#' Read in CIFTI data as a single matrix
#'
#' @description Reads CIFTI data as a single large matrix. This uses the -cifti-convert -to-gifti-ext Connectome
#'  Workbench command.
#'
#' @inheritParams cifti_fname_Param
#' @param keep \code{read_cifti_flat} works by saving the CIFTI as a GIfTI file, and then reading it in. If 
#'  a new GIfTI file was made by this function call, should it be deleted once it is read in? Default: FALSE (delete it). 
#' @param gifti_fname File path of GIfTI-format data to save the CIFTI as. Default:
#'  the CIFTI_fname but with the extension replaced with "flat.gii".
#' @param write_dir The directory in which to save the GIfTI, if it is being kept. If NULL, 
#'  defaults to the current working directory.
#' @inheritParams wb_path_Param
#' 
#' @importFrom gifti readGIfTI
#'
#' @return A T x B matrix, where T is the number of time points and B is the number of brainordinates in the CIFTI file.
#' @export
#'
#' @details This function uses a system wrapper for the 'wb_command' executable. The user must first download and 
#'  install the Connectome Workbench, available from https://www.humanconnectome.org/software/get-connectome-workbench. 
#'  The 'wb_path' argument is the full file path to the 'wb_command' executable file.
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
read_cifti_flat <- function(cifti_fname, keep=FALSE, gifti_fname=NULL, 
  write_dir=NULL, wb_path=NULL) {

  wb_cmd <- get_wb_cmd_path(wb_path)

  cifti_fname <- format_path(cifti_fname)
  if (!file.exists(cifti_fname)) stop('cifti_fname does not exist.')
  # Get the components of the CIFTI file path.
  bname_cifti <- basename(cifti_fname) 
  extn_cifti <- get_cifti_extn(bname_cifti)  # "dtseries.nii" or "dscalar.nii"

  # If gifti_fname is not provided, use the CIFTI_fname but replace the extension with "flat.gii".
  if (identical(gifti_fname, NULL)) {
    gifti_fname <- gsub(extn_cifti, "flat.gii", bname_cifti, fixed=TRUE)
  }
  if (!keep) { write_dir <- tempdir() }
  gifti_fname <- format_path(gifti_fname, write_dir, mode=2)
  # Write the file and read it in.
  cmd <- paste(sys_path(wb_cmd), "-cifti-convert -to-gifti-ext", sys_path(cifti_fname), sys_path(gifti_fname))
  cmd_code <- system(cmd)
  if (cmd_code != 0) {
    stop(paste0("The Connectome Workbench command failed with code ", cmd_code, 
                ". The command was:\n", cmd))
  }
  result <- readGIfTI(gifti_fname)
  result <- result$data$normal

  # [TO DO]: don't delete since it's in tempdir(). Better way? safe to read (above line)?
  # # Delete the GIfTI only if it is new and keep==FALSE. Also delete the ".data" file (Note: I don't know what it is?)
  # if (!keep) { 
  #   file.remove(gifti_fname) 
  #   if (file.exists(paste0(gifti_fname, ".data"))) {
  #     file.remove(paste0(gifti_fname, ".data"))
  #   }
  # }

  result
}

#' @rdname read_cifti_flat
#' @export
readCIfTI_flat <- readcii_flat <- function(
  cifti_fname, keep=FALSE, gifti_fname=NULL, 
  write_dir=NULL, wb_path=NULL){

  read_cifti_flat(
    cifti_fname, keep=FALSE, gifti_fname=NULL, 
    write_dir=NULL, wb_path=NULL
  )
}