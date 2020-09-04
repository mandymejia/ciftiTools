#' Read Only the Data from a CIFTI File
#'
#' @description Reads CIFTI data as a matrix by converting it to a GIFTI.
#'  This uses the \code{-cifti-convert -to-gifti-ext} Connectome 
#'  Workbench command. The result will be a T x B matrix (T measurements, B 
#'  non-empty brainordinates). All brainstructures will be obtained, but 
#'  they will be undifferentiable. Medial wall vertices and voxels outside the
#'  brain mask will not be included. No spatial information is included. This is 
#'  the fastest way to read in CIFTI data. 
#' 
#' @inheritParams cifti_fname_Param
#' @param keep \code{read_cifti_minimal} works by saving the CIFTI as a GIFTI file, 
#'  and then reading it in. Should the GIFTI file be kept? If \code{FALSE}
#'  (default), write it in a temporary directory regardless of \code{write_dir}.
#' @param gifti_fname File path of GIFTI-format data to save the CIFTI as. 
#'  Default: the CIFTI_fname but with the extension replaced with "flat.gii".
#' @param write_dir The directory in which to save the GIFTI, if it is being 
#'  kept. If \code{NULL} (default), use the current working directory.
#' @inheritParams wb_path_Param
#'
#' @importFrom gifti readgii
#'
#' @return A T x B matrix of class "cifti_data", where T is the number of 
#'  measurements and B is the number of brainordinates in the CIFTI file.
#' @inheritSection Connectome_Workbench_Description Connectome Workbench Requirement
#' @keywords internal
#'
read_cifti_flat <- function(
  cifti_fname, keep=FALSE, gifti_fname=NULL,
  write_dir=NULL, wb_path=NULL) {

  # ----------------------------------------------------------------------------
  # Get the output file name ---------------------------------------------------
  # ----------------------------------------------------------------------------

  cifti_fname <- format_path(cifti_fname)
  if (!file.exists(cifti_fname)) stop('cifti_fname does not exist.')
  # Get the components of the CIFTI file path.
  bname_cifti <- basename(cifti_fname)
  extn_cifti <- get_cifti_extn(bname_cifti)  # "dtseries.nii" or "dscalar.nii"

  # If gifti_fname is not provided, use the CIFTI_fname but replace the 
  #   extension with "flat.gii".
  if (identical(gifti_fname, NULL)) {
    gifti_fname <- gsub(extn_cifti, "flat.gii", bname_cifti, fixed=TRUE)
  }
  if (!keep) { write_dir <- tempdir() }
  gifti_fname <- format_path(gifti_fname, write_dir, mode=2)
  
  # ----------------------------------------------------------------------------
  # Write the file and read it in. ---------------------------------------------
  # ----------------------------------------------------------------------------
  
  cmd <- paste(
    "-cifti-convert -to-gifti-ext", 
    sys_path(cifti_fname), 
    sys_path(gifti_fname)
  )
  run_wb_cmd(cmd, wb_path)
  result <- readgii(gifti_fname)
  result <- result$data$normal

  # [TO DO]: don't delete since it's in tempdir(). Better way? safe to read (above line)?
  # # Delete the GIFTI only if it is new and keep==FALSE. Also delete the ".data" file (Note: I don't know what it is?)
  # if (!keep) {
  #   file.remove(gifti_fname)
  #   if (file.exists(paste0(gifti_fname, ".data"))) {
  #     file.remove(paste0(gifti_fname, ".data"))
  #   }
  # }
}