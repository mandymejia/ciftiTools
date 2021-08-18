#' Read only the data matrix in a CIFTI file
#'
#' Reads the CIFTI data matrix by converting it to a GIFTI using the 
#'  \code{-cifti-convert -to-gifti-ext} Connectome Workbench command. The result 
#'  will be a \eqn{T x G} matrix (\eqn{T} measurements, \eqn{G} non-empty 
#'  greyordinates). All brainstructures in the CIFTI will be obtained, with no 
#'  indication for which brainstructure each brainordinate corresponds to.    
#'  Medial wall vertices and voxels outside the subcortical mask will not be 
#'  included. No spatial information is included. This is the fastest way to 
#'  read in CIFTI data. 
#' 
#' @inheritParams cifti_fname_Param
#' @param keep This function works by converting the CIFTI file to a GIFTI file 
#'  and then reading it in. Should the GIFTI file be kept? If \code{FALSE}
#'  (default), write it in a temporary directory regardless of \code{write_dir}.
#' @param gifti_fname File path of GIFTI-format data to save the CIFTI as. 
#'  Should end with ".func.gii". Default: the CIFTI_fname but with the extension
#'  replaced with ".func.gii".
#' @inheritParams idx_Param
#' @param write_dir The directory in which to save the GIFTI, if it is being 
#'  kept. If \code{NULL} (default), use the current working directory.
#'
#' @importFrom gifti readgii
#'
#' @return A \eqn{T x G} matrix, where \eqn{T} is the number of measurements 
#'  and \eqn{G} is the number of greyordinates in the CIFTI file.
#' 
#' @keywords internal
#'
read_cifti_flat <- function(
  cifti_fname, keep=FALSE, gifti_fname=NULL,
  idx=NULL, write_dir=NULL) {

  # ----------------------------------------------------------------------------
  # Get the output file name ---------------------------------------------------
  # ----------------------------------------------------------------------------

  cifti_fname <- format_path(cifti_fname)
  if (!is.fname(cifti_fname)) { 
    stop("`", cifti_fname, "` is not an existing file.") 
  }
  
  # Get the components of the CIFTI file path.
  bname_cifti <- basename(cifti_fname)
  extn_cii <- get_cifti_extn(bname_cifti)
  extn_cii <- paste0(".", extn_cii) # e.g. ".dtseries.nii"
  is_label = FALSE# extn_cii == ".dlabel.nii"

  # If gifti_fname is not provided, use the CIFTI_fname but replace the 
  #   extension with "flat.gii".
  if (is.null(gifti_fname)) {
    extn_gii <- ifelse(is_label, ".label.gii", ".func.gii")
    extn_gii2 <- gsub(".", "\\.", extn_gii, fixed=TRUE)
    extn_cii2 <- paste0(gsub(".", "\\.", extn_cii, fixed=TRUE), "$")
    if (grepl(bname_cifti, extn_cii2)) {
      gifti_fname <- gsub(extn_cii2, extn_gii2, bname_cifti)
    } else {
      gifti_fname <- paste0(bname_cifti, extn_gii)
    }
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
  run_wb_cmd(cmd)

  # Create new GIFTI with selected columns, if specified.
  if (!is.null(idx)) {
    stopifnot(all(idx > 0) && all(idx == round(idx)))
    idx_is_seq <- (length(idx) > 2 && all(diff(idx) == 1))
    if (idx_is_seq) {
      idx_string <- paste("-column", idx[1], "-up-to", idx[length(idx)])
    } else {
      idx_string <- paste("-column", paste(idx, collapse=" -column "))
    }
    gifti_fname_original <- gifti_fname
    gifti_fname <- file.path(
      tempdir(), 
      gsub(
        paste0(extn_gii2, "$"), paste0("\\.sel_idx", extn_gii2), 
        basename(gifti_fname_original)
      )
    )

    run_wb_cmd(paste(
      "-metric-merge", gifti_fname, "-metric", gifti_fname_original, idx_string
    ))
  }

  result <- readgii(gifti_fname)
  do.call(cbind, result$data)
}