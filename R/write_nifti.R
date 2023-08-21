
#' Write subcortical data to NIFTI files
#'
#' Write subcortical data to NIFTI files representing the data values,
#'  subcortical structure labels, and volumetric mask. The input formats of
#'  \code{subcortVol}, \code{subcortLabs}, and \code{subcortMask}
#'  correspond to the data structures of \code{xifti$data$subcort},
#'  \code{xifti$meta$subcort$labels} , and \code{xifti$meta$subcort$mask}
#'  respectively. \code{subcortVol} and \code{subcortLabs} should be vectorized,
#'  so if they are volumes consider using \code{RNifti::writeNIfTI}.
#'
#' All file path arguments are required except \code{ROIsubcortVol_fname}. If
#'  not provided, the volumetric mask will not be written. (It's redundant with
#'  the 0 values in \code{subcortLabs_fname} because valid labels have positive
#'  indexes.)
#' 
#' Note that for label data (i.e. if \code{label_table} is provided) only one
#'  label table can be saved.
#'
#' @param subcortVol A vectorized data matrix: V voxels by T measurements
#' @param subcortLabs Numeric (0 and 3-21) or factor vector corresponding to
#'  subcortical structure labels. See \code{\link{substructure_table}}.
#' @param subcortMask Logical volumetric mask. Values of 0 represent out-of-mask
#'  voxels (not subcortical), and values of 1 represent in-mask voxels
#'  (subcortical),
#' @param trans_mat The TransformationMatrixIJKtoXYZ, or equivalently the desired
#'  sform matrix (srow_x, srow_y and srow_z) to write. If \code{NULL}, do not
#'  write it (all zeroes).
#' @param trans_units The units of \code{trans_mat}. Currently not used.
#' @param col_names (Optional) Column names.
#' @param label_table (Optional) \code{data.frame} of labels and their colors.
#' @param subcortVol_fname,subcortLabs_fname,ROIsubcortVol_fname File path to
#'  a NIFTI to save the corresponding data. \code{ROIsubcortVol_fname} is
#'  optional but the rest is required.
#' @param fill Values to use for out-of-mask voxels. Default: \code{0}.
#'
#' @return Named character vector with the \code{"subcortVol"},
#'  \code{"subcortLabs"}, and \code{"ROIsubcortVol"} file names (if written)
#'
#' @importFrom RNifti writeNifti sform<-
#' @family writing
#' @export
#'
#' @section Connectome Workbench:
#' This function interfaces with the \code{"-volume-label-import"} Workbench
#'  Command.
#'
write_subcort_nifti <- function(
  subcortVol, subcortLabs, subcortMask,
  trans_mat=NULL, trans_units=NULL, col_names=NULL, label_table=NULL,
  subcortVol_fname, subcortLabs_fname, ROIsubcortVol_fname=NULL,
  fill=0){

  nC <- ncol(subcortVol)

  if (!is.null(col_names)) {
    stopifnot(is.character(col_names) && length(col_names) == nC)
  }

  if (!is.null(trans_mat)) {
    stopifnot(is.nummat(trans_mat))
    trans_mat <- structure(trans_mat, code = 2L)
  }

  # Data.
  subcortVol <- unmask_subcortex(subcortVol, subcortMask, fill=fill)
  ## https://github.com/jonclayden/RNifti/issues/5
  if (!is.null(trans_mat)) {
    subcortVol <- RNifti::`sform<-`(subcortVol, trans_mat)
  }
  RNifti::writeNifti(subcortVol, subcortVol_fname)

  ### Add back names: if `dlabel` or `dscalar`.
  if (!is.null(col_names)) {
    cmd <- paste(
      "-set-map-names",
      sys_path(subcortVol_fname),
      paste("-map", seq(length(col_names)), shQuote(col_names), collapse=" ")
    )
    run_wb_cmd(cmd, ignore.stderr=TRUE)
  }

  ### Add back labels: if `dlabel`.
  if (!is.null(label_table)) {
    # Prepare Workbench command.
    label_table_tfile <- paste0(tempfile(), ".txt")
    cmd <- paste(
      "-volume-label-import",
      sys_path(subcortVol_fname),
      sys_path(label_table_tfile),
      sys_path(subcortVol_fname)
    )
    # Correct `label_table` and the command if needed.
    stopifnot(is.data.frame(label_table))
    if ((0 %in% label_table$Key)) {
      zero_key <- rownames(label_table)[label_table$Key==0]
      if (zero_key == "???" && nrow(label_table)>1) {
        # Do not specify 'unlabeled value' in label table.
        # But we need at least one label, so if all data is unlabeled
        #   use Workbench to assign a new 'unlabeled value'...
        label_table <- label_table[label_table$Key!=0,]
      } else {
        # Set 'unlabeled value' to something else
        cmd <- paste(cmd, "-unlabeled-value", min(label_table$Key)-1)
      }
    }
    # Write `label_table` and add it.
    write_label_table(label_table, label_table_tfile)
    run_wb_cmd(cmd, ignore.stderr=TRUE)
  }

  # Labels.
  stopifnot(is.subcort_labs(subcortLabs))
  subcortLabs <- as.numeric(subcortLabs) #- 2
  subcortLabs <- unmask_subcortex(subcortLabs, subcortMask, fill=fill)
  if (!is.null(trans_mat)) {
    subcortLabs <- RNifti::`sform<-`(subcortLabs, trans_mat)
  }
  RNifti::writeNifti(subcortLabs, subcortLabs_fname)

  ### Add back labels: subcortical structures.
  ### https://www.humanconnectome.org/software/workbench-command/-volume-help
  subcort_lab_list <- system.file(
    "extdata", "subcort_label_list.txt",
    package="ciftiTools"
  )
  cmd <- paste(
    "-volume-label-import",
    sys_path(subcortLabs_fname),
    sys_path(subcort_lab_list),
    sys_path(subcortLabs_fname)
  )
  run_wb_cmd(cmd, ignore.stderr=TRUE)

  # Mask (as numeric).
  subcortMask <- subcortMask + 0
  if (!is.null(trans_mat)) {
    subcortMask <- RNifti::`sform<-`(subcortMask, trans_mat)
  }
  if (!is.null(ROIsubcortVol_fname)) {
    RNifti::writeNifti(subcortMask, ROIsubcortVol_fname)
  }

  c(
    subcortVol=subcortVol_fname,
    subcortLabs=subcortLabs_fname,
    ROIsubcortVol=ROIsubcortVol_fname
  )
}
