
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
#' @param subcortLabs Numeric (0 and 3-22) or factor vector corresponding to
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

  # Checks. --------------------------------------------------------------------

  nC <- ncol(subcortVol)
  nV <- dim(subcortMask)

  if (!is.null(col_names)) {
    stopifnot(is.character(col_names) && length(col_names) == nC)
  }

  if (!is.null(trans_mat)) {
    stopifnot(is.nummat(trans_mat))
    trans_mat <- structure(trans_mat, code = 2L)
    pixdims <- sqrt(colSums(trans_mat[1:3,1:3]^2)) # should not be negative
  }

  # Data. ----------------------------------------------------------------------

  # Size check for `subcortVol`.
  subcortVol_GB <- nC * prod(nV) * 8 / 1024^3
  do_columnwise <- subcortVol_GB > 3

  # If the full array would be too big to construct, we will just construct
  ##  the first volume, write that, and then concatenate the rest of the data
  ##  directly into the file.
  # `subcortVol_initial` is what we'll construct as an array to write.
  subcortVol_initial <- if (!do_columnwise) {
    subcortVol
  } else {
    subcortVol[,1,drop=FALSE]
  }
  subcortVol_initial <- unvec_vol(subcortVol_initial, subcortMask, fill=fill)

  ## Write (the full array, or just the first volume). -------------------------
  ## https://github.com/jonclayden/RNifti/issues/5
  if (!is.null(trans_mat)) {
    subcortVol_initial <- RNifti::`pixdim<-`(subcortVol_initial, pixdims)
    subcortVol_initial <- RNifti::`sform<-`(subcortVol_initial, trans_mat)
    # # Alternative to above, if memory use is high?
    # attr(subcortVol_initial, "srow_x") <- trans_mat[1, ]
    # attr(subcortVol_initial, "srow_y") <- trans_mat[2, ]
    # attr(subcortVol_initial, "srow_z") <- trans_mat[3, ]
    # Do not set qform: redundant.
  }
  if (!is.null(trans_units)) {
    subcortVol_initial <- RNifti::`pixunits<-`(subcortVol_initial, trans_units)
  }
  RNifti::writeNifti(subcortVol_initial, subcortVol_fname)

  ## Add back names: if `dlabel` or `dscalar`. ---------------------------------
  if (!is.null(col_names)) {
    cmd <- paste(
      "-set-map-names",
      sys_path(subcortVol_fname),
      paste("-map", seq(length(col_names)), shQuote(col_names), collapse=" ")
    )
    run_wb_cmd(cmd, ignore.stderr=TRUE)
  }

  ## Add back labels: if `dlabel`. ---------------------------------------------
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
    run_wb_cmd(cmd, ignore.stderr=FALSE)
  }

  ## Labels. -------------------------------------------------------------------
  # Add "Other" level for older `xifti` objects.
  if (length(levels(subcortLabs)) != length(substructure_table()$ciftiTools_Name)) {
    subcortLabs <- factor(
      subcortLabs,
      levels = substructure_table()$ciftiTools_Name
    )
    stopifnot(is.subcort_labs(subcortLabs))
  }
  subcortLabs <- as.numeric(subcortLabs)
  subcortLabs <- unvec_vol(subcortLabs, subcortMask, fill=fill)
  if (!is.null(trans_mat)) {
    subcortLabs <- RNifti::`pixdim<-`(subcortLabs, pixdims)
    subcortLabs <- RNifti::`sform<-`(subcortLabs, trans_mat)
  }
  if (!is.null(trans_units)) {
    subcortLabs <- RNifti::`pixunits<-`(subcortLabs, trans_units)
  }
  RNifti::writeNifti(subcortLabs, subcortLabs_fname)

  # Add back labels: subcortical structures.
  # https://www.humanconnectome.org/software/workbench-command/-volume-help
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
  run_wb_cmd(cmd, ignore.stderr=FALSE)

  ## Write the rest of the volumes directly into the file if needed. -----------
  if (do_columnwise) {
    ### Begin: Adapted from Claude! --------------------------------------------
    # Get bytes per vox. Verify datatype and endianness.
    hdr <- RNifti::niftiHeader(subcortVol_fname)
    datatype <- hdr$datatype
    if (datatype == 16) {
      bytes_per_vox <- 4
      convert_fun <- function(x) as.single(x)
    } else if (datatype == 64) {
      bytes_per_vox <- 8
      convert_fun <- function(x) as.double(x)
    } else {
      stop("Unsupported datatype (", datatype, ") in written NIfTI. Expected 16 (float32) or 64 (float64).")
    }
    endian <- ifelse(hdr$sizeof_hdr == 348, "little", "big")
    vox_offset <- hdr$vox_offset

    # Patch header to reflect target number of timepoints.
    ## Use read-modify-write on raw bytes. Note: `seek` did not seem to work.

    ## Read and check before.
    con <- file(subcortVol_fname, open="rb")
    header_raw <- readBin(con, raw(), n=vox_offset)
    close(con)
    sizeof_hdr <- readBin(header_raw[1:4], integer(), size=4, endian=endian)
    if (sizeof_hdr != 348) {
      stop("sizeof_hdr is ", sizeof_hdr, " not 348 -- file may be corrupt or wrong endianness.")
    }

    ## Overwrite the relevant metadata. 
    ### dims. yes, this is needed for RNifti
    header_raw[41:42] <- writeBin(as.integer(4), raw(), size=2, endian=endian)
    ### the number of volumes.
    header_raw[49:50] <- writeBin(as.integer(nC), raw(), size=2, endian=endian)

    ## Check after. 
    sizeof_hdr_after <- readBin(header_raw[1:4], integer(), size=4, endian=endian)
    if (sizeof_hdr_after != 348) {
      stop("sizeof_hdr was corrupted during dim[4] patch.")
    }

    ## Write the header. 
    con <- file(subcortVol_fname, open="r+b")
    writeBin(header_raw, con, size=1)
    close(con)
    
    ## Verify dim[4].
    con <- file(subcortVol_fname, open="rb")
    dim4_check <- readBin(readBin(con, raw(), n=50)[49:50], integer(), size=2, endian=endian)
    close(con)
    if (dim4_check != nC) {
      stop("dim[4] write failed: expected ", nC, " but got ", dim4_check, ".")
    }

    # Append remaining volumes.
    if (nC > 1) {
      con <- file(subcortVol_fname, open="ab")
      for (ii in 2:nC) {
        vol_ii <- array(fill, dim=dim(subcortMask))
        vol_ii[subcortMask] <- subcortVol[, ii]
        writeBin(convert_fun(as.vector(vol_ii)), con, size=bytes_per_vox, endian=endian)
        rm(vol_ii)
      }
      close(con)
    }
    ### End: Adapted from Claude! ----------------------------------------------
  }

  # Mask (as numeric). --------------------------------------------------------
  subcortMask <- subcortMask + 0
  if (!is.null(trans_mat)) {
    subcortMask <- RNifti::`pixdim<-`(subcortMask, pixdims)
    subcortMask <- RNifti::`sform<-`(subcortMask, trans_mat)
  }
  if (!is.null(trans_units)) {
    subcortMask <- RNifti::`pixunits<-`(subcortMask, trans_units)
  }
  if (!is.null(ROIsubcortVol_fname)) {
    RNifti::writeNifti(subcortMask, ROIsubcortVol_fname)
  }

  # Return! --------------------------------------------------------------------
  c(
    subcortVol=subcortVol_fname,
    subcortLabs=subcortLabs_fname,
    ROIsubcortVol=ROIsubcortVol_fname
  )
}
