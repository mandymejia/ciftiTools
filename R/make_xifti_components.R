#' Make the cortical components of a \code{"xifti"}
#'
#' Coerce a path to a GIFTI file, metric or label \code{"gifti"} object,
#'  data matrix or vector to a data matrix representing cortical data
#'  (and optionally a corresponding mask).
#'  That is, entries for \code{xifti$data$cortex_\[left/right\]} and
#'  \code{xifti$meta$cortex$medial_wall_mask$\[left/right\]}. If \code{cortex} is
#'  a path to a GIFTI file or a metric \code{"gifti"}, any column names or
#'  a non-empty label table will also be extracted.
#'
#' @param cortex A path to a metric or label GIFTI file, metric or label
#'  \code{"gifti"}, or numeric vector/matrix representing cortical data.
#'
#'  If \code{cortex} is a path to a GIFTI file or \code{"gifti"},
#'  the column names and the label table will also be extracted if they
#'  exist in the GIFTI.
#' @param mwall A path to a metric or label GIFTI file, metric or label \code{"gifti"},
#'  \eqn{V_{[L/R]} x 1} logical matrix or length \eqn{V_{[L/R]}} logical vector
#'  representing the medial wall mask. \code{FALSE} values should indicate
#'  vertices that make up the medial wall. If the medial wall is unknown, use
#'  \code{NULL} (default).
#' @param idx Only applies if \code{cortex} is a GIFTI file path. This is
#'  a numeric vector indicating the data indices to read. If \code{NULL}
#'  (default), read all the data. Must be a subset of the indices present in the
#'  file, or an error will occur.
#' @param cortex_is_masked Has the medial wall been masked from \code{cortex}
#'  yet? \code{NULL} (default) indicates whether it has been masked or not is
#'  unknown.
#'
#' If \code{!cortex_is_masked}, \code{cortex} should be \eqn{V_{[L/R]} x T}.
#'  If \code{cortex_is_masked}, \code{cortex} should be
#'  \eqn{(V_{[L/R]} - mwall_{[L/R]}) x T}. If \code{is.null(cortex_is_masked)},
#'  it may be either or.
#' @param rm_blank_mwall If the medial wall mask is all \code{TRUE}
#'  (indicating no medial wall vertices), should it be discarded? Default:
#'  \code{TRUE}. If \code{FALSE}, keep it.
#' @param rm_bad_mwall If the medial wall mask doesn't match up with the
#'  data (e.g. the vertex count doesn't add up), should it be discarded?
#'  Default: \code{TRUE}. If \code{FALSE}, raise an error.
#' @param mwall_values If the medial wall mask was not provided (or if it was
#'  discarded), infer it from rows in \code{cortex} that are constantly one of
#'  these values. Example: \code{c(0, NA, NaN)}. If \code{NULL} (default),
#'  do not attempt to infer the medial wall mask from the data values.
#' @param side \code{"left"} or \code{"right"}? Just used to print warnings.
#' @param mwall_source Description of where the mwall came from. Just used
#'  to print warnings.
#'
#' @return A list with components "data", "mwall", "col_names" and "labels".
#'
#' @importFrom gifti readgii is.gifti
#'
#' @keywords internal
#'
make_cortex <- function(
  cortex, mwall=NULL, idx=NULL,
  cortex_is_masked=NULL,
  rm_blank_mwall=TRUE,
  rm_bad_mwall=TRUE,
  mwall_values=NULL,
  side=NULL,
  mwall_source=NULL) {

  if (is.null(side)) {side <- ""}
  if (is.null(mwall_source)) {mwall_source <- ""}

  col_names <- label_table <- NULL

  # Cannot infer the medial wall if the cortex has been masked.
  infer_mwall <- !is.null(mwall_values)
  if (!is.null(cortex_is_masked) && cortex_is_masked) { infer_mwall <- FALSE }

  # Cortex:
  #   File --> GIFTI.
  if (is.fname(cortex)) {

    # Create new GIFTI with selected columns, if specified.
    if (!is.null(idx)) {
      stopifnot(all(idx > 0) && all(idx == round(idx)))
      idx_is_seq <- (length(idx) > 2 && all(diff(idx) == 1))
      if (idx_is_seq) {
        idx_string <- paste("-column", idx[1], "-up-to", idx[length(idx)])
      } else {
        idx_string <- paste("-column", paste(idx, collapse=" -column "))
      }
      cortex_original <- cortex

      # Name for new GIFTI file
      if (endsWith(cortex, ".func.gii")) {
        cortex <- file.path(
          tempdir(),
          gsub("\\.func\\.gii$", "\\.sel_idx\\.func\\.gii", basename(cortex_original))
        )
      } else if (endsWith(cortex, ".label.gii")) {
        cortex <- file.path(
          tempdir(),
          gsub("\\.label\\.gii$", "\\.sel_idx\\.label\\.gii", basename(cortex_original))
        )
      } else {
        # Guess func.
        cortex <- file.path(
          tempdir(), paste0(basename(cortex_original), ".func.gii")
        )
      }

      run_wb_cmd(paste(
        "-metric-merge", sys_path(cortex),
        "-metric", sys_path(cortex_original), idx_string
      ))
    }

    cortex <- readgii(cortex)
  }
  #   GIFTI --> matrix.
  if (is.gifti(cortex)) {

    ## Extract column names if present.
    get_col_name <- function(x){
      if (!is.matrix(x)) { return("") }
      if (!all(sort(colnames(x)) == sort(c("names", "vals")))) { return("") }
      name_match <- x[,colnames(x) == "names"] == "Name"
      if (sum(name_match) > 0) {
        if (sum(name_match) > 1) {
          ciftiTools_warn("Multiple \"Name\" metadata entries. Using first only.")
        }
        return(x[,colnames(x) == "vals"][name_match])
      } else {
        return("")
      }
    }
    col_names <- as.character(vapply(cortex$data_meta, get_col_name, ""))
    if (length(unique(col_names)) == 1 && unique(col_names)=="") {
      col_names <- NULL
    }

    ## Extract labels if present.
    if (!is.null(nrow(cortex$label)) && nrow(cortex$label) > 1) {
      label_table <- as.data.frame(cortex$label)
      label_table[,] <- apply(label_table, 2, as.numeric)
    }

    ## Extract data matrix.
    cortex <- do.call(cbind, cortex$data)
    dimnames(cortex) <- NULL

  } else {
    if (!is.numeric(cortex) && !is.matrix(cortex)) {
      stop(paste(
        "`cortex` was not an existing file (check file name?),",
        "a result of `gifti::readgii()`,",
        "or a numeric matrix."
      ))
    }

  }

  if (is.vector(cortex)) { cortex <- matrix(cortex, ncol=1) }

  # Check if medial wall mask is valid.
  msg <- ""
  if (!is.null(mwall)) {

    # File --> GIFTI.
    if (is.fname(mwall)) {
      mwall <- readgii(mwall)
    }
    # GIFTI --> vector.
    if (is.gifti(mwall)) {
      mwall <- do.call(cbind, mwall$data)
      dimnames(mwall) <- NULL
    }
    if (!is.vector(mwall) && ncol(mwall) > 1) {
      stop("`mwall` should be a vector of values (or column vector) but it had more than one column.")
    }
    mwall <- as.logical(mwall)

    # Handle case if no medial wall vertices were detected.
    if (all(mwall)) {
      msg <- paste0(msg,
        "The ",side," medial wall mask obtained from ",mwall_source,
        " was all TRUE (indicating no medial wall vertices). "
      )
      if (rm_blank_mwall) {
        msg <- paste0(msg,"It was deleted. ")
        mwall <- NULL
      }
    } else {
      same_len <- length(mwall) == nrow(cortex)
      same_pos <- sum(mwall) == nrow(cortex)
      if (!is.null(cortex_is_masked)) {
        if (cortex_is_masked) {
          if (!same_pos) {
            msg <- paste0(msg,
              "The ",side," medial wall mask obtained from ",mwall_source," has ",
              sum(mwall)," non-medial-wall locations,but there are ",nrow(cortex),
              " vertices in the masked data matrix."
            )
            if (rm_bad_mwall) {
              msg <- paste0(msg,"The ",side," medial wall mask was deleted. ")
              mwall <- NULL
            } else { stop(msg) }
          }
        } else if (!cortex_is_masked) {
          if (!same_len) {
            msg <- paste0(msg,
              "The ",side," medial wall mask obtained from ",mwall_source," has ",
              length(mwall)," total locations,but there are ",nrow(cortex),
              " total vertices in the data matrix."
            )
            if (rm_bad_mwall) {
              msg <- paste0(msg,"The ",side," medial wall mask was deleted. ")
              mwall <- NULL
            } else { stop(msg) }
          }
        }
      } else {
        # Only one of `same_len` and `same_pos` will be TRUE b/c !all(mwall)
        cortex_is_masked <- same_pos
      }
    }
  }

  # If no valid medial wall mask, optionally infer it.
  if (is.null(mwall)) {
    if (infer_mwall) {
      new_mwall <- !apply(matrix(cortex %in% mwall_values, nrow=nrow(cortex)), 1, all)
      if (any(!new_mwall)) {
        msg <- paste0(
          msg,"A new medial wall mask for the ",side," cortex was inferred."
        )
        mwall <- new_mwall
      } else {
        msg <- paste0(
          msg,"No medial wall vertices inferred from data. Leaving the medial ",
          "wall mask field empty for ", side, " cortex. "
        )
      }
    } else {
      msg <- paste0(
        msg,"Leaving medial wall mask field empty for ", side, " cortex. "
      )
    }
  }

  if (!(msg == "")) { ciftiTools_msg(msg) }

  # Apply a valid medial wall mask to an unmasked cortex.
  if (!is.null(mwall)) {
    if (is.null(cortex_is_masked)) { cortex_is_masked <- FALSE }
    if (!cortex_is_masked) {
      if (nrow(cortex) != length(mwall)) {
        stop("Cortex and medial wall have different numbers of data locations.")
      }
      cortex <- cortex[mwall,, drop=FALSE]
      cortex_is_masked <- TRUE
    }
  }

  list(data = cortex, mwall = mwall, col_names=col_names, label_table=label_table)
}

#' Make the subcortical transformation matrix
#'
#' Make the transformation matrix for the subcortical volume from the sform
#'  data in the header.
#'
#' @param nii_fname Path to NIFTI file
#'
#' @return 4x4 matrix representing the transformation matrix. (This includes
#'  the last row, \code{c(0,0,0,1)}).
#'
#' @importFrom oro.nifti nifti_header
#'
#' @keywords internal
#'
make_trans_mat <- function(nii_fname) {
  head <- oro.nifti::nifti_header(nii_fname)
  labs_trans_mat <- rbind(head@srow_x, head@srow_y, head@srow_z)
  if (!isTRUE(all.equal(dim(labs_trans_mat), c(3, 4)))) {
    stop("trans_mat had unexpected dimensions.")
  }
  rbind(labs_trans_mat, matrix(c(0,0,0,1), nrow=1))
}

#' Make the subcortical components of a \code{"xifti"}
#'
#' Coerce subcortical data into valid entries for \code{xifti$data$subcort}
#'  and \code{xifti$meta$subcort}. The data arguments can be matrices/arrays or
#'  NIFTI file paths. If the mask is not provided, it will be inferred from the
#'  labels.
#'
#' To read in the labels as the primary data, use the labels NIFTI for both
#'  \code{vol} and \code{labs}.
#'
#' @inheritSection labels_Description Label Levels
#'
#' @param vol represents the data values of the subcortex. It is either a NIFTI
#'  file path, 3D/4D data array (\eqn{i \times j \times k \times T}), or a vectorized data
#'  matrix (\eqn{V_S} voxels by \eqn{T} measurements). If it's vectorized, the voxels
#'  should be in spatial order.
#' @param labs represents the brainstructure labels of each voxel: see
#'  \code{\link{substructure_table}}. It is either a NIFTI file path, a 3D data array
#'  (\eqn{i \times j \times k}) of numeric brainstructure indices, or a \eqn{V_S} length
#'  vector in spatial order with brainstructure names as factors or integer
#'  indices. The indices should be 3-22 (2 and 3 correspond to left and right
#'  cortex, respectively) or 1-19 (cortex labels omitted), with 0 representing
#'  out-of-mask voxels.
#' @param mask is a NIFTI file path or logical 3D data array (\eqn{i \times j \times k}) where \code{TRUE}
#'  values indicate subcortical voxels (in-mask). If it is not provided, the
#'  mask will be inferred from voxels with labels \code{0} or \code{NA} in
#'  \code{subcortLabs}. If \code{subcortLabs} are vectorized and \code{subcortMask}
#'  is not provided, the mask cannot be inferred so an error will occur.
#' @param idx Only applies if \code{vol} is a NIFTI file path. This is a numeric
#'  vector indicating the data indices to read. If \code{NULL} (default), read
#'  all the data. Must be a subset of the indices present in the file, or an
#'  error will occur.
#' @param validate_mask If \code{mask} is provided, set this to \code{TRUE} to
#'  check that the mask only removes voxels with \code{NA} and \code{0} values
#'  in \code{vol} and \code{labs}. Default: \code{FALSE} (saves time).
#'
#' @return A list with components "data", "labels", "mask", and "trans_mat". The
#'  first two will be vectorized and ordered spatially.
#'
#'  The volume can be recovered using:
#'    \code{vol <- unvec_vol(data, mask, fill=NA)}
#'    \code{labs <- unvec_vol(labels, mask, fill=0)}
#'
#' @importFrom RNifti readNifti
#' @importFrom oro.nifti nifti_header
#'
#' @keywords internal
#'
make_subcort <- function(
  vol, labs, mask=NULL, idx=NULL, validate_mask=FALSE) {

  vol_trans_mat <- labs_trans_mat <- mask_trans_mat <- NULL

  # Get vol.
  if (is.fname(vol)) {
    vol_trans_mat <- try(make_trans_mat(vol), silent=TRUE)
    vol <- readNifti(vol, volumes=idx)
  }
  vol_ndims <- length(dim(vol))
  if (vol_ndims == 0) { stop("`vol` did not have any dimensions. Check that it is a matrix or array?") }
  if (vol_ndims == 1) { vol <- matrix(vol, ncol=1) }
  vol_is_vectorized <- vol_ndims < 3

  # Get labels.
  labs_from_fname <- is.fname(labs)
  if (labs_from_fname) {
    labs_trans_mat <- try(make_trans_mat(labs), silent=TRUE)
    labs_table_fname <- file.path(tempdir(), "temp.txt")
    run_wb_cmd(
      paste(
        "-volume-label-export-table", sys_path(labs), 1,
        sys_path(labs_table_fname)
      )
    )
    labs_table <- readLines(labs_table_fname)
    labs_names <- labs_table[seq(length(labs_table)/2)*2 - 1]
    labs_levels <- as.numeric(gsub(" .*", "", labs_table[seq(length(labs_table)/2)*2]))
    labs <- readNifti(labs)
    # use names, dont assume the numbers will match. done after masking below.
  }
  labs_ndims <- length(dim(labs))
  labs_is_vectorized <- labs_ndims < 3

  # Infer mask if not provided.
  if (is.null(mask)) {
    if (!labs_is_vectorized) {
      mask <- labs > 0 & (!is.na(labs) & !is.nan(labs))
      if (validate_mask) {
        mask_vol <- apply(vol!=0 & !is.na(vol), c(1,2,3), all)
        if(!isTRUE(all.equal(mask, mask_vol))) {
          stop("The mask inferred from the labels did not match the mask inferred from the volume (NA/0 values).")
        }
      }
    #} else if (!vol_is_vectorized) {
    #  mask <- apply(vol!=0 & !is.na(vol), c(1,2,3), all)
    } else {
      stop("The mask could not be inferred: if the mask is not provided, the labels must not be vectorized.")
    }
  # Otherwise, validate it if requested.
  } else {
    if (is.fname(mask)) {
      mask_trans_mat <- try(make_trans_mat(mask), silent=TRUE)
      mask <- readNifti(mask)
    }
    if (is.numeric(mask)) { mask <- mask > 0 }
    stopifnot( is.logical(mask) )
    if (validate_mask) {
      stop_msg <- ""
      if (!labs_is_vectorized) {
        mask_labs <- labs > 0 | !is.na(labs)
        if(!isTRUE(all.equal(mask, mask_labs))) {
          stop_msg <- paste0(stop_msg, "The input mask did not match the mask inferred from the labels (NA/0 values). ")
        }
      }
      if (!vol_is_vectorized) {
        mask_vol <- apply(vol!=0 | !is.na(vol), c(1,2,3), all)
        if(!isTRUE(all.equal(mask, mask_vol))) {
          stop_msg <- paste0(stop_msg, "The input mask did not match the mask inferred from the volume (NA/0 values). ")
        }
      }
      if (stop_msg != "") { stop(stop_msg) }
    }
  }

  if (sum(mask) == 0) { stop("The subcortical mask was empty (all `FALSE`).") }

  # Apply mask.
  if (!vol_is_vectorized) {
    vol <- matrix(vol[mask], nrow=sum(mask))
  } else {
    if (nrow(vol) != sum(mask)) {
      stop(paste0(
        "The number of subcortical voxels in the vectorized data (`vol`), ",
        nrow(vol), ", ",
        "did not match the size of the subcortical mask, ",
        sum(mask), "."
      ))
    }
  }
  if (!labs_is_vectorized) {
    labs <- labs[mask]
  } else {
    if (length(labs) != sum(mask)) {
      stop(paste0(
        "The number of subcortical voxels in the vectorized labels (`labs`), ",
        length(labs), ", ",
        "did not match the size of the subcortical mask, ",
        sum(mask), "."
      ))
    }
  }
  rm(labs_is_vectorized) # it is definitely a vector now.

  # Get the labels
  sub_table <- substructure_table()
  if (labs_from_fname) {
    labs <- labs_names[match(labs, labs_levels)]
  } else if (is.numeric(labs)) {
    stopifnot(all(labs %in% c(0, seq(3, 22))))
    labs <- sub_table$Original_Name[labs]
  } else if (!identical(levels(labs), sub_table$ciftiTools_Name)) {
    labs <- factor(
      labs,
      levels=sub_table$Original_Name,
      labels=sub_table$ciftiTools_Name
    )
  }
  stopifnot(is.subcort_labs(labs))
  stopifnot(!any(is.na(labs)))

  # Get trans_mat. Only use if all were the same.
  trans_mat <- list(vol=vol_trans_mat, labs=labs_trans_mat, mask=mask_trans_mat)
  trans_mat <- suppressMessages(trans_mat[vapply(trans_mat, is.nummat, FALSE)])
  if (length(trans_mat) > 0) {
    if (length(trans_mat) == 1) {
      trans_mat <- trans_mat[[1]]
    } else {
      for (ii in seq_len(length(trans_mat))) {
        for (jj in seq_len(length(trans_mat))) {
          if (ii <= jj) {next}
          trans_mat_diff <- max(abs(as.vector(trans_mat[[ii]] - trans_mat[[jj]])))
          if (trans_mat_diff > ciftiTools.getOption("EPS")) {
            warning(paste0(
              "sform transformation matrix for ", names(trans_mat)[[ii]], " and ",
              names(trans_mat)[[jj]], " (for the subcortical data) did not ",
              " match. Discarding both."
            ))
            trans_mat <- NULL
            break
          }
        }
      }
      trans_mat <- trans_mat[[1]]
    }
  } else {
    trans_mat <- NULL
  }

  list(
    data = vol,
    labels = labs,
    mask = mask,
    trans_mat = trans_mat
  )
}
