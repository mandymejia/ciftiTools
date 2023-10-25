#' Assemble a \code{"xifti"} object
#'
#' Assembles cortical data, subcortical data, and/or surface geometry to form a
#'  \code{"xifti"} object. The inputs can be file paths, GIFTI or NIFTI files
#'  which have been read in, or data objects (vectors, matrices or arrays,
#'  depending on the argument). See \code{as.xifti} for a user-function wrapper
#'  that only works with data objects.
#'
#' Each data or surface component is optional. Metadata components
#'  (\code{cortex[L/R]_mwall}, \code{subcortLabs}, and \code{subcortMask}) will
#'  be ignored if its corresponding data component is not provided. If no data or
#'  surface components are provided, then the \code{\link{template_xifti}} will
#'  be returned.
#'
#'  If cortical data are provided without a corresponding medial wall mask, or
#'  if the provided mask is invalid or empty, then the medial wall will be
#'  inferred from data rows that are constantly a value in \code{mwall_values}.
#'  But if \code{mwall_values} is \code{NULL}, no attempt to infer the medial
#'  wall will be made and the medial wall metadata entry will be \code{NULL}.
#'
#'  The total number of grayordinates will be
#'  \eqn{G = (V_L - mwall_L) + (V_R - mwall_R) + V_S}: \eqn{V_L - mwall_L} left
#'  vertices, \eqn{V_R - mwall_R} right vertices and \eqn{V_S} subcortical
#'  voxels. \eqn{T}, the total number of measurements (columns of data), must be
#'  the same for each brainstructure.
#'
#' @inheritSection labels_Description Label Levels
#'
#' @param cortexL,cortexL_mwall Left cortex data and ROI. Each must be a path to
#'  a GIFTI file, \code{"gifti"} object, data matrix or vector. (GIFTI data
#'  should be metric or label, not surface.)
#'
#'  If \code{cortexL_mwall} is not provided, \code{cortexL} should have data for
#'  all vertices on the left cortical surface (\eqn{V_L \times T} data matrix). There
#'  will not be a mask for the medial wall. Not providing the medial wall mask
#'  is appropriate for ".dlabels.nii" files where the medial wall may have its
#'  own label and therefore should not be treated as missing data.
#'
#'  If \code{cortexL_mwall} is provided, \code{cortexL} should either have data
#'  for all vertices on the left cortical surface (\eqn{V_L \times T} data matrix, with
#'  filler values e.g. \code{0} or \code{NaN} for medial wall vertices), or have data
#'  only for non-medial wall vertices (\eqn{(V_L - mwall_L) \times T} data matrix).
#'  The medial wall mask will be the \code{0} values in \code{cortexL_mwall}.
#'  The medial wall mask should be provided whenever the medial wall should be
#'  treated as missing data.
#'
#'  Since the unmasked cortices must have the same number of vertices,
#'  \code{V_L} should match \code{V_R}.
#' @param cortexR,cortexR_mwall Right cortex data and ROI. Each must be a path to
#'  a GIFTI file, \code{"gifti"} object, data matrix or vector. (GIFTI data
#'  should be metric or label, not surface.)
#'
#'  If \code{cortexR_mwall} is not provided, \code{cortexR} should have data for
#'  all vertices on the right cortical surface (\eqn{V_R \times T} data mre
#'  will not be a mask for the medial wall. Not providing the medial wall mask
#'  is appropriate for ".dlabels.nii" files where the medial wall may have its
#'  own label and therefore should not be treated as missing data.
#'
#'  If \code{cortexR_mwall} is provided, \code{cortexR} should either have data
#'  for all vertices on the right cortical surface (\eqn{V_R \times T} data matrix, with
#'  filler values e.g. \code{0} or \code{NaN} for medial wall vertices), or have data
#'  only for non-medial wall vertices (\eqn{(V_R - mwall_R) \times T} data matrix).
#'  The medial wall mask will be the \code{0} values in \code{cortexR_mwall}.
#'  The medial wall mask should be provided whenever the medial wall should be
#'  treated as missing data.
#'
#'  Since the unmasked cortices must have the same number of vertices,
#'  \code{V_L} should match \code{V_R}.
#' @param subcortVol,subcortLabs,subcortMask \code{subcortVol} represents the
#'  data values of the subcortex. It is either a NIFTI file path, 3D/4D data array
#'  (\eqn{i \times j \times k \times T}), or a vectorized data matrix (\eqn{V_S} voxels by \eqn{T}
#'  measurements). If it's vectorized, the voxels should be in spatial order
#'  (\eqn{i} index increasing fastest, then \eqn{j}, then \eqn{k}).
#'
#'  \code{subcortLabs} represents the brainstructure labels of each voxel: see
#'  \code{\link{substructure_table}}. It is either a NIFTI file path, 3D data array
#'  (\eqn{i \times j \times k}) of integer brainstructure indices, or a \eqn{V_S} length
#'  vector in spatial order with brainstructure names as factors or integer
#'  indices. The indices should be 3-21 (1 and 2 correspond to left and right
#'  cortex, respectively) or 1-19 (cortex labels omitted), with 0 representing
#'  out-of-mask voxels.
#'
#'  \code{subcortMask} is NIFTI file path or logical 3D data array (\eqn{i \times j \times k})
#'  where \code{TRUE} values indicate subcortical voxels (in-mask). If it is not
#'  provided, the mask will be inferred from voxels with labels \code{0},
#'  \code{NA}, or \code{NaN} in \code{subcortLabs}. If \code{subcortLabs} are
#'  vectorized and \code{subcortMask} is not provided, the mask cannot be
#'  inferred so an error will occur.
#' @param mwall_values If \code{cortex[L/R]_mwall} was not provided, or if it
#'  was invalid (i.e. bad length or all \code{TRUE}), the medial wall mask will
#'  be inferred from rows in \code{cortex[L/R]} that are constantly one of these
#'  values. Default: \code{c(NA, NaN)}. If \code{NULL}, do not attempt to infer
#'  the medial wall from the data values. \code{NULL} should be used if \code{NA}
#'  or \code{NaN} are legitimate values that non-medial wall vertices might
#'  take on.
#' @param cifti_info (Optional) The result of \code{\link{info_cifti}}. If
#'  GIFTI and/or NIFTI components from a CIFTI are being provided,
#'  providing \code{cifti_info} gives metadata information that would otherwise
#'  have to be inferred: the NIFTI intent, brainstructures present in the
#'  original file, and miscellaneous metadata.
#'
#'  This argument is probably not necessary for end users: reading a CIFTI
#'  should be done by providing \code{cifti_fname}, and for reading separate
#'  GIFTI/NIFTI components \code{cifti_info} is not applicable.
#'
#'  Column names from \code{cortexL} and \code{cortexR} take precedence over column
#'  names from \code{cifti_info}.
#' @param surfL,surfR (Optional) Surface geometries for the left or right cortex.
#'  Can be a surface GIFTI file path or \code{"surf"} object; see
#'  \code{\link{make_surf}} for a full description of valid inputs.
#' @param idx For file paths provided to \code{cortexL}, \code{cortexR}, and
#'  \code{subcortVol}: a numeric matrix indicating the data indices to read. If\
#'  \code{NULL} (default), read all the data. Must be a subset of the indices
#'  present in the file, or an error will occur. Note that if these arguments
#'  are data matrices instead of file paths, they are unaffected.
#' @param col_names Names of each measurement/column in the data. Overrides
#'  names indicated in \code{cifti_info} or in the data components.
#' @param HCP_32k_auto_mwall If left and/or right cortex data is provided, and
#'  the number of vertices matches that of the HCP 32k mesh (29696 on left, and
#'  29716 on right), should the medial wall masks be added to the \code{"xifti"}
#'  if not provided? Default: \code{TRUE}.
#' @param read_dir (Optional) Append a directory to all file names in the
#'  arguments. If \code{NULL} (default), do not modify file names.
#' @param validate Validate that the result is a \code{"xifti"} object? Default:
#'  \code{TRUE}. If \code{FALSE}, the result may not be properly formatted
#'  if the inputs were invalid.
#'
#' @return A \code{"xifti"} object; see \code{\link{template_xifti}}
#'
#' @keywords internal
#'
make_xifti <- function(
  cortexL=NULL, cortexL_mwall=NULL,
  cortexR=NULL, cortexR_mwall=NULL,
  subcortVol=NULL, subcortLabs=NULL, subcortMask=NULL,
  mwall_values=c(NA, NaN), cifti_info=NULL,
  surfL=NULL, surfR=NULL,
  idx=NULL,
  col_names=NULL, HCP_32k_auto_mwall=TRUE,
  read_dir=NULL, validate=TRUE) {

  # Add `read_dir` and check file paths.
  if (is.fname(cortexL)) { cortexL <- format_path(cortexL, read_dir, mode=4) }
  if (is.fname(cortexR)) { cortexR <- format_path(cortexR, read_dir, mode=4) }
  if (is.fname(subcortVol)) { subcortVol <- format_path(subcortVol, read_dir, mode=4) }
  if (is.fname(subcortLabs)) { subcortLabs <- format_path(subcortLabs, read_dir, mode=4) }
  if (is.fname(surfL)) { surfL <- format_path(surfL, read_dir, mode=4) }
  if (is.fname(surfR)) { surfR <- format_path(surfR, read_dir, mode=4) }

  if (is.fname(cortexL)) {
    if (!(endsWith(cortexL, "label.gii") || endsWith(cortexL, "func.gii"))) {
      warning("File path for `cortexL`, '", cortexL, "', does not look like metric GIFTI data.")
    }
  }
  if (is.fname(cortexR)) {
    if (!(endsWith(cortexR, "label.gii") || endsWith(cortexR, "func.gii"))) {
      warning("File path for `cortexR`, '", cortexR, "', does not look like metric GIFTI data.")
    }
  }
  if (is.fname(surfL)) {
    if (!(endsWith(surfL, "surf.gii"))) {
      warning("File path for `surfL`, '", surfL, "', does not look like GIFTI surface geometry.")
    }
  }
  if (is.fname(surfR)) {
    if (!(endsWith(surfR, "surf.gii"))) {
      warning("File path for `surfR`, '", surfR, "', does not look like GIFTI surface geometry.")
    }
  }

  # Template.
  xifti <- template_xifti()

  # CIFTI metadata.
  if (!is.null(cifti_info)) {
    xifti$meta$cifti <- cifti_info$cifti
    # misc_meta <- c("intent", "brainstructures", "misc")
    # xifti$meta$cifti[misc_meta] <- cifti_info$cifti[misc_meta]
    # if (!is.null(xifti$meta$cifti$intent) && xifti$meta$cifti$intent == 3002) {
    #   time_meta <- c("time_start", "time_step", "time_unit")
    #   xifti$meta$cifti[time_meta] <- cifti_info$cifti[time_meta]
    # } else if (xifti$meta$cifti$intent %in% c(3006, 3007)) {
    #   xifti$meta$cifti$names <- cifti_info$cifti$names
    # }
  }

  # Cortex data.
  if (!is.null(cortexL)) {
    x <- make_cortex(
      cortexL, cortexL_mwall, idx=idx, mwall_values=mwall_values,
      side="left", mwall_source="the input `cortexL_mwall`"
    )
    xifti$data$cortex_left <- x$data
    xifti$meta$cortex$medial_wall_mask["left"] <- list(x$mwall)

    ## Column names and label table, if intent is not dtseries.
    if (is.null(xifti$meta$cifti$intent) || xifti$meta$cifti$intent != 3002) {
      xifti$meta$cifti$names <- x$col_names
      if (!is.null(x$label_table)) {
        xifti$meta$cifti$labels <- rep(list(x$label_table), ncol(x$data))
        if (!is.null(x$col_names)) {
          names(xifti$meta$cifti$labels) <- x$col_names
        }
      }
    }

    ## HCP 32k medial wall
    if ((HCP_32k_auto_mwall & is.null(cortexL_mwall)) & nrow(xifti$data$cortex_left) == 29696) {
      if (is.null(xifti$meta$cortex$medial_wall_mask$left)) {
        # from ciftiTools R/sysdata.rda
        xifti$meta$cortex$medial_wall_mask$left <- ciftiTools.data$HCP_32k_mwall_template[,1]
      }
    }
  } else {
    if (!is.null(cortexL_mwall)) {
      warning("Ignoring left cortex ROI, since no left cortex data was provided.")
    }
  }
  if (!is.null(cortexR)) {
    x <- make_cortex(
      cortexR, cortexR_mwall, idx=idx, mwall_values=mwall_values,
      side="right", mwall_source="the input `cortexR_mwall`"
    )
    xifti$data$cortex_right <- x$data
    xifti$meta$cortex$medial_wall_mask["right"] <- list(x$mwall)

    ## Column names and label table.
    if (is.null(xifti$meta$cifti$intent) || xifti$meta$cifti$intent != 3002) {
      if (!is.null(x$col_names)) {
        if (!is.null(xifti$meta$cifti$names)) {
          if (length(x$col_names) != length(xifti$meta$cifti$names)) {
            stop("The left and right cortex data had a different number of columns.")
          }
          if (!all(x$col_names == xifti$meta$cifti$names)) {
            warning(paste0(
              "The column names of the left cortex did not match the column names ",
              "of the right cortex. The column names of the left cortex were:\n\t",
              paste(xifti$meta$cifti$names, collapse=","), "\n\n",
              "whereas the column names of the right cortex were:\n\t",
              paste(x$col_names, collapse=","), "\n\n",
              "Pasting them together.\n"
            ))
            xifti$meta$cifti$names <- paste(x$col_names, xifti$meta$cifti$names, " ||| ")
          }
        }
      } else {
        xifti$meta$cifti$names <- x$col_names
      }
      if (!is.null(x$label_table)) {
        if (!is.null(xifti$meta$cifti$labels)) {
          if (!identical(x$label_table, xifti$meta$cifti$labels[[1]])) {
            warning(paste(
              "The label tables for the left and right cortex were not",
              "identical. Using the label table from the left cortex.\n"
            ))
          }
        } else {
          xifti$meta$cifti$labels <- rep(list(x$label_table), ncol(x$data))
          if (!is.null(xifti$meta$cifti$col_names)) {
            names(xifti$meta$cifti$labels) <- xifti$meta$cifti$col_names
          }
        }
      }
    }

    ## HCP 32k medial wall
    if ((HCP_32k_auto_mwall & is.null(cortexR_mwall)) & nrow(xifti$data$cortex_right) == 29716) {
      if (is.null(xifti$meta$cortex$medial_wall_mask$right)) {
        # from ciftiTools R/sysdata.rda
        xifti$meta$cortex$medial_wall_mask$right <- ciftiTools.data$HCP_32k_mwall_template[,2]
      }
    }
  } else {
    if (!is.null(cortexR_mwall)) {
      warning("Ignoring right cortex ROI, since no right cortex data was provided.")
    }
  }

  # Subcortical data.
  if (xor(is.null(subcortVol), is.null(subcortLabs))) {
    stop("subcortVol and subcortLabs must be provided together.")
  }
  if (!is.null(subcortVol)) {
    x <- make_subcort(subcortVol, subcortLabs, subcortMask, idx=idx, validate_mask=FALSE)
    xifti$data$subcort <- x$data
    xifti$meta$subcort$labels <- x$labels
    xifti$meta$subcort$mask <- x$mask
    xifti$meta$subcort["trans_mat"] <- list(x$trans_mat)
    # No subcortical column names.
  }

  # Surfaces.
  if (!is.null(surfL)) { xifti$surf$cortex_left <- make_surf(surfL, "left") }
  if (!is.null(surfR)) { xifti$surf$cortex_right <- make_surf(surfR, "right") }

  # Column names.
  if (!is.null(col_names)) { xifti$meta$cifti$names <- col_names }

  # Brainstructures.
  # bs_names <- names(xifti$data)[!vapply(xifti$data, is.null, FALSE)]
  # bs_names <- c(cortex_left="left", cortex_right="right", subcort="subcortical")[bs_names]
  # xifti$meta$cifti$brainstructures <- bs_names
  xifti$meta$cifti["brainstructures"] <- list(NULL)

  # idx metadata.
  if (!is.null(idx)) { xifti$meta$cifti$misc$idx <- idx }

  # Validate.
  if (validate) {
    if (!is.xifti(xifti)) { stop("Could not make a valid \"xifti\" object.") }

    # Special case: disable future warnings about medial wall mask size.
    for (h in c("left", "right")) {
      if (!is.null(xifti$meta$cortex$medial_wall_mask[[h]]) && mean(xifti$meta$cortex$medial_wall_mask[[h]]) < .5) {
        if (is.null(xifti$meta$cifti$small_ROI)) { xifti$meta$cifti$small_ROI <- TRUE }
      }
    }
  }

  structure(xifti, class="xifti")
}
