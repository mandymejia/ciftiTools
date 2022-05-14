#' Validate a numeric matrix
#' 
#' Check if object is a numeric matrix.
#' 
#' This is a helper function for \code{\link{is.xifti}}.
#' 
#' @param x The putative numeric matrix
#' 
#' @return Logical. Is \code{x} a valid numeric matrix?
#' 
#' @keywords internal
#' 
is.nummat <- function(x) {
  if (!is.matrix(x) || !is.numeric(x)) {
    message("The data must be a numeric matrix.\n"); return(FALSE)
  }

  TRUE
}

#' Validate the "data" component of a \code{"xifti"} object
#'
#' Check if object is valid for \code{xifti$data}, where \code{xifti} is a 
#'  \code{"xifti"} object.
#' 
#'  This is a helper function for \code{\link{is.xifti}}.
#'
#'  Requirements: a list with entries "cortex_left", "cortex_right", and
#'  "subcort". Each must be either \code{NULL} or a numeric matrix with
#'  grayordinates along the rows and measurements along the columns.
#'  The cortical matrices should have the same number of rows (vertices), and
#'  all should have the same number of columns (measurements). At least one data
#'  entry should be present.
#'
#' @param x The putative "data" component.
#'
#' @return Logical. Is \code{x} a valid "data" component?
#' 
#' @keywords internal
#'
is.xifti_data <- function(x) {
  # Check that the entries are as expected.
  y <- template_xifti()$data
  if (!match_exactly(names(x), names(y))) { 
    message("List names are not correct.\n"); return(FALSE) 
  }

  ## There must be at least one non-empty data entry.
  #not_null <- names(x)[!vapply(x, is.null, FALSE)]
  #if (length(not_null) < 1) { 
  #  message("At least one entry in \"data\" must be non-empty.\n"); return(FALSE)
  #}

  # Non-empty entries should be numeric matrices.
  not_null <- names(x)[!vapply(x, is.null, FALSE)]
  for (ii in not_null) { 
    if (!is.nummat(x[[ii]])) {
      message(
        "`xifti` data can be coerced to numeric matrices using `fix_xifti`.\n"
      )
      return(FALSE)
    } 
  }

  if (!is.null(x$cortex_left) && nrow(x$cortex_left) > 200000) {
    warning("The left cortex has over 200,000 vertices. Is this a mistake?")
  }
  if (!is.null(x$cortex_right) && nrow(x$cortex_right) > 200000) {
    warning("The right cortex has over 200,000 vertices. Is this a mistake?")
  }
  if (!is.null(x$subcort) && nrow(x$subcort) > 200000) {
    warning("The subcortex has over 200,000 voxels. Is this a mistake?")
  }

  # Present entries must have the same number of measurements (columns).
  n_meas <- vapply(x[not_null], ncol, 1)
  if (length(unique(n_meas)) > 1) {
    message(paste0(
      paste( 
        "The left cortex, right cortex, and subcortical data (when present)",
        "must all have the same number of measurements (columns),",
        "but they do not."
      ),
      "\nThe column counts are: ",
      paste(n_meas, collapse=", "),
      "\n"
    ))
    return(FALSE)
  }

  TRUE
}

#' Validate a \code{"surf"} object (vertices + faces)
#'
#' Check if object is valid for \code{xifti$surf$cortex_left} or 
#'  \code{xifti$surf$cortex_right}, where \code{xifti} is a \code{"xifti"}
#'  object.
#' 
#'  This is a helper function for \code{\link{is.xifti}}.
#' 
#'  Requirements: the \code{"surf"} must be a list of three components: "vertices", 
#'  "faces", and "hemisphere". The first two should each be a numeric matrix 
#'  with three columns. The values in "vertices" represent spatial coordinates 
#'  whereas the values in "faces" represent vertex indices defining the face. 
#'  Thus, values in "faces" should be integers between 1 and the number of 
#'  vertices. The last list entry, "hemisphere", should be "left", "right", 
#'  or NULL indicating the brain hemisphere which the surface represents.
#'
#' @param x The putative \code{"surf"}.
#'
#' @return Logical. Is \code{x} a valid \code{"surf"}?
#' 
#' @family surfing
#' @export
#' 
is.surf <- function(x) {
  if (!is.list(x)) { message("x must be a list.\n"); return(FALSE) }

  if (!match_exactly(names(x), c("vertices", "faces", "hemisphere"))) {
    return(FALSE)
  }

  if (!is.matrix(x$vertices)) {
    message("x$vertices must be a 3-column matrix.\n"); return(FALSE)
  }
  if (!is.matrix(x$faces)) {
    message("x$faces must be a 3-column matrix.\n"); return(FALSE)
  }

  if (ncol(x$vertices) != 3) {
    message("x$vertices must have 3 columns.\n"); return(FALSE)
  }
  if (ncol(x$faces) != 3) {
    message("x$faces must have 3 columns.\n"); return(FALSE)
  }

  if (!is.numeric(x$faces)) {
    message("x$faces must be numeric.\n"); return(FALSE)
  }
  if (!is.numeric(x$vertices)) {
    message("x$vertices must be numeric.\n"); return(FALSE)
  }

  if (!isTRUE(all.equal(x$faces, round(x$faces), is.attributes=FALSE))) {
    message("x$faces must be only integers.\n"); return(FALSE)
  }

  if (max(x$faces) > nrow(x$vertices)) {
    message("The max vertex index in x$faces is too high.\n"); return(FALSE)
  }
  if (min(x$faces) < 1) {
    message("The min vertex index in x$faces is too low.\n"); return(FALSE)
  }

  if (!is.null(x$hemisphere)) {
    if (!(x$hemisphere %in% c("left", "right"))) {
      message("x$hemisphere must be \"left\" or \"right\".\n"); return(FALSE)
    }
  }

  TRUE
}

#' Validate a factor vector of subcortical labels
#' 
#' Check if object is a factor vector of subcortical structures.This is a helper
#'  function for \code{\link{is.xifti}}.
#' 
#' Requirements: see the "Label Levels" section for the expected factor levels.
#'
#' @inheritSection labels_Description Label Levels
#' 
#' @param x The putative factor vector of brain substructures.
#'
#' @return Logical. Is \code{x} a factor vector of subcortical
#'  structures?
#' 
#' @keywords internal
#' 
is.subcort_labs <- function(x) {
  if (!is.factor(x)) {
    message("The labels must be a factor.\n"); return(FALSE)
  }

  substructure_levels <- substructure_table()$ciftiTools_Name
  if (!match_exactly(levels(x), substructure_levels)) { 
    message("Level names did not match.\n"); return(FALSE) 
  }

  TRUE
}

#' Validate a 3d binary mask
#'
#' Check if object is a 3D binary mask.
#'
#'  This is a helper function for \code{\link{is.xifti}}.
#' 
#'  Requirements: the mask must be a boolean 3D array. \code{TRUE} should
#'  indicate voxels included in the mask, whereas \code{FALSE} 
#'  should indicate voxels outside of it.
#'
#' @param x The putative subcortical mask.
#'
#' @return Logical. Is \code{x} a valid subcortical mask?
#' 
#' @keywords internal
#' 
is.3D_mask <- function(x) {
  if (!is.array(x) || !is.logical(x)) {
    message("Mask must be a logical array.\n"); return(FALSE)
  }

  if (length(dim(x)) != 3) {
    message("Mask must be 3-dimensional, but it is not.\n")
    return(FALSE)
  }

  TRUE
}

#' Validate the \code{"meta"} component of a \code{"xifti"} object
#'
#' Check if object is valid for \code{xifti$meta}, where \code{xifti} is a 
#'  \code{"xifti"} object.
#' 
#'  This is a helper function for \code{\link{is.xifti}}.
#'
#'  Requirements: the structure must match that of the \code{"meta"} component
#'  of \code{\link{template_xifti}}. 
#'
#' @param x The putative \code{"meta"} component.
#'
#' @return Logical. Is \code{x} a valid \code{"meta"} component?
#' 
#' @keywords internal
#' 
is.xifti_meta <- function(x) {
  if (!is.list(x)) { message("x must be a list.\n"); return(FALSE) }

  y <- template_xifti()$meta
  if (!match_exactly(names(x), names(y))) { 
    message("List names are not correct.\n"); return(FALSE) 
  }

  # Cortex.
  if (!match_exactly(names(x$cortex), names(y$cortex))) { 
    message("Cortex sublist names are not correct.\n"); return(FALSE) 
  }
  if (!is.null(x$cortex$medial_wall_mask$left)) {
    if (!is.logical(x$cortex$medial_wall_mask$left)) {
      message("Left cortex medial wall mask must be logical.\n"); return(FALSE)
    }
    # [TO DO]: Cleanup.
    if (mean(x$cortex$medial_wall_mask$left) < .5) {
      if (!(!is.null(x$cifti$small_ROI) && isTRUE(x$cifti$small_ROI))) {
        warning(paste(
          "Warning: FALSE values in left medial wall mask should indicate",
          "the medial wall vertices. But, there were more FALSE values than TRUE."
        ))
      }
    }
  }
  if (!is.null(x$cortex$medial_wall_mask$right)) {
    if (!is.logical(x$cortex$medial_wall_mask$right)) {
      message("Right cortex medial wall mask must be logical.\n"); return(FALSE)
    }
    # [TO DO]: Cleanup.
    if (mean(x$cortex$medial_wall_mask$right) < .5) {
      if (!(!is.null(x$cifti$small_ROI) && isTRUE(x$cifti$small_ROI))) {
        warning(paste(
          "Warning: FALSE values in right medial wall mask should indicate",
          "the medial wall vertices. But, there were more FALSE values than TRUE."
        ))
      }
    }
  }
  if ((!is.null(x$cortex$medial_wall_mask$left)) && !is.null(x$cortex$medial_wall_mask$right)) {
    if (length(x$cortex$medial_wall_mask$left) != length(x$cortex$medial_wall_mask$right)) {
      # warning(
      #   'Support for `"xifti"` objects in which the cortices do not have the same number of vertices is limited.'
      # )
      #return(FALSE)
    }
  }

  # Subcortical.
  if (!match_exactly(names(x$subcort), names(y$subcort), fail_action="nothing")) { 
    ny2 <- names(y$subcort)[names(y$subcort) != "trans_units"]
    if (!match_exactly(names(x$subcort), ny2, fail_action="message")) { 
      message("Subcortical sublist names are not correct.\n"); return(FALSE) 
    }
  }
  if (!is.null(x$subcort$labels) && !is.subcort_labs(x$subcort$labels)) {
    message("Subcortical labels are invalid.\n"); return(FALSE)
  }
  if (!is.null(x$subcort$mask) && !is.3D_mask(x$subcort$mask)) {
    message("Subcortical mask is invalid.\n"); return(FALSE)
  }
  if (!is.null(x$subcort$labels) && !is.null(x$subcort$mask)) {
    if (length(x$subcort$labels) != sum(x$subcort$mask)) {
      message("Number of subcortical labels doesn't match the mask size.\n")
      return(FALSE)
    }
  }
  if (!is.null(x$subcort$trans_mat) && !is.nummat(x$subcort$trans_mat)) {
    message("Subcortical transformation matrix is invalid.\n"); return(FALSE)
  }
  if (!is.null(x$subcort$trans_units)) {
    if (!is.character(x$subcort$trans_units)) {
      message("Subcortical trans. matrix units are invalid.\n"); return(FALSE)
    } else if (!endsWith(x$subcort$trans_units, "m")) {
      message("Subcortical trans. matrix units are invalid.\n"); return(FALSE)
    }
  }

  # cifti
  if (!is.null(x$cifti)) {
    if (!is.null(x$cifti$brainstructures)) {
      # All entries must be valid.
      if (!all(x$cifti$brainstructures %in% c("left", "right", "subcortical"))) {
        message(paste(
          "CIFTI brainstructures must be one or several of the following:",
          "left, right, subcortical.\n"
        ))
        return(FALSE)
      }
      # All brainstructures with data must be included.
      bs_expected <- names(x$data)[!vapply(x$data, is.null, FALSE)]
      bs_missing <- setdiff(bs_expected, x$cifti$brainstructures)
      if (length(bs_missing) > 0) {
        message(paste(
          "These brainstructures with data are not in $meta$cifti$brainstructures:",
          paste(bs_missing, collapse=", "), 
          ". Add them or set this metadata entry to `NULL`.\n"
        ))
        return(FALSE)
      }
      # [TO DO]: check `bs_expected`
      # I forget if `brainstructures` are those originally in the `xifti`, or just those present now?
    }

    if (!is.null(x$cifti$intent)) {
      intent <- x$cifti$intent
      if (intent == 3002) {
        if (!is.null(x$cifti$time_start)) {
          if (!is.numeric(x$cifti$time_start) || length(x$cifti$time_start)!=1) {
            message("cifti$time_start must be a number.\n"); return(FALSE)
          }
        }
        if (!is.null(x$cifti$time_step)) {
          if (!is.numeric(x$cifti$time_step) || length(x$cifti$time_step)!=1) {
            message("cifti$time_step must be a number.\n"); return(FALSE)
          }
        }
        if (!is.null(x$cifti$time_unit)) {
          time_units <- c("second", "hertz", "meter", "radian")
          if (!(x$cifti$time_unit %in% time_units)) {
            message("cifti$time_unit must be second, hertz, meter, or radian.\n")
            if (tolower(x$cifti_time_unit) %in% time_units) {
              message("Use lowercase.\n")
            }
            return(FALSE)
          }
        }
      } else if (intent == 3006) {
        if (!is.null(x$cifti$names)) {
          if (!is.character(x$cifti$names)) {
            message("cifti$names must be a character vector.\n"); return(FALSE)
          }
        }
      } else if (intent == 3007) {
        for (lab in x$cifti$labels) {
          if (!is.null(lab)) {
            if (!is.data.frame(lab)) {
              message("cifti$labels must be a data.frame.\n"); return(FALSE)
            }
            if (!all(colnames(lab) == c("Key", "Red", "Green", "Blue", "Alpha"))) {
              message("cifti$labels columns must be Key, Red, Green, Blue, Alpha.\n")
              return(FALSE)
            }
          }
        }
      }
      intent_specific_names <- switch(as.character(intent),
        `3002` = c("time_start", "time_step", "time_unit"),
        `3006` = c("names"),
        `3007` = c("names", "labels")
      )
      cifti_meta_names <- c(
        "intent", "brainstructures", "misc", 
        intent_specific_names
      )
      bad_names <- !(names(x$cifti) %in% cifti_meta_names)
      if (any(bad_names)) {
        extn_name <- supported_intents()$extension[which(supported_intents()$value == x$cifti$intent)]
        message(paste0(
          "The following metadata field(s) in $cifti is invalid for ",
          "intent ", x$cifti$intent, " (", extn_name, "):",
          paste(names(x$cifti)[bad_names], collapse=", ")
        ))
      }
    }
  }

  TRUE
}

#' Validate a \code{"xifti"} object.
#' 
#' Check if object is valid for a \code{"xifti"} object.
#'
#' Requirements: it is a list with the same structure as 
#'  \code{\link{template_xifti}}. The size of each data entry must be 
#'  compatible with its corresponding mask (medial wall for the cortex and 
#'  volumetric mask for the subcortex). Metadata should be present if and only 
#'  if the corresponding data is also present. The surfaces can be present
#'  whether or not the cortex data are present.
#' 
#'  See the "Label Levels" section for the requirements of 
#'  \code{xifti$meta$subcort$labels}.
#' 
#' @inheritSection labels_Description Label Levels
#' 
#' @param x The putative \code{"xifti"} object.
#' @param messages If \code{x} is not a \code{"xifti"} object, print messages 
#'  explaining the problem? Default is \code{TRUE}.
#' 
#' @return Logical. Is \code{x} a valid \code{"xifti"} object?
#' 
#' @export
#' 
is.xifti <- function(x, messages=TRUE) {
  if (!messages) { return(suppressMessages(is.xifti(x, messages=TRUE))) }

  if (!is.list(x)) { message("x must be a list.\n"); return(FALSE) }

  y <- template_xifti()
  if (!match_exactly(names(x), names(y))) { 
    message("List names are not correct.\n"); return(FALSE) 
  }

  # ----------------------------------------------------------------------------
  # Check individual components. -----------------------------------------------
  # ----------------------------------------------------------------------------

  if (!is.xifti_data(x$data)) { message('"data" is invalid.\n'); return(FALSE) }
  for (s in names(x$surf)) {
    if (!is.null(x$surf[[s]]) && !is.surf(x$surf[[s]])) {
      message(paste(s, "is invalid.\n")); return(FALSE)
    }
  }
  if (!is.xifti_meta(x$meta)) { message('"meta" is invalid.\n'); return(FALSE) }

  # ----------------------------------------------------------------------------
  # Check compatibility between components. ------------------------------------
  # ----------------------------------------------------------------------------

  # Metadata should be present only if the corresponding data is present.
  # Note: okay if cortex data is present but medial wall is not, because
  #   read_cifti_convert() sometimes the metadata does not indicate the
  #   medial wall.
  if (!is.null(x$data$cortex_left) && !is.null(x$meta$cortex$medial_wall_mask$left)) {
    if (nrow(x$data$cortex_left) != sum(x$meta$cortex$medial_wall_mask$left)) {
      message(paste0(
        "Number of left cortex vertices (rows), ", nrow(x$data$cortex_left), 
        ", doesn't match ",
        "the number of non-medial wall locations in the mask, ", 
        sum(x$meta$cortex$medial_wall_mask$left), ".\n"
      ))
      return(FALSE)
    }
  }
  if (is.null(x$data$cortex_left) && !is.null(x$meta$cortex$medial_wall_mask$left)) {
    message("Left medial wall mask is present, but the data is not.\n"); return(FALSE)
  }
  if (!is.null(x$data$cortex_right) && !is.null(x$meta$cortex$medial_wall_mask$right)) {
    if (nrow(x$data$cortex_right) != sum(x$meta$cortex$medial_wall_mask$right)) {
      message(paste0(
        "Number of right cortex vertices (rows), ", nrow(x$data$cortex_right), 
        ", doesn't match ",
        "the number of non-medial wall locations in the mask, ", 
        sum(x$meta$cortex$medial_wall_mask$right), ".\n"
      ))
      return(FALSE)
    }
  }
  if (is.null(x$data$cortex_right) && !is.null(x$meta$cortex$medial_wall_mask$right)) {
    message("Right medial wall mask is present, but the data is not.\n"); return(FALSE)
  }
  if (!is.null(x$data$subcort)) {
    if (is.null(x$meta$subcort$mask)) {
      message("Subcortical data is missing its mask.\n"); return(FALSE)
    }
    if (nrow(x$data$subcort) != sum(x$meta$subcort$mask)) {
      message("Number of subcortical observations (rows) doesn't match its mask size.\n")
      return(FALSE)
    }
    if (is.null(x$meta$subcort$labels)) {
      message("Subcortical data is missing its labels.\n"); return(FALSE)
    }
  } else {
    if (!is.null(x$meta$subcort$mask)) {
      message("Subcortical mask is present, but the data is not.\n"); return(FALSE)
    }
    if (!is.null(x$meta$subcort$labels)) {
      message("Subcortical labels is present, but the data is not.\n"); return(FALSE)
    }
  }

  # The surface geometry of each cortex, if present, must be compatible with it.
  for (side in c("left", "right")) {
    cortex <- paste0("cortex_", side)
    if (!is.null(x$surf[[cortex]]) && !is.null(x$meta$cortex$medial_wall_mask[[side]])) {
      if (length(x$meta$cortex$medial_wall_mask[[side]]) != nrow(x$surf[[cortex]]$vertices)) {
        message(paste0(
          "Number of vertices in ", side, 
          " cortex data (including the medial wall), ",
          length(x$meta$cortex$medial_wall_mask[[side]]),
          ", does not match the number of vertices in the corresponding surface, ",
          nrow(x$surf[[cortex]]$vertices), ". Are they the same resolution?\n"
        ))
        return(FALSE)
      }
    }
  }

  # For intent 3006 (.dscalar.nii), each measurement should be named.
  if (!is.null(x$meta$cifti$intent) && (x$meta$cifti$intent == 3006)) {
    if (!is.null(x$meta$cifti$names)) {
      if (length(x$meta$cifti$names) != ncol(x)) {
        message("There must be as many meta$cifti$names as there are data columns.\n")
        return(FALSE)
      }
    }
  }

  # For intent 3007 (.dlabels.nii), each data measurement (column) should
  #   have a corresponding label table, and all labels in the data should be
  #   listed in the corresponding table.
  if (!is.null(x$meta$cifti$intent) && (x$meta$cifti$intent == 3007)) {
    data_mat <- as.matrix(x)
    if (!is.null(x$meta$cifti$labels) && !is.null(data_mat) && nrow(data_mat) > 0) {
      if (ncol(data_mat) != length(x$meta$cifti$labels)) {
        message("Number of labels does not match number of data columns.\n")
        return(FALSE)
      }

      vl_show_max <- 20

      if (!is.null(x$data$cortex_left)) {
        for (ii in seq_len(ncol(x$data$cortex_left))) {
          all_labels <- unique(data_mat[,ii])
          valid_label <- all_labels %in% x$meta$cifti$labels[[ii]]$Key
          if (!all(valid_label)) {
            vl_show <- which(!valid_label)
            vl_trnc <- length(vl_show) > vl_show_max
            if (vl_trnc) { vl_show <- vl_show[seq(vl_show_max)] }
            message(paste(
              "These label values in the left cortex data column", ii, 
              "are not in the corresponding label table:\n\t",
              paste(all_labels[vl_show], collapse=", "), 
              ifelse(vl_trnc, "[TRUNCATED]", ""), "\n"
            ))
            return(FALSE)
          }
        }
      }

      if (!is.null(x$data$cortex_right)) {
        for (ii in seq_len(ncol(x$data$cortex_right))) {
          all_labels <- unique(data_mat[,ii])
          valid_label <- all_labels %in% x$meta$cifti$labels[[ii]]$Key
          if (!all(valid_label)) {
            vl_show <- which(!valid_label)
            vl_trnc <- length(vl_show) > vl_show_max
            if (vl_trnc) { vl_show <- vl_show[seq(vl_show_max)] }
            message(paste(
              "These label values in the right cortex data column", ii, 
              "are not in the corresponding label table:\n\t",
              paste(all_labels[vl_show], collapse=", "), 
              ifelse(vl_trnc, "[TRUNCATED]", ""), "\n"
            ))
            return(FALSE)
          }
        }
      }

      if (!is.null(x$data$subcort)) {
        for (ii in seq_len(ncol(x$data$subcort))) {
          all_labels <- unique(data_mat[,ii])
          valid_label <- all_labels %in% x$meta$cifti$labels[[ii]]$Key
          if (!all(valid_label)) {
            vl_show <- which(!valid_label)
            vl_trnc <- length(vl_show) > vl_show_max
            if (vl_trnc) { vl_show <- vl_show[seq(vl_show_max)] }
            message(paste(
              "These label values in the subcortex data column", ii, 
              "are not in the corresponding label table:\n\t",
              paste(all_labels[vl_show], collapse=", "), 
              ifelse(vl_trnc, "[TRUNCATED]", ""), "\n"
            ))
            return(FALSE)
          }
        }
      }

    }
  }

  TRUE
}

#' @rdname is.xifti
#' @export
is_xifti <- function(x, messages=TRUE){
  is.xifti(x, messages=messages)
}

#' Validate a \code{"xifti"} object
#' 
#' Check if object is valid for a \code{"xifti"}. This alias for 
#'  \code{\link{is.xifti}} is offered as a convenience, and a message will warn 
#'  the user. We recommend using \code{\link{is.xifti}} instead.
#'
#' Requirements: it is a list with the same structure as 
#'  \code{\link{template_xifti}}. The size of each data entry must be 
#'  compatible with its corresponding mask (medial wall for the cortex and 
#'  volumetric mask for the subcortex). Metadata should be present if and only 
#'  if the corresponding data is also present. The surfaces can be present
#'  whether or not the cortex data are present.
#' 
#'  See the "Label Levels" section for the requirements of 
#'  \code{xifti$meta$subcort$labels}.
#' 
#' @inheritSection labels_Description Label Levels
#' 
#' @param x The putative \code{"xifti"}.
#' @param messages If \code{x} is not a \code{"xifti"}, print messages 
#'  explaining the problem? Default is \code{TRUE}.
#' 
#' @return Logical. Is \code{x} a valid \code{"xifti"}?
#' 
#' @family common
#' @export
#' 
is.cifti <- function(x, messages=TRUE){
  warning("is.cifti() is an alias for is.xifti().\n")
  is.xifti(x, messages=messages)
}

#' @rdname is.cifti
#' @export
is_cifti <- function(x, messages=TRUE){
 is.cifti(x, messages=messages) 
}

#' @rdname is.cifti
#' @export
isCIfTI <- function(x, messages=TRUE){
 is.cifti(x, messages=messages) 
}