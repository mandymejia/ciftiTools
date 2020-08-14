## [TO DO]: is.xifti_map

#' Validate a numeric matrix.
#' 
#' Check if object is a numeric matrix.
#' 
#' This is a helper function for \code{\link{is.xifti}}.
#' 
#' @param x The putative numeric matrix
#' 
#' @return Logical indicating whether x is a valid numeric matrix.
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
#'  brainordinates along the rows and measurements along the columns.
#'  The cortical matrices should have the same number of rows (vertices), and
#'  all should have the same number of columns (measurements). At least one data
#'  entry should be present.
#'
#' @param x The putative "data" component.
#'
#' @return Logical indicating whether x is a valid "data" component.
#'
is.xifti_data <- function(x) {
  # Check that the entries are as expected.
  y <- template_xifti()$data
  if (!match_exactly(names(x), names(y))) { 
    message("List names are not correct.\n"); return(FALSE) 
  }

  ## There must be at least one non-empty data entry.
  #not_null <- names(x)[!sapply(x, is.null)]
  #if (length(not_null) < 1) { 
  #  message("At least one entry in \"data\" must be non-empty.\n"); return(FALSE)
  #}

  # Non-empty entries should be numeric matrices.
  not_null <- names(x)[!sapply(x, is.null)]
  for (ii in not_null) { if (!is.nummat(x[[ii]])) { return(FALSE) } }

  # Present entries must have the same number of measurements (columns).
  n_meas <- sapply(x[not_null], ncol)
  if (length(unique(n_meas)) > 1) {
    message(paste0(
      paste( 
        "The left cortex, right cortex, and subcortical data (when present)",
        "must all have the same number of measurements (columns),",
        "but they do not."
      ),
      "\nThe column counts are:",
      paste(n_meas, collapse=","),
      "\n"
    ))
    return(FALSE)
  }

  TRUE
}

#' Validate a surface (vertices + faces)
#'
#' Check if object is valid for \code{xifti$surf$cortex_left} or 
#'  \code{xifti$surf$cortex_right}, where \code{xifti} is a \code{"xifti"}
#'  object.
#' 
#'  This is a helper function for \code{\link{is.xifti}}.
#' 
#'  Requirements: the surface must be a list of two components: "vertices" and 
#'  "faces", each a numeric matrix with three columns. The values in "vertices"
#'  represent spatial coordinates whereas the values in "faces" represent
#'  vertex indices defining the face. Thus, values in "faces" should be integers
#'  between 1 and the number of vertices. 
#'
#' @param x The putative surface.
#'
#' @return Logical indicating whether x is a valid surface.
#' @export
#' 
is.xifti_surface <- function(x) {
  if (!match_exactly(names(x), c("vertices", "faces"))) {
    return(FALSE)
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

  if (!all.equal(x$faces, round(x$faces), is.attributes=FALSE)) {
    message("x$faces must be only integers.\n"); return(FALSE)
  }

  if (max(x$faces) > nrow(x$vertices)) {
    message("The max vertex index in x$faces is too high.\n"); return(FALSE)
  }
  if (min(x$faces) < 1) {
    message("The min vertex index in x$faces is too low.\n"); return(FALSE)
  }

  TRUE
}

#' Validate a factor vector of subcortical labels
#' 
#' Check if object is a factor vector of subcortical structures.
#'  This is a helper function for \code{\link{is.xifti}}.
#' 
#'  Requirements: see the "Label Levels" section for the expected factor levels.
#'
#' @param x The putative factor vector of brain substructures.
#'
#' @return Logical indicating whether x is a factor vector of subcortical 
#'  structures.
#'
#' @inheritSection labels_Description Label Levels
#' 
is.subcort_labels <- function(x) {
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
#' @return Logical indicating whether x is a valid subcortical mask.
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

#' Validate the "meta" component of a \code{"xifti"} object
#'
#' Check if object is valid for \code{xifti$meta}, where \code{xifti} is a 
#'  \code{"xifti"} object.
#' 
#'  This is a helper function for \code{\link{is.xifti}}.
#'
#'  Requirements: the structure must match that of the "meta" component of 
#'  \code{\link{template_xifti}}. 
#'
#' @param x The putative "meta" component.
#'
#' @return Logical indicating whether x is a valid "meta" component.
#' 
is.xifti_meta <- function(x) {
  y <- template_xifti()$meta
  if (!match_exactly(names(x), names(y))) { 
    message("List names are not correct.\n"); return(FALSE) 
  }

  # Cortex.
  if (!match_exactly(names(x$cortex), names(y$cortex))) { 
    message("Cortex sublist names are not correct.\n"); return(FALSE) 
  }
  if (!is.null(x$cortex$resamp_resolution) && !is.numeric(x$cortex$resamp_resolution)) {
    message("Cortex resampling resolution must be numeric.\n"); return(FALSE)
  }
  if (!is.null(x$cortex$medial_wall_mask$left)) {
    if (!is.logical(x$cortex$medial_wall_mask$left)) {
      message("Left cortex medial wall mask must be logical.\n"); return(FALSE)
    }
    if (mean(x$cortex$medial_wall_mask$left) < .5) {
      message(paste(
        "Warning: FALSE values in left medial wall mask should indicate",
        "the medial wall vertices. But, there were more FALSE values than TRUE."
      ))
      return(FALSE)
    }
  }
  if (!is.null(x$cortex$medial_wall_mask$right)) {
    if (!is.logical(x$cortex$medial_wall_mask$right)) {
      message("Right cortex medial wall mask must be logical.\n"); return(FALSE)
    }
    if (mean(x$cortex$medial_wall_mask$right) < .5) {
      message(paste(
        "Warning: FALSE values in right medial wall mask should indicate",
        "the medial wall vertices. But, there were more FALSE values than TRUE."
      ))
      return(FALSE)
    }
  }
  if ((!is.null(x$cortex$medial_wall_mask$left)) && !is.null(x$cortex$medial_wall_mask$right)) {
    if (length(x$cortex$medial_wall_mask$left) != length(x$cortex$medial_wall_mask$right)) {
      message(paste(
        "The medial wall masks must be the same lengths, because the cortices",
        "must have the same number of vertices."
      ))
      return(FALSE)
    }
  }

  # Subcortical.
  if (!match_exactly(names(x$subcort), names(y$subcort))) { 
    message("Subcortical sublist names are not correct.\n"); return(FALSE) 
  }
  if (!is.null(x$subcort$labels) && !is.subcort_labels(x$subcort$labels)) {
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

  # [TO DO]: Check `cifti` component

  TRUE
}

#' Validate a \code{"xifti"} object.
#' 
#' Check if object is valid for a \code{"xifti"} object.
#'
#'  Requirements: the structure must match that of \code{\link{template_xifti}}. 
#'  The size of each data entry must be compatible with the corresponding mask.
#'  Metadata should be present if and only if the corresponding data is also 
#'  present (except for the cortex resampling resolution).
#' 
#'  See the "Label Levels" section for the requirements of 
#'  \code{xifti$meta$subcort$labels}.
#' 
#' @param x The putative \code{"xifti"} object.
#' @param messages If \code{x} is not a \code{"xifti"}, print messages 
#'  explaining the problem? Default is \code{TRUE}.
#' 
#' @return Logical indicating whether x is a valid \code{"xifti"} object.
#' @export
#' 
#' @inheritSection labels_Description Label Levels
#' 
is.xifti <- function(x, messages=TRUE) {
  if (!messages) { return(suppressMessages(is.xifti(x, messages=FALSE))) }

  y <- template_xifti()
  if (!match_exactly(names(x), names(y))) { 
    message("List names are not correct.\n"); return(FALSE) 
  }

  # ----------------------------------------------------------------------------
  # Check individual components. -----------------------------------------------
  # ----------------------------------------------------------------------------

  if (!is.xifti_data(x$data)) { message('"data" is invalid.\n'); return(FALSE) }
  for (s in names(x$surf)) {
    if (!is.null(x$surf[[s]]) && !is.xifti_surface(x$surf[[s]])) {
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
    if (!is.null(x$surf[[cortex]])) {
      # [TO DO]: Remove this check?
      if (is.null(x$meta$cortex$medial_wall_mask[[side]])) {
        message(paste0("The ", side, " cortex surface geometry is present, but the data is not.\n"))
        return(FALSE)
      }

      if (length(x$meta$cortex$medial_wall_mask[[side]]) != nrow(x$surf[[cortex]]$vertices)) {
        message(paste0(
          "Number of vertices in", side, 
          "cortex data (including the medial wall)",
          "(length(x$meta$cortex$medial_wall_mask$[side]))",
          "does not match its corresponding surface geometry.",
          "(nrow(x$surf$cortex_[side]))", "\n"
        ))
        return(FALSE)
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

#' Validate a \code{"xifti"} object.
#' 
#' Check if object is valid for a \code{"xifti"} object. This alias for 
#' \code{\link{is.xifti}} is offered as a convenience, and a message will warn 
#' the user. We recommend using \code{\link{is.xifti}} instead.
#' 
#' @param x The putative \code{"xifti"} object.
#' @param messages If \code{x} is not a \code{"xifti"}, print messages 
#'  explaining the problem? Default is \code{TRUE}.
#' 
#' @return Logical indicating whether x is a valid \code{"xifti"} object.
#' @export
#' 
#' @inheritSection labels_Description Label Levels
#' 
is.cifti <- is_cifti <- isCIfTI <- function(x, messages=TRUE){
  warning("is.cifti() is an alias for is.xifti().\n")
  is.xifti(x, messages=messages)
}