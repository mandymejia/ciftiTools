## [TO DO]: is.cifti_map

#' Validate a "cifti_data" object.
#'
#' Check if object is valid for
#'  \code{cifti$CORTEX_LEFT},
#'  \code{cifti$CORTEX_RIGHT},
#'  \code{cifti$SUBCORT}, or
#'  \code{cifti$DAT},
#'  where \code{cifti} is a \code{"cifti"} or  \code{"cifti_flat"} object.
#' 
#'  This is a helper function for \code{\link{is.cifti}}.
#'
#'  Requirements: the data must be a numeric matrix.
#'
#' @param x The putative "cifti_data" object.
#'
#' @return Logical indicating whether x is a valid "cifti_data" object.
#' @export
#'
is.cifti_data <- function(x) {
  if (!is.matrix(x) || !is.numeric(x)) {
    message("CIFTI data must be a numeric matrix.\n"); return(FALSE)
  }

  TRUE
}

#' Check a factor vector of brain substructures.
#' 
#' Check if object is a factor vector of brain susbstructures.
#'  This is a helper function for \code{\link{is.cifti}}.
#' 
#'  Requirements: see the "Label Levels" section for the expected factor levels.
#'
#' @param x The putative factor vector of brain substructures.
#'
#' @return Logical indicating whether x is a factor vector of brain 
#'  substructures.
#' @export
#'
#' @inheritSection labels_Description Label Levels
#' 
is.cifti_substructures <- function(x) {

  # [TO-DO]: Check factor is a vector? is.vector(factor()) fails...

  if (!is.factor(x)) {
    message("The labels must be a factor.\n"); return(FALSE)
  }

  substructure_levels <- substructure_table()$ciftiTools_Name
  length_mismatch <- length(levels(x))!=length(substructure_levels)
  if (length_mismatch || !all(levels(x) == substructure_levels)) {
    message(paste0(
      "The labels must have the following factor levels:\n",
      "\t\"", paste(substructure_levels, collapse="\", \""), "\".\n",
      "Instead, its levels are:\n",
      "\t\"", paste(levels(x), collapse="\", \""), "\".\n"
    ))
    return(FALSE)
  }

  TRUE
}

#' Validate a "cifti_labels" object.
#'
#' Check if object is valid for \code{cifti$LABELS}, where \code{cifti} may be
#'  a "cifti" or \code{"cifti_flat"} object. 
#'  This is a helper function for \code{\link{is.cifti}}.
#'
#'  Requirements when \code{!flat}: the labels must be a length-3 list with
#'  entries "CORTEX_LEFT", "CORTEX_RIGHT", and "SUBCORT". Each must be a factor 
#'  vector, or \code{NULL} if that element is not included in the "cifti". Note
#'  that the subcortical voxels should be in spatial order, so the labels should
#'  also be in spatial order instead of ascending/alphabetical order.
#' 
#'  Requirements when \code{flat}: the labels must be a data.frame with 
#'  brainordinates along the rows, and two columns "BRAINSTRUCTURE" and 
#'  "SUBSTRUCTURE". The "BRAINSTRUCTURE" column must be a factor with levels 
#'  "CORTEX_LEFT", "CORTEX_RIGHT", and "SUBCORT". Any left cortex vertices
#'  should be listed first (in order), followed by any right cortex
#'  vertices (in order), followed by any subcortical voxels (in 
#'  ascending/alphabetical order). The "SUBSTRUCTURE" column must indicate
#'  the substructures; see \code{\link{is.cifti_substructures}}.
#'
#' @param x The putative "cifti_labels" object.
#' @param flat Is \code{x} for a "cifti_flat"? Default: \code{FALSE} (\code{x}
#'  is for a "cifti"). The required structure of the "cifti_labels" object
#'  is different depending on \code{FLAT}.
#'
#' @return Logical indicating whether x is a valid "cifti_labels" object.
#' @export
#' 
#' @inheritSection labels_Description Label Levels
#'
is.cifti_labels <- function(x, flat=FALSE) {
  stopifnot(is.logical(flat) && length(flat)==1)

  bs_names <- c("CORTEX_LEFT", "CORTEX_RIGHT", "SUBCORT")

  # Requirements if x is for a \code{"cifti"} object.
  if (!flat) {
    if (!valid_list(x, bs_names, user_value_label="Labels")) {
      return(FALSE)
    }

    for (name in bs_names) {
      if (!is.null(x[[name]]) && !is.cifti_substructures(x[[name]])) {
        message("Entry \"", name, "\" must be valid substructure labels.\n")
        return(FALSE)
      }
    }

  # Requirements if x is for a \code{"cifti_flat"} object.
  } else {
    if (!is.data.frame(x) || colnames(x) != c("BRAINSTRUCTURE", "SUBSTRUCTURE")) {
      message(paste(
        "Labels must be a `data.frame` with column names",
        "\"BRAINSTRUCTURE\" and \"SUBSTRUCTURE\".\n"
      ))
      return(FALSE)
    }

    if (!is.cifti_substructures(x$SUBSTRUCTURE)) {
      message("SUBSTRUCTURE column must be valid substructure labels.\n")
      return(FALSE)
    }

    if (!is.factor(x$BRAINSTRUCTURE) || !identical(levels(x$BRAINSTRUCTURE), bs_names)) {
      message(paste(
        "BRAINSTRUCTURE column must be a factor vector with levels:",
        "\"CORTEX_LEFT\", \"CORTEX_RIGHT\", and \"SUBCORT\".\n"
      ))
      return(FALSE)
    }
  }

  TRUE
}

#' Validate a "cifti_surface" object.
#'
#' Check if object is valid for \code{cifti$SURF_LEFT} or 
#'  \code{cifti$SURF_RIGHT}, where \code{cifti} is a \code{"cifti"} or 
#'   \code{"cifti_flat"} object.
#' 
#'  This is a helper function for \code{\link{is.cifti}}.
#' 
#'  Requirements: the surface must be a list of two components: "vertices" and 
#'  "faces", each a numeric matrix with three columns. The values in "vertices"
#'  represent spatial coordinates whereas the values in "faces" represent
#'  vertex indices defining the face. Thus, values in "faces" should be integers
#'  between 1 and the number of vertices. 
#'
#' @param x The putative "cifti_surface" object.
#'
#' @return Logical indicating whether x is a valid "cifti_surface" object.
#' @export
#' 
is.cifti_surface <- function(x) {
  if (!valid_list(x, c("vertices", "faces"), user_value_label="Surface")) {
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

  if (!all.equal(x$faces, round(x$faces), check.attributes=FALSE)) {
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

#' Validate a "cifti_subcort_mask" object.
#'
#' Check if object is valid for \code{cifti$META$SUBCORT_MASK}, where 
#'  \code{cifti} is a \code{"cifti"} or  \code{"cifti_flat"} object.
#'
#'  This is a helper function for \code{\link{is.cifti}}.
#' 
#'  Requirements: the mask must be a boolean 3D array. \code{TRUE} should
#'  indicate voxels within the subcortical structure, whereas \code{FALSE} 
#'  should indicate voxels outside of it.
#'
#' @param x The putative "cifti_subcort_mask" object.
#'
#' @return Logical indicating whether x is a valid "cifti_subcort_mask" object.
#' @export
#' 
is.cifti_subcort_mask <- function(x) {
  if (!is.array(x) || !is.logical(x)) {
    message("Subcortical mask must be a logical array.\n"); return(FALSE)
  }

  if (length(dim(x)) != 3) {
    message("Subcortical mask must be 3-dimensional, but it is not.\n")
    return(FALSE)
  }

  TRUE
}

#' Check a subcortical mask padding.
#' 
#' Check if object is valid for \code{cifti$META$SUBCORT_MASK_PADDING}, where 
#'  \code{cifti} is a \code{"cifti"} or  \code{"cifti_flat"} object.
#'
#'  Standard NIFTI dimensions are 91 x 109 x 91. Often, slices at the edges of 
#'  each dimension are empty. \code{ciftiTools} removes this padding from the
#'  volumetric subcortical mask, \code{cifti$META$SUBCORT_MASK}. 
#'  \code{cifti$META$SUBCORT_MASK_PADDING} enables the recovery of the original
#'  NIFTI dimensions. 
#' 
#'  This is a helper function for \code{\link{is.cifti}}.
#' 
#'  Requirements: it must be a list with entries named "i", "j", and "k". Each
#'  should be a length-2 integer vector indicating the number of slices removed
#'  before and after the brain volume along the corresponding dimension.
#'
#' @param x The putative subcortical mask padding.
#'
#' @return Logical indicating whether x is a valid subcortical mask padding.
#' @export
#' 
is.cifti_subcort_mask_padding <- function(x) {
  dim_names <- c("i", "j", "k")
  if (!valid_list(x, dim_names, user_value_label="padding metadata")) {
    return(FALSE)
  }

  for (name in dim_names) {
    # [TO DO]: Check for NA or numeric.
    if (!is.vector(x[[name]]) || length(x[[name]]) != 2) {
      message(paste("Entry", name, "must be valid substructure labels.\n"))
      return(FALSE)
    }
  }

  TRUE
}

#' Validate a "cifti_meta" object.
#'
#' Check if object is valid for \code{cifti$META}, where 
#'  \code{cifti} is a \code{"cifti"} or  \code{"cifti_flat"} object.
#'
#'  This is a helper function for \code{\link{is.cifti}}.
#' 
#'  Requirements: the metadata is a list with at least the entries 
#'  "CORTEX_RESOLUTION", "SUBCORT_MASK" and "SUBCORT_MASK_PADDING". There
#'  may be additional entries. The cortex resolution must be a positive integer
#'  or \code{NULL} if unknown. "SUBCORT MASK" cannot be \code{NULL}.
#'  See \code{\link{is.cifti_subcort_mask}} for its requirements.
#'  See \code{\link{is.cifti_subcort_mask_padding}} for the 
#'  requirements for the subcortical mask padding if it is not \code{NULL}.
#'
#' @param x The putative "cifti_meta" object.
#'
#' @return Logical indicating whether x is a valid "cifti_meta" object.
#' @export
#' 
is.cifti_meta <- function(x) {
  meta_names <- c("CORTEX_RESOLUTION", "SUBCORT_MASK", "SUBCORT_MASK_PADDING")
  # Allow for other entries too.
  if (!is.list(x) || !(all(meta_names %in% names(x)))) {
    message(paste(
      "Metatdata must be a list with at least the entries",
      "\"CORTEX_RESOLUTION\", \"SUBCORT_MASK\" and \"SUBCORT_MASK_PADDING\".\n"
    ))
    return(FALSE)
  }
  
  if (!is.null(x$CORTEX_RESOLUTION)) {
    if (!is.numeric(x$CORTEX_RESOLUTION) || length(x$CORTEX_RESOLUTION)>1) {
      message("x$CORTEX_RESOLUTION should be a single integer.\n")
      return(FALSE)
    } 

    if (x$CORTEX_RESOLUTION != round(x$CORTEX_RESOLUTION)) {
      message("x$CORTEX_RESOLUTION should be a single integer.\n")
      return(FALSE)
    }
  }

  if (!is.null(x$SUBCORT_MASK) && !is.cifti_subcort_mask(x$SUBCORT_MASK)) {
    message("x$SUBCORT_MASK is not a \"cifti_subcort_mask\".\n")
    return(FALSE)
  }

  if (!is.null(x$SUBCORT_MASK_PADDING)) {
    if (!is.cifti_subcort_mask_padding(x$SUBCORT_MASK_PADDING)) {
      message("x$SUBCORT_MASK_PADDING is not valid.\n")
      return(FALSE)
    }
  }

  TRUE
}

#' Validate a \code{"cifti"} or \code{"cifti_flat"} object.
#' 
#' Check if object is valid for a \code{"cifti"} or \code{"cifti_flat"} object.
#'
#'  Requirements if \code{!flat}: \code{"cifti"} must be a list with entries 
#'  "CORTEX_LEFT", "CORTEX_RIGHT", "SUBCORT", "LABELS", "SURF_LEFT", 
#'  "SURF_RIGHT" and "META". Each (non-empty) entry must be valid. At least one 
#'  of "CORTEX_LEFT", "CORTEX_RIGHT" or "SUBCORT" must be non-empty. The number 
#'  of brainordinates in "CORTEX_LEFT", "CORTEX_RIGHT", "SURF_LEFT" and 
#'  "SURF_RIGHT" must match. The number of measurements in "CORTEX_LEFT", 
#'  "CORTEX_RIGHT", and "SUBCORT" must match. For each of "CORTEX_LEFT", 
#'  "CORTEX_RIGHT" and "SUBCORT", their corresponding "LABELS" must be present 
#'  if and only if they are non-empty. The number of voxels in "SUBCORT" 
#'  should match the size of "META$SUBCORT_MASK".
#' 
#'  Requirements if \code{flat}: \code{"cifti"} must be a list with entries 
#'  "DAT", "LABELS", "SURF_LEFT", "SURF_RIGHT" and "META". Each (non-empty) 
#'  entry must be valid. The number of brainordinates in "DAT" should match
#'  the number of labels. The number of brainordinates with brainstructure
#'  "CORTEX_LEFT" should be the same as those with brainstructure 
#'  "CORTEX_RIGHT"; this number should also match the number of vertices in
#'  "SURF_LEFT" and "SURF_RIGHT" if present. The number of voxels in "SUBCORT" 
#'  should match the size of "META$SUBCORT_MASK".
#' 
#' @param x The putative \code{"cifti"} or \code{"cifti_flat"} object.
#' @param flat Validate \code{"cifti_flat"}? Default: \code{FALSE} (validate
#'  a \code{"cifti"}).
#' 
#' @return Logical indicating whether x is a valid \code{"cifti"} or 
#'  \code{"cifti_flat"} object.
#' @export
#' 
is.cifti <- function(x, flat=FALSE) {
  stopifnot(is.logical(flat) && length(flat)==1)
  object_class <- ifelse(flat, "\"cifti_flat\"", "\"cifti\"")

  # Check list structure.
  dat_names <- c("CORTEX_LEFT", "CORTEX_RIGHT", "SUBCORT")
  surf_names <- c("SURF_LEFT", "SURF_RIGHT")

  entry_names_unflat <- c(dat_names, "LABELS", surf_names, "META")
  entry_names_flat <- c("DAT", "LABELS", surf_names, "META")
  
  if (!flat) {
    entry_names <- list(match=entry_names_unflat, swap=entry_names_flat)
  } else {
    entry_names <- list(match=entry_names_flat, swap=entry_names_unflat)
  }

  if (!valid_list(x, entry_names$match, user_value_label=object_class)) {
    message(paste0(
      "Invalid ", object_class, " object. Is this a ", 
      ifelse(flat, "\"cifti\"", "\"cifti_flat\""), "?"
    ))
    return(FALSE)
  }

  # ----------------------------------------------------------------------------
  # Check individual components. -----------------------------------------------
  # ----------------------------------------------------------------------------

  # Data.
  if (!flat) {
    for (dat in dat_names) {
      if (!is.null(x[[dat]]) && !is.cifti_data(x[[dat]])) {
        message(paste(dat, "is invalid.\n")); return(FALSE)
      }
    }
  } else {
    if (!is.cifti_data(x$DAT)) { message("DAT is invalid.\n"); return(FALSE) }
  }

  # Labels.
  if (!is.cifti_labels(x$LABELS, flat=flat)) {
    message(paste("LABELS is invalid.\n")); return(FALSE)
  }

  # Surfaces.
  for (surf in surf_names) {
    if (!is.null(x[[surf]]) && !is.cifti_surface(x[[surf]])) {
      message(paste(surf, "is invalid.\n")); return(FALSE)
    }
  }

  # Metadata.
  if (!is.cifti_meta(x$META)) {
    message(paste("META is invalid.\n")); return(FALSE)
  }

  # ----------------------------------------------------------------------------
  # Check compatibility between components. ------------------------------------
  # ----------------------------------------------------------------------------

  # There must be at least one non-empty data entry.
  if (!flat) {
    if (sum(lapply(x[dat_names], is.null) > 2)) {
      message(paste(
        "At least one of",
        "\"CORTEX_LEFT\", \"CORTEX_RIGHT\" or \"SUBCORT\"",
        "must be non-empty.\n"
      ))
      return(FALSE)
    }
  } else {
    if (nrow(x$DAT) < 1) { message("DAT must be non-empty.\n"); return(FALSE) }
  }

  # Each non-empty data entry must have its corresponding labels.
  if (!flat) {
    for (dat in dat_names) {
      if (is.null(x[[dat]])) { next }
      if (!is.cifti_data(x[[dat]])) {
        message(paste(dat, "is invalid.\n")); return(FALSE)
      }
      if (is.null(x$LABELS[[dat]])) {
        message(paste(dat, "is present, but its LABELS entry is empty.\n"))
        return(FALSE)
      }
      if (nrow(x[[dat]]) != sum(x$LABELS[[dat]] != "Medial Wall")) {
        message(paste(
          "The data matrix and the labels of", dat, "have different lengths (not including medial wall).\n"
        ))
        return(FALSE)
      }
    }
  }

  # The surface geometry of each cortex, if present, must be compatible with it.
  for (side in c("LEFT", "RIGHT")) {
    cortex <- paste0("CORTEX_", side)
    surf <- paste0("SURF_", side)
    if (!flat) {
      if (!is.null(x[[cortex]]) && !is.null(x[[surf]])) {
        if (nrow(x[[surf]]$vertices) != nrow(x[[cortex]])) {
          message(paste0(
            "Number of vertices in", cortex, "and", surf, "must match.\n"
          ))
          return(FALSE)
        }
      } 
    } else {
      cortex_num_vert <- sum(x$LABELS$BRAINSTRUCTURE == cortex)
      if (cortex_num_vert > 0 && !is.null(x[[surf]])) {
        if (nrow(x[[surf]]$vertices) != cortex_num_vert) {
          message(paste0(
            "Number of vertices in", cortex, "and", surf, "must match.\n"
          ))
          return(FALSE)
        }
      }
    }
  }

  # Each cortex must have the same number of vertices.
  cortexL_num_vert <- sum(x$LABELS$BRAINSTRUCTURE == "CORTEX_LEFT")
  cortexR_num_vert <- sum(x$LABELS$BRAINSTRUCTURE == "CORTEX_RIGHT")
  if (cortexL_num_vert > 0 && cortexR_num_vert > 0) {
    if (cortexL_num_vert != cortexR_num_vert) {
      message(paste0(
        "Number of vertices in CORTEX_LEFT and CORTEX_RIGHT must match.\n"
      ))
      return(FALSE)
    }
  }

  # The subcortical data must have the same number of voxels as in the mask.
  subcort_present <- (!flat && !is.null(x$SUBCORT)) || (flat && sum(x$LABELS$BRAINSTRUCTURE== "SUBCORT") > 0)
  if (subcort_present) {
    if (!flat) { 
      subcort_num_vox <- nrow(x$SUBCORT) 
    } else { 
      subcort_num_vox <- sum(x$LABELS$BRAINSTRUCTURE== "SUBCORT")
    }

    if (subcort_num_vox != sum(x$META$SUBCORT_MASK)) {
      message(paste0(
        "The number of voxels in the subcortical volume and must match ",
        "the number size of the mask. ",
        "But, the number of rows (voxels) in SUBCORT is ",
        nrow(x$SUBCORT), ", ",
        "whereas the size of the mask (number of `TRUE` elements) is ",
        sum(x$META$SUBCORT_MASK), ".\n"
      ))
      return(FALSE)
    }
  }

  # The data must have the same number of measurements.
  if (!flat) {
    n_meas <- sapply(x[dat_names], ncol)
    n_meas <- n_meas[[!is.null(n_meas)]]
    if (length(unique(n_meas)) > 1) {
      # [TO DO]: Clean this up.
      message(paste(
        "If present, CORTEX_LEFT, CORTEX_RIGHT, and SUBCORT must all",
        "have the same number of columns (measurements), but they do not."
      ))
      message(paste("Column counts:"))
      message(paste(n_meas, collapse=","))
      message("\n")
      return(FALSE)
    }
  }

  TRUE
}

#' @rdname is.cifti
#' @export
isCIfTI <- is_cifti <- function(x, flat=FALSE){

  is.cifti(x, flat=flat)
}

#' @rdname is.cifti
#' @export
isCIfTI_flat <- is_cifti_flat <- function(x){

  is.cifti(x, flat=TRUE)
}