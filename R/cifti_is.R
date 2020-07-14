#' Check if object is cortex data
#'
#' Check if object is valid for \code{cifti$CORTEX_LEFT} or 
#'  \code{cifti$CORTEX_RIGHT}. This is a helper function for 
#'  \code{\link{is.cifti}}.
#'
#' @param x The object to check
#'
#' @return Logical indicating whther x is valid cortex data
#'
is.cifti_cortex <- function(x) {
  if (!is.numeric(x)) {
    message("x must be numeric.\n"); return(FALSE)
  }

  TRUE
}

#' Check if objects are subcortical data
#'
#' Check if objects are valid for \code{cifti$VOL} and \code{cifti$LABELS}. This
#'  is a helper function for \code{\link{is.cifti}}.
#'
#' @param vol The object that is potentially valid for \code{cifti$VOL}
#' @param labels The object that is potentially valid for \code{cifti$LABELS}
#'
is.cifti_subcortical <- function(vol, labels) {
  # Check them separately.
  if (!is.array(vol) | !is.numeric(vol))  {
    message("vol must be a numeric array.\n"); return(FALSE)
  }
  if (!is.array(labels) | !is.numeric(labels))  {
    message("labels must be a numeric array.\n"); return(FALSE)
  }

  # Check them together.
  if (!identical(dim(vol), dim(labels))){
    message("The volume and label must have the same dimensions.\n"); return(FALSE)
  }

  # [TO DO]: More checks.

  TRUE
}

#' Check if objects are a valid 2D matrix/3D mask pair.
#'
#' Check if objects are a valid 2D matrix/3D mask pair, e.g. a vectorized
#'   \code{cifti$VOL}. This is a helper function for \code{\link{is.cifti}}.
#'
#' @param mat Data matrix for subcortical locations, with voxels in rows
#' @param mask Volumetric brain mask for subcortical locations
#'
is.cifti_matmask_pair <- function(mat, mask) {
  if (class(mat) != "matrix") {
    message("If provided, subcortMat must be a matrix, but it is not.\n")
    return(FALSE)
  }
  if (length(dim(mask)) != 3) {
    message("subcortMask must be a 3-dimensional array, but it is not.\n")
    return(FALSE)
  }
  vals_mask <- sort(unique(as.vector(mask)))*1
  if (!all.equal(vals_mask, c(0,1))) {
    message("subcortMask must be logical or 0/1, but it is not.\n")
    return(FALSE)
  }
  # Check agreement.
  if (sum(mask) != nrow(mat)) {
    message(paste0(
      "The number of voxels in subcortMask (", sum(mask), ") must equal ",
      "the number of rows in subcortMat (", nrow(mat), "), ", 
      "but they do not match.\n"
    ))
    return(FALSE)
  }

  TRUE
}

#' Check whether object is a valid "cifti_surface" object.
#'
#' @param x A list in the format of a "cifti_surface" object.
#'
#' @return Logical indicating whether x is a valid "cifti_surface" object
#' @export
is.cifti_surface <- function(x) {
  if (!is.list(x)) {
    message("Not a list"); return(FALSE)
  }
  if (length(x) != 2) {
    message("Must be a list with 2 elements.\n"); return(FALSE)
  }
  if (!all.equal(names(x), c("vertices","faces"))) { 
    message("Elements of x must be named \"vertices\" and \"faces\"")
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
    message("Max vertex index in x$faces is too high.\n"); return(FALSE)
  }
  if (min(x$faces) < 1) {
    message("Min vertex index in x$faces is too low.\n"); return(FALSE)
  }

  TRUE
}

#' Validate a "cifti" object.
#'
#' @param x A list in the format of a "cifti" object.
#'
#' @return Logical indicating whether x is a valid "cifti" object
#' @export
#'
is.cifti <- function(x) {
  # Check list structure.
  names_expected <- c(
    "CORTEX_LEFT", "CORTEX_RIGHT", "SURF_LEFT", 
    "SURF_RIGHT", "VOL", "LABELS"
  )
  if (!is.list(x)) {message("x is not a list"); return(FALSE)}
  names_x <- names(x)
  if (length(names_x) != 6) { 
    message(paste0(
      "x must contain 6 elements: ", 
      paste(names_expected, collapse=", "), ".\n"
    )) 
    return(FALSE) 
  }
  if (!all(names_x %in% names_expected)) {
    message(paste0(
      "elements of x must be named: ", 
      paste(names_expected, collapse=", "), ".\n"
    )) 
    return(FALSE) 
  }

  # Check individual components.
  if (!is.null(x$CORTEX_LEFT) && !is.cifti_cortex(x$CORTEX_LEFT)) { 
    message("x$CORTEX_LEFT not a matrix.\n"); return(FALSE) 
  }
  if (!is.null(x$CORTEX_LEFT) && !is.cifti_cortex(x$CORTEX_RIGHT)) { 
    message("x$CORTEX_RIGHT not a matrix.\n"); return(FALSE) 
  }
  if (!is.null(x$SURF_LEFT) && !is.cifti_surface(x$SURF_LEFT)) {
    message("x$SURF_LEFT not a surface.\n"); return(FALSE)
  }
  if (!is.null(x$SURF_RIGHT) && !is.cifti_surface(x$SURF_RIGHT)) {
    message("x$SURF_RIGHT not a surface.\n"); return(FALSE)
  }
  if (sum(c(is.null(x$VOL), is.null(x$LABELS))) == 1) {
    message("x$VOL and x$LABELS must both be present, or both be NULL\n.")
    return(FALSE)
  }
  if (!is.null(x$VOL) && !is.cifti_subcortical(x$VOL, x$LABELS)) { 
    message("x$VOL and x$LABELS are not compatible\n.")
    return(FALSE) 
  }

  # Check compatibility between components.
  if (!is.null(x$CORTEX_LEFT) && !is.null(x$SURF_LEFT)) { 
    if (nrow(x$SURF_LEFT$vertices) != nrow(x$CORTEX_LEFT)) { 
      message(paste0(
        "Number of vertices in x$CORTEX_LEFT",
        "and surfaces in x$SURF_LEFT--must match\n."
      ))
      return(FALSE) 
    }
  }
  if (!is.null(x$CORTEX_RIGHT) && !is.null(x$SURF_RIGHT)) { 
    if (nrow(x$SURF_RIGHT$vertices) != nrow(x$CORTEX_RIGHT)) { 
      message(paste0(
        "Number of vertices in x$CORTEX_RIGHT",
        "and surfaces in x$SURF_RIGHT--must match\n."
      ))
      return(FALSE) 
    }
  }
  if (!is.null(x$SURF_LEFT) && !is.null(x$SURF_RIGHT)) { 
    if (nrow(x$SURF_LEFT$vertices) != nrow(x$SURF_RIGHT$vertices)) { 
      message(
        "Number of surfaces in x$SURF_LEFT and x$SURF_RIGHT--must match\n."
      )
      return(FALSE) 
    }
  }
  # [TO DO]: Add x$VOL to this check.
  check_cols <- c(ncol(x$CORTEX_LEFT), ncol(x$CORTEX_RIGHT))
  if (length(unique(check_cols)) > 1) {
    warning(paste(
      "If provided, x$CORTEX_LEFT and x$CORTEX_RIGHT must all have",
      "the same number of columns (measurements), but they do not\n."
    ))
  }

  TRUE
}
