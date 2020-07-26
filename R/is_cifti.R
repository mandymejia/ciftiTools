#' Check if object is cortex data
#'
#' Check if object is valid for \code{cifti$CORTEX_LEFT} or
#'  \code{cifti$CORTEX_RIGHT}. This is a helper function for
#'  \code{\link{is_cifti}}.
#'
#' Requirements: the cortical data must be a numeric matrix.
#'
#' @param x The object to check
#'
#' @return Logical indicating whether x is valid cortex data
#'
is_cifti_cortex <- function(x) {
  if (!is.matrix(x) | !is.numeric(x))  {
    message("Cortex data must be a numeric matrix.\n"); return(FALSE)
  }

  TRUE
}

#' Check if object is subcortical data
#'
#' Check if object is valid for \code{cifti$SUBCORT$DAT}. This is a helper
#'  function for \code{\link{is_cifti}}.
#'
#' Requirements: the subcortical data must be a numeric matrix.
#'
#' @param x The object to check
#'
#' @return Logical indicating whether x is valid subcortical data
#'
is_cifti_subcort_dat <- function(x) {

  if (!is.matrix(x) || !is.numeric(x))  {
    message("Subcortical data must be a numeric matrix.\n"); return(FALSE)
  }

  TRUE
}

#' Check if object is subcortical labels
#'
#' Check if object is valid for \code{cifti$SUBCORT$LABELS}. This is a helper
#'  function for \code{\link{is_cifti}}.
#'
#' Requirements: the labels must be a numeric vector; the labels must be in
#'  spatial order (and not sorted in order of the parcels); the labels should
#'  all be integers greater than zero.
#'
#' @param x The object to check
#'
#' @return Logical indicating whether x is valid subcortical labels
#'
is_cifti_subcort_lab <- function(x) {

  if (!is.vector(x) && !is.numeric(x))  {
    message("Subcortical labels must be numeric vector.\n"); return(FALSE)
  }

  if (all(order(x) == 1:length(x))) {
    warning("The subcortical labels should be in spatial order, not in order of the parcels. Double-check? Proceeding anyway.\n")
  }

  if (any(x == 0)) {
    message("There shouldn't be zero-valued labels.\n"); return(FALSE)
  }

  if (!all.equal(x, round(x), check.attributes=FALSE)) {
    message("The labels must be only integers.\n"); return(FALSE)
  }

  # [TO DO]: check proper values

  TRUE
}

#' Check if object is subcortical mask
#'
#' Check if object is valid for \code{cifti$SUBCORT$MASK}. This is a helper
#'  function for \code{\link{is_cifti}}.
#'
#' Requirements: the mask must be a boolean 3D array.
#'
#' @param x The object to check
#'
#' @return Logical indicating whether x is valid subcortical mask
#'
is_cifti_subcort_mask <- function(x) {

  if (!is.array(x) || !is.logical(x))  {
    message("Subcortical mask must be a logical array.\n"); return(FALSE)
  }

  if (length(dim(x)) != 3) {
    message("Subcortical mask must be 3-dimensional, but it is not.\n")
    return(FALSE)
  }

  TRUE
}

#' Check if an object represents subcortical data
#'
#' Check if the object is valid for \code{cifti$SUBCORT}, namely that its
#'  components \code{DAT}, \code{LABELS}, \code{MASK} are valid and compatible.
#'  It is a helper function for \code{\link{is_cifti}}.
#'
#' Requirements: must be a list of three components: "DAT", "LABELS", "MASK".
#'  Each must be valid, and they should have the same number of voxels.
#'
#' @param x The object to check
#'
#' @return Logical indicating whether x is a valid "cifti_subcort" object
#'
is_cifti_subcort <- function(x) {

  if (!is.list(x)) {
    message("x is not a list.\n")
    return(FALSE)
  }

  names_expected <- c("DAT", "LABELS", "MASK")
  names_x <- names(x)
  if ((length(names_x) != 3) || (!all(names_x %in% names_expected))) {
    message(paste0(
      "x must contain 3 elements: ",
      paste(names_expected, collapse=", "), ".\n"
    ))
    return(FALSE)
  }
  if (any(sapply(x, is.null))) {
    message("All components of x (DAT, LABELS, MASK) must be non-empty.")
    return(FALSE)
  }

  # Check each component separately.
  if (!all(c(
    is_cifti_subcort_dat(x$DAT),
    is_cifti_subcort_lab(x$LABELS),
    is_cifti_subcort_mask(x$MASK)
  ))) {
    return(FALSE)
  }

  # Check the compatibility of the components together.
  if ((nrow(x$DAT) != length(x$LABELS)) | (length(x$LABELS) != sum(x$MASK))) {
    message(paste0(
      "The number of voxels in the subcortical volume and must match ",
      "the number of labels and the size of the mask. ",
      "But, the number of rows (voxels) in the volume matrix is ",
      nrow(x$DAT), ", ",
      "the number of non-empty labels is ",
      sum(x$LABELS > 0), ", ",
      "and the size of the mask (nonzero elements in the array) is ",
      sum(x$MASK), "."
    ))
    return(FALSE)
  }

  # [TO DO]: More checks.

  TRUE
}

#' Check whether object is a valid "cifti_surface" object.
#'
#' Requirements: must be a list of two components: "vertices" and "faces".
#'  Each must be a numeric matrix with three columns. The values in "vertices"
#'  represent spatial coordinates whereas the values in "faces" represent
#'  vertex indices defining the face. Thus, values in "faces" should be integers
#'  between 1 and the number of vertices. 
#'
#' @param x A list in the format of a "cifti_surface" object.
#'
#' @return Logical indicating whether x is a valid "cifti_surface" object
#' @export
is_cifti_surface <- function(x) {
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
#' Requirements: must be a list of five components: "CORTEX_LEFT", 
#'  "CORTEX_RIGHT", "SUBCORT", "SURF_LEFT" and "SURF_RIGHT". Each non-empty
#'  entry must be valid. The number of brainordinates in "CORTEX_LEFT", 
#'  "CORTEX_RIGHT", "SURF_LEFT" and "SURF_RIGHT" must match. The number of 
#'  measurements in "CORTEX_LEFT", "CORTEX_RIGHT", and "SUBCORT$VOL" must match.
#'
#' @param x A list in the format of a "cifti" object.
#'
#' @return Logical indicating whether x is a valid "cifti" object
#' @export
#'
is_cifti <- function(x) {
  # Check list structure.
  names_expected <- c(
    "CORTEX_LEFT", "CORTEX_RIGHT", "SUBCORT", "SURF_LEFT", "SURF_RIGHT"
  )
  if (!is.list(x)) { message("x is not a list.\n"); return(FALSE) }
  names_x <- names(x)
  if (length(names_x) != 5) {
    message(paste0(
      "x must contain 5 elements: ",
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
  if (!is.null(x$CORTEX_LEFT) && !is_cifti_cortex(x$CORTEX_LEFT)) {
    message("x$CORTEX_LEFT not a matrix.\n"); return(FALSE)
  }
  if (!is.null(x$CORTEX_RIGHT) && !is_cifti_cortex(x$CORTEX_RIGHT)) {
    message("x$CORTEX_RIGHT not a matrix.\n"); return(FALSE)
  }
  if (!is.null(x$SUBCORT) && !is_cifti_subcort(x$SUBCORT)) {
    message("x$SUBCORT is not valid subcortical data.\n"); return(FALSE)
  }
  if (!is.null(x$SURF_LEFT) && !is_cifti_surface(x$SURF_LEFT)) {
    message("x$SURF_LEFT not a surface.\n"); return(FALSE)
  }
  if (!is.null(x$SURF_RIGHT) && !is_cifti_surface(x$SURF_RIGHT)) {
    message("x$SURF_RIGHT not a surface.\n"); return(FALSE)
  }

  # Check compatibility between components.
  if (!is.null(x$CORTEX_LEFT) && !is.null(x$SURF_LEFT)) {
    if (nrow(x$SURF_LEFT$vertices) != nrow(x$CORTEX_LEFT)) {
      message(paste0(
        "Number of vertices in x$CORTEX_LEFT ",
        "and surfaces in x$SURF_LEFT must match\n."
      ))
      return(FALSE)
    }
  }
  if (!is.null(x$CORTEX_RIGHT) && !is.null(x$SURF_RIGHT)) {
    if (nrow(x$SURF_RIGHT$vertices) != nrow(x$CORTEX_RIGHT)) {
      message(paste0(
        "Number of vertices in x$CORTEX_RIGHT ",
        "and surfaces in x$SURF_RIGHT must match\n."
      ))
      return(FALSE)
    }
  }
  if (!is.null(x$SURF_LEFT) && !is.null(x$SURF_RIGHT)) {
    if (nrow(x$SURF_LEFT$vertices) != nrow(x$SURF_RIGHT$vertices)) {
      message(
        "Number of vertices in x$SURF_LEFT and x$SURF_RIGHT must match\n."
      )
      return(FALSE)
    }
  }
  if (!is.null(x$CORTEX_LEFT) && !is.null(x$CORTEX_RIGHT)) {
    if (nrow(x$CORTEX_RIGHT) != nrow(x$CORTEX_LEFT)) {
      message(paste0(
        "Number of vertices in x$CORTEX_LEFT and x$CORTEX_RIGHT must match\n."
      ))
      return(FALSE)
    }
  }

  check_cols <- c(ncol(x$CORTEX_LEFT), ncol(x$CORTEX_RIGHT), ncol(x$SUBCORT$VOL))
  if (length(unique(check_cols)) > 1) {
    warning(paste(
      "If provided, x$CORTEX_LEFT, x$CORTEX_RIGHT, and x$SUBCORT$VOL must all have",
      "the same number of columns (measurements), but they do not\n."
    ))
  }

  TRUE
}

#' @rdname is_cifti
#' @export
isCIfTI <- is.cifti <- function(x){

  is_cifti(x)
}

#' Validate a "cifti_flat" object.
#'
#' Requirements: must be a list of two components: "DAT" and "META".
#'
#' "DAT" must be a numeric matrix.
#'
#' "META" must be a list with 1-3 of these components: "CORTEX_LEFT",
#'  "CORTEX_RIGHT", and "SUBCORT". The cortical components are lists with
#'  elements "rows" (the first and last rows in $DAT which they occupy) and 
#'  "NA_mask" (vector of original brainordinates, with FALSE values indicating
#'  the medial wall). "SUBCORT" is a list with the "rows" element as well as 
#'  "labels" (the vector of labels for the subcortical data).
#'
#' @param x A list in the format of a "cifti_flat" object.
#'
#' @return Logical indicating whether x is a valid "cifti_flat" object
#' @export
#'
is_cifti_flat <- function(x){
  # Check list structure.
  names_expected <- c(
    "DAT", "META"
  )
  if (!is.list(x)) { message("x is not a list.\n"); return(FALSE) }
  names_x <- names(x)
  if (length(names_x) != 2) {
    message(paste0(
      "x must contain 2 elements: ",
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

  if (!is.numeric(x$DAT) | !is.matrix(x$DAT)) {
    message("DAT must be a numeric matrix.\n"); return(FALSE)
  }

  if (!is.list(x$META)) { 
    message("x$META is not a list.\n"); return(FALSE) 
  }
  if (!all(names(x$META) %in% c("CORTEX_LEFT", "CORTEX_RIGHT", "SUBCORT"))) {
    message("x$META must only contain these components: CORTEX_LEFT, CORTEX_RIGHT, SUBCORT.")
    return(FALSE)
  }
  if (!is.null(x$META$CORTEX_LEFT)) {
    if (!identical(names(x$META$CORTEX_LEFT), c("rows", "NA_mask"))) {
      message("x$META$CORTEX_LEFT must be a list with components \"rows\" and \"NA_mask\".")
      return(FALSE)
    }
    # [TO DO]: Detailed check within this component.
  }
  if (!is.null(x$META$CORTEX_RIGHT)) {
    if (!identical(names(x$META$CORTEX_RIGHT), c("rows", "NA_mask"))) {
      message("x$META$CORTEX_RIGHT must be a list with components \"rows\" and \"NA_mask\".")
      return(FALSE)
    }
    # [TO DO]: Detailed check within this component.
  }
  if (!is.null(x$META$SUBCORT)) {
    if (!identical(names(x$META$SUBCORT), c("rows", "labels", "mask"))) {
      message("x$META$SUBCORT must be a list with components \"rows\", \"labels\", and \"mask\".")
      return(FALSE)
    }
    # [TO DO]: Detailed check within this component.
    # diff(x$META$SUBCORT$rows) + 1 - length(x$META$SUBCORT$labels)
  }

  TRUE
}