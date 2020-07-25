#' Check if object is cortex data
#'
#' Check if object is valid for \code{cifti$CORTEX_LEFT} or
#'  \code{cifti$CORTEX_RIGHT}. This is a helper function for
#'  \code{\link{is_cifti}}.
#'
#' @param x The object to check
#'
#' @return Logical indicating whther x is valid cortex data
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
#' @param x The object to check
#'
#' @return Logical indicating whther x is valid subcortical data
#'
is_cifti_subcort_dat <- function(x) {
  if (!is.matrix(x) | !is.numeric(x))  {
    message("Subcortical data must be a numeric matrix.\n"); return(FALSE)
  }

  TRUE
}

#' Check if object is subcortical labels
#'
#' Check if object is valid for \code{cifti$SUBCORT$LABELS}. This is a helper
#'  function for \code{\link{is_cifti}}.
#'
#' @param x The object to check
#'
#' @return Logical indicating whther x is valid subcortical labels
#'
is_cifti_subcort_lab <- function(x) {
  if !is.numeric(x)  {
    message("Subcortical labels must be numeric.\n"); return(FALSE)
  }

  if (order(x) == 1:length(x)) {
    warning("The labels should be in spatial order, not in order of the parcels.")
  }

  if (any(x == 0)) {
    message("There shouldn't be zero-valued labels"); return(FALSE)
  }

  # [TO DO]: check proper values

  TRUE
}

#' Check if object is subcortical mask
#'
#' Check if object is valid for \code{cifti$SUBCORT$MASK}. This is a helper
#'  function for \code{\link{is_cifti}}.
#'
#' @param x The object to check
#'
#' @return Logical indicating whther x is valid subcortical mask
#'
is_cifti_subcort_mask <- function(x) {
  if (!is.array(x) | !is.numeric(x))  {
    message("Subcortical mask must be a numeric array.\n"); return(FALSE)
  }

  if (length(dim(x)) != 3) {
    message("Subcortical mask must be 3-dimensional, but it is not.\n")
    return(FALSE)
  }

  vals <- sort(unique(as.vector(x)))*1
  if (!all.equal(vals, c(0,1))) {
    message("Values in subcortical mask must be logical or 0/1, but it is not.\n")
    return(FALSE)
  }

  TRUE
}

#' Check if objects are subcortical data
#'
#' Check if the object is valid for \code{cifti$SUBCORT}, namely that its
#'  components \code{DAT}, \code{LABELS}, \code{MASK} are valid and compatible.
#'  It is a helper function for \code{\link{is_cifti}}.
#'
is_cifti_subcortical <- function(x) {

  if (!is.list(x)) {
    message("x is not a list.")
    return(FALSE)
  }

  names_expected <- c("DAT", "LABELS", "MASK")
  names_x <- names(x)
  if (length(names_x) != 3) | (!all(names_x %in% names_expected)) {
    message(paste0(
      "x must contain 3 elements: ",
      paste(names_expected, collapse=", "), ".\n"
    ))
    return(FALSE)
  }

  # Check each component separately.
  if any(
    !is_cifti_subcort_dat(x$DAT),
    !is_cifti_subcort_lab(x$LABELS),
    !is_cifti_subcort_mask(x$MASK)
  ) {
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
#' @param x A list in the format of a "cifti" object.
#'
#' @return Logical indicating whether x is a valid "cifti" object
#' @export
#'
is_cifti <- function(x) {
  # Check list structure.
  names_expected <- c(
    "CORTEX_LEFT", "CORTEX_RIGHT", "SURF_LEFT", "SURF_RIGHT", "SUBCORT"
  )
  if (!is.list(x)) { message("x is not a list."); return(FALSE) }
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
  if (!is.null(x$SURF_LEFT) && !is_cifti_surface(x$SURF_LEFT)) {
    message("x$SURF_LEFT not a surface.\n"); return(FALSE)
  }
  if (!is.null(x$SURF_RIGHT) && !is_cifti_surface(x$SURF_RIGHT)) {
    message("x$SURF_RIGHT not a surface.\n"); return(FALSE)
  }
  if (!is.null(x$SUBCORT) && !is_cifti_subcortical(x$SUBCORT)) {
    message("x$SUBCORT is not valid subcortical data.\n."); return(FALSE)
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
        "Number of vertices in x$SURF_LEFT and x$SURF_RIGHT--must match\n."
      )
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
