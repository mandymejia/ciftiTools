#' Replace the data in a \code{"xifti"}
#' 
#' Replace the data in a \code{"xifti"} with new data from a data matrix.
#' 
#' If the \code{"xifti"} has \eqn{V} greyordinates and \code{T} timepoints
#'  in total, \code{newdata} should be a \eqn{V \times T} matrix.
#' @param xifti The \code{"xifti"}
#' @param newdata The \eqn{V \ times T} matrix of data values to replace those
#'  in \code{xifti} with. The left cortex vertices should be at the top, right
#'  cortex vertices in the middle, and subcortex vertices at the bottom (when
#'  present).
#' @param newnames Replace the names in the \code{xifti}. If \code{NULL}
#'  (default), keep the original names.
#' @return The new \code{"xifti"}
#' @export
newdata_xifti <- function(xifti, newdata, newnames=NULL) {
  newdata_dim <- dim(newdata)
  if (length(newdata_dim)==1) { newdata <- matrix(newdata, ncol=1) }
  stopifnot(length(newdata_dim)==2)

  xifti_dim <- dim(do.call(rbind, xifti$data))
  if (!all(xifti_dim == newdata_dim)) {
    if (all(xifti_dim == rev(newdata_dim))) {
      warning("Transposing `newdata`.\n")
      newdata <- t(newdata)
    } else {
      stop(
        "`xifti` and `newdata` do not have the same dimensions.\n",
        "The `xifti` has ", xifti_dim[1], " greyordinates and ", xifti_dim[2], " timepoints.\n",
        "Meanwhile, `newdata` is ", newdata_dim[1], " by ", newdata_dim[2], "."
      )
    }
  }

  # New names.
  if (!is.null(newnames)) {
    if (length(newnames) != xifti_dim[2]) {
      stop("The length of `newnames` does not match the number of columns in the `xifti`.")
    }
    if (!is.null(xifti$meta$cifti$intent) && xifti$meta$cifti$intent == 3002) {
      warning("The dtseries intent (3002) does not use column names. Ignoring `newnames`.")
    } else {
      xifti$meta$cifti$names <- newnames
    }
  }

  # New data.
  V_start <- 0
  for (bs in names(xifti$data)) {
    if (!is.null(xifti$data[[bs]])) {
      V_bs <- nrow(xifti$data[[bs]])
      xifti$data[[bs]] <- newdata[seq(V_start+1, V_bs+V_start),]
      V_start <- V_bs+V_start
    }
  }

  if (!is.xifti(xifti)) { stop("Could not make a valid \"xifti\" object.") }
  xifti
}