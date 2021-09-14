#' Replace the data in a \code{"xifti"}
#' 
#' Replace the data in a \code{"xifti"} with new data from a data matrix.
#' 
#' If the \code{"xifti"} has \eqn{V} greyordinates and \code{T} measurements
#'  in total, \code{newdata} should be a \eqn{V \times T} matrix.
#' @param xifti The \code{"xifti"}
#' @param newdata The \eqn{V \ times T} matrix of data values to replace those
#'  in \code{xifti} with. The left cortex vertices should be at the top, right
#'  cortex vertices in the middle, and subcortex vertices at the bottom (when
#'  present). Can also be a length-one vector to set all values equally.
#' @param newnames Replace the names in the \code{xifti}. If \code{NULL}
#'  (default), keep the original names.
#' @return The new \code{"xifti"}
#' 
#' @family manipulating
#' @export
newdata_xifti <- function(xifti, newdata, newnames=NULL) {
  stopifnot(is.xifti(xifti))

  xifti_dim <- dim(xifti)

  newdata_dim <- dim(newdata)
  if (is.null(newdata_dim)) {
    if (length(newdata) == 1) {
      newdata <- matrix(newdata, nrow=xifti_dim[1], ncol=xifti_dim[2])
    } else {
      newdata <- matrix(newdata, ncol=xifti_dim[2])
    }
    newdata_dim <- dim(newdata)
  }
  stopifnot(length(newdata_dim)==2)

  xifti_dim <- dim(xifti)
  if (!all(xifti_dim == newdata_dim)) {
    if (all(xifti_dim == rev(newdata_dim))) {
      warning("Transposing `newdata`.\n")
      newdata <- t(newdata)
    } else {
      stop(
        "`xifti` and `newdata` do not have the same dimensions.\n",
        "The `xifti` has ", xifti_dim[1], " greyordinates and ", xifti_dim[2], " measurements.\n",
        "Meanwhile, `newdata` is ", newdata_dim[1], " by ", newdata_dim[2], "."
      )
    }
  }

  # For `dlabel` xifti, check that newdata values are valid.
  if (!is.null(xifti$meta$cifti$intent) && xifti$meta$cifti$intent == 3007) {
    for (cc in seq(xifti_dim[2])) {
      if (!all(newdata[,cc] %in% xifti$meta$cifti$labels[[cc]]$Key)) {
        stop("`newdata` has values that are not in the label table for column ", cc, ".")
      }
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
      xifti$data[[bs]] <- newdata[seq(V_start+1, V_bs+V_start),,drop=FALSE]
      V_start <- V_bs+V_start
    }
  }

  if (!is.xifti(xifti)) { stop("Could not make a valid \"xifti\" object.") }
  xifti
}