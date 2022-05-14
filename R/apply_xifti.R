#' Apply a function along the rows or columns of a \code{"xifti"}
#' 
#' Apply a many-to-N function (e.g. mean) to the rows or columns of a
#'  \code{"xifti"}. If applied row-wise, a \code{"xifti"} with N data column(s)
#'  is returned. (If the \code{"xifti"} had the dlabel intent, and values that 
#'  are not labels are created, then it is converted to dscalar.) If applied 
#'  column-wise, a numeric matrix with N rows is returned. 
#'  
#' For univariate functions, use \code{\link{transform_xifti}} instead. 
#' 
#' @inheritParams xifti_Param
#' @param margin The dimension along which to apply \code{FUN}: 1 for rows (default)
#'  and 2 for columns.
#' @param FUN The function. It should take in a numeric vector and return a
#'  length-N numeric vector. 
#' @param ... Additional arguments to \code{FUN}
#' 
#' @return A \code{"xifti"} if \code{margin == 1}, or a numeric matrix if 
#'  \code{margin == 2}
#' 
#' @family manipulating
#' @export
#' 
apply_xifti <- function(xifti, margin=c(1,2), FUN, ...) {
  if (!is.xifti(xifti, messages=FALSE)) {
    stop("`xifti` is not a xifti.")
  }
  if (!is.function(FUN)) {stop("`FUN` is not a function.")}
  margin <- as.numeric(margin)
  if (!(length(margin)==1 && margin %in% seq(2))) {stop("`margin` must be 1 or 2.")}

  if (margin == 1) {
    
    # Apply function along rows, for each brainstructure. Keep track of names. 
    colnames <- vector("list", length(xifti$data))
    names(colnames) <- names(xifti$data)
    for (bs in names(xifti$data)) {
      if (!is.null(xifti$data[[bs]])) {
        q <- apply(xifti$data[[bs]], MARGIN=margin, FUN=FUN, ... )
        if (is.matrix(q)) {
          q <- t(q); colnames[[bs]] <- colnames(q)
        } else {
          q <- as.matrix(q); colnames[[bs]]  <- "Column 1"
        }
        dimnames(q) <- NULL
        xifti$data[[bs]] <- q
      }
    }

    # Convert to dscalar. 
    # [TO DO]: consider keeping dlabel if all values are labels?
    out <- xifti
    if (is.null(xifti$meta$cifti$intent) || xifti$meta$cifti$intent != 3006) {
      out <- convert_xifti(out, "dscalar")
    }

    # Use names from applied function (e.g. quantiles) if consistent
    #   across brainstructures.
    colnames <- do.call(cbind, colnames)
    if (!is.null(colnames) && all(apply(colnames, 1, function(x){length(unique(x))})==1)) {
      colnames <- colnames[,1]
    } else {
      colnames <- paste("Column", seq(ncol(out)))
    }
    out$meta$cifti$names <- colnames

  } else if (margin == 2) {
    out <- apply(as.matrix(xifti), margin, FUN, ...)
    if (is.vector(out)) {
      names(out) <- xifti$meta$cifti$names
    } else {
      colnames(out) <- xifti$meta$cifti$names
    }

  } else { stop() }

  out
}