#' Apply a univariate transformation to a \code{"xifti"} or pair of \code{"xifti"}s.
#'
#' Apply a univariate transformation to each value in a \code{"xifti"} or pair of
#'  \code{"xifti"}s. If a pair, they must share the same dimensions (brainstructures)
#'  and number of measurements.
#'
#' If the \code{"xifti"} had the dlabel intent, and the transformation creates
#'  any value that is not a label value (e.g. a non-integer), then it is converted
#'  to a dscalar.
#'
#' Technically, the function does not have to be univariate: it only has to return the
#'  same number of values as the input. The function will be applied to the matrix
#'  for each brain structure separately. For example, the function
#'  \code{function(q){(q - mean(q)) / sd(q)}} will scale each brainstructure, while
#'  \code{scale} will scale each column of each brainstructure.
#'
#' @inheritParams xifti_Param
#' @param FUN The function. If \code{xifti2} is not provided, it should be
#'  a univariate function like \code{log} or \code{sqrt}. If
#'  \code{xifti2} is provided, it should take in two arguments, like \code{`+`}
#'  or \code{pmax}.
#' @param xifti2 The second xifti, if applicable. Otherwise, \code{NULL} (default)
#' @param idx The column indices for which to apply the transformation.
#'  If \code{NULL} (default), apply to all columns. If two \code{"xifti"} objects,
#'  were provided, the values in the first (\code{xifti}) will be retained for columns
#'  that are not transformed.
#' @param ... Additional arguments to \code{FUN}
#' @return A \code{"xifti"} storing the result of applying \code{FUN} to the input(s).
#'  The data dimensions will be the same. The metadata of \code{xifti} will be retained,
#'  and the metadata of \code{xifti2} will be discarded (if provided).
#'
#' @export
#' @family manipulating xifti
#' @importFrom utils capture.output
#'
transform_xifti <- function(xifti, FUN, xifti2=NULL, idx=NULL, ...) {
  if (!is.xifti(xifti, messages=FALSE) && (!is.null(xifti2) && !is.xifti(xifti2, messages=FALSE))) {
    stop("Neither argument is a xifti.")
  }

  # Check function.
  if (!is.function(FUN)) {stop("`FUN` is not a function.")}
  badFUNs <- c("sum", "min", "max")
  FUN_char <- paste(as.character(substitute(FUN)), collapse="")
  if (FUN_char %in% badFUNs) {
    newFUN <- switch(FUN_char, sum=`+`, min=pmin, max=pmax)
    warning(
      "Replacing ", FUN_char, " with: ",
      capture.output(print(substitute(newFUN)))
    )
    # Not working...
    # warning(
    #   paste("Use", substitute(newFUN), "instead of", FUN_char, "\n")
    # )
    FUN <- newFUN
  }

  try_apply <- function(x, x2=NULL, FUN, ...) {
    if (is.null(x2)) {
      out <- FUN(x, ...)
    } else {
      out <- FUN(x, x2, ...)
    }
    if (length(out) != max(length(x), length(x2))) {
      stop(
        "`FUN` does not properly vectorize. ",
        "It needs to return a vector the same length as its arguments. ",
        "For example, `+` should be used instead of `sum`, and ",
        "`pmin` should be used instead of `min`."
      )
    }
    out
  }

  tapp_wrap <- function(x, x2, FUN, idx, ...) {
    if (is.null(idx)) {
      return(try_apply(x, x2=x2, FUN=FUN, ...))
    } else {
      x_ <- if (is.matrix(x)) { x[,idx,drop=FALSE] } else { x }
      x2_ <- if (is.matrix(x2)) { x2[,idx,drop=FALSE] } else { x2 }
      if (is.matrix(x)) {
        x[,idx] <- try_apply(x_, x2=x2_, FUN=FUN, ...)
        return(x)
      } else {
        x2[,idx] <- try_apply(x_, x2=x2_, FUN=FUN, ...)
        return(x2)
      }
    }
  }

  # Unary
  if (is.null(xifti2)) {

    if (!is.xifti(xifti)) { stop("`xifti` is invalid.") }

    if (!is.null(idx)) { stopifnot(all(idx %in% seq(ncol(xifti)))) }

    for (bs in names(xifti$data)) {
      if (!is.null(xifti$data[[bs]])) {
        xifti$data[[bs]][] <- tapp_wrap(xifti$data[[bs]], x2=NULL, FUN=FUN, idx=idx, ...)
      }
    }
  # xifti + unary
  } else if (is.numeric(xifti2) && length(xifti2)==1){
    for (bs in names(xifti$data)) {
      if (!is.null(xifti$data[[bs]])) {
        xifti$data[[bs]][] <- tapp_wrap(xifti$data[[bs]], x2=xifti2, FUN=FUN, idx=idx, ...)
      }
    }
  # unary + xifti
  } else if (is.numeric(xifti) && length(xifti)==1 && is.xifti(xifti2, messages=FALSE)) {
    for (bs in names(xifti2$data)) {
      if (!is.null(xifti2$data[[bs]])) {
        xifti2$data[[bs]][] <- tapp_wrap(xifti, x2=xifti2$data[[bs]], FUN=FUN, idx=idx, ...)
      }
    }
    xifti <- xifti2
  # xifti + matrix
  } else if (is.numeric(xifti2) && length(dim(xifti2))==2 && all(dim(xifti) == dim(xifti2))){
    xifti2 <- newdata_xifti(xifti, xifti2)
    for (bs in names(xifti$data)) {
      if (!is.null(xifti$data[[bs]])) {
        xifti$data[[bs]][] <- tapp_wrap(xifti$data[[bs]], x2=xifti2$data[[bs]], FUN=FUN, idx=idx, ...)
      }
    }
  # matrix + xifti
  } else if (is.numeric(xifti) && length(dim(xifti))==2 && is.xifti(xifti2, messages=FALSE && all(dim(xifti) == dim(xifti2)))) {
    xifti <- newdata_xifti(xifti2, xifti)
    for (bs in names(xifti2$data)) {
      if (!is.null(xifti2$data[[bs]])) {
        xifti2$data[[bs]][] <- tapp_wrap(xifti$data[[bs]], x2=xifti2$data[[bs]], FUN=FUN, idx=idx, ...)
      }
    }
    xifti <- xifti2
  # xifti + xifti
  } else {
    if (!is.xifti(xifti2, messages=FALSE)) { stop('`xifti2` is not a `"xifti"` object, single number, or of same dimensions as "xifti".') }
    # Checks
    bs1 <- names(xifti$data)[!vapply(xifti$data, is.null, FALSE)]
    bs2 <- names(xifti2$data)[!vapply(xifti2$data, is.null, FALSE)]
    if (!identical(sort(bs1), sort(bs2))) {
      stop(
        "The first xifti had brainstructures ", paste(bs1, collapse=", "), ".\n",
        "But, the second xifti had brainstructures ", paste(bs2, collapse=", "), ".\n"
      )
    }
    T1 <- ncol(xifti$data[[bs1[1]]])
    T2 <- ncol(xifti2$data[[bs2[1]]])
    if (T1 != T2) {
      stop(
        "The first xifti had ", T1, " measurements.\n",
        "But, the second xifti had ", T2, " measurements.\n"
      )
    }
    if (!identical(xifti$meta$cifti$names, xifti2$meta$cifti$names)) {
      ciftiTools_warn("The xiftis have different column names.\n")
    }

    for (bs in names(xifti$data)) {
      if (!is.null(xifti$data[[bs]])) {
        if (nrow(xifti$data[[bs]]) != nrow(xifti2$data[[bs]])) {
          if (grepl("cortex", bs)) {
            len_mwall1 <- length(xifti$meta$cortex$medial_wall_mask[[gsub("cortex_", "", bs)]])
            len_mwall2 <- length(xifti2$meta$cortex$medial_wall_mask[[gsub("cortex_", "", bs)]])
            if (!is.null(len_mwall1) && len_mwall1 == len_mwall2) {
              stop("The xiftis have the same resolution but different medial wall masks in the ", bs, " brainstructure.")
            } else {
              stop("The xiftis have different number of vertices/voxels for the ", bs, " brainstructure.")
            }
          } else {
            stop("The xiftis have different number of vertices/voxels for the ", bs, " brainstructure.")
          }
        }
        xifti$data[[bs]][] <- tapp_wrap(xifti$data[[bs]], x2=xifti2$data[[bs]], FUN=FUN, idx=idx, ...)
      }
    }
  }

  # Convert from dlabel to dscalar if non-label values were introduced by
  #   the transformation function.
  if (!is.null(xifti$meta$cifti$intent) && xifti$meta$cifti$intent == 3007) {
    v <- as.matrix(xifti)
    for (T_ in seq(ncol(xifti))) {
      if (!all(unique(v[,T_]) %in% xifti$meta$cifti$labels[[T_]]$Key)) {
        warning(
          "New data values outside the label table for column ", T_,
          " were introduced. Changing the xifti intent from dlabel to dscalar."
        )
        xifti$meta$cifti$intent <- 3006
        xifti$meta$cifti$labels <- NULL
        break
      }
    }
  }

  xifti
}

#' \code{"xifti"} S3 Math methods
#' 
#' Math methods for \code{"xifti"} objects.
#' 
#' Uses \code{\link{transform_xifti}}.
#' @param x The \code{"xifti"}
#' @param ... Additional arguments to the function
#' @name S3_Math
#' @method Math xifti
#' @export
Math.xifti <- function(x, ...) {
  transform_xifti(x, function(q){do.call(.Generic, c(list(q), ...))})
}

#' \code{"xifti"} S3 Ops methods
#' 
#' Ops methods for \code{"xifti"} objects.
#' 
#' Uses \code{\link{transform_xifti}}.
#' @param e1,e2 The arguments to the operation. \code{"xifti"} objects will be converted to matrices temporarily
#' @name S3_Ops
#' @method Ops xifti
#' @export
Ops.xifti <- function(e1, e2=NULL) {
  transform_xifti(e1, function(q1, q2){do.call(.Generic, list(q1, q2))}, e2)
}

#' \code{"xifti"} S3 Summary methods
#' 
#' Summary methods for \code{"xifti"} objects.
#' @param ... The \code{"xifti"} and additional numeric arguments will be converted to matrices
#' @param na.rm Remove \code{NA} values? Default: \code{FALSE}.
#' @name S3_Summary
#' @method Summary xifti
#' @export
Summary.xifti <- function(..., na.rm=FALSE) {
  args <- list(...)
  args <- lapply(args, as.matrix)
  do.call(.Generic, c(args, na.rm=na.rm))
}