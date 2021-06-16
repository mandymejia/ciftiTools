#' Apply a univariate transformation to a \code{"xifti"} or pair of \code{"xifti"}s.
#' 
#' Apply a univariate transformation to each value in a \code{"xifti"} or pair of 
#'  \code{"xifti"}s. If a pair, they must share the same brainstructures and 
#'  data dimensions.
#' 
#' If the \code{"xifti"} had the dlabel intent, and the transformation creates
#'  any value that is not a label value (e.g. a non-integer), then it is converted
#'  to a dscalar.
#' 
#' @param xifti The xifti
#' @param FUN The function. If \code{xifti2} is not provided, it should be
#'  a univariate function like \code{log} or \code{sqrt}. If 
#'  \code{xifti2} is provided, it should take in two arguments, like \code{`+`}
#'  or \code{pmax}.
#' @param xifti2 The second xifti, if applicable. Otherwise, \code{NULL} (default)
#' @return A \code{xifti} storing the result of applying \code{FUN} to the input(s).
#'  The data dimensions will be the same. The metadata of \code{xifti} will be retained, 
#'  and the metadata of \code{xifti2} will be discarded (if provided).
#' @export
#' @importFrom utils capture.output
#' 
transform_xifti <- function(xifti, FUN, xifti2=NULL) {
  if (!is.xifti(xifti, messages=FALSE) && (!is.null(xifti2) && !is.xifti(xifti2, messages=FALSE))) {
    stop("Neither argument is a xifti.")
  }
  
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

  try_apply <- function(x, x2=NULL, FUN) {
    if (is.null(x2)) {
      out <- FUN(x)
    } else {
      out <- FUN(x, x2)
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

  # Unary
  if (is.null(xifti2)) {
    if (!is.xifti(xifti)) { stop("`xifti` is invalid.") }
    for (bs in names(xifti$data)) {
      if (!is.null(xifti$data[[bs]])) {
        xifti$data[[bs]][] <- try_apply(xifti$data[[bs]], FUN=FUN)
      }
    }
  # xifti + unary
  } else if (is.numeric(xifti2) && length(xifti2)==1){
    for (bs in names(xifti$data)) {
      if (!is.null(xifti$data[[bs]])) {
        xifti$data[[bs]][] <- try_apply(xifti$data[[bs]], xifti2, FUN=FUN)
      }
    }
  # unary + xifti
  } else if (is.numeric(xifti) && length(xifti)==1 && is.xifti(xifti2, messages=FALSE)) { 
    for (bs in names(xifti2$data)) {
      if (!is.null(xifti2$data[[bs]])) {
        xifti2$data[[bs]][] <- try_apply(xifti, xifti2$data[[bs]], FUN=FUN)
      }
    }
    xifti <- xifti2
  # xifti + xifti
  } else {
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
        "The first xifti had ", T1, " timepoints.\n",
        "But, the second xifti had ", T2, " timepoints.\n"
      )
    }
    if (!identical(xifti$meta$cifti$names, xifti2$meta$cifti$names)) {
      warning("The xiftis have different column names.\n")
    }

    for (bs in names(xifti$data)) {
      if (!is.null(xifti$data[[bs]])) {
        if (nrow(xifti$data[[bs]]) != nrow(xifti2$data[[bs]])) {
          stop("The xiftis have different number of vertices/voxels for the ", bs, " brainstructure.")
        }
        xifti$data[[bs]][] <- try_apply(xifti$data[[bs]], xifti2$data[[bs]], FUN=FUN)
      }
    }
  }

  # Convert from dlabel to dscalar if non-label values were introduced by
  #   the transformation function.
  if (!is.null(xifti$meta$cifti$intent) && xifti$meta$cifti$intent == 3007) {
    v <- unique(as.matrix(xifti))
    for (T_ in seq(ncol(v))) {
      if (!all(v[,T_] %in% xifti$meta$cifti$labels[[T_]]$Key)) {
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

#' @rdname transform_xifti
#' 
#' @export
`+.xifti` <- function(xifti,xifti2) {
  transform_xifti(xifti, xifti2, FUN=`+`)
}

#' @rdname transform_xifti
#' 
#' @export
`-.xifti` <- function(xifti,xifti2) {
  transform_xifti(xifti, xifti2, FUN=`-`)
}

#' @rdname transform_xifti
#' 
#' @export
`*.xifti` <- function(xifti,xifti2) {
  transform_xifti(xifti, xifti2, FUN=`*`)
}

#' @rdname transform_xifti
#' 
#' @export
`^.xifti` <- function(xifti,xifti2) {
  transform_xifti(xifti, xifti2, FUN=`^`)
}

#' @rdname transform_xifti
#' 
#' @export
`%%.xifti` <- function(xifti,xifti2) {
  transform_xifti(xifti, xifti2, FUN=`%%`)
}

#' @rdname transform_xifti
#' 
#' @export
`%/%.xifti` <- function(xifti,xifti2) {
  transform_xifti(xifti, xifti2, FUN=`%/%`)
}

#' @rdname transform_xifti
#' 
#' @export
`/.xifti` <- function(xifti,xifti2) {
  transform_xifti(xifti, xifti2, FUN=`/`)
}

#' @rdname transform_xifti
#' 
#' @param x The \code{"xifti"}
#' @export
#' @method abs xifti
abs.xifti <- function(x) {
  transform_xifti(x, FUN=abs)
}

#' @rdname transform_xifti
#' 
#' @param x The \code{"xifti"}
#' @export
#' @method sign xifti
sign.xifti <- function(x) {
  transform_xifti(x, FUN=sign)
}

#' @rdname transform_xifti
#' 
#' @param x The \code{"xifti"}
#' @export
#' @method sqrt xifti
sqrt.xifti <- function(x) {
  transform_xifti(x, FUN=sqrt)
}

#' @rdname transform_xifti
#' 
#' @param x The \code{"xifti"}
#' @export
#' @method floor xifti
floor.xifti <- function(x) {
  transform_xifti(x, FUN=floor)
}

#' @rdname transform_xifti
#' 
#' @param x The \code{"xifti"}
#' @export
#' @method ceiling xifti
ceiling.xifti <- function(x) {
  transform_xifti(x, FUN=ceiling)
}

#' @rdname transform_xifti
#' 
#' @param x The \code{"xifti"}
#' @param digits The number of digits to round by
#' @export
#' @method round xifti
round.xifti <- function(x, digits=0) {
  transform_xifti(x, FUN=function(y){round(y,digits=digits)})
}

#' @rdname transform_xifti
#' 
#' @param x The \code{"xifti"}
#' @export
#' @method exp xifti
exp.xifti <- function(x) {
  transform_xifti(x, FUN=exp)
}

#' @rdname transform_xifti
#' 
#' @param x The \code{"xifti"}
#' @param base The log base
#' @export
#' @method log xifti
log.xifti <- function(x, base=exp(1)) {
  transform_xifti(x, FUN=function(y){log(y,base=base)})
}