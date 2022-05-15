#' @describeIn convert_xifti
#' 
#' Give the ".dlabel" intent (code 3007/ConnDenseLabel) to an input
#'  \code{"xifti"}. Will use the same label table for each data column.
#' 
#' @param x The CIFTI file name or \code{"xifti"} object to convert.
#' @param cifti_target_fname File name for the converted CIFTI. Only used if
#'  \code{x} is a CIFTI file name. If \code{NULL} (default), will use the same
#'  name as \code{x} but with the extension updated.
#' @param values (Optional) A vector of the original data values. They should all
#'  be unique. They may not all occur in the \code{"xifti"} data, but every
#'  datapoint in the \code{"xifti"} must occur in \code{values}. Data will be 
#'  mapped to integers from $0$ to $N-1$ (the "keys"), with $N$ being the
#'  number of \code{values}. They will be sorted.
#' 
#'  If \code{values} is a named vector, the rownames of the label table in the resulting
#'  \code{"xifti"} will be those names. Otherwise, the rownames will be the values
#'  themselves.  
#' @param nsig Take this many significant digits for the data values. If 
#'  \code{Inf} (default), do not round.
#' @param colors (Optional) "ROY_BIG_BL", the name of a ColorBrewer palette 
#'  (see \code{RColorBrewer::brewer.pal.info} and colorbrewer2.org), the name of
#'  a viridisLite palette, or a character vector of colors. Default: 
#'  \code{"Set2"}.
#' @param add_white Append white to the beginning of the colors? Default: \code{TRUE}.
#' @param return_conversion_table Return the conversion table along with the 
#'  converted \code{"xifti"}? Default: \code{FALSE}
#' 
#' @keywords internal
convert_to_dlabel <- function(x, cifti_target_fname=NULL,
  values=NULL, nsig=Inf, colors="Set2", add_white=TRUE, return_conversion_table=FALSE) {

  # If the input is a CIFTI file name, we need to read it into R.
  input_is_xifti <- is.xifti(x, messages=FALSE)
  if (!input_is_xifti) { 
    x_original <- x
    if (is.null(cifti_target_fname)) {
      cifti_target_fname <- basename(gsub(
        get_cifti_extn(x), "dlabel.nii", x, fixed=TRUE
      ))
    }
    x <- read_xifti(x, brainstructures=info_cifti(x)$cifti$brainstructures) 
  }

  stopifnot(is.xifti(x))

  if (!is.null(x$meta$cifti$intent) && x$meta$cifti$intent == 3007) {
    if (!input_is_xifti) {
      file.copy(x_original, cifti_target_fname)
      return(cifti_target_fname)
    } else {
      ciftiTools_warn("The input is already a dlabel `xifti`.\n")
      return(x)
    }
  }

  # Get the label values.
  convert_NA <- FALSE
  if (is.null(values)) {
    # Infer the new values.
    values <- sort(unique(signif(as.vector(as.matrix(x)), nsig)))
    if (any(is.na(as.vector(as.matrix(x))))) {
      if (!any(is.nan(as.vector(as.matrix(x))))) {
        convert_NA <- TRUE
        values <- c(NaN, values[!is.na(values)])
      } else {
        # [TO DO]: Workaround
        stop("Not implemented: handling presence of both NA and NaN values in data. Change one of them to a new data value.\n")
      }
    }
  } else {
    # Check the new values.
    if (any(duplicated(values))) { warning("Removing duplicate `values`.\n"); values <- unique(values) }
    values <- sort(values)
  }
  if (length(values) > 1000) { warning("Over 1000 unique `values` in the `xifti`.\n") }
  conversion_table <- data.frame(values=values, label=seq(length(values))-1)

  # Convert data to label values.
  for (bs in names(x$data)) {
    if (is.null(x$data[[bs]])) { next }

    x$data[[bs]][] <- signif(x$data[[bs]][], nsig)

    if (convert_NA) {
      x$data[[bs]][] <- ifelse(
        is.na(as.vector(x$data[[bs]])), 
        NaN, as.vector(x$data[[bs]])
      )
    }

    stopifnot(all(x$data[[bs]][] %in% values))
    x$data[[bs]][] <- as.numeric(factor(x$data[[bs]][], levels=values)) - 1
  }

  # Make color table.
  # [TO DO]: Allow input of custom colors
  N_ <- length(values)
  if (N_ == 1 && add_white) {
    pal <- data.frame(value=0, color=factor("white", "white"))
  } else {
    pal <- make_color_pal(
      colors=colors, color_mode="qualitative", zlim=ifelse(add_white, N_-1, N_)
    )
    if (length(pal$color) != length(unique(pal$color))) {
      warning("Some colors are assigned to more than one label.")
    }
    if (add_white) {
      pal$color <- as.character(pal$color)
      pal <- rbind(c("white", 0), pal)
      pal$color <- factor(pal$color, levels=unique(pal$color))
      pal$value <- seq(nrow(pal)) - 1
    }
  }
  if (nrow(pal) < N_) {
    pal <- expand_color_pal(pal, N_)
  } else if (nrow(pal) > N_) {
    pal <- pal[seq(N_),]
    pal$color <- factor(pal$color, levels=unique(pal$color))
  }

  # Format color table in .dlabel style
  col_table <- col2rgb(pal$color, alpha=TRUE)/255
  col_table <- rbind(seq(N_)-1, col_table)
  rownames(col_table) <- c("Key", "Red", "Green", "Blue", "Alpha")
  col_table <- as.data.frame(t(col_table))
  rownames(col_table) <- if (!is.null(names(values))) { names(values) } else { values }

  # Add components to xifti
  T_ <- ncol_xifti(x)
  if (is.null(x$meta$cifti$names)) {
    # [TO DO]: Double check this is correct default name?
    x$meta$cifti$names <- paste("Column", seq(T_))
  }
  x$meta$cifti$labels <- rep(list(col_table), T_)
  names(x$meta$cifti$labels) <- x$meta$cifti$names

  # Change intent and check it.
  x$meta$cifti$intent <- 3007
  x$meta$cifti[c("time_start", "time_step", "time_unit")] <- NULL
  stopifnot(is.xifti(x))

  # Return the result.
  if (!input_is_xifti) {
    return(write_xifti(x, cifti_target_fname))
  } else {
    if (return_conversion_table) {
      return(list(xifti=x, conversion_table=conversion_table))
    } else {
      return(x)
    }
  }

  stop()
}

#' @describeIn convert_xifti
#' 
#' Give the ".dscalar" intent (code 3006/ConnDenseScalar) to an input
#'  CIFTI file or \code{"xifti"} object. 
#' 
#' @param x The CIFTI file name or \code{"xifti"} object to convert.
#' @param cifti_target_fname File name for the converted CIFTI. Only used if
#'  \code{x} is a CIFTI file name. If \code{NULL} (default), will use the same
#'  name as \code{x} but with the extension updated.
#' @param names The column names. If \code{NULL} (default), will be set to
#'  "Column 1", "Column 2", ... .
#' 
#' @keywords internal
convert_to_dscalar <- function(x, cifti_target_fname=NULL, names=NULL) {
  
  input_is_xifti <- is.xifti(x, messages=FALSE)
  if (!input_is_xifti) { 
    if (is.null(cifti_target_fname)) {
      cifti_target_fname <- basename(gsub(
        get_cifti_extn(x), "dscalar.nii", x, fixed=TRUE
      ))
    }
  }

  # `xifti` input
  if (is.xifti(x, messages=FALSE)) {
    if (!is.null(x$meta$cifti$intent)) {
      if (x$meta$cifti$intent == 3006) {
        ciftiTools_warn("The input is already a dscalar `xifti`.\n")
        return(x)
      }
    }

    T_ <- ncol_xifti(x)

    # Change intent.
    x$meta$cifti$intent <- 3006
    x$meta$cifti[c("time_start", "time_step", "time_unit", "labels")] <- NULL
    if (!is.null(names)) {
      if (length(names) != T_) { 
        stop("The data has ", T_, " columns but `names` is length ", length(names), ".") 
      }
      x$meta$cifti$names <- as.character(names)
    } else {
      # [TO DO]: Double check this is correct default name?
      x$meta$cifti$names <- paste("Column", seq(T_))
    }

    stopifnot(is.xifti(x))

    return(x)

  # CIFTI input
  } else {
    stopifnot(is.character(x))
    stopifnot(length(x) == 1)
    stopifnot(file.exists(x))

    if (is.null(names)) {
      names <- paste("Column", seq(ncol(read_xifti(x, brainstructures=info_cifti(x)$cifti$brainstructures) )))
    }

    cmd <- paste(
      "-cifti-change-mapping", x, 
      "ROW", cifti_target_fname, "-scalar"
    )

    names_fname <- tempfile()
    cat(names, file = names_fname, sep = "\n")
    cmd <- paste(cmd, "-name-file", names_fname)
    run_wb_cmd(cmd)

    return(cifti_target_fname)
  }
}

#' @describeIn convert_xifti
#' 
#' Give the ".dtseries" intent (code 3002/ConnDenseSeries) to an input
#'  \code{"xifti"} object. 
#' 
#' @param x The CIFTI file name or \code{"xifti"} object to convert.
#' @param cifti_target_fname File name for the converted CIFTI. Only used if
#'  \code{x} is a CIFTI file name. If \code{NULL} (default), will use the same
#'  name as \code{x} but with the extension updated.
#' @param time_start,time_step,time_unit (Optional) metadata for the new dtseries
#' 
#' @keywords internal
convert_to_dtseries <- function(
  x, cifti_target_fname=NULL,
  time_start=0, time_step=1, time_unit=c("second", "hertz", "meter", "radian")) {
  
  time_start <- as.numeric(time_start[1])
  time_step <- as.numeric(time_step[1])
  time_unit <- match.arg(tolower(time_unit), c("second", "hertz", "meter", "radian"))

  input_is_xifti <- is.xifti(x, messages=FALSE)
  if (!input_is_xifti) { 
    if (is.null(cifti_target_fname)) {
      cifti_target_fname <- basename(gsub(
        get_cifti_extn(x), "dtseries.nii", x, fixed=TRUE
      ))
    }
  }

  if (is.xifti(x, messages=FALSE)) {

    if (!is.null(x$meta$cifti$intent)) {
      if (x$meta$cifti$intent == 3002) {
        ciftiTools_warn("The input is already a dtseries `xifti`.\n")
        return(x)
      }
    }

    T_ <- ncol_xifti(x)

    # Change intent.
    x$meta$cifti$intent <- 3002
    x$meta$cifti[c("names", "labels")] <- NULL
    x$meta$cifti$time_start <- time_start
    x$meta$cifti$time_step <- time_step
    x$meta$cifti$time_unit <- time_unit

    stopifnot(is.xifti(x))

    return(x)

  } else {

    stopifnot(is.character(x))
    stopifnot(length(x) == 1)
    stopifnot(file.exists(x))

    cmd <- paste(
      "-cifti-change-mapping", x, 
      "ROW", cifti_target_fname, "-series",
      time_step, time_start, "-unit", toupper(time_unit)
    )

    run_wb_cmd(cmd)

    return(cifti_target_fname)
  }
}

#' Convert the intent of a CIFTI file or \code{"xifti"} object
#' 
#' @param x The CIFTI file name or \code{"xifti"} object to convert.
#' @param to The desired intent: \code{"dscalar"} (default), \code{"dtseries"},
#'  or \code{"dlabel"}
#' @param cifti_target_fname File name for the converted CIFTI. Only used if
#'  \code{x} is a CIFTI file name. If \code{NULL} (default), will use the same
#'  name as \code{x} but with the extension updated.
#' @param ... Only used if \code{x} is a \code{"xifti"} object. Additional 
#'  options specific to the target type and intent, e.g. for
#'  \code{convert_to_dlabel}.
#' 
#' @return If \code{x} is a CIFTI, the target is a \code{"dlabel"} and 
#'  \code{return_conversion_table}, a length-2 list with the first entry being 
#'  the ".dlabel" \code{"xifti"} and the second being the conversion table. 
#'  Otherwise, the \code{"xifti"} or the output CIFTI file name is directly 
#'  returned.
#' 
#' @family manipulating
#' 
#' @export
convert_xifti <- function(x, to=c("dscalar", "dtseries", "dlabel"), 
  cifti_target_fname=NULL, ...){

  to <- match.arg(to, c("dscalar", "dtseries", "dlabel"))

  switch(to,
      dscalar = convert_to_dscalar(x, cifti_target_fname, ...),
      dtseries = convert_to_dtseries(x, cifti_target_fname, ...),
      dlabel = convert_to_dlabel(x, cifti_target_fname, ...)
  )
}