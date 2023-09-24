#' @describeIn convert_xifti
#'
#' Give the ".dlabel" intent (code 3007/ConnDenseLabel) to an input
#'  \code{"xifti"}. Will use the same label table for each data column. Can also
#'  be used to re-assign values in the label table, or to change label names.
#'
#' @param x The CIFTI file name or \code{"xifti"} object to convert.
#' @param cifti_target_fname File name for the converted CIFTI. Only used if
#'  \code{x} is a CIFTI file name. If \code{NULL} (default), will use the same
#'  name as \code{x} but with the extension updated.
#' @param levels,levels_new,labels_new (Optional) \code{levels} is a vector of 
#'  the original data values. They should all be unique. They may not all occur 
#'  in the \code{"xifti"} data, but every datapoint in the \code{"xifti"} must 
#'  occur in \code{levels}. If \code{levels} is not provided it will be set to 
#'  the vector of all unique values in the data, in ascending order.
#'
#'  If \code{levels_new} is not provided, the original values will be re-mapped
#'  to integers from $0$ to $N-1$ (the "Keys" of a "dlabel" CIFTI), with $N$ 
#'  being the length of \code{levels}. Otherwise, \code{levels_new} can be a 
#'  vector the same length as \code{values} specifying the corresponding new 
#'  integers to use (rather than $0$ to $N-1$). If \code{x} is already "dlabel", 
#'  then by setting \code{levels} to the current label table values and 
#'  \code{levels_new} to the desired new values, the data can be re-leveled
#'  (see examples in function documentation). Note that duplicate levels are
#'  allowed, to map multiple existing levels to the same new level. 
#' 
#'  If \code{levels_new} was provided, new label names can also be set with 
#'  \code{labels_new}. If provided, it must be a character vector with the same
#'  length as \code{levels_new}. If there are duplicates in \code{levels_new},
#'  the first label for a given level will be used. If \code{labels_new} is 
#'  not provided, the new label names will be set to \code{levels_new} if it was
#'  provided, and \code{levels} if it was not. 
#'
#'  Note: \code{NA} and \code{NaN} values are handled a bit differently. Data
#'  locations that are \code{NA} or \code{NaN} will remain unchanged. \code{NA}
#'  and \code{NaN} should not be included in \code{levels} or \code{levels_new}.
#   The function will still try to handle this case, but may result in error.
#'
#' @param nsig Take this many significant digits for the data values. If
#'  \code{Inf} (default), do not round.
#' @param colors (Optional) "ROY_BIG_BL", the name of a ColorBrewer palette
#'  (see \code{RColorBrewer::brewer.pal.info} and colorbrewer2.org), the name of
#'  a viridisLite palette, or a character vector of colors. Default:
#'  \code{"Set2"}.
#' @param add_white Append white to the beginning of the colors? Default: \code{TRUE}.
#' @param return_conversion_table Return the conversion table along with the
#'  converted \code{"xifti"}? Default: \code{FALSE}. It will give the original
#'  \code{values}, the \code{values_new} (i.e. the "Keys"), and the new
#'  \code{label} names. 
#'
#' @examples
#' \dontrun{
#' # Example: change label names
#' levels <- xii$meta$cifti$labels[[1]]$Key
#' newLabels <- paste0("New Label #", seq(length(levels)))
#' xii <- convert_to_dlabel(xii, levels=levels, levels_new=levels, labels_new=newLabels)
#' # Example: add an empty level
#' levels <- xii$meta$cifti$labels[[1]]$Key
#' levels <- c(levels, max(levels)+1)
#' labels_new <- c(rownames(xii$meta$cifti$labels[[1]]), "Empty")
#' xii <- convert_to_dlabel(xii, levels=levels, levels_new=levels, labels_new=labels_new)
#' # Example: set all but the lowest value to the same value & re-label
#' levels <- xii$meta$cifti$labels[[1]]$Key
#' levels_new <- ifelse(levels==min(levels), min(levels), min(levels)+1)
#' labels_new <- ifelse(levels==min(levels), "Minimum", "Not minimum")
#' xii <- convert_to_dlabel(xii, levels=levels, levels_new=levels_new, labels_new=labels_new)
#' } 
#' 
#' @export
convert_to_dlabel <- function(x, cifti_target_fname=NULL,
  levels=NULL, levels_new=NULL, labels_new=NULL, nsig=Inf, 
  colors="Set2", add_white=TRUE,
  return_conversion_table=FALSE) {

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

  # Users might want to use this function to re-assign values.
  # if (!is.null(x$meta$cifti$intent) && x$meta$cifti$intent == 3007) {
  #   if (!input_is_xifti) {
  #     file.copy(x_original, cifti_target_fname)
  #     return(cifti_target_fname)
  #   } else {
  #     ciftiTools_warn("The input is already a dlabel `xifti`.\n")
  #     return(x)
  #   }
  # }

  # Get the label levels.
  if (is.null(levels)) {
    # Infer the new levels.
    levels <- unique(signif(as.vector(as.matrix(x)), nsig))
    # Sort.
    levels <- sort(levels, na.last=FALSE)
  } else {
    # Check the new levels.
    if (any(duplicated(levels))) {
      warning("Removing duplicate `levels`.\n")
      levels <- unique(levels)
    }
    # Notice: no sorting.
  }
  levels_hasNA <- any(is.na(levels) & !is.nan(levels))
  levels_hasNaN <- any(is.nan(levels))
  valfac_exclude <- list(
    NULL,
    c(NA),
    c(NaN),
    c(NA, NaN)
  )[[1 + levels_hasNA*1 + levels_hasNaN*2]]
  if (length(levels) > 1000) { warning("Over 1000 unique `levels` in the `xifti`.\n") }
  levels <- levels[!is.na(levels)]

  if (is.null(labels_new)) {
    labels_new <- if (!is.null(levels_new)) {
      as.character(levels_new)
    } else {
      as.character(levels)
    }
  }

  if (!is.null(levels_new)) {
    stopifnot(is.numeric(levels_new))
    stopifnot(all(levels_new[!is.na(levels_new)] == round(levels_new[!is.na(levels_new)])))
    if (length(levels) != length(levels_new)) {
      stop(
        "`levels` is length ", length(levels),
        " but `levels_new` is length `", length(levels_new), ". ",
        ifelse(
          levels_hasNA||levels_hasNaN,
          "Note that `NA` `NaN` are handled separately and should not be included in `levels`.",
          ""
        )
      )
    }
  } else {
    levels_new <- seq(length(levels))-1
  }

  conversion_table <- data.frame(
    levels=levels,
    levels_new=levels_new,
    labels=labels_new
  )

  levels_new_unique <- conversion_table$levels_new[!duplicated(conversion_table$levels_new)]
  labels_unique <- conversion_table$labels[!duplicated(conversion_table$levels_new)]

  # Convert data to label levels.
  for (bs in names(x$data)) {
    if (is.null(x$data[[bs]])) { next }
    # Round to `nsig` decimal places.
    x$data[[bs]][] <- signif(x$data[[bs]][], nsig)
    # Get the mask of `NaN` levels to convert back later.
    valmask_NaN <- if (levels_hasNaN) { is.nan(x$data[[bs]][]) } else { NULL }
    # Convert `NA` and `NaN` to `NaN` levels.
    if (levels_hasNaN) {
      x$data[[bs]][] <- ifelse(
        is.na(as.vector(x$data[[bs]])) | is.nan(as.vector(x$data[[bs]])),
        NaN, as.vector(x$data[[bs]])
      )
    }
    # Check all levels (except `NA` and `NaN` if any) are in `levels`.
    stopifnot(all(x$data[[bs]][] %in% c(NA, NaN, conversion_table$levels)))
    # Reassign from `levels` to 1, 2, 3, ..., N.
    x$data[[bs]][] <- as.numeric(factor(
      x$data[[bs]][],
      levels=conversion_table$levels,
      exclude=valfac_exclude
    ))
    # Reassign from 1, 2, 3, ..., N to `levels_new`.
    x$data[[bs]][] <- conversion_table$levels_new[x$data[[bs]][]]
    # NaN got converted to NA in previous line, so here we convert them back.
    # (NA became NaN and then was converted back to NA--all good there.)
    if (levels_hasNaN) { x$data[[bs]][][valmask_NaN] <- NaN }
  }

  # Make color table.
  # [TO DO]: Allow input of custom colors
  N_ <- length(levels_new_unique)
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
  col_table <- rbind(levels_new_unique, col_table)
  rownames(col_table) <- c("Key", "Red", "Green", "Blue", "Alpha")
  col_table <- as.data.frame(t(col_table))
  rownames(col_table) <- labels_unique
  col_table <- col_table[order(col_table$Key),]

  # Add components to xifti
  T_ <- ncol_xifti(x)
  if (is.null(x$meta$cifti$names)) {
    x$meta$cifti$names <- paste("Column", seq(T_))
  }
  x$meta$cifti$labels <- rep(list(col_table), T_)
  names(x$meta$cifti$labels) <- x$meta$cifti$names

  # Change intent and check it.
  x$meta$cifti$intent <- 3007
  x$meta$cifti[c("time_start", "time_step", "time_unit")] <- NULL
  stopifnot(is.xifti(x))

  # Return the result.
  if (!input_is_xifti) { x <-  write_xifti(x, cifti_target_fname) }
  if (return_conversion_table) {
    out <- list(xifti=x, conversion_table=conversion_table)
  } else {
    out <- x
  }
  out
}

#' @describeIn convert_xifti
#'
#' Give the ".dscalar" intent (code 3006/ConnDenseScalar) to an input
#'  CIFTI file or \code{"xifti"} object. Can also be used to set the names for
#'  each column with \code{names}.
#'
#' @param x The CIFTI file name or \code{"xifti"} object to convert.
#' @param cifti_target_fname File name for the converted CIFTI. Only used if
#'  \code{x} is a CIFTI file name. If \code{NULL} (default), will use the same
#'  name as \code{x} but with the extension updated.
#' @param names The column names. If \code{NULL} (default), will be set to
#'  "Column 1", "Column 2", ... .
#'
#' @export
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
    # if (!is.null(x$meta$cifti$intent)) {
    #   if (x$meta$cifti$intent == 3006) {
    #     ciftiTools_warn("The input is already a dscalar `xifti`.\n")
    #     return(x)
    #   }
    # }

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
      "ROW", sys_path(cifti_target_fname), "-scalar"
    )

    names_fname <- tempfile()
    writeLines(names, names_fname)
    cmd <- paste(cmd, "-name-file", sys_path(names_fname))
    run_wb_cmd(cmd)

    return(cifti_target_fname)
  }
}

#' @describeIn convert_xifti
#'
#' Give the ".dtseries" intent (code 3002/ConnDenseSeries) to an input
#'  \code{"xifti"} object. Can also be used to set the time metadata.
#'
#' @param x The CIFTI file name or \code{"xifti"} object to convert.
#' @param cifti_target_fname File name for the converted CIFTI. Only used if
#'  \code{x} is a CIFTI file name. If \code{NULL} (default), will use the same
#'  name as \code{x} but with the extension updated.
#' @param time_start,time_step,time_unit (Optional) metadata for the new dtseries
#'
#' @export
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
    # if (!is.null(x$meta$cifti$intent)) {
    #   if (x$meta$cifti$intent == 3002) {
    #     ciftiTools_warn("The input is already a dtseries `xifti`.\n")
    #     return(x)
    #   }
    # }

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
      "-cifti-change-mapping", sys_path(x),
      "ROW", sys_path(cifti_target_fname), "-series",
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
