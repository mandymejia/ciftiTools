#' Convert the intent of a \code{"xifti"} to ".dlabel"
#' 
#' Give the ".dlabel" intent (code 3007/ConnDenseLabel) to an input
#'  \code{"xifti"} object. Will use the same label table for each data column.
#' 
#' @param xifti The \code{"xifti"}
#' @param values (Optional) A vector of the original data values. They should all
#'  be unique. They may not all occur in the \code{"xifti"} data, but every
#'  datapoint in the \code{"xifti"} must occur in \code{values}. Data will be 
#'  mapped to integers from $0$ to $N-1$, with $N$ being the length of 
#'  \code{values}.
#' @param colors (Optional) "ROY_BIG_BL", the name of a ColorBrewer palette 
#'  (see \code{RColorBrewer::brewer.pal.info} and colorbrewer2.org), the name of
#'  a viridisLite palette, or a character vector of colors. Default: 
#'  \code{"Set2"}.
#' @param add_white Append white to the beginning of the colors? Default: \code{TRUE}.
#' @param return_conversion_table Return the conversion table along with the 
#'  converted \code{"xifti"}? Default: \code{FALSE}
#' 
#' @return If \code{return_conversion_table}, a length-2 list with the first
#'  entry being the ".dlabel" \code{"xifti"} and the second being the conversion
#'  table. Otherwise, only the \code{"xifti"} is returned.
#' @export
convert_to_dlabel <- function(xifti, values=NULL, colors="Set2", add_white=TRUE, return_conversion_table=FALSE) {
  
  stopifnot(is.xifti(xifti))

  if (!is.null(xifti$meta$cifti$intent) && xifti$meta$cifti$intent == 3007) {
    ciftiTools_warn("The input is already a dlabel `xifti`.\n")
    return(xifti)
  }

  # Get the label values.
  convert_NA <- FALSE
  if (is.null(values)) {
    # Infer the new values.
    values <- sort(unique(as.vector(do.call(rbind, xifti$data))))
    if (any(is.na(as.vector(do.call(rbind, xifti$data))))) {
      if (!any(is.nan(as.vector(do.call(rbind, xifti$data))))) {
        convert_NA <- TRUE
        values <- c(NaN, values[!is.na(values)])
      } else {
        # [TO DO]: Workaround
        stop("Not implemented: handling presence of both NA and NaN values in data. Change one of them to a new data value.\n")
      }
    }
  } else {
    # Check the new values.
    if (any(duplicated(values))) { warning("Removing duplicate `values`.\n") }
    values <- sort(unique(values))
  }
  if (length(values) > 100) { warning("Over 100 unique `values` in the `xifti`.\n") }
  conversion_table <- data.frame(values=values, label=seq(length(values))-1)

  # Convert data to label values.
  for (bs in names(xifti$data)) {
    if (is.null(xifti$data[[bs]])) { next }

    if (convert_NA) {
      xifti$data[[bs]][] <- ifelse(
        is.na(as.vector(xifti$data[[bs]])), 
        NaN, as.vector(xifti$data[[bs]])
      )
    }

    stopifnot(all(xifti$data[[bs]][] %in% values))
    xifti$data[[bs]][] <- as.numeric(factor(xifti$data[[bs]][], levels=values)) - 1
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
  rownames(col_table) <- values

  # Add components to xifti
  T_ <- ncol_xifti(xifti)
  if (is.null(xifti$meta$cifti$names)) {
    # [TO DO]: Double check this is correct default name?
    xifti$meta$cifti$names <- paste("Column", seq(T_))
  }
  xifti$meta$cifti$labels <- rep(list(col_table), T_)
  names(xifti$meta$cifti$labels) <- xifti$meta$cifti$names

  # Change intent and check it.
  xifti$meta$cifti$intent <- 3007
  xifti$meta$cifti[c("time_start", "time_step", "time_unit")] <- NULL
  stopifnot(is.xifti(xifti))

  if (return_conversion_table) {
    return(list(xifti=xifti, conversion_table=conversion_table))
  } else {
    return(xifti)
  }
  stop()
}

#' Convert the intent of a \code{"xifti"} to ".dscalar"
#' 
#' Give the ".dscalar" intent (code 3006/ConnDenseScalar) to an input
#'  \code{"xifti"} object. 
#' 
#' @param xifti The \code{"xifti"}
#' 
#' @return The ".dscalar" \code{"xifti"}
#' @export
convert_to_dscalar <- function(xifti) {
  
  stopifnot(is.xifti(xifti))

  if (!is.null(xifti$meta$cifti$intent)) {
    if (xifti$meta$cifti$intent == 3006) {
      ciftiTools_warn("The input is already a dscalar `xifti`.\n")
      return(xifti)
    }
  }

  # Change intent and check it.
  xifti$meta$cifti$intent <- 3006
  xifti$meta$cifti[c("time_start", "time_step", "time_unit", "labels")] <- NULL
  #[TO DO] add names
  stopifnot(is.xifti(xifti))

  xifti
}