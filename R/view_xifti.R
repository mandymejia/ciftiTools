#' The ROY_BIG_BL color palette, the default palette from the Connectome Workbench.
#'
#' Yields the landmark color hex codes and values for the "ROY_BIG_BL" palette. This is the same color palette as the
#' default Connectime Workbench palette. Source: github.com/Washington-University/workbench/blob/master/src/Files/PaletteFile.cxx
#'
#' @param min The minimum value for the color mapping. As in the original palette, the last color (aqua) is actually
#'  placed at the bottom .5\% between the minimum and maximum.
#' @param max The maximum value for the color mapping. If this value is lower than the minimum, the color mapping will
#'  be reversed.
#' @param mid (Optional) The midpoint value for the color mapping. If \code{NULL} (default), the true midpoint is used.
#' @param pos_half Use the positive half (black --> red --> yellow) only? Default: \code{FALSE}.
#'
#' @export
#' @return A data.frame with two columns: color (character: color hex codes) and value (numeric).
#'
ROY_BIG_BL <- function(min=0, max=1, mid=NULL, pos_half=FALSE) {
  if (min==max) { stop("The minimum and maximum value should not be equal.") }
  # if (min==max) {
  #   return( data.frame(color = c("white", "white"), value = c(min, max)) )
  # }
  rev_order <- min > max
  if (rev_order) {
    temp <- min
    min <- max
    max <- temp
  }
  stopifnot(max > min)

  # Use the same landmark color RGB values, and same spacing. Note the spacing is not equidistant between landmarks.
  color <- c(
    "#ffff00", "#ffc800", "#ff7800", "#ff0000", "#c80000", "#960000",
    "#640000", "#3c0000", "#000000", "#000050", "#0000aa", "#4b007d",
    "#7d00a0", "#4b7d00", "#00c800", "#00ff00", "#00ffff"#, "#00ffff"
  )
  value <- c(
    1.00, 0.875, 0.750, 0.625, 0.500, 0.375,
    0.250, 0.125, 0.000, -0.125, -0.250, -0.375,
    -0.500, -0.625, -0.750, -0.875, -0.990#, -1.00
  )

  if (pos_half) {
    # Only use the latter half.
    value <- value[1:9]
    color <- color[1:9]
    # Normalize the values to [min, max].
    value <- value * (max - min) + min

  } else {
    # Normalize the values to [min, max]. Note that the bottom .5% are all #00ffff.
    value <- (value + 1)/2
    value <- value * (max - min) + min

    # Normalize middle value (black) to mid, if specified.
    if (!is.null(mid)) {
      stopifnot(mid > min & mid < max)
      old_mid <- (min + max)/2
      value[1:8] <- (value[1:8] - old_mid) / (max - old_mid) * (max - mid) + mid
      value[9] <- mid
      value[10:17] <- (value[10:17] - min) / (old_mid - min) * (mid - min) + min
    }
  }
  if (rev_order) { value <- value[length(value):1] }
  return(data.frame(color=color, value=value))
}

#' Control the mapping of values to colors with \code{colors}, \code{color_mode}, and \code{zlim}.
#'
#' There are three argument types for \code{colors}: \code{"ROY_BIG_BL"}, the name of an \code{RColorBrewer} palette, or
#'  a character vector of color names.
#'
#' If \code{colors=="ROY_BIG_BL"}, the "ROY_BIG_BL" pallete will be used. It is the same palette as the default used in
#'  the Connectome Workbench application (see github.com/Washington-University/workbench/blob/master/src/Files/PaletteFile.cxx).
#'  The midpoint will be colored black. From the midpoint toward the upper bound, colors will proceed from black to red
#'  to yellow. From the midpoint toward the lower bound, colors will proceed from black to blue to purple to green to aqua.
#'  Note that the colors are not equally-spaced, and the bottom .5\% of the color range has the same color. Here is how
#'  each color mode behaves if \code{colors=="ROY_BIG_BL"}:
#'
#' \describe{
#'  \item{\code{color_mode=="sequential"}}{Only the second half of the pallete will be used (black --> red --> yellow).
#'    If \code{identical(zlim, NULL)}, the colors will be mapped between \code{DATA_MIN} (black) to
#'    \code{DATA_MAX} (yellow). If \code{length(zlim)==2}, \code{zlim[1]} will be the lower bound (black)
#'    and \code{zlim[2]} will be the upper bound (yellow). If \code{zlim[1] > zlim[2]}, the
#'    first value will be used as the maximum and the second will be used as the minimum, and the color scale will be
#'    reversed with the highest value colored black and the lowest value colored yellow.
#'  }
#'  \item{\code{color_mode=="qualitative"}}{The "ROY_BIG_BL" pallete is not recommended for qualitative data, so a
#'    warning will be issued. Colors will be based on the landmark colors in the "ROY_BIG_BL" pallete. If
#'    \code{identical(zlim, NULL)}, the colors will be mapped onto each integer between \code{DATA_MIN} and
#'    \code{DATA_MAX}, inclusive. Color interpolation will be used if the number of colors in the palette (17) is less
#'    than this range. If \code{length(zlim)==length(colors)}, each color will be mapped to each corresponding value.
#'  }
#'  \item{\code{color_mode=="diverging"}}{If \code{identical(zlim, NULL)}, the colors will be mapped from
#'    \code{DATA_MIN} (aqua) to \code{DATA_MAX} (yellow). If \code{length(zlim)==1}, this value will be used as
#'    the midpoint (black) instead of the data midpoint. If \code{length(zlim)==2}, \code{zlim[1]} will
#'    be the lower bound (aqua) and \code{zlim[2]} will be the upper bound (yellow). If \code{length(zlim)==3},
#'    these values will correspond to the lowest bound (aqua), midpoint (black), and upper bound (yellow) respectively.
#'    If the \code{zlim} are in descending order, the first value will be used as the maximum and the last
#'    will be used as the minimum, and the color scale will be reversed with the highest values colored aqua and the
#'    lowest values colored yellow.
#'  }
#' }
#'
#' If \code{colors} is the name of an RColorBrewer palette (see a listing at `RColorBrewer::brewer.pal.info`), the colors
#'  in that pallete will be used, and the below behavior applies. If \code{colors} is a character vector of color names
#'  (hex codes or standard R color names), the below behavior applies directly:
#'
#' \describe{
#'  \item{\code{color_mode=="sequential"}}{If \code{identical(zlim, NULL)}, the colors will be mapped with equal
#'    spacing from \code{DATA_MIN} to \code{DATA_MAX}. If \code{length(zlim)==2}, these values will be used as
#'    the upper and lower bounds instead. If \code{zlim[1] > zlim[2]}, the first value will be used as
#'    the maximum and the second will be used as the minimum, and the color scale will be reversed. If
#'    \code{length(zlim)==length(colors)}, each color will be mapped to each corresponding value.
#'  }
#'  \item{\code{color_mode=="qualitative"}}{If \code{identical(zlim, NULL)}, the colors will be mapped onto
#'    each integer between \code{DATA_MIN} and \code{DATA_MAX}, inclusive. Color interpolation will be used if the
#'    number of colors in the palette is less than this range. If \code{length(zlim)==length(colors)}, each
#'    color will be mapped to each corresponding value.
#'  }
#'  \item{\code{color_mode=="diverging"}}{If \code{identical(zlim, NULL)}, the colors will be mapped with equal
#'    spacing from \code{DATA_MIN} to \code{DATA_MAX}. Thus, the middle color will correspond to the midpoint of the
#'    data. If \code{length(zlim)==1}, the middle color will correspond to this value instead. The preceeding
#'    colors will be equally-spaced between \code{DATA_MIN} and this value; the following colors will be equally-spaced
#'    between this value and\code{DATA_MAX}. If \code{length(zlim)==2}, \code{zlim[1]} will be the lower
#'    bound (first color) and \code{zlim[2]} will be the upper bound (last color). If \code{length(zlim)==3},
#'    these values will correspond to the lowest bound, midpoint, and upper bound respectively. There must be an odd
#'    number of colors, since the diverging color mode requires a midpoint. If the \code{zlim} are in descending
#'    order, the first value will be used as the maximum and the last will be used as the minimum, and the color scale
#'    will be reversed. Finally, if \code{length(zlim)==length(colors)}, each color will be mapped to each
#'    corresponding value. Thus, the middle color will correspond to the middle color_value. The length of \code{colors}
#'    must be odd and >= 3.
#'  }
#' }
#'
#' @param colors (Optional) "ROY_BIG_BL", the name of a ColorBrewer palette (see RColorBrewer::brewer.pal.info and
#'  colorbrewer2.org), or a character vector of colors. Defaults are \code{"ROY_BIG_BL"} (sequential color mode),
#'  \code{"Set2"} (qualitative color mode), and \code{"ROY_BIG_BL"} (diverging color mode). See the description for more
#'  details.
#' @param color_mode (Optional) \code{"sequential"}, \code{"qualitative"}, or \code{"diverging"}. Default: "sequential".
#'  See the description for more details.
#' @param zlim (Optional) Controls the mapping of values to each color in \code{colors}. If the length is longer
#'  than one, using -Inf will set the value to \code{DATA_MIN}, and Inf will set the value to \code{DATA_MAX}. See the
#'  description for more details.
#' @param DATA_MIN (Optional) The minimum value of the data to make the palette for. Overrided by certain \code{zlim}.
#' @param DATA_MAX (Optional) The maximum value of the data to make the palette for. Overrided by certain \code{zlim}.
#'
#' @export
#' @importFrom grDevices colorRampPalette
#' @import RColorBrewer
#'
#' @return A data.frame with two columns: color (character: standard color names or hex codes) and value (numeric).
#'
make_color_pal <- function(colors=NULL, color_mode=c("sequential", "qualitative", "diverging"), zlim=NULL,
                           DATA_MIN=0, DATA_MAX=1) {
  color_mode <- match.arg(color_mode, c("sequential", "qualitative", "diverging"))

  if (DATA_MIN >= DATA_MAX) { stop("DATA_MAX must be greater than DATA_MIN") }

  # Use default palettes if the colors are not specified.
  if (identical(colors, NULL)) {
    colors <- switch(color_mode,
                     sequential="ROY_BIG_BL", # will use pos half
                     qualitative="Set2",
                     diverging="ROY_BIG_BL")
  }

  N_COLORS_PRE <- length(colors)
  N_COLOR_VALUES_PRE <- length(zlim)
  if (N_COLORS_PRE == 1) {
    # ROY_BIG_BL
    if (colors == "ROY_BIG_BL") {
      if (color_mode=="sequential") {
        if (identical(zlim, NULL)) {
          RBB <- ROY_BIG_BL(DATA_MIN, DATA_MAX, pos_half=TRUE)
        } else if (N_COLOR_VALUES_PRE==2) {
          RBB <- ROY_BIG_BL(zlim[1], zlim[2], pos_half=TRUE)
        } else {
          stop("The sequential ROY_BIG_BL palette (default) requires two (min and max values) or NULL/none zlim.")
        }

      } else if (color_mode=="qualitative") {
        warning("The ROY_BIG_BL palette is not recommended for qualitative data.")
        RBB <- ROY_BIG_BL(DATA_MIN, DATA_MAX)

      } else if (color_mode=="diverging") {
        if (identical(zlim, NULL)) {
          RBB <- ROY_BIG_BL(DATA_MIN, DATA_MAX)
        } else if (N_COLOR_VALUES_PRE==1) {
          RBB <- ROY_BIG_BL(DATA_MIN, DATA_MAX, mid=zlim)
        } else if (N_COLOR_VALUES_PRE==2) {
          RBB <- ROY_BIG_BL(zlim[1], zlim[2],
                            mid=(zlim[1]+zlim[2])/2)
        } else if (N_COLOR_VALUES_PRE==3) {
          RBB <- ROY_BIG_BL(zlim[1], zlim[3], mid=zlim[2])
        }
      }

      colors <- RBB$color
      if (color_mode != "qualitative") { zlim <- RBB$value }

    # RColorBrewer
    } else if (colors %in% row.names(brewer.pal.info)) {
      colors_info <- brewer.pal.info[row.names(brewer.pal.info) == colors,]
      brewer_mode <- match.arg(as.character(colors_info$category), c("sequential", "qualitative", "diverging"))
      if (brewer_mode != color_mode) {
        warning(paste("The RColorBrewer palette type is", brewer_mode, "but the color_mode is", color_mode))
      }
      colors <- brewer.pal(as.numeric(colors_info$maxcolors), colors)

    } else {
      stop("The `colors` argument must be 'ROY_BIG_BL', an palette listed in `RColorBrewer::brewer.pal.info`, or
        a character vector of color hex codes or standard R names with a length greater than one.")
      }
  }

  N_COLORS <- length(colors)
  N_COLOR_VALUES <- length(zlim)
  if (!identical(zlim, NULL)) {
    # Check that the color values are valid.
    valid_color_values_lengths <- switch(color_mode,
                                         sequential=c(2, N_COLORS),
                                         qualitative=N_COLORS,
                                         diverging=c(1, 2, 3, N_COLORS))
    if (!(N_COLOR_VALUES %in% valid_color_values_lengths)) {
      stop(paste("There are", N_COLOR_VALUES, "color values--this is not compatible for the", color_mode, "color mode. See the description for details."))
    }

    # Order color values from lowest to highest, if not already sorted.
    color_values_order <- order(zlim)
    zlim <- zlim[color_values_order]
    # If the color values are descending, reverse the color scale.
    if (identical(color_values_order, length(zlim):1)) {
      colors <- colors[length(colors):1]
    } else if (N_COLOR_VALUES==N_COLORS) {
      colors <- colors[color_values_order]
    }

    # Replace infinite values with data bounds.
    if (identical(zlim[1], -Inf)) { zlim[1] <- DATA_MIN }
    if (identical(zlim[N_COLOR_VALUES], Inf)) { zlim[N_COLOR_VALUES] <- DATA_MAX }
  }

  # Sequential
  if (color_mode == "sequential") {
    pal_cols <- colors
    if (identical(zlim, NULL)) {
      pal_vals <- seq(DATA_MIN, DATA_MAX, length.out=length(colors))
    } else {
      if (N_COLOR_VALUES==2) {
        pal_vals <- seq(zlim[1], zlim[2], length.out=length(colors))
      } else if (N_COLOR_VALUES == N_COLORS) {
        pal_vals <- zlim
      } else {
        stop("The sequential color mode requires length(zlim) == 0 (NULL), 2, or length(colors).")
      }
    }

    #Qualitative
  } else if (color_mode=="qualitative") {
    if (!identical(c(DATA_MIN, DATA_MAX), round(c(DATA_MIN, DATA_MAX)))) {
      stop("Data bounds must be integers for qualitative color mode.")
      }
    N_DATA_VALUES <- DATA_MAX - DATA_MIN + 1
    if (identical(zlim, NULL)) {
      pal_vals <- c(DATA_MIN:DATA_MAX)
      if (length(colors) >= N_DATA_VALUES) {
        pal_cols <- colors[1:N_DATA_VALUES]
      } else {
        # Might look weird for the ROY_BIG_BL pallete, but ROY_BIG_BL is not recommended anyway for qualitative data.
        pal_cols <- colorRampPalette(colors)(N_DATA_VALUES)
      }
    } else if (N_COLOR_VALUES==N_COLORS) {
      pal_vals <- zlim
      pal_cols <- colors
    } else {
      stop("The qualitative color mode requires length(zlim) == 0 (NULL) or length(colors).")
    }

    # Diverging
  } else if (color_mode=="diverging") {
    pal_cols <- colors
    if (identical(zlim, NULL)) {
      pal_vals <- seq(DATA_MIN, DATA_MAX, length.out=length(colors))
    } else {
      if (N_COLOR_VALUES==N_COLORS) {
        pal_vals <- zlim
      } else {
        # Get the minimum, middle, and maximum value for the color scale.
        if (N_COLOR_VALUES==1) {
          mid_val <- zlim
          if ((mid_val <= DATA_MIN) | (mid_val >= DATA_MAX)) {
            stop("If one color_value is used with the diverging color_mode, it represents the midpoint of the data scale
              and must be between the data minimum and maximum. (It does not have to be the true midpoint.) Different
              bounds can be set with color_value=c(new_min, midpoint, new_max).")
          }
          min_val <- DATA_MIN
          max_val <- DATA_MAX
        } else if (N_COLOR_VALUES==2) {
          mid_val <- (DATA_MIN + DATA_MAX)/2
          min_val <- zlim[1]
          max_val <- zlim[2]
        } else if (N_COLOR_VALUES==3) {
          mid_val <- zlim[2]
          min_val <- zlim[1]
          max_val <- zlim[3]
        }

        # Interpolate between the min/mid/max to get the color values.
        # To-do: allow for odd-length color palettes?
        if (length(colors) %% 2 != 1) { stop("There must be an odd number of colors for the diverging color mode, to have a middle color.") }
        low_vals <- seq(min_val, mid_val, length.out=floor(N_COLORS/2)+1)
        low_vals <- low_vals[1:(length(low_vals)-1)]
        high_vals <- seq(mid_val, max_val, length.out=floor(N_COLORS/2)+1)
        high_vals <- high_vals[2:length(high_vals)]
        pal_vals <- c(low_vals, mid_val, high_vals)
      }
    }
  } else {
    stop(paste("Unrecognized color mode", color_mode))
  }

  return(data.frame(color=pal_cols, value=pal_vals))
}

#' Interpolates between entries in the input palette to make a larger palette with at least MIN_COLOR_RES entries.
#'
#' @param pal The color palette to expand: a data.frame with two columns, color (character: standard color names
#'  or hex codes) and value (numeric).
#' @param MIN_COLOR_RES The minimum number of entries to have in the output palette. Because of rounding, there may be more
#'  than this number of entries.
#'
#' @export
#' @importFrom grDevices colorRampPalette
#'
#' @return A data.frame with two columns: color (character: standard color names or hex codes) and value (numeric).
#'
expand_color_pal <- function(pal, MIN_COLOR_RES=255) {
  if (nrow(pal) < MIN_COLOR_RES) {
    colors <- as.character(pal$color)
    # Interpolate between palette values to obtain at least MIN_COLOR_RES color levels.
    color_res <- MIN_COLOR_RES * diff(pal$value)/(max(pal$value) - min(pal$value))
    color_res <- as.integer(round(pmax(color_res, 2)))
    vals <- vector(length=0, mode="numeric")
    cols <- vector(length=0, mode="character")
    for(ii in 1:(nrow(pal)-1)) {
      next_vals <- seq(pal$value[ii], pal$value[ii+1], length.out=color_res[ii])
      next_vals <- next_vals[1:(length(next_vals)-1)]
      next_cols <- colorRampPalette(c(colors[ii], colors[ii+1]))(color_res[ii])
      next_cols <- next_cols[1:(length(next_cols)-1)]
      vals <- c(vals, next_vals)
      cols <- c(cols, next_cols)
    }
    vals <- c(vals, pal$value[nrow(pal)])
    cols <- c(cols, colors[nrow(pal)])
  } else {
    vals <- pal$value
    cols <- as.character(pal$color)
  }
  data.frame(color=cols, value=vals)
}

#' Applies a palette to a data vector to yield a vector of colors.
#'
#' @param data_values The values to map to colors
#' @param pal The palette to use to map values to colors
#' @param color_NA The color to use for NA values (default is "white").
#'
#' @return A character vector of color names
use_color_pal <- function(data_values, pal, color_NA="white") {
  mask <- is.na(data_values)
  colors <- as.character(pal$color)
  pal$cut <- -Inf
  pal$cut[2:nrow(pal)] <- diff(pal$value)/2 + pal$value[1:(length(pal$value)-1)]
  out <- vector("character", length(data_values))
  out[!mask] <- colors[apply(outer(as.numeric(data_values[!mask]), pal$cut, '>='), 1, sum)]
  out[mask] <- color_NA
  out
}

#' Visualize "xifti" cortical data. The \code{rgl} package is required.
#'
#' @inheritParams xifti_Param
#' @param idx The time/column index of the xifti data to plot. 
#'  Currently one a single time point is supported. Default: the first index.
#' @param hemisphere Which brain cortex to display: "both", "left", or "right". 
#'  If \code{NULL} (default), each available surface (e.g. if \code{surfL}
#'  or \code{xifti$surf$cortex_right} is not empty) will be displayed. Surfaces without
#'  data (e.g. \code{xifti$CORTEX_LEFT} is empty) will still be displayed,
#'  colored by \code{color_NA}. Each cortex will be shown in a separate panel
#'  column within the RGL window.
#' @param view Which view to display: "lateral", "medial", or "both". 
#'  If \code{NULL} (default), both views will be shown. Each view
#'  will be shown in a separate panel row within the RGL window.
#' @param mode One of "widget" (Default), "image", or "video":
#' 
#'  "widget" will open an interactive RGL window. Left click and drag to rotate.
#'  Use the scroll wheel to zoom. Run the R function 
#'  \code{rgl::snapshot("my_file.png")} to save the RGL window as a png. 
#'  See \code{\link[rgl]{snapshot}} for more information.
#'  
#'  "image" will open the RGL window, take a screenshot using
#'  \code{\link[rgl]{snapshot}}, and close it. The screenshot will be saved
#'  as a png in \code{write_dir} and its name will be \code{fname}.
#' 
#'  "video" will take a series of screenshots of the RGL window, while 
#'  rotating the brain surface(s), saving each to a file in \code{write_dir} 
#'  named by \code{fname}, and then close the RGL window. The frames can 
#'  be converted to a video file using multimedia software such as Adobe 
#'  Premiere Pro. The "video" mode is not yet supported.
#' @param width,height The dimensions of the RGL window, in pixels. If both are 
#'  \code{NULL} (default), the dimensions will be set to 
#'  1000 (width) x 700 (height) for 1x1 and 2x2 subplots,
#'  1500 x 525 for 2x1 subplots, and
#'  500 x 700 for 1x2 subplots. These defaults are chosen to fit comfortably
#'  within a 1600 x 900 screen. Specyfing only one will set the other to maintain
#'  the same aspect ratio. Both could be specified to set the dimensions exactly.
#' @param zoom Adjustment to size of brain meshes. Default: \code{3/5} 
#'  (100\% + 3/5*100\% = 160\% the original size).
#' @param bg Background color. \code{NULL} will not color the background (white).
#' @param title Optional title for the plot. It will be printed at the top in
#'  a separate subplot with 1/4 the height of the brain cortex subplots.
#'  \code{NULL} (default) will use the time index (.dtseries) or name
#'  (.dscalar or .dlabel) of the data column being plotted. Set to an empty
#'  string \code{""} to omit the title. If the title is non-empty but does not
#'  appear, \code{cex.title} may need to be lowered.
#' @param cex.title Font size multiplier for the title. \code{NULL} (default)
#'  will use \code{2.5} for titles less than 20 characters long, and smaller
#'  sizes for increasingly longer titles.
#' @param text_color Color for text in title and colorbar legend. Default: 
#'  "black".
#' @param fname An identifier to use for naming the saved images 
#'  ("[fname].png") and video frames ("[fname]_1.png", "[fname]_2.png", ...).
#'  Default: "xifti". 
#' @param write_dir Where should any output images be written. NULL (default) 
#'  will write them to the current working directory. 
#' 
#'  \code{write_dir} must already exist, or an error will occur.
#' @param colors (Optional) "ROY_BIG_BL", vector of colors to use, 
#'  OR the name of a ColorBrewer palette (see RColorBrewer::brewer.pal.info 
#'  and colorbrewer2.org). Defaults are \code{"ROY_BIG_BL"} (sequential), 
#'  \code{"Set2"} (qualitative), and \code{"ROY_BIG_BL"} (diverging). An exception
#'  to these defaults is if the "xifti" represents a .dlabel CIFTI (intent 3007),
#'  then the qualitative colors in the label table will be used.
#'  See the \code{ciftiTools::make_color_pal()} description for more details.
#' @param color_mode (Optional) \code{"sequential"}, \code{"qualitative"}, 
#'  or \code{"diverging"}. \code{NULL} will use the qualitative color mode if 
#'  the "xifti" represents a .dlabel CIFTI (intent 3007), and the sequential
#'  color mode otherwise. See the \code{ciftiTools::make_color_pal()} 
#'  description for more details.
#' @param zlim (Optional) Controls the mapping of values to each 
#'  color in \code{colors}. If the length is longer than
#'  one, using -Inf will set the value to \code{DATA_MIN}, and Inf will set 
#'  the value to \code{DATA_MAX}. See the
#'  \code{ciftiTools::make_color_pal()} description for more details.
#' @param surfL,surfR (Optional if \code{xifti$surf$cortex_left} and 
#'  \code{xifti$surf$cortex_right} are not empty) The brain surface model to use. 
#'  Each can be a file path for a GIFTI, a file read by gifti::readgii, 
#'  or a list with components "vertices" and "faces". If provided, they will override 
#'  \code{xifti$surf$cortex_left} and \code{xifti$surf$cortex_right} if they exist. 
#'  Otherwise, leave these arguments as \code{NULL} (default) to use 
#'  \code{xifti$surf$cortex_left} and \code{xifti$surf$cortex_right}.
#' @param colorbar_embedded Should the colorbar be embedded in the RGL window?
#'  It will be positioned in the bottom-left corner, in a separate subplot 
#'  with 1/4 the height of the brain cortex subplots. Default: \code{TRUE}.
#'  If \code{FALSE}, print it separately instead.
#' @param colorbar_digits The number of digits for the colorbar legend ticks. 
#'  If \code{NULL} (default), let \code{\link{format}} decide.
#'
#' @export
#' @importFrom fields image.plot
#' @importFrom grDevices dev.list dev.off
view_xifti_surface <- function(xifti, idx=1, 
  hemisphere=NULL, view=c("both", "lateral", "medial"),
  mode=c("widget", "image", "video"), width=NULL, height=NULL, zoom=.6,
  bg=NULL, title=NULL, cex.title=NULL, text_color="black",
  fname="xifti", write_dir=NULL,
  colors=NULL, color_mode=NULL, zlim=NULL,
  surfL=NULL, surfR=NULL,
  colorbar_embedded=TRUE, colorbar_digits=NULL) {

  if (!requireNamespace("rgl", quietly = TRUE)) {
    stop("Package \"rgl\" needed to use `view_xifti_surface`. Please install it.", call. = FALSE)
  }
  if (!capabilities("X11")) {
    warning("X11 capability is needed to open the rgl window for `view_xifti_surface`.")
  }

  # Try to avoid this error with colorbar: 
  #   Error in par(old.par) : 
  #   invalid value specified for graphical parameter "pin"
  while (!is.null(dev.list()))  dev.off()

  # ----------------------------------------------------------------------------
  # Check arguments ------------------------------------------------------------
  # ----------------------------------------------------------------------------

  # Check xifti, idx, and surfaces.
  # [TO DO]: Check input surface matches the cortex!
  stopifnot(is.xifti(xifti))
  if (length(idx) > 1) stop("Only one time/column index is supported right now.")
  if (is.null(surfL)) {
    if (is.null(xifti$surf$cortex_left) & !is.null(hemisphere)) {
      if (hemisphere %in% c("both", "left")) {
        stop("The left hemisphere was requested, but no surface data was provided (xifti$surf$cortex_left or the surfL argument).")
      }
    } else {
      surfL <- xifti$surf$cortex_left
    }
  } else { 
    surfL <- make_surface(surfL) 
  }
  if (is.null(surfR)) {
    if (is.null(xifti$surf$cortex_right) & !is.null(hemisphere)) {
      if (hemisphere %in% c("both", "right")) {
        stop("The right hemisphere was requested, but no surface data was provided (xifti$surf$cortex_right or the surfR argument).")
      }    
    } else {
      surfR <- xifti$surf$cortex_right
    }
  } else { 
    surfR <- make_surface(surfR) 
  }

  if (is.null(surfL) && is.null(surfR)) { stop("No valid surface data for either hemisphere.") }

  # Check hemisphere and view.
  if (is.null(hemisphere)) {
    hemisphere <- c("left", "right", "both")[1*(!is.null(surfL)) + 2*(!is.null(surfR))]
  }
  view <- match.arg(view, c("both", "lateral", "medial"))
  if (hemisphere=="both") { hemisphere=c("left", "right") } # reformat
  if (view=="both") { view=c("lateral", "medial") } # reformat
  brain_panels_nrow <- length(view)
  brain_panels_ncol <- length(hemisphere)

  if (is.null(title)) {
    no_title = FALSE
  } else {
    no_title = title == ""
  }
  all_panels_nrow <- brain_panels_nrow + 1*(!no_title) + 1*colorbar_embedded
  all_panels_ncol <- brain_panels_ncol
  
  # Check other arguments.
  mode <- match.arg(mode, c("widget", "image", "video"))
  if (mode == "video") { stop("The video mode is not yet supported.") }

  if (mode=="image") {
    if (!endsWith(fname, ".png")) { fname <- paste0(fname, ".png") }
    img_fname <- format_path(fname, write_dir, mode=2)
  }

  # Color mode
  if (is.null(color_mode)) {
    if (!is.null(xifti$meta$cifti$intent) && xifti$meta$cifti$intent==3007) {
      color_mode <- "qualitative"
    } else {
      color_mode <- "sequential"
    }
  } else {
    color_mode <- match.arg(color_mode, c("sequential", "qualitative", "diverging"))
  }

  # Check width and height.
  if (is.null(width) | is.null(height)) {
    DEF_ASPECT_PER_PANEL <- c(10, 7) # aspect ratio
    def_aspect <- DEF_ASPECT_PER_PANEL * c(brain_panels_ncol, brain_panels_nrow)
    DEF_MAX_SIZE <- c(1500, 700)

    if (is.null(width) & is.null(height)) {
      window_dims <- def_aspect*floor(min(DEF_MAX_SIZE/def_aspect))
    } else if (is.null(width)) {
      height <- as.integer(height)
      window_dims <- c(floor(height*def_aspect[1]/def_aspect[2]), height)
    } else if (is.null(height)) {
      width <- as.integer(width)
      window_dims <- c(width, floor(width*def_aspect[2]/def_aspect[1]))
    }
    brain_panels_width <- window_dims[1]
    brain_panels_height <- window_dims[2]
  } else {
    brain_panels_width <- as.integer(width)
    brain_panels_height <- as.integer(height)
  }

  indiv_panel_width <- brain_panels_width/brain_panels_ncol
  indiv_panel_height <- brain_panels_height/brain_panels_nrow

  TITLE_AND_LEGEND_HEIGHT_RATIO <- 1/6
  all_panels_width <- brain_panels_width
  all_panels_height <- brain_panels_height + 
    (indiv_panel_height * TITLE_AND_LEGEND_HEIGHT_RATIO) * 
      (all_panels_nrow - brain_panels_nrow)

  # ----------------------------------------------------------------------------
  # Get the data values and surface models, and construct the mesh. ------------
  # ----------------------------------------------------------------------------

  # Left cortex:
  if ("left" %in% hemisphere) {

    # Check for surface data.
    if (is.null(surfL)) { 
      stop(paste0("The left hemisphere was requested, but no surface data ",
        "(xifti$surf$cortex_left or the surfL argument to view_xifti) was provided."))
    }

    if (is.null(xifti$meta$cortex$medial_wall_mask$left)) {
      xifti$meta$cortex$medial_wall_mask$left <- rep(TRUE, nrow(surfL$vertices))
    }
    if (nrow(surfL$vertices) != length(xifti$meta$cortex$medial_wall_mask$left)) {
      stop(paste(
        "The left surface does not have the same number of vertices as the data",
        "(length of medial wall mask, or rows in data if medial wall mask is absent).",
        "Are they in the same resolution?"
      ))
      #surfL <- resample_surf(surfL, length(xifti$meta$cortex$medial_wall_mask$left), "left", sphereL_fname)
    }

    # Get data values.
    valuesL <- matrix(NA, ncol=length(idx), nrow=nrow(surfL$vertices))
    if (!is.null(xifti$data$cortex_left)) { 
      valuesL[xifti$meta$cortex$medial_wall_mask$left,] <- xifti$data$cortex_left[,idx, drop=FALSE]
    }
    nvoxL <- nrow(valuesL)

    ## Construct the mesh.
    mesh_left <- rgl::tmesh3d(t(cbind(surfL$vertices,
                                rep(1, nrow(surfL$vertices)))), # add homogenous coordinate
                              t(surfL$faces),
                              meshColor = "vertices")
    mesh_left <- rgl::addNormals(mesh_left) # for smooth coloring
  } 
  # Right cortex:
  if ("right" %in% hemisphere) {

    # Check for surface data.
    if (is.null(surfR)) { 
      stop(paste0("The right hemisphere was requested, but no surface data ",
        "(xifti$surf$cortex_right or the surfR argument to view_xifti) was provided."))
    }

    if (is.null(xifti$meta$cortex$medial_wall_mask$right)) {
      xifti$meta$cortex$medial_wall_mask$right <- rep(TRUE, nrow(surfR$vertices))
    }
    if (nrow(surfR$vertices) != length(xifti$meta$cortex$medial_wall_mask$right)) {
      stop(paste(
        "The right surface does not have the same number of vertices as the data",
        "(length of medial wall mask, or rows in data if medial wall mask is absent).",
        "Are they in the same resolution?"
      ))
    }

    # Get data values.
    valuesR <- matrix(NA, ncol=length(idx), nrow=nrow(surfR$vertices))
    if (!is.null(xifti$data$cortex_right)) { 
      valuesR[xifti$meta$cortex$medial_wall_mask$right,] <- xifti$data$cortex_right[,idx, drop=FALSE]
    }
    nvoxR <- nrow(valuesR)

    ## Construct the mesh.
    mesh_right <- rgl::tmesh3d(t(cbind(surfR$vertices,
                                 rep(1, nrow(surfR$vertices)))), # add homogenous coordinate
                               t(surfR$faces),
                               meshColor = "vertices")
    mesh_right <- rgl::addNormals(mesh_right) # for smooth coloring
  }

  # Put the values together.
  if ("left" %in% hemisphere){
    if ("right" %in% hemisphere) {
      values <- unlist(list(left=valuesL, right=valuesR)[hemisphere], use.names=FALSE)
    } else {
      values <- valuesL
    }
  } else if ("right" %in% hemisphere) {
    values <- valuesR
  } 
  if (all(is.na(values))) {
    values <- NULL
  }

  if (!is.null(values)) {

    # ----------------------------------------------------------------------------
    # Assign colors to vertices based on intensity. ------------------------------
    # ----------------------------------------------------------------------------

    # Get the base palette.
    if (color_mode=="qualitative") {
      if (!is.null(xifti$meta$cifti$intent) && xifti$meta$cifti$intent==3007) {
        labs <- xifti$meta$cifti$labels[[idx]]
        N_VALUES <- length(labs$Key)
        pal_base <- data.frame(
          color = rgb(labs$Red, labs$Green, labs$Blue, labs$Alpha),
          value = labs$Key
        )
      } else {
        values <- as.numeric(factor(values, levels=sort(unique(values))))
        N_VALUES <- length(unique(values))
        pal_base <- make_color_pal(
          colors=colors, color_mode=color_mode, zlim=zlim,
          DATA_MIN=1, DATA_MAX=N_VALUES
        )
      }
    } else {
      pal_base <- make_color_pal(
        colors=colors, color_mode=color_mode, zlim=zlim,
        DATA_MIN=min(values, na.rm=TRUE), DATA_MAX=max(values, na.rm=TRUE)
      )
    }
    # Interpolate colors in the base palette for higher color resolution.
    if (color_mode %in% c("sequential", "diverging")) {
      pal <- expand_color_pal(pal_base)
    } else {
      pal <- pal_base
    }
    # Map each vertex to a color by its value.
    cols <- use_color_pal(values, pal) # color_NA?
    if (length(hemisphere)==2) {
      cols_left <- cols[1:nvoxL]
      cols_right <- cols[(nvoxL+1):(nvoxL+nvoxR)]
    } else if (hemisphere=="left") {
      cols_left <- cols
    } else if (hemisphere=="right") {
      cols_right <- cols
    }
    rm(cols)

    # ----------------------------------------------------------------------------
    # Make the colorbar ----------------------------------------------------------
    # ----------------------------------------------------------------------------

    colorbar_min <- ifelse(
      color_mode=="diverging" & (identical(colors, "ROY_BIG_BL") | identical(colors, NULL)),
      pal_base$value[1] - diff(pal_base$value[c(1,nrow(pal_base))]) / (1-.005) * .005,
      pal_base$value[1])
    colorbar_breaks <- c(
      colorbar_min,
      pal$value[1:(length(pal$value)-1)] + diff(pal$value)/2,
      pal$value[length(pal$value)]
    )
    colorbar_labs <- switch(color_mode,
      sequential=c(colorbar_min, 
                  pal_base$value[nrow(pal_base)]),
      qualitative=1:N_VALUES,
      diverging=c(colorbar_min,
                  pal_base$value[as.integer(ceiling(nrow(pal_base)/2))],
                  pal_base$value[nrow(pal_base)])
    )
    # To-do: use colorbar_position argument.
    colorbar_kwargs <- list(
      legend.only = TRUE, zlim = range(pal$value), col = as.character(pal$color),
      breaks=colorbar_breaks, #legend.lab=colorbar_label,
      axis.args=list(cex.axis=1.7, at=colorbar_labs, 
                    col=text_color, col.ticks=text_color, col.axis=text_color,
                    labels=format(colorbar_labs, digits=colorbar_digits))
    )
    #if (colorbar_position=="right") {
    #  colorbar_kwargs <- c(
    #    colorbar_kwargs, 
    #    list(legend.cex=2, legend.shrink=.5, legend.width=2, legend.line=7, legend.mar=12)
    #  )
    #} else if (colorbar_position=="bottom") {
    colorbar_kwargs <- c(colorbar_kwargs, 
      list(
        horizontal=TRUE, # horizontal legend
        legend.cex=2, # double size of labels (numeric limits)
        #legend.shrink=.5, # half the width of the legend #override by smallplot
        #legend.width=1.67, # height of colorbar #override by smallplot
        legend.line=5, # height of lines between labels and colorbar
        #legend.mar=4, # legend margin #override by smallplot
        smallplot=c(.15, .5, .65, 1) # x1 x2 y1 y2
      )
    )

  } else {
    cols_left <- "white"
    cols_right <- "white"
  }

  # ----------------------------------------------------------------------------
  # Color and arrange the meshes according to the layout. ----------------------
  # ----------------------------------------------------------------------------

  # Open a new RGL window.
  rgl::open3d()
  if (is.null(bg)) { bg <- "white" }
  rgl::bg3d(color=bg)
  rgl::par3d(windowRect = c(20, 20, all_panels_width, all_panels_height))
  Sys.sleep(1) #https://stackoverflow.com/questions/58546011/how-to-draw-to-the-full-window-in-rgl
  
  all_panels_heights <- rep.int(1, brain_panels_nrow)
  if (!no_title) {all_panels_heights <- c(TITLE_AND_LEGEND_HEIGHT_RATIO, all_panels_heights) }
  if (colorbar_embedded) {all_panels_heights <- c(all_panels_heights, TITLE_AND_LEGEND_HEIGHT_RATIO) }

  # Determine the panel layout.
  rgl::layout3d(
    matrix(1:(all_panels_ncol*all_panels_nrow), nrow=all_panels_nrow, byrow=T),
    widths=rep.int(1, all_panels_ncol),
    heights=all_panels_heights,
    parent = NA, sharedMouse = TRUE
  )
  brain_panels <- as.character(t(outer(view, hemisphere, paste0))) # by row
  n_brain_panels <- length(brain_panels)

  if (!no_title) {
    if (is.null(title)) {
      intent <- xifti$meta$cifti$intent
      if (is.null(intent)) {
        title <- paste("Index", idx)
      } else if (intent == 3002) {
        title <- paste("Index", idx)
        if (!any(sapply(xifti$meta$cifti[c("time_start", "time_step", "time_unit")], is.null))) {
          title <- paste0(title, " (", xifti$meta$cifti$time_start+xifti$meta$cifti$time_step*idx, " ", xifti$meta$cifti$time_unit, "s)")
        }
      } else if (intent == 3006) {
        if (!is.null(xifti$meta$cifti$names) && length(xifti$meta$cifti$names)>=idx) {
          title <- xifti$meta$cifti$names[idx]
        } else {
          title <- ""
        }
      } else if (intent == 3007) {
        if (!is.null(xifti$meta$cifti$labels) && length(xifti$meta$cifti$labels)>=idx) {
          title <- names(xifti$meta$cifti$labels)[idx]
        } else {
          title <- ""
        }
      }
    }
    if (is.null(cex.title)) {
      # Default: 250% font size, but increasingly smaller for longer titles
      if (nchar(title) > 20) {
        cex.title <- 50 / nchar(title)
      } else {
        cex.title <- 2.5
      }
    }
    rgl::text3d(x=0, y=0, z=0, #These values don't seem to do anything...
                cex=cex.title, 
                adj=c(.5,.5), #replace with adj(c(0, .5)) when coords are moved
                font=2, # Forget if this made a difference...
                color=text_color,
                text=title
    )
    rgl::next3d(current = NA, clear = FALSE, reuse = FALSE)

    if(all_panels_ncol==2){
      rgl::next3d(current = NA, clear = FALSE, reuse = FALSE)
    }
  }

  # Rotation matrices to orient meshes.
  rot <- list(
    left = rbind( # Outer side of left surface toward viewer
      c( 0,-1, 0, 0),
      c( 0, 0, 1, 0),
      c(-1, 0, 0, 0),
      c( 0, 0, 0, 1)),
    right = rbind( # Outer side of right surface toward viewer
      c( 0, 1, 0, 0),
      c( 0, 0, 1, 0),
      c( 1, 0, 0, 0),
      c( 0, 0, 0, 1)),
    ID = diag(4)
  )

  # Populate the RGL window.
  for(ii in 1:n_brain_panels) {
    p <- brain_panels[ii]

    # Select the mesh for this panel, and determine the orientation.
    if (grepl("left", p)) {
      this_surf <- surfL
      rgl::shade3d(mesh_left, col=cols_left, specular="black", legend=TRUE)
      if (grepl("lateral", p)) {
        this_rot <- rot$left
      } else if (grepl("medial", p)) {
        this_rot <- rot$right
      } else { this_rot <- rot$ID }
    }
    if (grepl("right", p)) {
      this_surf <- surfR
      rgl::shade3d(mesh_right, col=cols_right, specular="black", legend=TRUE)
      if (grepl("lateral", p)) {
        this_rot <- rot$right
      } else if (grepl("medial", p)) {
        this_rot <- rot$left
      } else { this_rot <- rot$ID }
    }

    ## shift brains to left to make room for legend on right
    #displacement <- .25 * diff(range(this_surf$vertices[,2]))
    #if (grepl("lateral", p)) { displacement <- -displacement }
    #if (grepl("left", p)) { displacement <- -displacement }
    #this_trans <- t(rgl::translationMatrix(0, displacement, 0))
    this_trans <- diag(4)

    this_mat <- this_rot %*% this_trans
    rgl::rgl.viewpoint(userMatrix=this_mat, fov=0, zoom=zoom) #Default: 167% size
    rgl::next3d(current = NA, clear = FALSE, reuse = FALSE)
  }

  if (!is.null(values)) {
    if (colorbar_embedded) {
      rgl::bgplot3d(
        # Warning: calling par(new=TRUE) with no plot
        # Error in par(old.par) : 
        #   invalid value specified for graphical parameter "pin"
        try(suppressWarnings(do.call(fields::image.plot, colorbar_kwargs)), silent=TRUE),
        bg.color=bg
      )
    } else {
      colorbar_kwargs$smallplot=c(.15, .85, .45, .6) # x1 x2 y1 y2
      try(suppressWarnings(do.call(fields::image.plot, colorbar_kwargs)), silent=TRUE)
    }
  }

  rgl::next3d(current = NA, clear = FALSE, reuse = FALSE)
  if(all_panels_ncol==2){
    rgl::next3d(current = NA, clear = FALSE, reuse = FALSE)
  }

  if (mode=="image") {
    rgl::rgl.snapshot(img_fname)
    rgl::rgl.close()
  }

  invisible()
}

#' Visualize xifti brain data
#'
#' @inheritParams xifti_Param
#' @param structural_img The structural MRI image on which to overlay the
#'  subcortical values. Can be a file name, \code{"MNI"} (default) to use
#'  the MNI T1-weighted template, or \code{NULL} to use a blank image.
#' @param idx The time/column index of the xifti data to plot.
#' @param plane If use_papaya=FALSE, the plane to display. 
#'  Default: "axial". Other options are "sagittal" and "coronal".
#' @param num.slices If use_papaya=FALSE, the number of slices to display.
#' @param use_papaya use_papaya=TRUE will use papayar to allows for interactive visualization.
#' @param z_min Floor value.
#' @param z_max Ceiling value.
#' @inheritParams verbose_Param_TRUE
#'
#' @export
#' 
#' @importFrom oro.nifti overlay readNIfTI
view_xifti_volume <- function(
  xifti, structural_img="MNI", idx=1, plane="axial", 
  num.slices=12, use_papaya=FALSE, z_min=NULL, z_max=NULL,
  verbose=TRUE) {

  if (use_papaya) {
    if (!requireNamespace("papayar", quietly = TRUE)) {
      stop("Package \"papayar\" needed for this function to work. Please install it.",
           call. = FALSE)
    }
  }

  #stop("Does not work.")

  stopifnot(is.xifti(xifti))

  # Get volume and labels.
  values <- xifti$data$subcort[,idx]
  vol <- unmask(values, xifti$meta$subcort$mask, fill=NA)
  labs <- unmask(as.numeric(xifti$meta$subcort$labels), xifti$meta$subcort$mask, fill=0)

  # Pick slices with a lot of subcortical voxels.
  if (!use_papaya) {
    if (plane=="axial") mask_count <- apply(xifti$meta$subcort$mask, 3, sum)
    if (plane=="coronal") mask_count <- apply(xifti$meta$subcort$mask, 2, sum)
    if (plane=="sagittal") mask_count <- apply(xifti$meta$subcort$mask, 1, sum)

    slices <- which(mask_count > max(mask_count)/2)
    inds <- round(seq(1,length(slices), length.out=num.slices))
    slices <- slices[inds]
  }

  if (!is.null(z_min)) values[values < z_min] <- z_min
  if (!is.null(z_max)) values[values > z_max] <- z_max
  if (verbose) {
    cat(paste0(
      "Values to be plotted range from ",
      min(xifti$data$subcort[,idx])," to ",
      max(xifti$data$subcort[,idx]), ".\n"
    ))
  }

  if (!is.null(structural_img)) {
    if (structural_img=="MNI") {
      img <- readNIfTI(system.file("extdata/MNI152_T1_2mm.nii.gz", package="ciftiTools"), reorient=FALSE)
    } else if (is.fname(structural_img)){
      img <- readNIfTI(structural_img, reorient=FALSE)
    } else {
      stop("`structural_img` argument not one of: NULL, \"MNI\", or an existing file.")
    }
    img_overlay <- img*0
    img_overlay@.Data <- vol
    img_overlay@.Data[labs==0] <- NA

    img_labels <- img*0
    img_labels@.Data <- labs
    img_labels@.Data[labs==0] <- NA
  } else {
    img <- img_overlay <- vol
    #img_labels <- vol*0
    #img_labels@.Data <- labs
    #img_labels@.Data[labs==0] <- NA
  }

  if (!use_papaya) {
    oro.nifti::overlay(x=img, y=img_overlay, plane=plane, z=slices)
  } else {
    if (is.null(structural_img)) {
      stop("Doesn't work; use an overlay or not papaya.")
      papayar::papaya(list(img, img_labels))
    } else {
      papayar::papaya(list(img, img_overlay, img_labels))
    }
  }
}

#' Switch for \code{\link{view_xifti_surface}} or \code{\link{view_xifti_volume}}
#'
#' @inheritParams xifti_Param
#' @param what Either "surface" or "volume". If NULL (default), view the surface if present in the xifti file, and
#'  volume otherwise
#' @param ... Additional arguments to pass to either view function.
#'
#' @return The return value of \code{view_xifti_surface} or 
#'  \code{view_xifti_volume}.
#'
#' @export
#'
view_xifti <- function(xifti, what=NULL, ...) {
  stopifnot(is.xifti(xifti))
  if (is.null(what)) {
    can_do_left <- (!is.null(xifti$surf$cortex_left)) || ("surfL" %in% names(list(...)))
    can_do_right <- (!is.null(xifti$surf$cortex_right)) || ("surfR" %in% names(list(...)))
    can_do_sub <- !is.null(xifti$data$subcort)
    what <- ifelse(
      can_do_left || can_do_right, 
      "surface", 
      ifelse(can_do_sub, "volume", "error")
    )
  }
  if (what == "surface") { 
    return(view_xifti_surface(xifti, ...)) 
  } else if (what == "volume") { 
    return(view_xifti_volume(xifti, ...)) 
  } else {
    stop(paste(
      "No valid cortical surface, and no valid subcortical data.",
      "Did you forget to provide surfL/surfR?",
      "Or, did you forget to read in the subcortical data too?"
    ))
  }
}

#' S3 method: use view_xifti to plot a xifti
#'
#' @inheritParams x_Param_xifti
#' @param ... Additional arguments to \code{\link{view_xifti}}, except 
#'  \code{what}, which will be set to \code{NULL}.
#'
#' @method plot xifti 
#' @export
plot.xifti <- function(x, ...){
  view_xifti(x, what=NULL, ...)
}

#' @rdname view_xifti
#' @export
view_cifti <- viewCIfTI <- viewcii <- function(xifti, what=NULL, ...){
  view_xifti(xifti, what=what, ...)
}

#' @rdname view_xifti_surface
#' @export
view_cifti_surface <- viewCIfTI_surface <- viewcii_surface <- function(xifti, idx=1, 
  hemisphere=NULL, view=c("both", "lateral", "medial"),
  mode=c("widget", "image", "video"), width=NULL, height=NULL,
  bg=NULL, title=NULL, text_color="black",
  fname="xifti", write_dir=NULL,
  colors=NULL, color_mode=c("sequential", "qualitative", "diverging"), zlim=NULL,
  surfL=NULL, surfR=NULL,
  colorbar_embedded=TRUE, colorbar_digits=NULL){

  view_xifti_surface(
    xifti, idx, 
    hemisphere, view, 
    mode, width, height,
    bg, title, text_color,
    fname, write_dir,
    colors, color_mode, zlim,
    surfL, surfR,
    colorbar_embedded, 
    colorbar_digits
  )
}

#' @rdname view_xifti_volume
#' @export
view_cifti_volume <- viewCIfTI_volume <- viewcii_volume <- function(
  xifti, structural_img="MNI", idx=1, plane="axial", 
  num.slices=12, use_papaya=FALSE, z_min=NULL, z_max=NULL,
  verbose=TRUE) {

  view_xifti_volume(
    xifti=xifti, 
    structural_img=structural_img,
    idx=idx, plane=plane,
    num.slices=num.slices,
    use_papaya=use_papaya,
    z_min=z_min, z_max=z_max,
    verbose=verbose
  )
}