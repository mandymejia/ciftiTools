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
#' @param pos_half Use the positive half (black --> red --> yellow) only? Default is \code{FALSE}.
#'
#' @export
#' @return A data.frame with two columns: color (character: color hex codes) and value (numeric).
#'
ROY_BIG_BL <- function(min=0, max=1, mid=NULL, pos_half=FALSE){
  if(min==max){ stop("The minimum and maximum value should not be equal.") }
  rev_order <- min > max
  if(rev_order){
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

  if(pos_half){
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
    if(!is.null(mid)){
      stopifnot(mid > min & mid < max)
      old_mid <- (min + max)/2
      value[1:8] <- (value[1:8] - old_mid) / (max - old_mid) * (max - mid) + mid
      value[9] <- mid
      value[10:17] <- (value[10:17] - min) / (old_mid - min) * (mid - min) + min
    }
  }
  if(rev_order){ value <- value[length(value):1] }
  return(data.frame(color=color, value=value))
}

#' Control the mapping of values to colors with \code{colors}, \code{color_mode}, and \code{color_values}.
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
#'    If \code{identical(color_values, NULL)}, the colors will be mapped between \code{DATA_MIN} (black) to
#'    \code{DATA_MAX} (yellow). If \code{length(color_values)==2}, \code{color_values[1]} will be the lower bound (black)
#'    and \code{color_values[2]} will be the upper bound (yellow). If \code{color_values[1] > color_values[2]}, the
#'    first value will be used as the maximum and the second will be used as the minimum, and the color scale will be
#'    reversed with the highest value colored black and the lowest value colored yellow.
#'  }
#'  \item{\code{color_mode=="qualitative"}}{The "ROY_BIG_BL" pallete is not recommended for qualitative data, so a
#'    warning will be issued. Colors will be based on the landmark colors in the "ROY_BIG_BL" pallete. If
#'    \code{identical(color_values, NULL)}, the colors will be mapped onto each integer between \code{DATA_MIN} and
#'    \code{DATA_MAX}, inclusive. Color interpolation will be used if the number of colors in the palette (17) is less
#'    than this range. If \code{length(color_values)==length(colors)}, each color will be mapped to each corresponding value.
#'  }
#'  \item{\code{color_mode=="diverging"}}{If \code{identical(color_values, NULL)}, the colors will be mapped from
#'    \code{DATA_MIN} (aqua) to \code{DATA_MAX} (yellow). If \code{length(color_values)==1}, this value will be used as
#'    the midpoint (black) instead of the data midpoint. If \code{length(color_values)==2}, \code{color_values[1]} will
#'    be the lower bound (aqua) and \code{color_values[2]} will be the upper bound (yellow). If \code{length(color_values)==3},
#'    these values will correspond to the lowest bound (aqua), midpoint (black), and upper bound (yellow) respectively.
#'    If the \code{color_values} are in descending order, the first value will be used as the maximum and the last
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
#'  \item{\code{color_mode=="sequential"}}{If \code{identical(color_values, NULL)}, the colors will be mapped with equal
#'    spacing from \code{DATA_MIN} to \code{DATA_MAX}. If \code{length(color_values)==2}, these values will be used as
#'    the upper and lower bounds instead. If \code{color_values[1] > color_values[2]}, the first value will be used as
#'    the maximum and the second will be used as the minimum, and the color scale will be reversed. If
#'    \code{length(color_values)==length(colors)}, each color will be mapped to each corresponding value.
#'  }
#'  \item{\code{color_mode=="qualitative"}}{If \code{identical(color_values, NULL)}, the colors will be mapped onto
#'    each integer between \code{DATA_MIN} and \code{DATA_MAX}, inclusive. Color interpolation will be used if the
#'    number of colors in the palette is less than this range. If \code{length(color_values)==length(colors)}, each
#'    color will be mapped to each corresponding value.
#'  }
#'  \item{\code{color_mode=="diverging"}}{If \code{identical(color_values, NULL)}, the colors will be mapped with equal
#'    spacing from \code{DATA_MIN} to \code{DATA_MAX}. Thus, the middle color will correspond to the midpoint of the
#'    data. If \code{length(color_values)==1}, the middle color will correspond to this value instead. The preceeding
#'    colors will be equally-spaced between \code{DATA_MIN} and this value; the following colors will be equally-spaced
#'    between this value and\code{DATA_MAX}. If \code{length(color_values)==2}, \code{color_values[1]} will be the lower
#'    bound (first color) and \code{color_values[2]} will be the upper bound (last color). If \code{length(color_values)==3},
#'    these values will correspond to the lowest bound, midpoint, and upper bound respectively. There must be an odd
#'    number of colors, since the diverging color mode requires a midpoint. If the \code{color_values} are in descending
#'    order, the first value will be used as the maximum and the last will be used as the minimum, and the color scale
#'    will be reversed. Finally, if \code{length(color_values)==length(colors)}, each color will be mapped to each
#'    corresponding value. Thus, the middle color will correspond to the middle color_value. The length of \code{colors}
#'    must be odd and >= 3.
#'  }
#' }
#'
#' @param colors (Optional) "ROY_BIG_BL", the name of a ColorBrewer palette (see RColorBrewer::brewer.pal.info and
#'  colorbrewer2.org), or a character vector of colors. Defaults are \code{"ROY_BIG_BL"} (sequential color mode),
#'  \code{"Set2"} (qualitative color mode), and \code{"ROY_BIG_BL"} (diverging color mode). See the description for more
#'  details.
#' @param color_mode (Optional) \code{"sequential"}, \code{"qualitative"}, or \code{"diverging"}. Default is "sequential".
#'  See the description for more details.
#' @param color_values (Optional) Controls the mapping of values to each color in \code{colors}. If the length is longer
#'  than one, using -Inf will set the value to \code{DATA_MIN}, and Inf will set the value to \code{DATA_MAX}. See the
#'  description for more details.
#' @param DATA_MIN (Optional) The minimum value of the data to make the palette for. Overrided by certain \code{color_values}.
#' @param DATA_MAX (Optional) The maximum value of the data to make the palette for. Overrided by certain \code{color_values}.
#'
#' @export
#' @importFrom grDevices colorRampPalette
#' @import RColorBrewer
#'
#' @return A data.frame with two columns: color (character: standard color names or hex codes) and value (numeric).
#'
make_color_pal <- function(colors=NULL, color_mode=c("sequential", "qualitative", "diverging"), color_values=NULL,
                           DATA_MIN=0, DATA_MAX=1){
  color_mode <- match.arg(color_mode, c("sequential", "qualitative", "diverging"))

  if(DATA_MIN >= DATA_MAX){ stop("DATA_MAX must be greater than DATA_MIN") }

  # Use default palettes if the colors are not specified.
  if(identical(colors, NULL)){
    colors <- switch(color_mode,
                     sequential="ROY_BIG_BL", # will use pos half
                     qualitative="Set2",
                     diverging="ROY_BIG_BL")
  }

  N_COLORS_PRE <- length(colors)
  N_COLOR_VALUES_PRE <- length(color_values)
  if(N_COLORS_PRE == 1){
    # ROY_BIG_BL
    if(colors == "ROY_BIG_BL"){
      if(color_mode=="sequential"){
        if(identical(color_values, NULL)){
          RBB <- ROY_BIG_BL(DATA_MIN, DATA_MAX, pos_half=TRUE)
        } else if(N_COLOR_VALUES_PRE==2){
          RBB <- ROY_BIG_BL(color_values[1], color_values[2], pos_half=TRUE)
        } else {
          stop("The sequential ROY_BIG_BL palette (default) requires two (min and max values) or NULL/none color_values.")
        }

      } else if(color_mode=="qualitative"){
        warning("The ROY_BIG_BL palette is not recommended for qualitative data.")
        RBB <- ROY_BIG_BL(DATA_MIN, DATA_MAX)

      } else if(color_mode=="diverging"){
        if(identical(color_values, NULL)){
          RBB <- ROY_BIG_BL(DATA_MIN, DATA_MAX)
        } else if(N_COLOR_VALUES_PRE==1){
          RBB <- ROY_BIG_BL(DATA_MIN, DATA_MAX, mid=color_values)
        } else if(N_COLOR_VALUES_PRE==2){
          RBB <- ROY_BIG_BL(color_values[1], color_values[2],
                            mid=(color_values[1]+color_values[2])/2)
        } else if(N_COLOR_VALUES_PRE==3){
          RBB <- ROY_BIG_BL(color_values[1], color_values[3], mid=color_values[2])
        }
      }

      colors <- RBB$color
      if(color_mode != "qualitative"){ color_values <- RBB$value }

    # RColorBrewer
    } else if(colors %in% row.names(brewer.pal.info)){
      colors_info <- brewer.pal.info[row.names(brewer.pal.info) == colors,]
      brewer_mode <- match.arg(as.character(colors_info$category), c("sequential", "qualitative", "diverging"))
      if(brewer_mode != color_mode){
        warning(paste("The RColorBrewer palette type is", brewer_mode, "but the color_mode is", color_mode))
      }
      colors <- brewer.pal(as.numeric(colors_info$maxcolors), colors)

    } else {
      stop("The `colors` argument must be 'ROY_BIG_BL', an palette listed in `RColorBrewer::brewer.pal.info`, or
        a character vector of color hex codes or standard R names with a length greater than one.")
      }
  }

  N_COLORS <- length(colors)
  N_COLOR_VALUES <- length(color_values)
  if(!identical(color_values, NULL)){
    # Check that the color values are valid.
    valid_color_values_lengths <- switch(color_mode,
                                         sequential=c(2, N_COLORS),
                                         qualitative=N_COLORS,
                                         diverging=c(1, 2, 3, N_COLORS))
    if(!(N_COLOR_VALUES %in% valid_color_values_lengths)){
      stop(paste("There are", N_COLOR_VALUES, "color values--this is not compatible for the", color_mode, "color mode. See the description for details."))
    }

    # Order color values from lowest to highest, if not already sorted.
    color_values_order <- order(color_values)
    color_values <- color_values[color_values_order]
    # If the color values are descending, reverse the color scale.
    if(identical(color_values_order, length(color_values):1)){
      colors <- colors[length(colors):1]
    } else if(N_COLOR_VALUES==N_COLORS){
      colors <- colors[color_values_order]
    }

    # Replace infinite values with data bounds.
    if(identical(color_values[1], -Inf)){ color_values[1] <- DATA_MIN }
    if(identical(color_values[N_COLOR_VALUES], Inf)){ color_values[N_COLOR_VALUES] <- DATA_MAX }
  }

  # Sequential
  if(color_mode == "sequential"){
    pal_cols <- colors
    if(identical(color_values, NULL)){
      pal_vals <- seq(DATA_MIN, DATA_MAX, length.out=length(colors))
    } else {
      if(N_COLOR_VALUES==2){
        pal_vals <- seq(color_values[1], color_values[2], length.out=length(colors))
      } else if(N_COLOR_VALUES == N_COLORS) {
        pal_vals <- color_values
      } else {
        stop("The sequential color mode requires length(color_values) == 0 (NULL), 2, or length(colors).")
      }
    }

    #Qualitative
  } else if(color_mode=="qualitative"){
    if(!identical(c(DATA_MIN, DATA_MAX), round(c(DATA_MIN, DATA_MAX)))){
      stop("Data bounds must be integers for qualitative color mode.")
      }
    N_DATA_VALUES <- DATA_MAX - DATA_MIN + 1
    if(identical(color_values, NULL)){
      pal_vals <- c(DATA_MIN:DATA_MAX)
      if(length(colors) >= N_DATA_VALUES){
        pal_cols <- colors[1:N_DATA_VALUES]
      } else {
        # Might look weird for the ROY_BIG_BL pallete, but ROY_BIG_BL is not recommended anyway for qualitative data.
        pal_cols <- colorRampPalette(colors)(N_DATA_VALUES)
      }
    } else if(N_COLOR_VALUES==N_COLORS) {
      pal_vals <- color_values
      pal_cols <- colors
    } else {
      stop("The qualitative color mode requires length(color_values) == 0 (NULL) or length(colors).")
    }

    # Diverging
  } else if(color_mode=="diverging"){
    pal_cols <- colors
    if(identical(color_values, NULL)){
      pal_vals <- seq(DATA_MIN, DATA_MAX, length.out=length(colors))
    } else {
      if(N_COLOR_VALUES==N_COLORS){
        pal_vals <- color_values
      } else {
        # Get the minimum, middle, and maximum value for the color scale.
        if(N_COLOR_VALUES==1){
          mid_val <- color_values
          if((mid_val <= DATA_MIN) | (mid_val >= DATA_MAX)){
            stop("If one color_value is used with the diverging color_mode, it represents the midpoint of the data scale
              and must be between the data minimum and maximum. (It does not have to be the true midpoint.) Different
              bounds can be set with color_value=c(new_min, midpoint, new_max).")
          }
          min_val <- DATA_MIN
          max_val <- DATA_MAX
        } else if(N_COLOR_VALUES==2){
          mid_val <- (DATA_MIN + DATA_MAX)/2
          min_val <- color_values[1]
          max_val <- color_values[2]
        } else if(N_COLOR_VALUES==3){
          mid_val <- color_values[2]
          min_val <- color_values[1]
          max_val <- color_values[3]
        }

        # Interpolate between the min/mid/max to get the color values.
        # To-do: allow for odd-length color palettes?
        if(length(colors) %% 2 != 1){ stop("There must be an odd number of colors for the diverging color mode, to have a middle color.") }
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
expand_color_pal <- function(pal, MIN_COLOR_RES=255){
  if(nrow(pal) < MIN_COLOR_RES){
    colors <- as.character(pal$color)
    # Interpolate between palette values to obtain at least MIN_COLOR_RES color levels.
    color_res <- MIN_COLOR_RES * diff(pal$value)/(max(pal$value) - min(pal$value))
    color_res <- as.integer(round(pmax(color_res, 2)))
    vals <- vector(length=0, mode="numeric")
    cols <- vector(length=0, mode="character")
    for(i in 1:(nrow(pal)-1)){
      next_vals <- seq(pal$value[i], pal$value[i+1], length.out=color_res[i])
      next_vals <- next_vals[1:(length(next_vals)-1)]
      next_cols <- colorRampPalette(c(colors[i], colors[i+1]))(color_res[i])
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
  return(data.frame(color=cols, value=vals))
}

#' Applies a palette to a data vector to yield a vector of colors.
#'
#' @param data_values The values to map to colors
#' @param pal The palette to use to map values to colors
#' @param color_NA The color to use for NA values (default is "white").
#'
#' @return A character vector of color names
use_color_pal <- function(data_values, pal, color_NA="white"){
  NA_mask <- is.na(data_values)
  colors <- as.character(pal$color)
  pal$cut <- -Inf
  pal$cut[2:nrow(pal)] <- diff(pal$value)/2 + pal$value[1:(length(pal$value)-1)]
  out <- vector("character", length(data_values))
  out[!NA_mask] <- colors[apply(outer(as.numeric(data_values[!NA_mask]), pal$cut, '>='), 1, sum)]
  out[NA_mask] <- color_NA
  return(out)
}

#' Visualize cifti brain data. The \code{rgl} package is required.
#'
#' @param cifti Object of class "cifti". See \code{help(cifti_read_separate)}, \code{help(cifti_make)}, and \code{help(is.cifti)}.
#' @param idx The time/column index of the cifti data to plot. Currently one a single time point is supported. Default is the first index.
#' @param hemisphere Which brain cortex to display: "both", "left", or "right". If \code{NULL} (default), each available surface (e.g. if \code{surfL}
#'  or \code{cifti$SURF_LEFT} is not empty) will be displayed. Surfaces without data (e.g. \code{cifti$CORTEX_LEFT} is empty) will still be displayed,
#'  colored by \code{color_NA}. Each cortex will be shown in a separate panel row within the RGL window (exception: see \code{both_lateral_together}).
#' @param view Which view to display: "lateral", "medial", or "both". If \code{NULL} (default), both views will be shown. Each view
#'  will be shown in a separate panel column within the RGL window.
#' @param both_lateral_together If only the lateral views of both hemisphers are to be shown, the hemispheres can be viewed together spatially
#'  by setting this argument to \code{TRUE}. Otherwise, they are displayed in separate panels (default). This argument will not affect the layout in 
#'  other situations.
#' @param mode One of "widget" (Default), "image", or "video". "widget" will open an interactive RGL window. "image" will take a screenshot
#'  of the RGL window, save it to a file in \code{write_dir} namedial by \code{fname_prefix} and close it. "video" will take a series of screenshots
#'  of the RGL window, while rotating the brain surface(s), saving each to a file in \code{write_dir} namedial by \code{fname_prefix}, and then close 
#'  the RGL window. The frames can be converted to a video file using multimedia software such as Adobe Premiere Pro. Only the "widget" view mode is
#'  supported right now.
#' @param width,height The dimensions of the RGL window, in pixels. If both are \code{NULL} (default), it will use a 7x9 aspect ratio for each panel and
#'  the largest size that fits within a 1600x900 pixel area (a standard monitor size) . If one of these arguments is \code{NULL}, the other will be set 
#'  to make each panel have a 7x9 aspect ratio.
#' @param fname_prefix An identifier to use for naming the saved images ("prefix.png") and video frames ("prefix_1.png", "prefix_2.png", ...).
#'  Default is "cifti". Note: only the "widget" view mode is supported right now.
#' @param write_dir Where to save image or video files. If NULL (default), uses the current working directory. Note: only the "widget" view mode is 
#'  supported right now.
#' @param colors (Optional) "ROY_BIG_BL", vector of colors to use, OR the name of a ColorBrewer palette (see RColorBrewer::brewer.pal.info 
#'  and colorbrewer2.org). Defaults are \code{"ROY_BIG_BL"} (sequential), \code{"Set2"} (qualitative), and \code{"ROY_BIG_BL"} (diverging).
#'  See the \code{ciftiTools::make_color_pal()} description for more details.
#' @param color_mode (Optional) \code{"sequential"}, \code{"qualitative"}, or \code{"diverging"}. Default is sequential. See the
#'  \code{ciftiTools::make_color_pal()} description for more details.
#' @param color_values (Optional) Controls the mapping of values to each color in \code{colors}. If the length is longer than
#'  one, using -Inf will set the value to \code{DATA_MIN}, and Inf will set the value to \code{DATA_MAX}. See the
#'  \code{ciftiTools::make_color_pal()} description for more details.
#' @param color_NA The color for NA values. Default is "white".
#' @param surfL,surfR (Optional if \code{cifti$SURF_LEFT} and \code{cifti$SURF_RIGHT} are not empty) The brain surface model to use. Each can be a file path
#'  for a GIfTI, a file read by gifti::readGIfTI, or an object of class "cifti_surface". If provided, they will override \code{cifti$SURF_LEFT} and 
#'  \code{cifti$SURF_RIGHT} if they exist. Otherwise, leave these arguments as \code{NULL} (default) to use \code{cifti$SURF_LEFT} and \code{cifti$SURF_RIGHT}.
#' @param colorbar_position "bottom" (default), "right", or "separate" from the RGL window.
#' @param colorbar_label A title for the colorbar (none by default).
#'
#' @export
#' @importFrom fields image.plot
cifti_view_surface <- function(cifti, idx=1, 
  hemisphere=NULL, view=c("both", "lateral", "medial"), both_lateral_together=FALSE,
  mode=c("widget", "image", "video"), width=NULL, height=NULL,
  fname_prefix="cifti", write_dir=NULL,
  colors=NULL, color_mode=c("sequential", "qualitative", "diverging"), color_values=NULL, color_NA="white",
  surfL=NULL, surfR=NULL,
  colorbar_position=c("bottom", "right", "separate"), colorbar_label=""){

  if (!requireNamespace("rgl", quietly = TRUE)) {
    stop("Package \"rgl\" needed to use `cifti_view_surface`. Please install it.", call. = FALSE)
  }
  
  # Check that the arguments are valid.
  if(!is.cifti(cifti)) stop("cifti argument is not a valid cifti object. See is.cifti().")
  #if(length(idx) > 1) stop("Only one time/column index is supported right now.")
  # surfaces.
  if(is.null(surfL)){
    if(is.null(cifti$SURF_LEFT$surface1)){
      stop("The left hemisphere was requested, but no surface data was provided (cifti$SURF_LEFT or the surfL argument).")
    } else {
      surfL <- cifti$SURF_LEFT$surface1 # to-edit
    }
  } else { surfL <- make_cifti_surface(surfL) }
  if(is.null(surfR)){
    if(is.null(cifti$SURF_RIGHT$surface1)){
      stop("The right hemisphere was requested, but no surface data was provided (cifti$SURF_RIGHT or the surfR argument).")
    } else {
      surfR <- cifti$SURF_RIGHT$surface1 # to-edit
    }
  } else { surfR <- make_cifti_surface(surfR) }
  # hemisphere and view.
  if(is.null(hemisphere)){
    hemisphere <- c("left", "right", "both")[1*(!is.null(surfL)) + 2*(!is.null(surfR))]
  }
  view <- match.arg(view, c("both", "lateral", "medial"))
  if(hemisphere=="both"){ hemisphere=c("left", "right") } # reformat
  if(view=="both"){ view=c("lateral", "medial") } # reformat
  # both_lateral_together
  if(length(hemisphere)==2 & identical(view, "lateral") & both_lateral_together){
    stop("both_lateral_together==TRUE is not supported yet.")
  } else {
    panel_nrow <- length(view)
    panel_ncol <- length(hemisphere)
  }
  # others...
  mode <- match.arg(mode, c("widget", "image", "video"))
  if(mode != "widget"){ stop("Only the widget view mode is supported right now.") }
  #if(mode != "widget"){ write_dir <- check_dir(write_dir) } # check_dir will be added in 1.1
  if(is.null(width) | is.null(height)){
    DEF_ASPECT_PER_PANEL <- c(9, 7) # 7:9 aspect ratio
    def_aspect <- DEF_ASPECT_PER_PANEL * c(panel_ncol, panel_nrow)
    DEF_MAX_SIZE <- c(1600, 900)

    if(is.null(width) & is.null(height)){
      window_dims <- def_aspect*floor(min(DEF_MAX_SIZE/def_aspect))
    } else if(is.null(width)){
      height <- as.integer(height)
      window_dims <- c(floor(height*def_aspect[1]/def_aspect[2]), height)
    } else if(is.null(height)){
      width <- as.integer(width)
      window_dims <- c(width, floor(width*def_aspect[2]/def_aspect[1]))
    }
    width <- window_dims[1]; height <- window_dims[2]
  } else {
    width <- as.integer(width); height <- as.integer(height)
  }
  color_mode <- match.arg(color_mode, c("sequential", "qualitative", "diverging"))
  colorbar_position <- match.arg(colorbar_position, c("bottom", "right", "separate"))

  #################################################################
  # Get the data values and surface models, and construct the mesh.
  #################################################################

  EPS <- 1e-8 # TODO : ciftiTools option?
  valuesL <- valuesR <- NULL
  # Left cortex:
  if("left" %in% hemisphere){

    if(is.null(surfL)){ 
      stop(paste0("The left hemisphere was requested, but no surface data ",
        "(cifti$SURF_LEFT or the surfL argument to cifti_view) was provided."))
    }

    if(is.null(cifti$CORTEX_LEFT)){ valuesL <- matrix(NA, ncol=length(idx), nrow=nrow(surfL$vertices)) }
    else if(!all(idx %in% 1:ncol(cifti$CORTEX_LEFT))){ valuesL <- matrix(NA, ncol=length(idx), nrow=nrow(surfL$vertices)) }
    else {
      # Mask out (near-)constant voxels.
      NA_mask_left <- apply(abs(cifti$CORTEX_LEFT), 1, sum) < EPS
      cifti$CORTEX_LEFT[NA_mask_left,] <- NA
      valuesL <- matrix(cifti$CORTEX_LEFT[,idx], ncol=length(idx))
      #valuesL <- apply(matrix(cifti$CORTEX_LEFT[,idx][surfL$surface$faces], ncol=3), 1, mean, na.rm=TRUE) # FACES
      cifti$CORTEX_LEFT <- NULL #save memory
    }

    ## Construct the mesh.
    mesh_left <- rgl::tmesh3d(t(cbind(surfL$vertices,
                                rep(1, nrow(surfL$vertices)))), # add homogenous coordinate
                              t(surfL$faces),
                              meshColor = "vertices")
    mesh_left <- rgl::addNormals(mesh_left) # for smooth coloring
  }
  # Right cortex:
  if("right" %in% hemisphere){

    if(is.null(surfR)){ 
      stop(paste0("The right hemisphere was requested, but no surface data ",
        "(cifti$SURF_RIGHT or the surfR argument to cifti_view) was provided."))
    }

      if(is.null(cifti$CORTEX_RIGHT)){ valuesR <- matrix(NA, ncol=length(idx), nrow=nrow(surfR$vertices)) }
      else if(!all(idx %in% 1:ncol(cifti$CORTEX_RIGHT))){ valuesR <- matrix(NA, ncol=length(idx), nrow=nrow(surfR$vertices)) }
      else {
      # Mask out (near-)constant voxels.
      NA_mask_right <- apply(abs(cifti$CORTEX_RIGHT), 1, sum) < EPS
      cifti$CORTEX_RIGHT[NA_mask_right,] <- NA
      valuesR <- matrix(cifti$CORTEX_RIGHT[,idx], ncol=length(idx))
      #valuesR <- apply(matrix(cifti$CORTEX_RIGHT[,idx][surfR$surface$faces], ncol=3), 1, mean, na.rm=TRUE) # FACES
      cifti$CORTEX_RIGHT <- NULL #save memory
    }

    ## Construct the mesh.
    mesh_right <- rgl::tmesh3d(t(cbind(surfR$vertices,
                                 rep(1, nrow(surfR$vertices)))), # add homogenous coordinate
                               t(surfR$faces),
                               meshColor = "vertices")
    mesh_right <- rgl::addNormals(mesh_right) # for smooth coloring
  }

  # Put the values together.
  nvoxL <- nrow(valuesL)
  nvoxR <- nrow(valuesR)
  values <- unlist(list(left=valuesL, right=valuesR)[hemisphere], use.names=FALSE)

  ###############################################
  # Assign colors to vertices based on intensity.
  ###############################################

  # Get the base palette.
  if(color_mode=="qualitative"){
    print("Warning: qualitative values must be integers 1:N_VALUES. Will be fixed in future.") # will fix in future.
    N_VALUES <- length(unique(values))
    if(N_VALUES > 30){stop("Too many qualitative values.")} #fix
    pal_base <- make_color_pal(colors=colors, color_mode=color_mode, color_values=color_values,
      DATA_MIN=1, DATA_MAX=N_VALUES)
  } else {
    pal_base <- make_color_pal(colors=colors, color_mode=color_mode, color_values=color_values,
      DATA_MIN=min(values, na.rm=TRUE), DATA_MAX=max(values, na.rm=TRUE))
  }
  # Interpolate colors in the base palette for higher color resolution.
  if(color_mode %in% c("sequential", "diverging")){
    pal <- expand_color_pal(pal_base)
  } else {
    pal <- pal_base
  }
  # Map each vertex to a color by its value.
  #cols <- apply(values, 2, use_color_pal, pal) # ???
  cols <- use_color_pal(values, pal, color_NA=color_NA)
  if(length(hemisphere)==2) {
    cols_left <- cols[1:nvoxL]
    cols_right <- cols[(nvoxL+1):(nvoxL+nvoxR)]
  } else if(hemisphere=="left"){
    cols_left <- cols
  } else if(hemisphere=="right"){
    cols_right <- cols
  }
  rm(cols)

  ###################
  # Make the colorbar
  ###################

  colbar_min <- ifelse(
    color_mode=="diverging" & (identical(colors, "ROY_BIG_BL") | identical(colors, NULL)),
    pal_base$value[1] - diff(pal_base$value[c(1,nrow(pal_base))]) / (1-.005) * .005,
    pal_base$value[1])
  colbar_breaks <- c(
    colbar_min,
    pal$value[1:(length(pal$value)-1)] + diff(pal$value)/2,
    pal$value[length(pal$value)]
  )
  colbar_labs <- switch(color_mode,
    sequential=c(colbar_min, 
                 pal_base$value[nrow(pal_base)]),
    qualitative=1:N_VALUES,
    diverging=c(colbar_min,
                pal_base$value[as.integer(ceiling(nrow(pal_base)/2))],
                pal_base$value[nrow(pal_base)])
  )
  # To-do: use colorbar_position argument.
  colorbar_kwargs = list(
    legend.only = TRUE, zlim = range(pal$value), col = as.character(pal$color),
    breaks=colbar_breaks, legend.lab=colorbar_label,
    axis.args=list(cex.axis=1.7, at=colbar_labs, labels=format(colbar_labs))
  )
  if(colorbar_position=="right"){
    colorbar_kwargs <- c(
      colorbar_kwargs, 
      list(legend.cex=2, legend.shrink=.5, legend.width=2, legend.line=7, legend.mar=12)
    )
  } else if(colorbar_position=="bottom"){
    colorbar_kwargs <- c(
      colorbar_kwargs, 
      list(horizontal=TRUE, legend.cex=2, legend.shrink=.5, legend.width=2, legend.line=7, legend.mar=4)
    )
  }


  ############################################################
  # Color and arrange the meshes according to the layout.
  ############################################################

  # Open a new RGL window.
  rgl::open3d()
  rgl::par3d(windowRect = c(20, 20, width, height))
  
  # Determine the panel layout.
  rgl::mfrow3d(panel_nrow, panel_ncol, byrow = TRUE, parent = NA, sharedMouse = TRUE)
  panels <- as.character(t(outer(hemisphere, view, paste0))) # by row
  n_panels <- length(panels)
  legend_panel <- switch(colorbar_position,
    bottom = ((panel_nrow-1) * panel_ncol) + 1,
    right = n_panels,
    separate = 0
  )

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
  for(i in 1:n_panels){
    p <- panels[i]

    # Select the mesh for this panel, and orient it.
    if(grepl("left", p)){
      rgl::shade3d(mesh_left, col=cols_left, specular="black", legend=TRUE)
      if(grepl("lateral", p)){
        this_rot <- rot$left
      } else if(grepl("medial", p)){
        this_rot <- rot$right
      } else { this_rot <- rot$ID }
    }
    if(grepl("right", p)){
      rgl::shade3d(mesh_right, col=cols_right, specular="black", legend=TRUE)
      if(grepl("lateral", p)){
        this_rot <- rot$right
      } else if(grepl("medial", p)){
        this_rot <- rot$left
      } else { this_rot <- rot$ID }
    } 
    if(colorbar_position=="right"){
      if("right" %in% hemisphere){
        surf_w_colorbar <- surfR
      } else {
        surf_w_colorbar <- surfL
      }
      displacement <- .25 * diff(range(surf_w_colorbar$vertices[,2]))
      if(grepl("lateral", p)){ displacement <- -displacement }
      if(grepl("left", p)){ displacement <- -displacement }
      this_trans <- t(rgl::translationMatrix(0, displacement, 0))
      # to-do: also displace meshes upward if colorbar_position=="bottom"
    } else {
      this_trans <- diag(4)
    }
    this_mat <- this_rot %*% this_trans
    rgl::rgl.viewpoint(userMatrix=this_mat)

    # Suppress this warning: "calling par(new=TRUE) with no plot"
    if(i == legend_panel){ 
      rgl::bgplot3d(suppressWarnings(do.call(fields::image.plot, colorbar_kwargs)))
    }

    rgl::next3d(current = NA, clear = FALSE, reuse = FALSE)
  }

  if(colorbar_position=="separate"){
    # Suppress this warning: "calling par(new=TRUE) with no plot"
    suppressWarnings(do.call(fields::image.plot, colorbar_kwargs))
  }

  return(invisible())
}

#' Visualize cifti brain data
#'
#' @param cifti Object of class "cifti". See \code{help(cifti_read_separate)}, \code{help(cifti_make)}, and \code{help(is.cifti)}.
#' @param structural_img The file name of the structural MRI image on which to overlay the subcortical values.  The MNI template is used by default.  Set to NULL to use a blank image.
#' @param idx The time/column index of the cifti data to plot.
#' @param plane If use_papaya=FALSE, the plane to display. Default is "axial". Other options are "sagittal" and "coronal".
#' @param num.slices If use_papaya=FALSE, the number of slices to display.
#' @param use_papaya use_papaya=TRUE will use papayar to allows for interactive visualization.
#' @param z_min Floor value.
#' @param z_max Ceiling value.
#'
#' @export
#' 
#' @importFrom oro.nifti overlay readNIfTI
cifti_view_volume <- function(cifti, structural_img="MNI", idx=1, plane="axial", num.slices=12, use_papaya=FALSE, z_min=NULL, z_max=NULL){
  if(use_papaya) {
    if (!requireNamespace("papayar", quietly = TRUE)) {
      stop("Package \"papayar\" needed for this function to work. Please install it.",
           call. = FALSE)
    }
  }

  # Pick slices with a lot of subcortical voxels.
  if(!use_papaya){
    labs <- cifti$LABELS
    mask <- (labs > 0)
    if(plane=="axial") mask_count <- apply(mask, 3, sum)
    if(plane=="coronal") mask_count <- apply(mask, 2, sum)
    if(plane=="sagittal") mask_count <- apply(mask, 1, sum)

    slices <- which(mask_count > max(mask_count)/2)
    inds <- round(seq(1,length(slices), length.out=num.slices))
    slices <- slices[inds]
  }

  if(is.null(structural_img)) {
    T1w <- NULL
  } else if(structural_img=="T1w") {
    T1w <- readNIfTI(system.file("extdata/MNI152_T1_2mm.nii.gz", package="ciftiTools"), reorient=FALSE)
  } else {
    T1w <- readNIfTI(structural_img, reorient=FALSE)
  }

  values <- cifti$VOL[,,,idx]
  if(!is.null(z_min)) values[values < z_min] <- z_min
  if(!is.null(z_max)) values[values > z_max] <- z_max
  print(paste0("Values to be plotted range from ",min(values[cifti$LABELS > 0])," to ",max(values[cifti$LABELS > 0])))

  img_overlay <- T1w*0
  img_overlay@.Data <- values
  img_overlay@.Data[cifti$LABELS==0] <- NA

  img_labels <- T1w*0
  img_labels@.Data <- cifti$LABELS
  img_labels@.Data[cifti$LABELS==0] <- NA

  if(use_papaya==FALSE) oro.nifti::overlay(x=T1w, y=img_overlay, plot.type="single", plane=plane, z=slices)#, col.y=pal)
  if(use_papaya==TRUE) papayar::papaya(list(T1w, img_overlay, img_labels))
}

#' Switch for cifti_view_surface or cifti_view_volume
#'
#' @param cifti Object of class "cifti". See \code{help(cifti_read_separate)}, \code{help(cifti_make)}, and \code{help(is.cifti)}.
#' @param surface_or_volume Either "surface" or "volume". If NULL (default), view the surface if present in the cifti file, and
#'  volume otherwise
#' @param ... Additional arguments to pass to either view function.
#'
#' @export
#'
cifti_view <- function(cifti, surface_or_volume=NULL, ...){
  if(is.null(surface_or_volume)){
    can_do_left <- (!is.null(cifti$CORTEX_LEFT)) & (!is.null(cifti$SURF_LEFT))
    can_do_right <- (!is.null(cifti$CORTEX_RIGHT)) & (!is.null(cifti$SURF_RIGHT))
    surface_or_volume <- ifelse(can_do_left | can_do_right, "surface", "volume")
  }
  hemispheres=c("left", "right", "both")
  if(surface_or_volume == "surface"){ 
    layout=c("left_2", "right_2", "both_4")[can_do_left + can_do_right*2]
    return(cifti_view_surface(cifti, ...)) 
  }
  else if(surface_or_volume == "volume"){ return(cifti_view_volume(cifti, ...)) }
  else{ stop() }
}
