#' "ROY_BIG_BL" color palette
#'
#' "ROY_BIG_BL", the default palette from the Connectome Workbench.
#'
#' Yields the landmark color hex codes and values for the "ROY_BIG_BL" palette. 
#'  This is the same color palette as the default Connectime Workbench palette. 
#'  Source: github.com/Washington-University/workbench/blob/master/src/Files/PaletteFile.cxx
#'
#' @param min The minimum value for the color mapping. As in the original 
#'  palette, the last color (aqua) is actually placed at the bottom .5\% between
#'  the minimum and maximum. Default: \code{0}
#' @param max The maximum value for the color mapping. If this value is lower 
#'  than the minimum, the color mapping will be reversed. If this is equal to 
#'  the minimum, a palette with only the color black will be returned.
#'  Default: \code{1}.
#' @param mid (Optional) The midpoint value for the color mapping. If 
#'  \code{NULL} (default), the true midpoint is used.
#' @param pos_half Use the positive half (black --> red --> yellow) only? 
#'  Default: \code{FALSE}.
#'
#' @return A data.frame with two columns: \code{"color"} (character: color hex 
#'  codes) and \code{"value"} (numeric)
#' 
#' @importFrom grDevices col2rgb
#'
#' @export 
#' 
ROY_BIG_BL <- function(min=0, max=1, mid=NULL, pos_half=FALSE) {
  if (min==max) {
    return( data.frame(color = c("#000000"), value = c(min)) )
  }
  rev_order <- min > max
  if (rev_order) {
    temp <- min
    min <- max
    max <- temp
  }

  # Use the same landmark color RGB values, and same spacing. Note the spacing
  #   is not equidistant between landmarks.
  # Added #fff400 at 0.975 to account for the repetition of #00ffff .
  #   #000000 should be in middle, and the number of colors should be odd.
  #   The RGB value is 20% toward #ffc800, and .975 is 20% toward .875, so there
  #     is no visual difference.
  color <- c(
    "#ffff00", "#fff400", "#ffc800",
    "#ff7800", "#ff0000", "#c80000", 
    "#960000", "#640000", "#3c0000", 
    "#000000", 
    "#000050", "#0000aa", "#4b007d",
    "#7d00a0", "#4b7d00", "#00c800", 
    "#00ff00", "#00ffff", "#00ffff"
  )
  value <- c(
    1.000,  0.975,  0.875, 
    0.750,  0.625,  0.500, 
    0.375,  0.250,  0.125, 
    0.000, 
    -0.125, -0.250, -0.375,
    -0.500, -0.625, -0.750, 
    -0.875, -0.990, -1.000
  )

  if (pos_half) {
    # Only use the latter half.
    value <- value[1:10]
    color <- color[1:10]
    # Normalize the values to [min, max].
    value <- value * (max - min) + min

  } else {
    # Normalize the values to [min, max]. 
    # Note that the bottom 0.5% are all #00ffff.
    value <- (value + 1)/2
    value <- value * (max - min) + min

    # Normalize middle value (black) to mid, if specified.
    if (!is.null(mid)) {
      stopifnot(mid > min & mid < max)
      old_mid <- (min + max)/2
      value[1:9] <- (value[1:9] - old_mid) / (max - old_mid) * (max - mid) + mid
      value[10] <- mid
      value[11:19] <- (value[11:19] - min) / (old_mid - min) * (mid - min) + min
    }
  }
  if (rev_order) { value <- value[length(value):1] }
  data.frame(color=color, value=value)
}

#' Make a color palette.
#' 
#' Control the mapping of values to colors with \code{colors}, 
#'  \code{color_mode}, and \code{zlim}.
#'
#' There are three argument types for \code{colors}: \code{"ROY_BIG_BL"}, the 
#'  name of an \code{RColorBrewer} palette, or a character vector of color 
#'  names.
#'
#' If \code{colors=="ROY_BIG_BL"}, the "ROY_BIG_BL" pallete will be used. It is
#'  the same palette as the default used in the Connectome Workbench application
#'  (see github.com/Washington-University/workbench/blob/master/src/Files/PaletteFile.cxx).
#'  The midpoint will be olored black. From the midpoint toward the upper 
#'  bound, colors will proceed from black to red to yellow. From the midpoint 
#'  toward the lower bound, colors will proceed from black to blue to purple to 
#'  green to aqua. Note that these colors are not equally-spaced, and the bottom
#'  0.5\% of the color range has the same color. Here is how each color mode 
#'  behaves if \code{colors=="ROY_BIG_BL"}:
#'
#' \describe{
#'  \item{\code{color_mode=="sequential"}}{Only the second half of the pallete 
#'    will be used (black --> red --> yellow). If \code{identical(zlim, NULL)}, 
#'    the colors will be mapped between \code{DATA_MIN} (black) to 
#'    \code{DATA_MAX} (yellow). If \code{length(zlim)==2}, \code{zlim[1]} will 
#'    be the lower bound (black) and \code{zlim[2]} will be the upper bound 
#'    (yellow). If \code{zlim[1] > zlim[2]}, the first value will be used as the
#'    maximum and the second will be used as the minimum, and the color scale 
#'    will be reversed with the highest value colored black and the lowest value
#'    colored yellow.
#'  }
#'  \item{\code{color_mode=="qualitative"}}{The "ROY_BIG_BL" pallete is not 
#'    recommended for qualitative data, so a warning will be issued. Colors will 
#'    be based on the landmark colors in the "ROY_BIG_BL" pallete. If 
#'    \code{identical(zlim, NULL)}, the colors will be mapped onto each integer 
#'    between \code{DATA_MIN} and \code{DATA_MAX}, inclusive. Color 
#'    interpolation will be used if the number of colors in the palette (17) is
#'    less than this range. If \code{length(zlim)==length(colors)}, each color 
#'    will be mapped to each corresponding value.
#'  }
#'  \item{\code{color_mode=="diverging"}}{If \code{identical(zlim, NULL)}, the 
#'    colors will be mapped from \code{DATA_MIN} (aqua) to \code{DATA_MAX} 
#'    (yellow). If \code{length(zlim)==1}, this value will be used as the 
#'    midpoint (black) instead of the data midpoint. If \code{length(zlim)==2}, 
#'    \code{zlim[1]} will be the lower bound (aqua) and \code{zlim[2]} will be 
#'    the upper bound (yellow). If \code{length(zlim)==3}, these values will 
#'    correspond to the lowest bound (aqua), midpoint (black), and upper bound 
#'    (yellow) respectively. If the \code{zlim} are in descending order, the 
#'    first value will be used as the maximum and the last will be used as the 
#'    minimum, and the color scale will be reversed with the highest values 
#'    colored aqua and the lowest values colored yellow.
#'  }
#' }
#'
#' If \code{colors} is the name of an RColorBrewer palette (see 
#'  \code{\link[RColorBrewer]{brewer.pal.info}}), the colors in that pallete will be used, 
#'  and the following behavior applies. If \code{colors} is a character vector
#'  of color names (hex codes or standard R color names), the below behavior 
#'  applies directly:
#'
#' \describe{
#'  \item{\code{color_mode=="sequential"}}{If \code{identical(zlim, NULL)}, the
#'    colors will be mapped with equal spacing from \code{DATA_MIN} to 
#'    \code{DATA_MAX}. If \code{length(zlim)==2}, these values will be used as
#'    the upper and lower bounds instead. If \code{zlim[1] > zlim[2]}, the first
#'    value will be used as the maximum and the second will be used as the 
#'    minimum, and the color scale will be reversed. If 
#'    \code{length(zlim)==length(colors)}, each color will be mapped to each 
#'    corresponding value.
#'  }
#'  \item{\code{color_mode=="qualitative"}}{If \code{identical(zlim, NULL)}, the
#'    colors will be mapped onto each integer between \code{DATA_MIN} and 
#'    \code{DATA_MAX}, inclusive. Color interpolation will be used if the number 
#'    of colors in the palette is less than this range. If 
#'    \code{length(zlim)==length(colors)}, each color will be mapped to each 
#'     corresponding value.
#'  }
#'  \item{\code{color_mode=="diverging"}}{If \code{identical(zlim, NULL)}, the 
#'    colors will be mapped with equal spacing from \code{DATA_MIN} to 
#'    \code{DATA_MAX}. Thus, the middle color will correspond to the midpoint of
#'    the data. If \code{length(zlim)==1}, the middle color will correspond to 
#'    this value instead. The preceeding colors will be equally-spaced between
#'    \code{DATA_MIN} and this value; the following colors will be 
#'    equally-spaced between this value and\code{DATA_MAX}. If 
#'    \code{length(zlim)==2}, \code{zlim[1]} will be the lower bound (first 
#'    color) and \code{zlim[2]} will be the upper bound (last color). If 
#'    \code{length(zlim)==3}, these values will correspond to the lowest bound, 
#'    midpoint, and upper bound respectively. There must be an odd number of 
#'    colors, since the diverging color mode requires a midpoint. If the 
#'    \code{zlim} are in descending order, the first value will be used as the
#'    maximum and the last will be used as the minimum, and the color scale
#'    will be reversed. Finally, if \code{length(zlim)==length(colors)}, each 
#'    color will be mapped to each corresponding value. Thus, the middle color 
#'    will correspond to the middle color_value. The length of \code{colors}
#'    must be odd and >= 3.
#'  }
#' }
#'
#' @param colors (Optional) "ROY_BIG_BL", the name of a ColorBrewer palette 
#'  (see \code{\link[RColorBrewer]{brewer.pal.info}} and colorbrewer2.org), or a character 
#'  vector of colors. \code{NULL} (default) will use \code{"ROY_BIG_BL"} 
#'  if \code{color_mode} is \code{"sequential"} or \code{"diverging"}, and 
#'  \code{"Set2"} if \code{color_mode} is \code{"qualitative"}. See the 
#'  description for more details.
#' @param color_mode (Optional) \code{"sequential"}, \code{"qualitative"}, or 
#'  \code{"diverging"}. Default: \code{"sequential"}. See the description for 
#'  more details.
#' @param zlim (Optional) Controls the mapping of values to each color in 
#'  \code{colors}. If the length is longer than one, using \code{-Inf} will set 
#'  the value to \code{DATA_MIN}, and \code{Inf} will set the value to 
#'  \code{DATA_MAX}. See the description for more details.
#' @param DATA_MIN (Optional) The minimum value of the data to make the palette
#'  for. Overrided by certain \code{zlim}.
#' @param DATA_MAX (Optional) The maximum value of the data to make the palette
#'  for. Overrided by certain \code{zlim}.
#'
#' @return A data.frame with two columns: \code{"color"} (character: color hex 
#'  codes) and \code{"value"} (numeric)
#' 
#' @importFrom grDevices colorRampPalette
#' @import RColorBrewer
#'
#' @export
#' 
make_color_pal <- function(
  colors=NULL, color_mode=c("sequential", "qualitative", "diverging"), zlim=NULL,
  DATA_MIN=0, DATA_MAX=1) {

  # ----------------------------------------------------------------------------
  # Check arguments. -----------------------------------------------------------
  # ----------------------------------------------------------------------------

  color_mode <- match.arg(color_mode, c("sequential", "qualitative", "diverging"))

  if (DATA_MIN > DATA_MAX) { 
    stop("DATA_MAX must be greater than DATA_MIN")
  }

  # Use default palettes if the colors are not specified.
  if (identical(colors, NULL)) {
    colors <- switch(
      color_mode,
      sequential="ROY_BIG_BL", # will use pos half
      qualitative="Set2",
      diverging="ROY_BIG_BL"
    )
  }

  N_COLORS_PRE <- length(colors)
  N_COLOR_VALUES_PRE <- length(zlim)
  if (N_COLORS_PRE == 1) {
    # --------------------------------------------------------------------------
    # ROY_BIG_BL ---------------------------------------------------------------
    # --------------------------------------------------------------------------
    if (colors == "ROY_BIG_BL") {
      if (color_mode=="sequential") {
        if (identical(zlim, NULL)) {
          RBB <- ROY_BIG_BL(DATA_MIN, DATA_MAX, pos_half=TRUE)
        } else if (N_COLOR_VALUES_PRE==2) {
          RBB <- ROY_BIG_BL(zlim[1], zlim[2], pos_half=TRUE)
        } else {
          stop(paste(
            "The sequential ROY_BIG_BL palette (default) requires",
            "two (min and max values) or NULL/none `zlim`."
          ))
        }

      } else if (color_mode=="qualitative") {
        ciftiTools_warn(
          "The ROY_BIG_BL palette is not recommended for qualitative data."
        )
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

    # --------------------------------------------------------------------------
    # RColorBrewer -------------------------------------------------------------
    # --------------------------------------------------------------------------
    } else if (colors %in% row.names(brewer.pal.info)) {
      colors_info <- brewer.pal.info[row.names(brewer.pal.info) == colors,]
      brewer_mode <- match.arg(
        as.character(colors_info$category), 
        c("sequential", "qualitative", "diverging")
      )
      if (brewer_mode != color_mode) {
        warning(paste(
          "The RColorBrewer palette type is", brewer_mode, 
          "but the color_mode is", color_mode
        ))
      }
      colors <- brewer.pal(as.numeric(colors_info$maxcolors), colors)

    } else {
      stop(paste(
        "The `colors` argument must be 'ROY_BIG_BL',",
        "a palette listed in `RColorBrewer::brewer.pal.info`, or",
        "a character vector of hex codes or standard R names for colors"
      ))
    }
  }

  # ----------------------------------------------------------------------------
  # Organize the colors --------------------------------------------------------
  # ----------------------------------------------------------------------------
  N_COLORS <- length(colors)
  N_COLOR_VALUES <- length(zlim)
  if (!identical(zlim, NULL)) {
    # Check that the color values are valid.
    valid_color_values_lengths <- switch(color_mode, 
      sequential=c(2, N_COLORS),
      qualitative=N_COLORS,
      diverging=c(1, 2, 3, N_COLORS)
    )
    if (!(N_COLOR_VALUES %in% valid_color_values_lengths)) {
      stop(paste(
        "There are", N_COLOR_VALUES, "color values.",
        "This is not compatible for the", color_mode, 
        "color mode. See the description for details."
      ))
    }

    # Order color values from lowest to highest.
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

  # ----------------------------------------------------------------------------
  # Sequential -----------------------------------------------------------------
  # ----------------------------------------------------------------------------
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
        stop(paste(
          "The sequential color mode requires `length(zlim)` to be `0`",
          "(`is.null(zlim`), `2`, or `length(colors)`."
        ))
      }
    }

  # ----------------------------------------------------------------------------
  # Qualitative ----------------------------------------------------------------
  # ----------------------------------------------------------------------------
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
        # Might look weird for the ROY_BIG_BL pallete, but ROY_BIG_BL is not 
        #   recommended anyway for qualitative data.
        pal_cols <- colorRampPalette(colors)(N_DATA_VALUES)
      }
    } else if (N_COLOR_VALUES==N_COLORS) {
      pal_vals <- zlim
      pal_cols <- colors
    } else {
      stop(paste(
        "The sequential color mode requires `length(zlim)` to be `0`",
        "(`is.null(zlim`) or `length(colors)`."
      ))
    }

  # ----------------------------------------------------------------------------
  # Diverging ------------------------------------------------------------------
  # ----------------------------------------------------------------------------
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
            stop(paste(
              "If one color_value is used with the diverging color_mode, it",
              "represents the midpoint of the data scale and must be between",
              "the data minimum and maximum. (It does not have to be the true",
              "midpoint.) Different bounds can be set with",
              "`color_value=c(new_min, midpoint, new_max)`."
            ))
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
        if (length(colors) %% 2 != 1) { 
          stop(paste(
            "There must be an odd number of colors for the diverging color",
            "mode, to have a middle color."
          ))
        }
        low_vals <- seq(min_val, mid_val, length.out=floor(N_COLORS/2)+1)
        low_vals <- low_vals[1:(length(low_vals)-1)]
        high_vals <- seq(mid_val, max_val, length.out=floor(N_COLORS/2)+1)
        high_vals <- high_vals[2:length(high_vals)]
        pal_vals <- c(low_vals, mid_val, high_vals)
      }
    }
  } else {
    stop(paste("Unrecognized color mode:", color_mode))
  }

  data.frame(color=pal_cols, value=pal_vals)
}

#' Interpolates between entries in the input palette to make a larger palette 
#'  with at least MIN_COLOR_RES entries.
#'
#' @param pal The color palette to expand, as a data.frame with two columns: 
#'  \code{"color"} (character: color hex codes) and \code{"value"} (numeric).
#' @param MIN_COLOR_RES The minimum number of entries to have in the output 
#'  palette. Because of rounding, there may be more than this number of entries.
#'
#' @return A data.frame with two columns: \code{"color"} (character: color hex 
#'  codes) and \code{"value"} (numeric)
#'
#' @importFrom grDevices colorRampPalette 
#'
#' @export
#' 
expand_color_pal <- function(pal, MIN_COLOR_RES=255) {
  if (nrow(pal) < MIN_COLOR_RES) {
    range <- max(pal$value) - min(pal$value)
    if (range == 0) {
      vals <- rep(pal$value[1], MIN_COLOR_RES)
      cols <- as.character( rep(pal$color[1], MIN_COLOR_RES) )
    } else {
      colors <- as.character(pal$color)
      # Interpolate between palette values to obtain at least MIN_COLOR_RES 
      #   colors levels.
      color_res <- MIN_COLOR_RES * diff(pal$value)/(range)
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
    }
  } else {
    vals <- pal$value
    cols <- as.character(pal$color)
  }
  data.frame(color=cols, value=vals)
}

#' Use a color palette
#' 
#' Applies a palette to a data vector to yield a vector of colors.
#'
#' @param data_values The values to map to colors
#' @param pal The palette to use to map values to colors
#' @param color_NA The color to use for \code{NA} values. Default: \code{"white"}.
#' @param indices Return the numeric indices of colors in \code{pal$value} 
#'  rather than the colors themselves. A value of \code{0} will be used for
#'  missing data. Default: \code{FALSE}.
#' 
#' @return A character vector of color names (or integers if \code{indices}).
#' 
#' @export
#' 
use_color_pal <- function(data_values, pal, color_NA="white", indices=FALSE) {

  stopifnot(is.character(color_NA) && length(color_NA==1))

  mask <- is.na(data_values)

  # Indices of colors for each datapoint in `data_values`
  if (nrow(pal) == 1) {
    out <- 1 - as.numeric(mask)
  } else {
    pal$cut <- -Inf
    pal$cut[2:nrow(pal)] <- diff(pal$value)/2 + pal$value[1:(length(pal$value)-1)]

    out <- rep(0, length(data_values))
    out[!mask] <- apply(outer(as.numeric(data_values[!mask]), pal$cut, '>='), 1, sum)
  }

  if (!indices) { 
    out <- c(color_NA, as.character(pal$color))[out + 1]
  }

  out
}