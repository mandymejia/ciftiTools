#' "ROY_BIG_BL" color palette
#'
#' "ROY_BIG_BL", the default palette from the Connectome Workbench.
#'
#' Yields the landmark color hex codes and values for the "ROY_BIG_BL" palette. 
#'  This is the same color palette as the default Connectome Workbench palette. 
#'  Source: https://github.com/Washington-University/workbench/blob/master/src/Files/PaletteFile.cxx
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
#' @param half \code{"positive"} or \code{"negative"} to use the positive half
#'  (black --> red --> yellow) or negative half (black --> blue --> purple -->
#'  green --> aqua) only. \code{NULL} (default) or \code{FALSE} to use entire 
#'  palette.
#' @param pos_half Deprecated. Use \code{half}.
#'
#' @return A data.frame with two columns: \code{"color"} (character: color hex 
#'  codes) and \code{"value"} (numeric)
#' 
#' @importFrom grDevices col2rgb
#'
#' @export 
#' 
ROY_BIG_BL <- function(min=0, max=1, mid=NULL, half=NULL, pos_half=FALSE) {
  stopifnot(length(min)==1)
  stopifnot(length(max)==1)

  if (min==max) {
    return( data.frame(color = c("#000000"), value = c(min)) )
  }
  rev_order <- min > max
  if (rev_order) {
    temp <- min
    min <- max
    max <- temp
  }

  if (pos_half) {
    # Deprecated.
    half <- "positive"
  }
  if (is.null(half) || identical(half, FALSE)) {
    half <- "no"
  } 
  half <- match.arg(half, c("no", "positive", "negative"))

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

  if (half=="positive") {
    # Only use the latter half.
    value <- value[1:10]
    color <- color[1:10]
    # Normalize the values to [min, max].
    value <- value * (max - min) + min
  } else if (half=="negative") {
    # Only use the latter half.
    value <- value[10:19]
    color <- color[10:19]
    # Normalize the values to [min, max].
    value <- (value + 1) * (max - min) + min
  } else {
    # Normalize the values to [min, max]. 
    # Note that the bottom 0.5% are all #00ffff.
    value <- (value + 1)/2
    value <- value * (max - min) + min

    # Normalize middle value (black) to mid, if specified.
    if (!is.null(mid)) {
      if(!(mid > min & mid < max)) { stop("`mid` not between `min` and `max`.") }
      old_mid <- (min + max)/2
      value[1:9] <- (value[1:9] - old_mid) / (max - old_mid) * (max - mid) + mid
      value[10] <- mid
      value[11:19] <- (value[11:19] - min) / (old_mid - min) * (mid - min) + min
    }
  }
  if (rev_order) { value <- value[seq(length(value), 1)] }
  RBB <- data.frame(color=color, value=value)
  RBB[order(value),]
}

#' Make a color palette.
#' 
#' Control the mapping of values to colors with \code{colors}, 
#'  \code{color_mode}, and \code{zlim}.
#'
#' There are three kinds of arguments for \code{colors}: \code{"ROY_BIG_BL"}, 
#'  the name of a ColorBrewer palette (see \code{RColorBrewer::brewer.pal.info} 
#'  and colorbrewer2.org), the name of a viridisLite palette, or a character 
#'  vector of color names.
#'
#' If \code{colors=="ROY_BIG_BL"}, the "ROY_BIG_BL" palette will be used. It is
#'  the same palette as the default for the Connectome Workbench application 
#'  (https://github.com/Washington-University/workbench/blob/master/src/Files/PaletteFile.cxx). 
#'  The midpoint will be colored 
#'  black. From the midpoint toward the upper bound, colors will proceed from 
#'  black to red to yellow. From the midpoint toward the lower bound, colors 
#'  will proceed from black to blue to purple to green to aqua. Here is how each
#'  color mode behaves if \code{colors=="ROY_BIG_BL"}:
#'
#' \describe{
#'  \item{\code{color_mode=="sequential"}}{Only half of the palette will be 
#'    used. If \code{zlim} is length 2, the higher value will be the maximum and
#'    the lower value will be the minimum. Set \code{zlim[1] > zlim[2]} to
#'    reverse the color scale. (Note that the second half, black --> red --> 
#'    yellow, is used by default. To use the negative half specify 
#'    \code{colors=="ROY_BIG_BL_neg"} instead. It will also be used automatically
#'    by \code{xifti_read_surface} when the data range is negative.) 
#'    \code{zlim} can also be length 10, in which case each value corresponds to
#'    the position of an individual color in the half palette.
#'  }
#'  \item{\code{color_mode=="qualitative"}}{"ROY_BIG_BL" is not recommended for 
#'    qualitative data, so a warning will be issued. Palette colors will be
#'    selected from the landmark "ROY_BIG_BL" colors, with interpolated colors
#'    added if the number of colors in the palette (18) is less than this range.
#'    \code{zlim} should be a single number: the number of unique colors to get.
#'  }
#'  \item{\code{color_mode=="diverging"}}{If \code{zlim} is length 2 or 3, the
#'    lowest number will be the lower bound and the highest number will
#'    be the upper bound. If \code{zlim} is length 3, the middle number will be the
#'    midpoint (black). The lower and upper bounds will be aqua and yellow,
#'    respectively, except if \code{zlim} is in descending order, in which case
#'    the color scale will be reversed (lowest is yellow; highest is aqua).
#'    \code{zlim} can also be length 19, in which case each value corresponds to
#'    the position of an individual color in the palette.
#'  }
#' }
#'
#' If \code{colors} is the name of an RColorBrewer palette (see 
#'  \code{RColorBrewer::brewer.pal.info}) or viridisLite palette, the colors in 
#'  that palette will be used, and the following behavior applies. 
#'  If \code{colors} is a character vector of color names (hex codes or standard
#'  R color names), the following behavior applies directly:
#'
#' \describe{
#'  \item{\code{color_mode=="sequential"}}{If \code{zlim} is length 2, the 
#'    higher value will be the maximum and the lower value will be the minimum.
#'    Set \code{zlim[1] > zlim[2]} to reverse the color scale. \code{zlim} can 
#'    also be the same length as the palette, in which case each value 
#'    corresponds to the position of an individual color in the palette.
#'  }
#'  \item{\code{color_mode=="qualitative"}}{\code{zlim} should be a single 
#'    number: the number of unique colors to get. Color interpolation will be 
#'    used if the number of colors in the palette is less than this range. If 
#'    \code{length(zlim)==length(colors)}, each color will be mapped to each 
#'     corresponding value.
#'  }
#'  \item{\code{color_mode=="diverging"}}{If \code{zlim} is length 2 or 3, the
#'    lowest number will be the lower bound and the highest number will
#'    be the upper bound. If \code{zlim} is length 3, the middle number will be the
#'    midpoint. Set \code{zlim} in descending order to reverse the color scale.
#'    \code{zlim} can also be the same length as the palette, in which case each 
#'    value corresponds to the position of an individual color in the palette.
#'  }
#' }
#'
#' @param colors (Optional) "ROY_BIG_BL", the name of a ColorBrewer palette 
#'  (see \code{RColorBrewer::brewer.pal.info} and colorbrewer2.org), the name of
#'  a viridisLite palette, or a character vector of colors. 
#'  \code{NULL} (default) will use \code{"ROY_BIG_BL"} 
#'  if \code{color_mode} is \code{"sequential"} or \code{"diverging"}, and 
#'  \code{"Set2"} if \code{color_mode} is \code{"qualitative"}. See the 
#'  description for more details.
#' @param color_mode (Optional) \code{"sequential"}, \code{"qualitative"}, or 
#'  \code{"diverging"}. Default: \code{"sequential"}. See the description for 
#'  more details.
#' @param zlim (Optional) Controls the mapping of values to each color in 
#'  \code{colors}. See the description for more details.
#'
#' @return A data.frame with two columns: \code{"color"} (character: color hex 
#'  codes) and \code{"value"} (numeric)
#' 
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal.info brewer.pal
#' @import viridisLite
#'
#' @export
#' 
make_color_pal <- function(
  colors=NULL, color_mode=c("sequential", "qualitative", "diverging"), zlim=NULL) {

  # ----------------------------------------------------------------------------
  # Check arguments. -----------------------------------------------------------
  # ----------------------------------------------------------------------------

  color_mode <- match.arg(color_mode, c("sequential", "qualitative", "diverging"))

  if (is.null(zlim)) {
    zlim <- switch(color_mode,
      sequential = c(0, 1),
      qualitative = 10,
      diverging = c(0, 1)
    )
  }

  # Use default palettes if the colors are not specified.
  if (is.null(colors)) {
    colors <- switch(
      color_mode,
      sequential="ROY_BIG_BL", # will use pos half
      qualitative="Set2",
      diverging="ROY_BIG_BL"
    )
  }

  if (color_mode == "qualitative") {
    if (length(zlim) != 1) { 
      warning("The number of colors will be the first element in `zlim`.\n")
      zlim <- zlim[1]
    }
    stopifnot(zlim > 0)
  }

  N_COLORS_PRE <- length(colors)
  zlim_length <- length(zlim)

  # ----------------------------------------------------------------------------
  # ROY_BIG_BL -----------------------------------------------------------------
  # ----------------------------------------------------------------------------
  if ((N_COLORS_PRE == 1) && grepl("ROY_BIG_BL", colors)) {
    half <- switch(colors,
      ROY_BIG_BL=ifelse(color_mode=="sequential", "pos", "no"),
      ROY_BIG_BL_pos="pos",
      ROY_BIG_BL_neg="neg"
    )

    if (color_mode=="sequential") {
      if (zlim_length==2) {
        pal <- ROY_BIG_BL(zlim[1], zlim[2], half=half)
      } else if (zlim_length==10) {
        pal <- ROY_BIG_BL(min(zlim), max(zlim), half=half)
        pal$value <- zlim
      } else {
        stop(paste(
          "The sequential ROY_BIG_BL palette (default) requires",
          "`zlim` to be length 2 (min and max) or 10 (each color)."
        ))
      }

    } else if (color_mode=="qualitative") {
      ciftiTools_warn(
        "The ROY_BIG_BL palette is not recommended for qualitative data."
      )
      pal <- ROY_BIG_BL(half="no")
      pal <- pal[seq(2, nrow(pal)),] # skip duplicate cyan
      pal <- expand_color_pal(pal, zlim)[seq(zlim),]
      pal$value <- seq(nrow(pal))

    } else if (color_mode=="diverging") {
      if (zlim_length==2) {
        pal <- ROY_BIG_BL(zlim[1], zlim[2], mid=(zlim[1]+zlim[2])/2)
      } else if (zlim_length==3) {
        pal <- ROY_BIG_BL(zlim[1], zlim[3], mid=zlim[2])
      } else if (zlim_length==19) {
        pal <- ROY_BIG_BL(min(zlim), max(zlim))
        pal$value <- zlim
      } else {
        stop(paste(
          "The diverging ROY_BIG_BL palette requires `zlim` to be length 2",
          "(min and max), 3 (min, mid, max), or 19 (each color)."
        ))
      }
    }

    pal <- pal[order(pal$value),]
    rownames(pal) <- NULL
    return(pal)
  }

  # ----------------------------------------------------------------------------
  # RColorBrewer / Individual colors -------------------------------------------
  # ----------------------------------------------------------------------------
 
  # RColor Brewer --> Individual colors
  was_viridis <- FALSE
  if ((N_COLORS_PRE == 1) && (colors %in% row.names(brewer.pal.info))) {
    colors_info <- brewer.pal.info[row.names(brewer.pal.info) == colors,]
    brewer_mode <- match.arg(
      as.character(colors_info$category), 
      c("sequential", "qualitative", "diverging")
    )
    if (brewer_mode != color_mode) {
      warning(paste0(
        "The RColorBrewer palette type is ", brewer_mode, 
        " but the color_mode is ", color_mode, ".\n"
      ))
    }
    colors <- brewer.pal(as.numeric(colors_info$maxcolors), colors)
  # viridis --> Individual colors
  viridis_cols <- c(
    "viridis", "magma", "plasma",
    "inferno", "cividis", "mako",
    "rocket", "turbo"
  )
  } else if ((N_COLORS_PRE == 1) && (colors %in% viridis_cols)) {
    was_viridis <- TRUE
    vir_opt <- switch(colors, 
      magma = "A", inferno = "B", plasma = "C", viridis = "D", cividis = "E"
    )
    colors <- viridisLite::viridis.map[viridisLite::viridis.map$opt==vir_opt,seq(3)]
    colors <- apply(colors, 1, function(x){rgb(red=x[1], green=x[2], blue=x[3])})
  }

  # Get `values` for sequential.
  if (color_mode == "sequential") {
    if (zlim_length==2) {
      values <- seq(zlim[1], zlim[2], length.out=length(colors))
    } else if (zlim_length == length(colors)) {
      values <- zlim
    } else {
      stop(paste(
        "This sequential palette requires `zlim` to be length 2 (min and max)",
        " or", length(colors), " (each color)."
      ))
    }

  # Get palette for qualitative.
  } else if (color_mode=="qualitative") {
    pal <- data.frame(color=colors, value=seq(length(colors)))
    pal <- expand_color_pal(pal, zlim)
    if (was_viridis) {
      pal <- pal[round(seq(1,nrow(pal),length.out=zlim)),]
    } else {
      pal <- pal[seq(zlim),]
    }
    pal$value <- seq(nrow(pal))
    return(pal)

  # Get `values` for diverging.
  } else if (color_mode=="diverging") {
    if (zlim_length==length(colors)) {
      values <- zlim
    } else {
      # Get the minimum, middle, and maximum value for the color scale.
      if (zlim_length==2) {
        min_val <- zlim[1]
        mid_val <- mean(zlim)
        max_val <- zlim[2]
      } else if (zlim_length==3) {
        if (!(all(diff(zlim) > 0) || all(diff(zlim) < 0))) {
          warning(paste(
            "Sorting `zlim` in ascending order.",
            "(Middle value was not between first & last.)"
          ))
          zlim <- sort(zlim)
        }
        min_val <- zlim[1]
        mid_val <- zlim[2]
        max_val <- zlim[3]
      }

      # Interpolate between the min/mid/max to get the color values.
      if (length(colors) %% 2 != 1) {
        # User probably doesn't care, if there are more than 64 colors
        # (e.g. for viridis).
        if (length(colors) < 64) {
          warning(paste(
            "There must be an odd number of colors for the diverging color",
            "mode, to have a middle color. Removing the last color."
          ))
        }
        colors <- colors[seq(length(colors)-1)]
      }
      low_vals <- seq(min_val, mid_val, length.out=floor(length(colors)/2)+1)
      low_vals <- low_vals[1:(length(low_vals)-1)]
      high_vals <- seq(mid_val, max_val, length.out=floor(length(colors)/2)+1)
      high_vals <- high_vals[2:length(high_vals)]
      values <- c(low_vals, mid_val, high_vals)
    }
  } else { stop() }

  # for sequential and diverging
  pal <- data.frame(color=colors, value=values)
  pal <- pal[order(pal$value),]
  rownames(pal) <- NULL
  pal
}

#' Interpolates between entries in the input palette to make a larger palette 
#'  with COLOR_RES entries.
#'
#' @param pal The color palette to expand, as a data.frame with two columns: 
#'  \code{"color"} (character: color hex codes) and \code{"value"} (numeric).
#' @param COLOR_RES The number of entries to have in the output palette. 
#'
#' @return A data.frame with two columns: \code{"color"} (character: color hex 
#'  codes) and \code{"value"} (numeric)
#'
#' @importFrom grDevices colorRampPalette 
#'
#' @export
#' 
expand_color_pal <- function(pal, COLOR_RES=255) {
  if (nrow(pal) >= COLOR_RES) { return(pal) }

  range <- max(pal$value) - min(pal$value)
  if (range == 0) {
    vals <- rep(pal$value[1], COLOR_RES)
    cols <- as.character( rep(pal$color[1], COLOR_RES) )
  } else {
    colors <- as.character(pal$color)
    # Interpolate between palette values to obtain at least COLOR_RES 
    #   colors levels.
    color_res <- COLOR_RES * diff(pal$value)/diff(range(pal$value))
    color_res <- as.integer(round(pmax(color_res, 2)))
    diff_to_fix <- sum(color_res) + 1 - COLOR_RES # +1 b/c last added later

    if (diff_to_fix < 0) {
      color_res[order(color_res)][seq(-diff_to_fix)] <- color_res[order(color_res)][seq(-diff_to_fix)] + 1
    } else if (diff_to_fix > 0) {
      color_res[rev(order(color_res))][seq(diff_to_fix)] <- color_res[rev(order(color_res))][seq(diff_to_fix)] - 1
    }

    vals <- vector(length=0, mode="numeric")
    cols <- vector(length=0, mode="character")
    
    for(ii in 1:(nrow(pal)-1)) {
      next_vals <- seq(pal$value[ii], pal$value[ii+1], length.out=color_res[ii]+1)
      next_vals <- next_vals[seq(length(next_vals)-1)]
      next_cols <- colorRampPalette(c(colors[ii], colors[ii+1]))(color_res[ii]+1)
      next_cols <- next_cols[seq(length(next_cols)-1)]
      vals <- c(vals, next_vals)
      cols <- c(cols, next_cols)
    }
    vals <- c(vals, pal$value[nrow(pal)])
    cols <- c(cols, colors[nrow(pal)])
  }

  pal <- data.frame(color=cols, value=vals)
  rownames(pal) <- NULL
  pal
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