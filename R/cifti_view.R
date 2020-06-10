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
#'
#' @return A character vector of color names
use_color_pal <- function(data_values, pal){
  colors <- as.character(pal$color)
  pal$cut <- -Inf
  pal$cut[2:nrow(pal)] <- diff(pal$value)/2 + pal$value[1:(length(pal$value)-1)]
  out <- colors[apply(outer(data_values, pal$cut, '>='), 1, sum)]
  return(out)
}

#' Visualize cifti brain data
#'
#' @param cifti Object of class "cifti". See \code{help(cifti_read_separate)}, \code{help(cifti_make)}, and \code{help(is.cifti)}.
#' @param surface Name of brain surface model to use.  Must equal one of the names of cifti$SURF_LEFT (or equivalently, 
#'  cifti$SURF_RIGHT). If NULL, first surface will be used. 
#' @param colors (Optional) "ROY_BIG_BL", vector of colors to use, OR the name of a ColorBrewer palette (see RColorBrewer::brewer.pal.info 
#'  and colorbrewer2.org). Defaults are \code{"ROY_BIG_BL"} (sequential), \code{"Set2"} (qualitative), and \code{"ROY_BIG_BL"} (diverging).
#'  See the \code{ciftiTools::make_color_pal()} description for more details.
#' @param color_mode (Optional) \code{"sequential"}, \code{"qualitative"}, or \code{"diverging"}. Default is sequential. See the 
#'  \code{ciftiTools::make_color_pal()} description for more details.
#' @param color_values (Optional) Controls the mapping of values to each color in \code{colors}. If the length is longer than
#'  one, using -Inf will set the value to \code{DATA_MIN}, and Inf will set the value to \code{DATA_MAX}. See the 
#'  \code{ciftiTools::make_color_pal()} description for more details.
#' @param brainstructure "left", "right", "surface" or "subcortical".
#' @param structural_img If brainstructure is "subcortical", the file name of the structural MRI image on which to overlay the subcortical values.  The MNI template is used by default.  Set to NULL to use a blank image.
#' @param w The time/column index of the cifti data to plot.
#' @param plane If brainstructure is "subcortical" and papaya=FALSE, the plane to display.  
#'  Default is "axial". Other options are "sagittal" and "coronal".
#' @param num.slices If brainstructure is "subcortical" and papaya=FALSE, the number of slices to display.  
#'  Default is "axial". Other options are "sagittal" and "coronal".
#' @param use_papaya If brainstructure is "subcortical", papaya=TRUE will use papayar to allows for interactive visualization.
#'
#' @export
#' @import rgl
#' @importFrom oro.nifti overlay readNIfTI
#' @importFrom fields image.plot
cifti_view <- function(cifti, surface=NULL,
  colors=NULL, color_mode=c("sequential", "qualitative", "diverging"), color_values=NULL, color_legend_label='',
  brainstructure, structural_img='MNI', w=1, plane='axial', num.slices=12, use_papaya=FALSE){
  
  color_mode <- match.arg(color_mode, c("sequential", "qualitative", "diverging"))

  if(!is.cifti(cifti)) stop('cifti argument is not a valid cifti object. See is.cifti().')

  do_left <- (brainstructure %in% c('left','surface'))
  do_right <- (brainstructure %in% c('right','surface'))

  if(do_left | do_right){
    values_left <- values_right <- NULL
    if(do_left){
      if(is.null(cifti$CORTEX_LEFT)) stop('No data in cifti$CORTEX_LEFT.')
      if(is.null(cifti$SURF_LEFT)) stop('No data in cifti$SURF_LEFT. Must provide a surface model for left cortex.')
      if(!(w %in% 1:ncol(cifti$CORTEX_LEFT))) stop('w is not a valid column index for cifti$CORTEX_LEFT')
    }
    if(do_right){
      if(is.null(cifti$CORTEX_RIGHT)) stop('No data in cifti$CORTEX_RIGHT.')
      if(is.null(cifti$SURF_RIGHT)) stop('No data in cifti$SURF_RIGHT. Must provide a surface model for right cortex.')
      if(!(w %in% 1:ncol(cifti$CORTEX_RIGHT))) stop('w is not a valid column index for cifti$CORTEX_RIGHT')
    }
    eps <- 1e-8

    #apply(array(cifti$CORTEX_LEFT[cifti$SURF_LEFT$surface$faces,1:10], dim=c(64980, 3, 10)), c(1,3), mean, na.rm=TRUE)
    if(do_left){
      NA_mask_left <- apply(abs(cifti$CORTEX_LEFT), 1, sum) < eps
      cifti$CORTEX_LEFT[NA_mask_left,] <- NA
      values_left <- cifti$CORTEX_LEFT[,w]
      #values_left <- apply(matrix(cifti$CORTEX_LEFT[,w][cifti$SURF_LEFT$surface$faces], ncol=3), 1, mean, na.rm=TRUE) # FACES
      cifti$CORTEX_LEFT <- NULL #save memory
    }
    if(do_right){
      NA_mask_right <- apply(abs(cifti$CORTEX_RIGHT), 1, sum) < eps
      cifti$CORTEX_RIGHT[NA_mask_right,] <- NA
      values_right <- cifti$CORTEX_RIGHT[,w]
      #values_right <- apply(matrix(cifti$CORTEX_RIGHT[,w][cifti$SURF_RIGHT$surface$faces], ncol=3), 1, mean, na.rm=TRUE) # FACES
      cifti$CORTEX_RIGHT <- NULL #save memory
    }
    nvox_left <- length(values_left)
    nvox_right <- length(values_right)
    if(brainstructure=='surface') values <- c(values_left, values_right)
    if(brainstructure=='left') values <- values_left
    if(brainstructure=='right') values <- values_right
    values[is.nan(values)] <- 0 # FACES
    values[is.na(values)] <- 0

    #assign colors to faces based on intensity values
    if(color_mode=="qualitative"){
      print("Warning: qualitative values must be integers 1:N_VALUES. Will be fixed in future.") # will fix in future.
      N_VALUES <- length(unique(values))
      if(N_VALUES > 30){stop("Too many qualitative values.")} #fix
      pal_base <- make_color_pal(colors=colors, color_mode=color_mode, color_values=color_values,
        DATA_MIN=1, DATA_MAX=N_VALUES)
    } else {
      pal_base <- make_color_pal(colors=colors, color_mode=color_mode, color_values=color_values,
        DATA_MIN=min(values), DATA_MAX=max(values))
    }

    if(color_mode %in% c("sequential", "diverging")){
      pal <- expand_color_pal(pal_base)
    } else {
      pal <- pal_base
    }

    #cols <- apply(values, 2, use_color_pal, pal)
    cols <- use_color_pal(values, pal)
    if(brainstructure=='surface') {
      cols_left <- cols[1:nvox_left]
      cols_right <- cols[(nvox_left+1):(nvox_left+nvox_right)]
    } else if(brainstructure=='left'){
      cols_left <- cols
    } else if(brainstructure=='right'){
      cols_right <- cols
    }
    rm(cols)

    #construct and plot mesh object
    open3d()
    
    if(do_left){
      if(is.null(cifti$SURF_LEFT)) stop("If brainstructure is 'left' or 'surface', cifti$SURF_LEFT must not be NULL.")
      if(is.null(surface)){
        surf_left <- cifti$SURF_LEFT[[1]]
      } else {
        if(!(surface %in% names(cifti$SURF_LEFT))) stop('If surface is provided, it must be one of the names of cifti$SURF_LEFT.')
        surf_left <- cifti$SURF_LEFT[names(cifti$SURF_LEFT) == surface]
      }
      plt_left <- tmesh3d(t(cbind(cifti$SURF_LEFT$surface$vertices,
                                  rep(1, nrow(cifti$SURF_LEFT$surface$vertices)))),
                          t(cifti$SURF_LEFT$surface$faces),
                          meshColor = "vertices")
      plt_left <- addNormals(plt_left)
    }

    if(do_right){
      if(is.null(cifti$SURF_RIGHT)) stop("If brainstructure is 'right' or 'surface', cifti$SURF_RIGHT must not be NULL.")
      if(is.null(surface)){
        surf_right <- cifti$SURF_RIGHT[[1]]
      } else {
        if(!(surface %in% names(cifti$SURF_RIGHT))) stop('If surface is provided, it must be one of the names of cifti$SURF_RIGHT.')
        surf_right <- cifti$SURF_RIGHT[names(cifti$SURF_RIGHT) == surface]
      }
      plt_right <- tmesh3d(t(cbind(cifti$SURF_RIGHT$surface$vertices,
                                   rep(1, nrow(cifti$SURF_RIGHT$surface$vertices)))),
                           t(cifti$SURF_RIGHT$surface$faces),
                           meshColor = "vertices")
      plt_right <- addNormals(plt_right)
    }

    par3d(windowRect = c(20, 20, 1200, 700))
    if(do_left){
      shade3d(plt_left, col=cols_left, specular="black", legend=TRUE)
    }
    if(do_right){
      shade3d(plt_right, col=cols_right, specular="black", legend=TRUE)
    }

    # Add color bar scale/legend.
    # Suppress this warning: "calling par(new=TRUE) with no plot"
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
    colbar_labs_digits <- ifelse(
      diff(range(pal$value)) >=1, 0, 
      1+ceiling(abs(log(diff(range(pal$value)), 10))))
    suppressWarnings(
      bgplot3d(image.plot(
        legend.only = TRUE, zlim = range(pal$value), col = as.character(pal$color), 
        breaks=colbar_breaks, legend.lab=color_legend_label,
        legend.cex=2, legend.shrink=.9, legend.width=2, legend.line=7, legend.mar=12,
        axis.args=list(cex.axis=1.7, at=colbar_labs, 
                       labels=round(colbar_labs, colbar_labs_digits))))
    )

    # TO-DO
    if(brainstructure=="left"){
      rot_left <- rbind(
        c( 0,-1, 0, 0),
        c( 0, 0, 1, 0),
        c(-1, 0, 0, 0),
        c( 0, 0, 0, 1))
      rgl.viewpoint(userMatrix=rot_left)
    } else if(brainstructure=="right") {
      rot_right <- rbind(
        c( 0, 1, 0, 0),
        c( 0, 0, 1, 0),
        c( 1, 0, 0, 0),
        c( 0, 0, 0, 1))
      rgl.viewpoint(userMatrix=rot_right)
    }
    #return(rgl_out)
  }

  if(brainstructure=='subcortical'){

    if(use_papaya) {
      if (!requireNamespace("papayar", quietly = TRUE)) {
        stop("Package \"papayar\" needed for this function to work. Please install it.",
             call. = FALSE)
      }
    }

    #pick slices with a lot of subcortical voxels
    if(!use_papaya){
      labs <- cifti$LABELS
      mask <- (labs > 0)
      if(plane=='axial') mask_count <- apply(mask, 3, sum)
      if(plane=='coronal') mask_count <- apply(mask, 2, sum)
      if(plane=='sagittal') mask_count <- apply(mask, 1, sum)

      slices <- which(mask_count > max(mask_count)/2)
      inds <- round(seq(1,length(slices), length.out=num.slices))
      slices <- slices[inds]
    }

    if(is.null(structural_img)) {
      T1w <- NULL
    } else if(structural_img=='T1w') {
      T1w <- readNIfTI(system.file('extdata/MNI152_T1_2mm.nii.gz', package='ciftiTools'), reorient=FALSE)
    } else {
      T1w <- readNIfTI(structural_img, reorient=FALSE)
    }

    values <- cifti$VOL[,,,w]
    if(!is.null(z_min)) values[values < z_min] <- z_min
    if(!is.null(z_max)) values[values > z_max] <- z_max
    print(paste0('Values to be plotted range from ',min(values[cifti$LABELS > 0]),' to ',max(values[cifti$LABELS > 0])))

    img_overlay <- T1w*0
    img_overlay@.Data <- values
    img_overlay@.Data[cifti$LABELS==0] <- NA

    img_labels <- T1w*0
    img_labels@.Data <- cifti$LABELS
    img_labels@.Data[cifti$LABELS==0] <- NA

    print("Note: colors not implemented yet for subcortical volume.")
    if(use_papaya==FALSE) oro.nifti::overlay(x=T1w, y=img_overlay, plot.type='single', plane=plane, z=slices)#, col.y=pal)
    if(use_papaya==TRUE) papayar::papaya(list(T1w, img_overlay, img_labels))
  }
}
