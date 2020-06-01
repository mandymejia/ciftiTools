#' Control the mapping of values to colors with \code{colors}, \code{color_mode}, and \code{color_values}. 
#'
#' \code{color_mode=="sequential"}: If \code{color_values=="NULL"}, the colors will be mapped with equal spacing from
#'  \code{DATA_MIN} to \code{DATA_MAX}. If \code{length(color_values)==2}, these values will be used as the upper and lower 
#'  bounds instead. If \code{length(color_values)==length(colors)}, each color will be mapped to each corresponding value. 
#'
#' \code{color_mode=="qualitative"}: If \code{color_values=="NULL"}, the colors will be mapped onto each integer between
#'  \code{DATA_MIN} and \code{DATA_MAX}, inclusive. Color interpolation will be used if the number of colors is less than 
#'  this range. If \code{length(color_values)==length(colors)}, each color will be mapped to each corresponding value.
#'
#' \code{color_mode=="diverging"}: If \code{color_values=="NULL"}, the colors will be mapped with equal spacing from
#'  \code{DATA_MIN} to \code{DATA_MAX}. Thus, the middle color will correspond to the midpoint of the data. If 
#'  \code{length(color_values)==1}, the middle color will correspond to this value instead. The preceeding colors will be
#'  equally-spaced between \code{DATA_MIN} and this value; the remaining colors will be equally-spaced between thsi value and
#'  \code{DATA_MAX}. If \code{length(color_values)==3}, these values will correspond to the first, middle, and last colors 
#'  respectively, with equal spacing between the first and middle and between the middle and last. If 
#'  \code{length(color_values)==length(colors)}, each color will be mapped to each corresponding value. Thus, the middle color
#'  will correspond to the middle color_value. For this color mode, the length of \code{colors} must be odd and >= 3.
#'
#' @param colors (Optional) Vector of colors to use, OR the name of a ColorBrewer palette (see RColorBrewer::brewer.pal.info 
#'  and colorbrewer2.org). Defaults are \code{"YlOrRd"} (sequential), \code{"Set2"} (qualitative), and \code{"RdYlBu"} (diverging).
#' @param color_mode (Optional) \code{"sequential"}, \code{"qualitative"}, or \code{"diverging"}. Default is sequential.
#' @param color_values (Optional) Controls the mapping of values to each color in \code{colors}. If the length is longer than
#'  one, using -Inf will set the value to \code{DATA_MIN}, and Inf will set the value to \code{DATA_MAX}. See the 
#'  description for more details.
#' @param DATA_MIN (Optional) The minimum value of the data to make the palette for. Overrided by certain \code{color_values}.
#' @param DATA_MAX (Optional) The maximum value of the data to make the palette for. Overrided by certain \code{color_values}.
#'
#' @export
#' @importFrom grDevices colorRampPalette
#' @import RColorBrewer
#'
#' @return A data.frame with two columns: color (character: standard color names or hex values) and value (numeric).
#'
make_color_pal <- function(colors=NULL, color_mode=c("sequential", "qualitative", "diverging"), color_values=NULL,
               DATA_MIN=0, DATA_MAX=1){
  color_mode <- match.arg(color_mode, c("sequential", "qualitative", "diverging"))
  
  if(DATA_MIN >= DATA_MAX){ stop("DATA_MAX must be greater than DATA_MIN") }
  
  # Use default palettes if the colors are not specified.
  if(identical(colors, NULL)){
    colors <- switch(color_mode,
                     sequential="YlOrRd",
                     qualitative="Set2",
                     diverging="RdYlBu")
  }
  
  if(length(colors) == 1){
    # Get the RColorBrewer colors.
    if(colors %in% row.names(brewer.pal.info)){
      colors_info <- brewer.pal.info[row.names(brewer.pal.info) == colors,]
      if(match.arg(as.character(colors_info$category), c("sequential", "qualitative", "diverging")) != color_mode){
        warning(paste("The RColorBrewer palette type is", colors_info$category, "but the color_mode is", color_mode))
      }
      colors <- brewer.pal(as.numeric(colors_info$maxcolors), colors)
      
    } else {stop("Only one color was provided!")}
  }
  
  N_COLORS <- length(colors)
  N_COLOR_VALUES <- length(color_values)
  if(!identical(color_values, NULL)){
    # Check that the color values are valid.
    valid_color_values_lengths <- switch(color_mode,
                                         sequential=c(2, N_COLORS),
                                         qualitative=N_COLORS,
                                         diverging=c(1, 3, N_COLORS))
    if(!(N_COLOR_VALUES %in% valid_color_values_lengths)){
      stop(paste("There are", N_COLOR_VALUES, "color values--this is not compatible for the", color_mode, "color mode. See the description for details."))
    }
    
    # Order color values from lowest to highest, if not already sorted.
    color_values_order <- order(color_values)
    colors <- colors[color_values_order]
    color_values <- color_values[color_values_order]
    
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
        pal_vals <- seq(color_values[1], color_values[2], (color_values[2] - color_values[1] + 1)/length(colors))
      } else if(N_COLOR_VALUES == N_COLORS) {
        pal_vals <- color_values
      } else {
        stop()
      }
    }
    
    #Qualitative
  } else if(color_mode=="qualitative"){
    if(!(is.integer(DATA_MIN) & is.integer(DATA_MAX))){ stop("Data bounds must be integers for qualitative color mode.") }
    N_DATA_VALUES <- DATA_MAX - DATA_MIN + 1
    if(identical(color_values, NULL)){
      pal_vals <- c(DATA_MIN:DATA_MAX)
      if(length(colors) >= N_DATA_VALUES){
        pal_cols <- colors[1:N_DATA_VALUES]
      } else {
        pal_cols <- colorRampPalette(colors)(N_DATA_VALUES)
      }
    } else if(N_COLOR_VALUES==N_COLORS) {
      pal_vals <- color_values
      pal_cols <- colors
    } else {
      stop()
    }
    
    # Diverging
  } else if(color_mode=="diverging"){
    if(length(colors) %% 2 != 1){ stop("There must be an odd number of colors for the diverging color mode, to have a middle color.") }
    pal_cols <- colors
    if(identical(color_values, NULL)){
      pal_vals <- seq(DATA_MIN, DATA_MAX, length.out=length(colors))
    } else {
      if(N_COLOR_VALUES==N_COLORS){
        pal_vals <- color_values
        pal_cols <- colors
      } else {
        if(N_COLOR_VALUES==1){
          mid_val <- color_values
          if((mid_val <= DATA_MIN) | (mid_val >= DATA_MAX)){
            stop("If length(mid_val)==1, the mid_val represents the midpoint and must be between the data minimum and maximum. 
              Different bounds can be used with mid_val=c(new_min, midpoint, new_max).")
          }
          min_val <- DATA_MIN
          max_val <- DATA_MAX
        } else if(N_COLOR_VALUES==3){
          mid_val <- color_values[2]
          min_val <- color_values[1]
          max_val <- color_values[3]
        }
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
#'  or hex values) and value (numeric).
#' @param MIN_COLOR_RES The minimum number of entries to have in the output palette. Because of rounding, there may be more
#'  than this number of entries.
#'
#' @export
#' @importFrom grDevices colorRampPalette
#'
#' @return A data.frame with two columns: color (character: standard color names or hex values) and value (numeric).
#'
expand_color_pal <- function(pal, MIN_COLOR_RES=60){
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
    cols <- colors
  }
  return(data.frame(color=cols, value=vals))
}

#' Applies a palette to a data vector to yield a vector of colors.
use_color_pal <- function(data_values, pal){
  colors <- as.character(pal$color)
  pal$cut <- -Inf
  pal$cut[2:nrow(pal)] <- diff(pal$value)/2 + pal$value[1:(length(pal$value)-1)]
  out <- colors[apply(outer(data_values, pal$cut, '>='), 1, sum)]
  return(out)
}

#' Visualize cifti brain data
#'
#' @param cifti Object of class 'cifti'. See \code{help(cifti_read_separate)}, \code{help(cifti_make)}, and \code{help(is.cifti)}.
#' @param surface Name of brain surface model to use.  Must equal one of the names of cifti$SURF_LEFT (or equivalently, 
#'  cifti$SURF_RIGHT). If NULL, first surface will be used. See the \code{ciftiTools::make_color_pal()} description for more details.
#' @param colors (Optional) Vector of colors to use, OR the name of a ColorBrewer palette (see RColorBrewer::brewer.pal.info 
#'  and colorbrewer2.org). Defaults are \code{"YlOrRd"} (sequential), \code{"Set2"} (qualitative), and \code{"RdYlBu"} (diverging).
#' @param color_mode (Optional) \code{"sequential"}, \code{"qualitative"}, or \code{"diverging"}. Default is sequential. See the 
#'  \code{ciftiTools::make_color_pal()} description for more details.
#' @param color_values (Optional) Controls the mapping of values to each color in \code{colors}. If the length is longer than
#'  one, using -Inf will set the value to \code{DATA_MIN}, and Inf will set the value to \code{DATA_MAX}. See the 
#'  \code{ciftiTools::make_color_pal()} description for more details.
#' @param brainstructure 'left', 'right', 'surface' or 'subcortical'.
#' @param structural_img If brainstructure is 'subcortical', the file name of the structural MRI image on which to overlay the subcortical values.  The MNI template is used by default.  Set to NULL to use a blank image.
#' @param w The time/column index of the cifti data to plot.
#' @param plane If brainstructure is 'subcortical' and papaya=FALSE, the plane to display.  Default is 'axial'. Other options are 'sagittal' and 'coronal'.
#' @param num.slices If brainstructure is 'subcortical' and papaya=FALSE, the number of slices to display.  Default is 'axial'. Other options are 'sagittal' and 'coronal'.
#' @param use_papaya If brainstructure is 'subcortical', papaya=TRUE will use papayar to allows for interactive visualization.
#'
#' @export
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @importFrom oro.nifti overlay readNIfTI
#' @importFrom stats quantile
#'
cifti_view <- function(cifti, surface=NULL,
  colors=NULL, color_mode=c("sequential", "qualitative", "diverging"), color_values=NULL,
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
    if(do_left) values_left <- apply(matrix(cifti$CORTEX_LEFT[,w][cifti$SURF_LEFT$surface$faces], ncol=3), 1, mean)
    if(do_right) values_right <- apply(matrix(cifti$CORTEX_RIGHT[,w][cifti$SURF_RIGHT$surface$faces], ncol=3), 1, mean)
    nvox_left <- length(values_left)
    nvox_right <- length(values_right)
    if(brainstructure=='surface') values <- c(values_left, values_right)
    if(brainstructure=='left') values <- values_left
    if(brainstructure=='right') values <- values_right

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
      pal <- expand_color_pal(pal_base, MIN_COLOR_RES=60)
    } else {
      pal <- pal_base
    }

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
                          meshColor = "faces")
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
                           meshColor = "faces")    
    }

    par3d(windowRect = c(20, 30, 1200, 700))
    if(do_left){
      shade3d(addNormals(plt_left), col=cols_left, specular="black", legend=TRUE)
    }
    if(do_right){
      shade3d(addNormals(plt_right), col=cols_right, specular="black", legend=TRUE)
    }
    legend3d("topleft", legend=round(pal_base$value), col=as.character(pal_base$color), pch=16, cex=2.5, pt.cex=2.5, inset=.02)
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
