#' Visualize cifti brain data
#'
#' @param cifti Object of class 'cifti'. See \code{help(cifti_read_separate)}, \code{help(cifti_make)}, and \code{help(is.cifti)}.
#' @param surface Name of brain surface model to use.  Must equal one of the names of cifti$SURF_LEFT (or equivalently, cifti$SURF_RIGHT). If NULL, first surface will be used.
#' @param z_lim (Optional) Lower and upper limits of values for color scale. Use -Inf for lower limit or Inf for upper limit to use the data value bounds.
#' @param colors (Optional) Vector of colors for color scale
#' @param brainstructure 'left', 'right', 'surface' or 'subcortical'.
#' @param structural_img If brainstructure is 'subcortical', the file name of the structural MRI image on which to overlay the subcortical values.  The MNI template is used by default.  Set to NULL to use a blank image.
#' @param w The time/column index of the cifti data to plot.
#' @param plane If brainstructure is 'subcortical' and papaya=FALSE, the plane to display.  Default is 'axial'. Other options are 'sagittal' and 'coronal'.
#' @param num.slices If brainstructure is 'subcortical' and papaya=FALSE, the number of slices to display.  Default is 'axial'. Other options are 'sagittal' and 'coronal'.
#' @param use_papaya If brainstructure is 'subcortical', papaya=TRUE will use papayar to allows for interactive visualization.
#'
#' @export
#' @importFrom grDevices colorRampPalette
#' @importFrom oro.nifti overlay readNIfTI
#' @importFrom stats quantile
#'
cifti_view <- function(cifti, surface=NULL, z_lim=NULL, colors=c('aquamarine','green','purple','blue','black','darkred','red','orange','yellow'), brainstructure, structural_img='MNI', w=1, plane='axial', num.slices=12, use_papaya=FALSE){

  if(!is.cifti(cifti)) stop('cifti argument is not a valid cifti object. See is.cifti().')

  nColors <- 60
  #pal <- viridis_pal()(nColors)
  pal <- colorRampPalette(colors)(nColors)

  do_left <- (brainstructure %in% c('left','surface'))
  do_right <- (brainstructure %in% c('right','surface'))

  if(do_left | do_right){

    if (!requireNamespace("INLA", quietly = TRUE)) {
      stop("Package \"INLA\" needed for this function to work. Please install it from http://www.r-inla.org/download.", call. = FALSE)
    }

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
    if(do_left) values_left <- cifti$CORTEX_LEFT[,w]
    if(do_right) values_right <- cifti$CORTEX_RIGHT[,w]
    nvox_left <- length(values_left)
    nvox_right <- length(values_right)
    if(brainstructure=='surface') values <- c(values_left, values_right)
    if(brainstructure=='left') values <- values_left
    if(brainstructure=='right') values <- values_right

    #assign colors to vertices based on intensity values
    if(sum(is.na(z_lim))>0) stop('z_lim must not contains NAs')
    if(!is.null(z_lim)){
      z_min <- z_lim[1]
      z_max <- z_lim[2]
    } else {
      z_min <- -Inf
      z_max <- Inf
    }
    values[values < z_min] <- z_min
    values[values > z_max] <- z_max
    if(z_min == -Inf) z_min <- min(values, na.rm=TRUE)
    if(z_max == Inf) z_max <- max(values, na.rm=TRUE)

    # breaks <- quantile(values[(values > z_min) & (values < z_max)],
    #                    probs = seq(0,1,length.out=nColors), na.rm=TRUE)
    if(z_min >= z_max) stop('Invalid value range for color scale. Check that z_lim is valid and compatible with the range of observed values.')
    breaks <- seq(z_min, z_max, length.out=nColors)
    colindex <- as.integer(cut(values,breaks=breaks))
    if(brainstructure=='surface') {
      colindex_left <- colindex[1:nvox_left]
      colindex_right <- colindex[(nvox_left+1):(nvox_left+nvox_right)]
    }

    #construct and plot mesh object
    if(do_left){
      if(is.null(cifti$SURF_LEFT)) stop("If brainstructure is 'left' or 'surface', cifti$SURF_LEFT must not be NULL.")
      if(is.null(surface)){
        surf_left <- cifti$SURF_LEFT[[1]]
      } else {
        if(!(surface %in% names(cifti$SURF_LEFT))) stop('If surface is provided, it must be one of the names of cifti$SURF_LEFT.')
        surf_left <- cifti$SURF_LEFT[names(cifti$SURF_LEFT) == surface]
      }
      mesh_left <- INLA::inla.mesh.create(loc=surf_left$vertices, tv=surf_left$faces)
    }


    if(do_right){
      if(is.null(cifti$SURF_RIGHT)) stop("If brainstructure is 'right' or 'surface', cifti$SURF_RIGHT must not be NULL.")
      if(is.null(surface)){
        surf_right <- cifti$SURF_RIGHT[[1]]
      } else {
        if(!(surface %in% names(cifti$SURF_RIGHT))) stop('If surface is provided, it must be one of the names of cifti$SURF_RIGHT.')
        surf_right <- cifti$SURF_RIGHT[names(cifti$SURF_RIGHT) == surface]
      }
      mesh_right <- INLA::inla.mesh.create(loc=surf_right$vertices, tv=surf_right$faces)
    }

    if(brainstructure=='left') INLA::plot.inla.mesh(mesh_left, rgl=TRUE, col=pal[colindex], draw.edges=FALSE)
    if(brainstructure=='right') INLA::plot.inla.mesh(mesh_right, rgl=TRUE, col=pal[colindex], draw.edges=FALSE)
    if(brainstructure=='surface') {
      INLA::plot.inla.mesh(mesh_left, rgl=TRUE, col=pal[colindex_left])
      INLA::plot.inla.mesh(mesh_right, rgl=TRUE, col=pal[colindex_left], add=TRUE)
    }
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

    if(use_papaya==FALSE) oro.nifti::overlay(x=T1w, y=img_overlay, plot.type='single', plane=plane, z=slices, col.y=pal)
    if(use_papaya==TRUE) papayar::papaya(list(T1w, img_overlay, img_labels))

  }

}
