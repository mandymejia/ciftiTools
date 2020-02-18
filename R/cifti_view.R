#' Visualize cifti brain data
#'
#' @param cifti Object of class 'cifti'. See \code{help(cifti_read_separate)}.
#' @param (Optional) z_min Lower limit of color scale for values
#' @param (Optional) z_max Upper limit of color scale for values
#' @param (Optional) colors Vector of colors for color scale
#' @param brainstructure 'left', 'right', 'surface' or 'subcortical'.
#' @param mesh_left If brainstructure is 'left' or 'surface', an inla.mesh object for the left hemisphere.  (Provide mesh_left OR vertices_left and faces_left, but not both.)
#' @param vertices_left If brainstructure is 'left' or 'surface', the Vx3 matrix of surface vertices for the left hemisphere. (Provide mesh_left OR vertices_left and faces_left, but not both.)
#' @param faces_left  If brainstructure is 'left' or 'surface', the Wx3 matrix of triangle membership for the left surface vertices. (Provide mesh_left OR vertices_left and faces_left, but not both.)
#' @param mesh_right If brainstructure is 'right' or 'surface', an inla.mesh object for the right hemisphere. (Provide mesh_right OR vertices_right and faces_right, but not both.)
#' @param vertices_right If brainstructure is 'right' or 'surface', the Vx3 matrix of surface vertices for the right hemisphere.(Provide mesh_right OR vertices_right and faces_right, but not both.)
#' @param faces_right  If brainstructure is 'right' or 'surface', the Wx3 matrix of triangle membership for the right surface vertices. (Provide mesh_right OR vertices_right and faces_right, but not both.)
#' @param structural_img If brainstructure is 'subcortical', the file name of the structural MRI image on which to overlay the subcortical values.  The MNI template is used by default.  Set to NULL to use a blank image.
#' @param w The time/column index of the cifti data to plot.
#' @param plane If brainstructure is 'subcortical', the plane to display.  Default is 'axial'. Other options are 'sagittal' and 'coronal'.
#' @param num.slices If brainstructure is 'subcortical', the number of slices to display.  Default is 'axial'. Other options are 'sagittal' and 'coronal'.
#' @param use_papaya If brainstructure is 'subcortical', papaya=TRUE will use papayar to allows for interactive visualization.
#'
#' @export
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @importFrom INLA inla.mesh.create
#' @importFrom INLA plot.inla.mesh
#' @importFrom oro.nifti overlay
#' @import papayar
#'
cifti_view <- function(cifti, z_min=NULL, z_max=NULL, colors=NULL, brainstructure, mesh_left=NULL, vertices_left=NULL, faces_left=NULL, mesh_right=NULL, vertices_right=NULL, faces_right=NULL, structural_img='MNI', w=1, plane='axial', num.slices=12, use_papaya=FALSE){

  if(brainstructure %in% c('left','right','surface')){

    values_left <- cifti$CORTEX_LEFT[,w]
    nvox_left <- length(values_left)
    values_right <- cifti$CORTEX_RIGHT[,w]
    nvox_right <- length(values_right)
    if(brainstructure=='surface') values <- c(values_left, values_right)
    if(brainstructure=='left') values <- values_left
    if(brainstructure=='right') values <- values_right

    #assign colors to vertices based on intensity values
    nColors <- 64
    #pal <- viridis_pal()(nColors)
    if(is.null(colors)) colors <- c('aquamarine','green','purple','blue','black','darkred','red','orange','yellow')
    pal <- colorRampPalette(colors)(nColors)
    if(!is.null(z_min)) values[values < z_min] <- z_min else z_min <- min(values, na.rm=TRUE)
    if(!is.null(z_max)) values[values > z_max] <- z_max else z_max <- max(values, na.rm=TRUE)
    breaks <- quantile(values[(values > z_min) & (values < z_max)],
                       probs = seq(0,1,length.out=nColors), na.rm=TRUE)
    colindex <- as.integer(cut(values,breaks=breaks))
    if(brainstructure=='surface') {
      colindex_left <- colindex[1:nvox_left]
      colindex_right <- colindex[(nvox_left+1):(nvox_left+nvox_right)]
    }

    #construct and plot mesh object
    if(brainstructure %in% c('left','surface')) mesh_left <- inla.mesh.create(loc=vertices_left, tv=faces_left)
    if(brainstructure %in% c('right','surface')) mesh_right <- inla.mesh.create(loc=vertices_right, tv=faces_right)

    if(brainstructure=='left') plot.inla.mesh(mesh_left, rgl=TRUE, col=pal[colindex])
    if(brainstructure=='right') plot.inla.mesh(mesh_right, rgl=TRUE, col=pal[colindex])
    if(brainstructure=='surface') {
      plot.inla.mesh(mesh_left, rgl=TRUE, col=pal[colindex_left])
      plot.inla.mesh(mesh_right, rgl=TRUE, col=pal[colindex_left], add=TRUE)
    }
  }

  if(brainstructure=='subcortical'){

    #pick slices with a lot of subcortical voxels
    labs <- cifti$LABELS
    mask <- (labs > 0)
    if(plane=='axial') mask_count <- apply(mask, 3, sum)
    if(plane=='coronal') mask_count <- apply(mask, 2, sum)
    if(plane=='sagittal') mask_count <- apply(mask, 1, sum)

    slices <- which(mask_count > max(mask_count)/2)
    inds <- round(seq(1,length(slices), length.out=num.slices))
    slices <- slices[inds]

    if(is.null(structural_img)) {
      T1w <- NULL
    } else if(structural_img=='T1w') {
      T1w <- readNIfTI(system.file('extdata/MNI152_T1_2mm.nii', package='BayesfMRI'), reorient=FALSE)
    } else {
      T1w <- readNIfTI(structural_img, reorient=FALSE)
    }

    img_overlay <- T1w*0
    img_overlay@.Data <- cifti$VOL[,,,w]
    img_overlay[img_overlay==0] <- NA

    if(use_papaya==FALSE) oro.nifti::overlay(x=T1w, y=img_overlay, plot.type='single', plane=plane, z=slices)
    if(use_papaya==TRUE) papaya(T1w, img_overlay)

  }

}
