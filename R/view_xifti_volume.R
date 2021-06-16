#' View subcortex
#' 
#' Visualize subcortex of a \code{"xifti"} object
#'
#' @inheritParams xifti_Param
#' @param structural_img The structural MRI image on which to overlay the
#'  subcortical values. Can be a file name, \code{"MNI"} (default) to use
#'  the MNI T1-weighted template, or \code{NULL} to use a blank image.
#' @param idx The time/column index of the \code{"xifti"} data to plot. Must
#'  be a single index (length 1).
#' @param plane If interactive=FALSE, the plane to display.
#'  Default: \code{"axial"}. Other options are \code{"sagittal"} and 
#'  \code{"coronal"}.
#' @param num.slices If interactive=FALSE, the number of slices to display.
#'  Default: \code{9}.
#' @param interactive interactive=TRUE will use papayar to allows for interactive visualization.
#' @param zlim A length-2 numeric vector giving the minimum and maximum values to
#'  plot. Data values beyond this range will be truncated to the min/max. If
#'  \code{NULL} (default), will use the min and max of the data.
#'  
#' @inheritParams verbose_Param_TRUE
#' @param ... Additional arguments to pass to \code{papayar::papaya} or \code{oro.nifti::overlay}
#'
#' @export
#' @importFrom oro.nifti overlay readNIfTI as.nifti
view_xifti_volume <- function(
  xifti, structural_img="MNI", idx=1, plane=c("axial", "sagittal", "coronal"),
  num.slices=9, interactive=FALSE, zlim=NULL,
  verbose=TRUE, ...) {

  if (interactive) {
    if (!requireNamespace("papayar", quietly = TRUE)) {
      stop("Package \"papayar\" needed for this function to work. Please install it.",
           call. = FALSE)
    }
  }

  if (length(idx) > 1) {
    stop("Only one `idx` at a time is supported for `view_xifti_volume`.")
  }

  stopifnot(is.xifti(xifti))

  # Get volume and labels.
  values <- xifti$data$subcort[,idx]
  vol <- unmask_vol(values, xifti$meta$subcort$mask, fill=NA)
  labs <- unmask_vol(as.numeric(xifti$meta$subcort$labels), xifti$meta$subcort$mask, fill=0)

  # Pick slices with a lot of subcortical voxels.
  if (!interactive) {
    plane <- match.arg(plane, c("axial", "sagittal", "coronal"))
    if (plane=="axial") mask_count <- apply(xifti$meta$subcort$mask, 3, sum)
    if (plane=="coronal") mask_count <- apply(xifti$meta$subcort$mask, 2, sum)
    if (plane=="sagittal") mask_count <- apply(xifti$meta$subcort$mask, 1, sum)

    slices <- which(mask_count > max(mask_count)/2)
    inds <- round(seq(1,length(slices), length.out=num.slices))
    slices <- slices[inds]
  }

  if (!is.null(zlim)) {
    if (length(zlim) != 2) {
      stop("`zlim` must be a length 2 vector giving the min and max values to plot.")
    }
    if (zlim[1] > zlim[2]) {
      warning(
        "The first entry of zlim was greater than the second. ",
        "Using the first as the max and second as the min."
      )
      zlim <- rev(zlim)
    }
    values[values < zlim[1]] <- zlim[1]
    values[values > zlim[2]] <- zlim[2]
  }

  if (verbose) {
    cat(paste0(
      "Values to be plotted range from ",
      min(xifti$data$subcort[,idx])," to ",
      max(xifti$data$subcort[,idx]), ".\n"
    ))
  }

  if (!is.null(structural_img)) {
    if (structural_img=="MNI") {
      img_fname <- system.file("extdata/MNI152_T1_2mm.nii.gz", package="ciftiTools")
    } else if (is.fname(structural_img)){
      img_fname <- structural_img
    } else {
      stop(paste(
        "`structural_img` argument not one of:",
        "`NULL`, `\"MNI\"`, or an existing file."
      ))
    }
    img <- readNIfTI(img_fname, reorient=FALSE)

    # Check data dimensions.
    if (!all.equal(dim(img), dim(vol))) {
      stop(paste0(
        "The subcortical data in the CIFTI and the `structural_img` are of ",
        "different dimensions: (", paste(dim(img), collapse=", "), ") and (",
        paste(dim(vol), collapse=", "), ") respectively."
      ))
    }

    # Check data orientation alignment.
    # This uses the sform method (srow_x, srow_y, and srow_z), not qform
    #   or ANALYZE-based methods.
    # This is because the Connectome Workbench seems to export the 
    #   TransformationMatrixIJKtoXYZ as the sform transformation matrix
    #   in -cifti-separate. 
    img_trans_mat <- make_trans_mat(img_fname)
    xii_trans_mat <- xifti$meta$subcort$trans_mat
    if (!is.null(xii_trans_mat)) {
      if (!all(dim(img_trans_mat) == dim(xii_trans_mat))) {
        warning(paste(
          "`meta$subcort$trans_mat` has different dimensions than image",
          "trans_mat, i.e. `rbind(srow_x, srow_y, srow_z)`. This may indicate",
          "that the volumes are not aligned."
        ))
      } else {
        trans_mat_diff <- max(abs(as.numeric(img_trans_mat - xii_trans_mat)))
        if (max(trans_mat_diff > ciftiTools.getOption("EPS"))) {
          warning(paste(
            "`meta$subcort$trans_mat` has different values than the image",
            "trans_mat, i.e. `rbind(srow_x, srow_y, srow_z)`. This may indicate",
            "that the volumes are not aligned."
          ))
        }
      }
    }

    img_overlay <- img*0
    img_labels <- img*0
  } else {
    img <- oro.nifti::as.nifti(vol*0)
    img@.Data <- xifti$meta$subcort$mask
    img_overlay <- img_labels <- img
  }

  img_overlay@.Data <- vol
  img_overlay@.Data[labs==0] <- NA
  # Patch: if img_overlay@.Data is int, an error occurs.
  img_overlay@.Data <- img_overlay@.Data * 1.0
  img_labels@.Data <- labs
  img_labels@.Data[labs==0] <- NA

  if (!interactive) {
    if (plane=="axial") {
      img <- img[,,slices]
      img_overlay <- img_overlay[,,slices]
    } else if (plane=="coronal") {
      img <- img[,slices,]
      img_overlay <- img_overlay[,slices,]
    } else if (plane=="sagittal") {
      img <- img[slices,,]
      img_overlay <- img_overlay[slices,,]
    }
    oro.nifti::overlay(x=img, y=img_overlay, plane=plane, ...)
  } else {
    papayar::papaya(list(img, img_labels, img_overlay), ...)
  }
}

#' @rdname view_xifti_volume
#' @export
view_cifti_volume <- function(
  xifti, structural_img="MNI", idx=1, plane=c("axial", "sagittal", "coronal"),
  num.slices=9, interactive=FALSE, zlim=NULL,
  verbose=TRUE, ...) {

  view_xifti_volume(
    xifti=xifti,
    structural_img=structural_img,
    idx=idx, plane=plane,
    num.slices=num.slices,
    interactive=interactive,
    zlim=zlim,
    verbose=verbose, ...
  )
}

#' @rdname view_xifti_volume
#' @export
viewCIfTI_volume <- function(
  xifti, structural_img="MNI", idx=1, plane=c("axial", "sagittal", "coronal"),
  num.slices=9, interactive=FALSE, zlim=NULL,
  verbose=TRUE, ...) {

  view_xifti_volume(
    xifti=xifti,
    structural_img=structural_img,
    idx=idx, plane=plane,
    num.slices=num.slices,
    interactive=interactive,
    zlim=zlim,
    verbose=verbose, ...
  )
}

#' @rdname view_xifti_volume
#' @export
viewcii_volume <- function(
  xifti, structural_img="MNI", idx=1, plane=c("axial", "sagittal", "coronal"),
  num.slices=9, interactive=FALSE, zlim=NULL,
  verbose=TRUE, ...) {

  view_xifti_volume(
    xifti=xifti,
    structural_img=structural_img,
    idx=idx, plane=plane,
    num.slices=num.slices,
    interactive=interactive,
    zlim=zlim,
    verbose=verbose, ...
  )
}