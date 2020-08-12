#' Summarise cifti objects
#'
#' Summary method for class "xifti"
#'
#' @param object Object of class "xifti". 
#'  See \code{\link{is.xifti}} and \code{\link{make_xifti}}.
#' @param ... further arguments passed to or from other methods.
#' @export
#' @method summary xifti
summary.xifti <- function(object, ...) {
  out <- list()
  class(out) <- "summary.xifti"
  out$includes <- c(
    !is.null(object$data$cortex_left),
    !is.null(object$data$cortex_right),
    !is.null(object$data$subcort),
    !is.null(object$surf$cortex_left),
    !is.null(object$surf$cortex_right)
  )
  names(out$includes) <- c("left cortex", "right cortex", "subcortex", "left geometry", "right geometry")
  if (out$includes["left cortex"]) out$cortex_left <- dim(object$data$cortex_left)
  if (out$includes["right cortex"]) out$cortex_right <- dim(object$data$cortex_right)
  if (out$includes["left geometry"]) out$surf_left <- TRUE
  if (out$includes["right geometry"]) out$surf_right <- TRUE
  if (out$includes["subcortex"]){
    out$subcort <- list()
    out$subcort$dat <- dim(object$data$subcort)
    out$subcort$labels <- table(object$meta$subcort$labels)
    out$subcort$mask <- dim(object$meta$subcort$mask)
  }
  return(out)
}

#' @inheritParams x_Param_xifti
#' @export
#' @method print summary.xifti
#' @rdname summary.xifti
print.summary.xifti <- function(x, ...) {
  cat("Brain Structures:", paste(names(x$includes)[x$includes], collapse=", "), " \n")

  if (x$includes["left cortex"]) {
    cat("left cortex:", x$cortex_left[1], "surface vertices,", 
      x$cortex_left[2], "measurements.\n")
    if (x$includes["left geometry"]) cat("\tleft surface model is present.\n")
  }

  if (x$includes["right cortex"]) {
    cat("right cortex:", x$cortex_right[1], "surface vertices,", 
      x$cortex_right[2], "measurements.\n")
    if (x$includes["right geometry"]) cat("\tright surface model is present.\n")
  }

  if (x$includes["subcortex"]) {
    cat("subcortical:", x$subcort$dat[[1]], "voxels,",
      x$subcort$dat[[2]], "measurements.\n")
    cat("subcortical labels:\n")
    print(x$subcort$labels)
  }
}

#' @export
#' @method print xifti
#' @rdname summary.xifti
print.xifti <- function(x, ...) {
  print.summary.xifti(summary(x))
}

#' Get CIFTI file extension
#'
#' @inheritParams cifti_fname_Param
#'
#' @return Character file extension of CIFTI file, e.g. "dtseries.nii", "dscalar.nii".
#'
#' @keywords internal
#' 
get_cifti_extn <- function(cifti_fname) {
  fname_parts <- unlist(strsplit(basename(cifti_fname), split=".", fixed = TRUE)) #split by "."
  extn <- paste(rev(fname_parts)[c(2,1)], collapse=".") #'dtseries.nii", "dscalar.nii", etc.
}

#' Get the default file name suffix for a certain type of GIFTI/NIFTI file
#'
#' @param label the file type: one of 
#'  \code{"cortexL"}, \code{"cortexR"}, 
#'  \code{"subcortVol"}, \code{"subcortLab"},
#'  \code{"ROIcortexL"}, \code{"ROIcortexR"}, 
#'  \code{"ROIsubcortVol"},
#'  \code{"validROIcortexL"}, or \code{"validROIcortexR"}.
#' @param GIFTI_type Used to make the suffix. Default: \code{"func"}.
#'
#' @return The default file name suffix.
#'
#' @keywords internal
#' 
cifti_component_suffix <- function(label, GIFTI_type="func") {
  label <- match.arg(label, c(
    "cortexL", "cortexR", "subcortVol", "subcortLab",
    "ROIcortexL", "ROIcortexR", "ROIsubcortVol", 
    "validROIcortexL", "validROIcortexR"
  ))
  switch(label,
    cortexL = paste0("L.", GIFTI_type, ".gii"),
    cortexR = paste0("R.", GIFTI_type, ".gii"),
    subcortVol = "nii",
    subcortLab = "labels.nii",
    ROIcortexL = paste0("ROI_L.", GIFTI_type, ".gii"),
    ROIcortexR = paste0("ROI_R.", GIFTI_type, ".gii"),
    ROIsubcortVol = "ROI.nii",
    validROIcortexL = paste0("valid_ROI_L.", GIFTI_type, ".gii"),
    validROIcortexR = paste0("valid_ROI_R.", GIFTI_type, ".gii")
  )
}

#' Get the default file name for a resampled file.
#'
#' @param original_fname The original file name
#' @inheritParams resamp_res_Param_required
#'
#' @return The default file name.
#'
#' @keywords internal
#' 
resample_cifti_default_fname <- function(original_fname, resamp_res) {
  stopifnot(!is.null(original_fname))
  bname <- basename(original_fname)
  paste("resampled", round(resamp_res), bname, sep="_")
}