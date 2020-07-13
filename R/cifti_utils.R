#' Summarise cifti objects
#'
#' Summary method for class "cifti"
#'
#' @param object an object of class "cifti"
#' @param ... further arguments passed to or from other methods.
#' @export
#' @method summary cifti
summary.cifti <- function(object, ...) {
  out <- list()
  class(out) <- "summary.cifti"
  out$includes <- names(object)[!sapply(object, is.null)]
  if ("VOL" %in% out$includes) out$includes <- c(out$includes[!(out$includes %in% c("VOL","LABELS"))],"SUBCORTICAL")
  if ("CORTEX_LEFT" %in% out$includes) out$CORTEX_LEFT <- dim(object$CORTEX_LEFT)
  if ("CORTEX_RIGHT" %in% out$includes) out$CORTEX_RIGHT <- dim(object$CORTEX_RIGHT)
  if ("SURF_LEFT" %in% out$includes) out$SURF_LEFT <- names(out$SURF_LEFT)
  if ("SURF_RIGHT" %in% out$includes) out$SURF_RIGHT <- names(out$SURF_RIGHT)
  if ("SUBCORTICAL" %in% out$includes) out$VOL <- list(dim_vol=dim(object$VOL), nvox_vol=sum(object$LABELS>0))
  if ("SUBCORTICAL" %in% out$includes) out$LABELS <- table(object$LABELS[object$LABELS>0])
  return(out)
}

#' @param x an object of class "summary.cifti"
#' @export
#' @method print summary.cifti
#' @rdname summary.cifti
print.summary.cifti <- function(x, ...) {
  cat("Brain Structures: ", paste(x$includes, collapse=", "), " \n")
  if ("CORTEX_LEFT" %in% x$includes) cat("Left Cortex: ", x$CORTEX_LEFT[1], "surface vertices, ", x$CORTEX_LEFT[2], "measurements \n")
  if ("CORTEX_RIGHT" %in% x$includes) cat("Right Cortex: ", x$CORTEX_RIGHT[1], "surface vertices, ", x$CORTEX_RIGHT[2], "measurements \n")
  if ("SURF_LEFT" %in% x$includes) cat("Left Surface Model is present.")
  if ("SURF_RIGHT" %in% x$includes) cat("Right Surface Model is present.")
  if ("SUBCORTICAL" %in% x$includes) {
    cat("Subcortical: ", x$VOL[[2]], "voxels, ", x$VOL[[1]][4], "measurements \n")
    cat("Subcortical Labels:")
    print(x$LABELS)
  }
}

#' @export
#' @method print cifti
#' @rdname summary.cifti
print.cifti <- function(x, ...) {
  print.summary.cifti(summary(x))
}

#' Get CIFTI file extension
#'
#' @param fname_cifti Path to CIFTI file, including full file name and extension
#'
#' @return Character file extension of CIFTI file, e.g. "dtseries.nii", "dscalar.nii".
#' @export
#'
get_cifti_extn <- function(fname_cifti) {
  fname_cifti <- basename(fname_cifti)
  fname_parts <- unlist(strsplit(fname_cifti, split=".", fixed = TRUE)) #split by "."
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
cifti_separate_default_suffix <- function(label, GIFTI_type="func") {
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
#' @inheritParams resamp_res_Param
#'
#' @return The default file name.
#'
original_to_target_fname <- function(original_fname, resamp_res) {
  bname <- basename(original_fname)
  paste("resampled", round(resamp_res), bname, sep="_")
}