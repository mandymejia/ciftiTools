#' Summarize cifti objects
#'
#' Summary method for class \code{"xifti"}
#'
#' @param object Object of class \code{"xifti"}. 
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
  names(out$includes) <- c("left cortex", "right cortex", "subcortex", "left surface", "right surface")
  if (out$includes["left cortex"]) out$cortex_left <- dim(object$data$cortex_left)
  if (out$includes["right cortex"]) out$cortex_right <- dim(object$data$cortex_right)
  if (out$includes["left surface"]) out$surf_left <- TRUE
  if (out$includes["right surface"]) out$surf_right <- TRUE
  if (out$includes["subcortex"]){
    out$subcort <- list()
    out$subcort$dat <- dim(object$data$subcort)
    out$subcort$labels <- table(object$meta$subcort$labels)
    out$subcort$mask <- dim(object$meta$subcort$mask)
  }
  out$intent <- object$meta$cifti$intent
  return(out)
}

#' @rdname summary.xifti
#' @export
#' 
#' @inheritParams x_Param_xifti
#' @method print summary.xifti
print.summary.xifti <- function(x, ...) {
  cat("Brain Structures:", paste(names(x$includes)[x$includes], collapse=", "), " \n")

  # [TO DO]: mention medial wall count?

  if (x$includes["left cortex"]) {
    cat("\tleft cortex:", x$cortex_left[1], "surface vertices,", 
      x$cortex_left[2], "measurements.\n")
    if (x$includes["left surface"]) cat("\t\tleft surface model is present.\n")
  }

  if (x$includes["right cortex"]) {
    cat("\tright cortex:", x$cortex_right[1], "surface vertices,", 
      x$cortex_right[2], "measurements.\n")
    if (x$includes["right surface"]) cat("\t\tright surface model is present.\n")
  }

  if (x$includes["subcortex"]) {
    cat("\tsubcortex:", x$subcort$dat[[1]], "voxels,",
      x$subcort$dat[[2]], "measurements.\n")
    cat("\t\tsubcortical labels:\n")
    print(x$subcort$labels)
  }

  if (!is.null(x$intent)) {
    cat(paste0(
      "\tintent: ", x$intent, " (", 
      c("dtseries", "dscalar", "dlabel")[match(x$intent, c(3002, 3006, 3007))], 
      ").\n"
    ))
  }

  cat("\n")
}

#' @rdname summary.xifti
#' @export
#' 
#' @method print xifti
print.xifti <- function(x, ...) {
  print.summary.xifti(summary(x))
}

#' Get CIFTI file extension
#'
#' @inheritParams cifti_fname_Param
#'
#' @return Character file extension of CIFTI file, e.g. "dtseries.nii", "dscalar.nii"
#'
#' @keywords internal
#' 
get_cifti_extn <- function(cifti_fname) {
  fname_parts <- unlist(strsplit(basename(cifti_fname), split=".", fixed = TRUE)) #split by "."
  extn <- paste(rev(fname_parts)[c(2,1)], collapse=".") #'dtseries.nii", "dscalar.nii", etc.
  extn
}

#' Get CIFTI component suffix default
#' 
#' Get the default file name suffix for a certain type of GIFTI/NIFTI file
#'
#' @param label the file type: one of 
#'  \code{"cortexL"}, \code{"cortexR"}, 
#'  \code{"subcortVol"}, \code{"subcortLabs"},
#'  \code{"ROIcortexL"}, \code{"ROIcortexR"}, 
#'  \code{"ROIsubcortVol"},
#'  \code{"validROIcortexL"}, or \code{"validROIcortexR"}.
#' @param GIFTI_type Used to make the suffix. Default: \code{"func"}.
#'
#' @return The default file name suffix
#'
#' @keywords internal
#' 
cifti_component_suffix <- function(label, GIFTI_type="func") {
  label <- match.arg(label, c(
    "cortexL", "cortexR", "subcortVol", "subcortLabs",
    "ROIcortexL", "ROIcortexR", "ROIsubcortVol"
  ))
  switch(label,
    cortexL = paste0("L.", GIFTI_type, ".gii"),
    cortexR = paste0("R.", GIFTI_type, ".gii"),
    subcortVol = "nii",
    subcortLabs = "labels.nii",
    ROIcortexL = paste0("ROI_L.", GIFTI_type, ".gii"),
    ROIcortexR = paste0("ROI_R.", GIFTI_type, ".gii"),
    ROIsubcortVol = "ROI.nii"
  )
}

#' Get resampled file name default
#' 
#' Get the default file name for a resampled file.
#'
#' @param original_fname The original file name
#' @inheritParams resamp_res_Param_required
#'
#' @return The default file name
#'
#' @keywords internal
#' 
resample_cifti_default_fname <- function(original_fname, resamp_res) {
  stopifnot(!is.null(original_fname))
  bname <- basename(original_fname)
  paste("resampled", round(resamp_res), bname, sep="_")
}

#' Unmask cortex
#' 
#' Get cortex data with medial wall vertices
#' 
#' @param cortex V vertices x T measurements matrix
#' @param mwall Logical vector with T \code{TRUE} values.
#' @param mwall_fill The fill value to use for medial wall vertices.
#' @return The unmasked cortex data
#' 
#' @export
#' 
unmask_cortex <- function(cortex, mwall, mwall_fill=NA) {
  cdat <- matrix(mwall_fill, nrow=length(mwall),  ncol=ncol(cortex))
  cdat[mwall,] <- cortex
  cdat
}

#' Counts the number of rows (vertices + voxels) in a \code{"xifti"}.
#' 
#' Counts the number of data locations in the \code{"xifti"} data. Doesn't bother
#'  to validate the input.
#' 
#' @param xifti The \code{"xifti"} object
#' @return The number of rows in the \code{"xifti"} data.
#' 
#' @keywords internal
#' 
nrow_xifti <- function(xifti) {
  # This function is generic because nrow(xii) will actually use dim(xii)[1]
  #   which calls this function.
  bs_present <- !vapply(xifti$data, is.null, FALSE)
  if (!any(bs_present)) { 
    return(0)
  } else {
    return(sum(vapply(xifti$data[bs_present], nrow, 0)))
  }
}

#' Counts the number of columns in a \code{"xifti"}.
#' 
#' Counts the number of columns in the \code{"xifti"} data. Doesn't bother
#'  to validate the input.
#' 
#' @param xifti The \code{"xifti"} object
#' @return The number of columns in the \code{"xifti"} data.
#' 
#' @keywords internal
#' 
ncol_xifti <- function(xifti) {
  bs_present <- !vapply(xifti$data, is.null, FALSE)
  if (!any(bs_present)) { 
    return(0)
  } else {
    return(ncol(xifti$data[[which(bs_present)[1]]]))
  }
}

#' Dimensions of a \code{"xifti"}
#' 
#' Returns the number of rows and columns in the \code{"xifti"} data. Doesn't
#'  bother to validate the input.
#' 
#' @param x The \code{"xifti"} object
#' @return The number of rows and columns in the \code{"xifti"} data.
#' 
#' @export 
#' @method dim xifti
dim.xifti <- function(x) {
  c(nrow_xifti(x), ncol_xifti(x))
}