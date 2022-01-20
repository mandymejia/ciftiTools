#' Summarize a \code{"xifti"} object
#'
#' Summary method for class \code{"xifti"}
#'
#' @param object Object of class \code{"xifti"}. 
#' @param ... further arguments passed to or from other methods.
#' @export
#' @method summary xifti
summary.xifti <- function(object, ...) {

  x <- list(
    measurements = ncol(object),
    brainstructure = !vapply(object$data, is.null, FALSE),
    verts_per_bs = lapply(object$data, nrow),
    surf = !vapply(object$surf, is.null, FALSE),
    medial_wall_mask = lapply(object$meta$cortex$medial_wall_mask, table),
    subcort_labels = table(object$meta$subcort$labels),
    subcort_mask = dim(object$meta$subcort$mask),
    intent = object$meta$cifti$intent
  )

  # Re-name the brainstructures
  bs_nice <- c(
    cortex_left = "left cortex", 
    cortex_right = "right cortex", 
    subcort = "subcortex"
  )
  names(x$brainstructure) <- bs_nice[names(x$brainstructure)]
  names(x$verts_per_bs) <- bs_nice[names(x$verts_per_bs)]
  bs_nice <- c(
    cortex_left = "left", 
    cortex_right = "right"
  )
  names(x$surf) <- bs_nice[names(x$surf)]
  
  # Handle NULL/0/etc.
  x$verts_per_bs[vapply(x$vertx_per_bs, is.null, FALSE)] <- 0
  x["verts_per_bs"] <- list(unlist(x$verts_per_bs))
  x$medial_wall_mask[vapply(x$medial_wall_mask, dim, 0) < 1] <- NULL
  if (all(vapply(x$medial_wall_mask, is.null, FALSE))) {
    x["medial_wall_mask"] <- list(NULL)
  }
  if (dim(x$subcort_labels) < 1) {
    x["subcort_labels"] <- list(NULL)
  }

  # Add intent-specific entries
  if (!is.null(x$intent)) {
    if (x$intent == 3002) {
      x$time <- c(
        start = object$meta$cifti$time_start,
        step = object$meta$cifti$time_step,
        unit = object$meta$cifti$time_unit
      )
    } else {
      x$names <- object$meta$cifti$names
    }
  }

  class(x) <- "summary.xifti"

  return(x)
}

#' @rdname summary.xifti
#' @export
#' 
#' @inheritParams x_Param_xifti
#' @method print summary.xifti
print.summary.xifti <- function(x, ...) {

  cat("====CIFTI METADATA===================\n")

  if (!is.null(x$intent)) {
    cat(paste0(
      "Intent:           ", x$intent, " (", 
      c("dtseries", "dscalar", "dlabel")[match(x$intent, c(3002, 3006, 3007))], 
      ")\n"
    ))

    if (x$intent == 3002) {
      cat(paste0(
        "- time step       ", x$time["step"], " (", x$time["unit"], 
        ifelse(x$time["unit"]=="hertz", "", "s"), ")\n"
      ))
      cat(paste0(
        "- time start      ", x$time["start"], "\n"
      ))
    } else {
      few_names <- x$names[seq(min(5, length(x$names)))]
      cat(paste0(
        "- names           \"", paste0(few_names, collapse="\", \""), 
        ifelse(length(few_names) < length(x$names), "\", ... ", "\""), "\n"
      ))
    }
  }

  cat(paste0(
    "Measurements:     ", x$measurements, 
    " column", ifelse(x$measurements>1, "s", ""), "\n"
  ))

  cat("\n====BRAIN STRUCTURES=================\n")


  blank <- "                  "

  if (x$brainstructure["left cortex"] | x$surf["left"]) {
    cat(paste0(
      "- left cortex     ", x$verts_per_bs["left cortex"], " data vertices\n"
    ))

    if (!is.null(x$medial_wall_mask$left)) {
      cat(paste0(
        blank, x$medial_wall_mask$left["FALSE"], " medial wall vertices (",
        sum(x$medial_wall_mask$left), " total)\n"
      ))
    }
    if (x$surf["left"]) {
      cat(paste0(blank, "left surface geometry is present\n"))
    }
    cat("\n")
  }

  if (x$brainstructure["right cortex"] | x$surf["right"]) {
    cat(paste0(
      "- right cortex    ", x$verts_per_bs["right cortex"], " data vertices\n"
    ))

    if (!is.null(x$medial_wall_mask$right)) {
      cat(paste0(
        blank, x$medial_wall_mask$right["FALSE"], " medial wall vertices (",
        sum(x$medial_wall_mask$right), " total)\n"
      ))
    }
    if (x$surf["right"]) {
      cat(paste0(blank, "right surface geometry is present\n"))
    }
    cat("\n")
  }

  if (x$brainstructure["subcortex"]) {
    cat(paste0(
      "- subcortex       ", x$verts_per_bs["subcortex"], " data voxels\n"
    ))

    cat(paste0(blank, "subcortical structures and number of voxels in each:\n"))
    labs2 <- unique(gsub("-.*", "", names(x$subcort_labels)))
    for (ii in seq(length(labs2))) {
      labs2_ii <- labs2[ii]
      if (labs2_ii != "Brain Stem") { 
        labs2_ii <- paste0(labs2_ii, c("-L", "-R")) 
      }
      cat(paste0(
        blank, "  ", 
        paste(labs2_ii, x$subcort_labels[labs2_ii], sep= " (", collapse="), "),
        ")", ifelse(ii == length(labs2), ".", ",")
      ))
      cat("\n")
    }

    # n_labs <- length(x$subcort_labels)
    # n_per_row <- 3
    # q <- ceiling(n_labs / n_per_row)
    # for (ii in seq(q)) {
    #   labs_ii <- (ii - 1) * n_per_row + seq(n_per_row)
    #   labs_ii <- pmin(labs_ii, n_labs)
    #   labs_ii <- x$subcort_labels[labs_ii]
    #   cat(paste0(
    #     blank, paste(names(labs_ii), labs_ii, sep= " (", collapse="), "), 
    #     ")", ifelse(ii == q, ".", ",")
    #   ))
    #   cat("\n")
    # }

    cat("\n")
  }
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
#' @inheritParams xifti_Param
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
#' @inheritParams x_Param_xifti
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
#' Returns the number of rows (vertices + voxels) and columns (measurements) in 
#'  the \code{"xifti"} data. 
#' 
#' @inheritParams x_Param_xifti
#' @return The number of rows and columns in the \code{"xifti"} data.
#' 
#' @export 
#' @method dim xifti
dim.xifti <- function(x) {
  c(nrow_xifti(x), ncol_xifti(x))
}

#' Convert a \code{"xifti"} to a matrix
#' 
#' Converts a \code{"xifti"} to a matrix by concatenating the data from each
#'  brainstructure along the rows. Surfaces and metadata are discarded.
#' 
#' @inheritParams x_Param_xifti
#' @param ... Unused
#' @return The input as a matrix. Each brainstructure's data is concatenated.
#' 
#' @export
#' @method as.matrix xifti
as.matrix.xifti <- function(x, ...) {
  do.call(rbind, x$data)
}

#' Infer resolution from \code{"xifti"} and surfaces
#' 
#' @inheritParams xifti_Param
#' @param surfL Left surface
#' @param surfR Right surface
#' 
#' @export
#' 
#' @return The inferred resolution
#' 
infer_resolution <- function(xifti, surfL=NULL, surfR=NULL) {
  res <- NULL
  if (!is.null(xifti$meta$cortex$medial_wall_mask$left)) {
    res <- length(xifti$meta$cortex$medial_wall_mask$left)
  } else if (!is.null(xifti$meta$cortex$medial_wall_mask$right)) {
    res <- length(xifti$meta$cortex$medial_wall_mask$right)
  } else {
    if (!is.null(xifti$data$cortex_left) && !is.null(xifti$data$cortex_right)) {
      if (nrow(xifti$data$cortex_left) == nrow(xifti$data$cortex_right)) {
        res <- nrow(xifti$data$cortex_left)
      }
    } else if (!is.null(xifti$data$cortex_left) && !is.null(surfL)) {
      prop_mwall <- nrow(xifti$data$cortex_left) / nrow(surfL$vertices)
      if (prop_mwall<=1 && prop_mwall >.85) { res <- nrow(surfL$vertices) } else { res <- nrow(xifti$data$cortex_left) }
    } else if (!is.null(xifti$data$cortex_right) && !is.null(surfR)) {
      prop_mwall <- nrow(xifti$data$cortex_right) / nrow(surfR$vertices)
      if (prop_mwall<=1 && prop_mwall >.85) { res <- nrow(surfR$vertices) } else { res <- nrow(xifti$data$cortex_right) }
    }
    if (is.null(res)) {
      nvL <- ifelse(is.null(surfL), Inf, nrow(surfL$vertices))
      nvR <- ifelse(is.null(surfR), Inf, nrow(surfR$vertices))
      res <- min(nvL, nvR)
      if (is.infinite(res)) { res <- NULL }
    }
  }

  res
}