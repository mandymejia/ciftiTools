#' Read a CIFTI file quickly
#'
#' Read a CIFTI file by exporting it as a single GIFTI 
#'  using \code{-cifti-convert -to-gifti-ext} (\code{\link{read_cifti_flat}}), 
#'  and obtaining the brainordinate mapping using 
#'  \code{-cifti-export-dense-mapping} (\code{\link{info_cifti}}). 
#' 
#' @inheritSection Connectome_Workbench_Description Connectome Workbench Requirement
#' 
#' @inheritParams cifti_fname_Param
#' @inheritParams surfL_fname_Param
#' @inheritParams surfR_fname_Param
#' @inheritParams brainstructures_Param_LR
#' @inheritParams idx_Param
#' @param mwall_values If the medial wall mask is not present in the CIFTI, 
#'  infer it from these data values. Default: \code{c(NA, NaN)}. If \code{NULL}, 
#'  do not attempt to infer the medial wall mask from the data values.
#' @inheritParams verbose_Param_FALSE
#' @param ... Additional arguments to \code{read_cifti_flat}.
#'
#' @return A \code{"xifti"} object. See \code{\link{is.xifti}}.
#' 
#' @keywords internal
#' 
read_cifti_convert <- function(
  cifti_fname, 
  surfL_fname=NULL, surfR_fname=NULL,
  brainstructures=c("left","right"), idx=NULL,
  mwall_values=c(NA, NaN), verbose=FALSE, ...){

  # Check arguments.
  brainstructures <- match_input(
    brainstructures, c("left","right","subcortical","all"),
    user_value_label="brainstructures"
  )
  if ("all" %in% brainstructures) { 
    brainstructures <- c("left","right","subcortical")
  }

  # Create the template.
  xifti <- template_xifti()

  # ----------------------------------------------------------------------------
  # Read files. ----------------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (verbose) { exec_time <- Sys.time() }
  if (verbose) { cat("Reading CIFTI file.\n") }

  # Read the CIFTI info.
  xifti$meta <- info_cifti(cifti_fname)
  bs_present <- brainstructures %in% xifti$meta$cifti$brainstructures
  if (!all(bs_present)) {
    warning(paste0(
      "Only the following brainstructures are present in the CIFTI file: ",
      paste(xifti$meta$cifti$brainstructures, collapse=", "), "\n"
    ))
    brainstructures <- brainstructures[bs_present]
  }

  # Select metadata columns if only some data columns will be read in.
  if (!is.null(idx)) {
    if (!is.null(xifti$meta$cifti$names)) {
      if (!all(idx %in% seq(length(xifti$meta$cifti$names)))) {
        stop(
          "`idx` is invalid: must be positive integer(s) indicating column(s) of ",
          "the `xifti`. There are ", length(xifti$meta$cifti$names), " columns."
        )
      }
      xifti$meta$cifti$names <- xifti$meta$cifti$names[idx]
    }
    if (!is.null(xifti$meta$cifti$labels)) {
      xifti$meta$cifti$labels <- xifti$meta$cifti$labels[idx]
    }
  }

  # Read the CIFTI data.
  xifti_data <- read_cifti_flat(cifti_fname, idx=idx, ...)
  if (get_cifti_extn(cifti_fname) == "dlabel.nii") {
    if (!all(round(xifti_data) == xifti_data)) {
      warning("The CIFTI file extension was \"dlabel.nii\" but the data values were not integers.")
    } else {
      mode(xifti_data) <- "integer"
    }
  }

  # Place cortex data into the \code{"xifti"} object.
  last_left <- sum(xifti$meta$cortex$medial_wall_mask$left)
  last_right <- last_left + sum(xifti$meta$cortex$medial_wall_mask$right)
  if ("left" %in% brainstructures) {
    cortex <- make_cortex(
      xifti_data[1:last_left,, drop=FALSE],
      side = "left", 
      mwall = xifti$meta$cortex$medial_wall_mask$left,
      mwall_values=mwall_values,
      mwall_source="the CIFTI being read in",
    )
    xifti$data$cortex_left <- cortex$data
    xifti$meta$cortex$medial_wall_mask["left"] <- list(cortex$mwall)
  } else {
    xifti$meta$cortex$medial_wall_mask["left"] <- list(template_xifti()$meta$cortex$medial_wall_mask$left)
  }
  if ("right" %in% brainstructures) {
    cortex <- make_cortex(
      xifti_data[(1+last_left):last_right,, drop=FALSE],
      side = "right",
      mwall = xifti$meta$cortex$medial_wall_mask$right,
      mwall_values=mwall_values,
      mwall_source="the CIFTI being read in",
    )
    xifti$data$cortex_right <- cortex$data
    xifti$meta$cortex$medial_wall_mask["right"] <- list(cortex$mwall)
  } else {
    xifti$meta$cortex$medial_wall_mask["right"] <- list(template_xifti()$meta$cortex$medial_wall_mask$right)
  }

  # Place subcortical data into the \code{"xifti"} object.
  if ("subcortical" %in% brainstructures) {
    alpha_to_spatial <- order(order(xifti$meta$subcort$labels))
    subcort_order <- c((1+last_right):nrow(xifti_data))[alpha_to_spatial]
    xifti$data$subcort <- xifti_data[subcort_order,, drop=FALSE]
  } else {
    xifti$meta$subcort <- template_xifti()$meta$subcort
  }

  # Read surfaces.
  if (!is.null(surfL_fname) | !is.null(surfR_fname)) { 
    if(verbose) { cat("...and surface(s).\n") }
  }
  if (!is.null(surfL_fname)) { 
    xifti$surf$cortex_left <- make_surf(surfL_fname, "left") 
  }
  if (!is.null(surfR_fname)) { 
    xifti$surf$cortex_right <- make_surf(surfR_fname, "right") 
  }

  # Finish.
  if (!is.xifti(xifti)) { stop("The \"xifti\" object was invalid.") }

  if (verbose) {
    print(Sys.time() - exec_time)
    exec_time <- Sys.time()
  }

  structure(xifti, class="xifti")
}