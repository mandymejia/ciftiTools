#' Read a CIFTI File Quickly
#'
#' @description Read a CIFTI file by exporting it as a single GIFTI 
#'  using \code{-cifti-convert -to-gifti-ext} (\code{\link{read_cifti_flat}}), 
#'  and obtaining the brainordinate mapping using 
#'  \code{-cifti-export-dense-mapping} (\code{\link{info_cifti}}). 
#'
#' @inheritParams cifti_fname_Param
#' @inheritParams surfL_fname_Param
#' @inheritParams surfR_fname_Param
#' @inheritParams brainstructures_Param_LR
#' @inheritParams wb_path_Param
#' @inheritParams verbose_Param_FALSE
#' @param ... Additional arguments to \code{read_cifti_flat}.
#'
#' @return A \code{"xifti"} object. See \code{\link{is.xifti}}.
#'
#' @details This function uses a system wrapper for the "wb_command"
#'  executable. The user must first download and install the Connectome
#'  Workbench, available from
#'  \url{https://www.humanconnectome.org/software/get-connectome-workbench}.
#'  The \code{wb_path} argument is the path to the Connectime Workbench folder
#'  or executable.
#'
#' @keywords internal
#' 
read_cifti_convert <- function(
  cifti_fname, 
  surfL_fname=NULL, surfR_fname=NULL,
  brainstructures=c("left","right"), 
  wb_path=NULL, verbose=FALSE, ...){

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
  xifti$meta <- info_cifti(cifti_fname, wb_path)
  if (!all(brainstructures %in% xifti$meta$cifti$brainstructures)) {
    stop(paste0(
      "Only the following brainstructures are present in the CIFTI file:",
      paste(xifti$meta$cifti$brainstructures, collapse=", ")
    ))
  }

  # Read the CIFTI data.
  xifti_data <- read_cifti_flat(cifti_fname, wb_path=wb_path, ...)

  # if (!is.null(xifti$meta$cifti$intent) && xifti$meta$cifti$intent == 3007) {
  #   xifti_data <- xifti_data + 1
  # }

  # Place cortex data into the "xifti" object.
  if (xifti$meta$cifti$intent == 3007) {
    # Label values begin at zero. In example file, it indicates "???"
    mwall_values <- c(NaN, NA)
  } else {
    mwall_values <- c(0, NaN, NA)
  }
  last_left <- sum(xifti$meta$cortex$medial_wall_mask$left)
  last_right <- last_left + sum(xifti$meta$cortex$medial_wall_mask$right)
  if ("left" %in% brainstructures) {
    cortex <- make_cortex(
      xifti_data[1:last_left,, drop=FALSE],
      side = "left", #cortex_is_masked=TRUE,
      mwall = xifti$meta$cortex$medial_wall_mask$left,
      mwall_source="the CIFTI being read in",
      mwall_values=mwall_values
    )
    xifti$data$cortex_left <- cortex$data
    xifti$meta$cortex$medial_wall_mask["left"] <- list(cortex$mwall)
  } else {
    xifti$meta$cortex$medial_wall_mask["left"] <- list(template_xifti()$meta$cortex$medial_wall_mask$left)
  }
  if ("right" %in% brainstructures) {
    cortex <- make_cortex(
      xifti_data[(1+last_left):last_right,, drop=FALSE],
      side = "right", #cortex_is_masked=TRUE,
      mwall = xifti$meta$cortex$medial_wall_mask$right,
      mwall_source="the CIFTI being read in",
      mwall_values=mwall_values
    )
    xifti$data$cortex_right <- cortex$data
    xifti$meta$cortex$medial_wall_mask["right"] <- list(cortex$mwall)
  } else {
    xifti$meta$cortex$medial_wall_mask["right"] <- list(template_xifti()$meta$cortex$medial_wall_mask$right)
  }

  # Place subcortical data into the "xifti" object.
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
    xifti$surf$cortex_left <- make_surf(surfL_fname) 
  }
  if (!is.null(surfR_fname)) { 
    xifti$surf$cortex_right <- make_surf(surfR_fname) 
  }

  # Finish.
  if (!is.xifti(xifti)) { stop("The \"xifti\" object was invalid.") }

  if (verbose) {
    print(Sys.time() - exec_time)
    exec_time <- Sys.time()
  }

  structure(xifti, class="xifti")
}