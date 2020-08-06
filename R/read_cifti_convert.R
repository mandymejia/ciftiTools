#' Read a CIFTI File Quickly
#'
#' @description Read a CIFTI file by exporting it as a single GIFTI 
#'  using \code{-cifti-convert -to-gifti-ext} (\code{\link{read_cifti_flat}}), 
#'  and obtaining the brainordinate mapping using 
#'  \code{-cifti-export-dense-mapping} (\code{\link{map_cifti}}). 
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
#' @export
#'
#' @details This function uses a system wrapper for the "wb_command"
#'  executable. The user must first download and install the Connectome
#'  Workbench, available from
#'  \url{https://www.humanconnectome.org/software/get-connectome-workbench}.
#'  The \code{wb_path} argument is the path to the Connectime Workbench folder
#'  or executable.
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
  
  # Rename the brainstructures.
  brainstructures_rename <- list(
    left="cortex_left",
    right="cortex_right",
    subcortical="subcort"
  )
  brainstructures <- as.character(brainstructures_rename[brainstructures])

  # Create the template.
  xifti <- template_xifti()

  # ----------------------------------------------------------------------------
  # Read files. ----------------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (verbose) { exec_time <- Sys.time() }
  if (verbose) { cat("Reading CIFTI file.\n") }

  # Map the CIFTI.
  cifti_map <- map_cifti(cifti_fname, wb_path)

  # Read the CIFTI data.
  xifti_data <- read_cifti_flat(cifti_fname, wb_path=wb_path, ...)
  last_left <- sum(cifti_map$cortex$medial_wall_mask$left)
  last_right <- last_left + sum(cifti_map$cortex$medial_wall_mask$right)

  # Place into the xifti object.
  if ("cortex_left" %in% brainstructures) {
    xifti$data$cortex_left <- xifti_data[1:last_left,, drop=FALSE]
    xifti$meta$cortex$medial_wall_mask$left <- cifti_map$cortex$medial_wall_mask$left
  }
  if ("cortex_right" %in% brainstructures) {
    xifti$data$cortex_right <- xifti_data[(1+last_left):last_right,, drop=FALSE]
    xifti$meta$cortex$medial_wall_mask$right <- cifti_map$cortex$medial_wall_mask$right
  }
  if ("subcort" %in% brainstructures) {
    alpha_to_spatial <- order(order(cifti_map$subcort$labels))
    subcort_order <- c((1+last_right):nrow(xifti_data))[alpha_to_spatial]
    xifti$data$subcort <- xifti_data[subcort_order,, drop=FALSE]
    xifti$meta$subcort$labels <- cifti_map$subcort$labels
    # Need to crop the subcortical mask. But, there may have been extra padding
    #   in the NIFTI but absent in the mapping. So, do not fill the
    #   SUBCORT_MASK_PADDING field.
    xifti$meta$subcort$mask <- crop_array(cifti_map$subcort$mask)$data
  } 

  # Read surfaces.
  if (!is.null(surfL_fname) | !is.null(surfR_fname)) { 
    if(verbose) { cat("...and surface(s).\n") }
  }
  if (!is.null(surfL_fname)) { 
    xifti$surf$cortex_left <- make_xifti_surface(surfL_fname) 
  }
  if (!is.null(surfR_fname)) { 
    xifti$surf$cortex_right <- make_xifti_surface(surfR_fname) 
  }

  # Finish.
  if (!is.xifti(xifti)) { stop("The \"xifti\" object was invalid.") }

  if (verbose) {
    print(Sys.time() - exec_time)
    exec_time <- Sys.time()
  }

  structure(xifti, class="xifti")
}