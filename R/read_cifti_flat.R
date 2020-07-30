#' Read Flat CIFTI Data
#'
#' @description Read a CIFTI file by exporting it as a single GIFTI 
#'  using the \code{-cifti-convert -to-gifti-ext}, and obtaining the
#'  brainordinate mapping using \code{-cifti-export-dense-mapping}. 
#'
#' @inheritParams cifti_fname_Param
#' @inheritParams surfL_fname_Param
#' @inheritParams surfR_fname_Param
#' @inheritParams brainstructures_Param_LR
#' @inheritParams wb_path_Param
#' @param ... Additional arguments to \code{read_cifti_minimal}.
#'
#' @return A \code{"cifti_flat"} object. See \code{\link{is.cifti}}.
#' @export
#'
#' @details This function uses a system wrapper for the "wb_command"
#'  executable. The user must first download and install the Connectome
#'  Workbench, available from
#'  \url{https://www.humanconnectome.org/software/get-connectome-workbench}.
#'  The \code{wb_path} argument is the path to the Connectime Workbench folder
#'  or executable.
#'
read_cifti_flat <- function(
  cifti_fname, 
  surfL_fname=NULL, surfR_fname=NULL,
  brainstructures=c("left","right"), 
  wb_path=NULL, ...){

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
    left="CORTEX_LEFT",
    right="CORTEX_RIGHT",
    subcortical="SUBCORT"
  )
  brainstructures <- as.character(brainstructures_rename[brainstructures])

  # Create the template.
  cifti <- list(
    DAT = NULL,
    LABELS = NULL,
    SURF_LEFT = NULL, 
    SURF_RIGHT = NULL, 
    META = list(
      CORTEX_RESOLUTION = NULL,
      SUBCORT_MASK = NULL,
      SUBCORT_MASK_PADDING = NULL
    )
  )

  # Map the CIFTI.
  cifti_map <- map_cifti(cifti_fname, wb_path)
  cifti$META$SUBCORT_MASK <- cifti_map$SUBCORT_MASK
  cifti$LABELS <- cifti_map$LABELS

  # Read the CIFTI data.
  cifti$DAT <- read_cifti_minimal(cifti_fname, wb_path=wb_path, ...)

  # If there is subcortical data, re-order it spatially (vs. alphabetically).
  if ("SUBCORT" %in% brainstructures) {
    subcort_mask <- cifti$LABELS$BRAINSTRUCTURE == "SUBCORT"
    cifti$DAT[subcort_mask,] <- cifti$DAT[subcort_mask,, drop=FALSE]#[order(order(cifti$LABELS)),]
  }

  # Remove undesired brainstructures.
  brainstructure_mask <- cifti$LABELS$BRAINSTRUCTURE %in% brainstructures
  medialwall_mask <- cifti$LABELS$SUBSTRUCTURE != "Medial Wall"
  cifti$DAT <- cifti$DAT[brainstructure_mask[medialwall_mask],, drop=FALSE]
  cifti$LABELS <- cifti$LABELS[brainstructure_mask,]
  
  # Read surfaces.
  if (!is.null(surfL_fname)) { cifti$SURF_LEFT <- make_cifti_surface(surfL_fname) }
  if (!is.null(surfR_fname)) { cifti$SURF_RIGHT <- make_cifti_surface(surfR_fname) }

  # Finish.
  if (!is.cifti(cifti, flat=TRUE)) { stop("The \"cifti_flat\" object was invalid.") }
  class(cifti) <- "cifti_flat"
  cifti
}