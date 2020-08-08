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

  # Place cortex data into the "xifti" object.
  last_left <- sum(cifti_map$cortex$medial_wall_mask$left)
  last_right <- last_left + sum(cifti_map$cortex$medial_wall_mask$right)
  verify_cortex_and_mwall <- function(cortex, side, medial_wall_mask){
    if (sum(medial_wall_mask) != nrow(cortex)) {
      warning(paste(
        "The medial wall mask obtained from the dense mapping metadata",
        "was length", length(medial_wall_mask), "and had",
        sum(medial_wall_mask), "vertices within the mask (non-medial wall).",
        "But there are", nrow(cortex), "rows in the flat", side, 
        "cortex, which doesn't match the number of vertices in the mask.",
        "If the medial wall mask is needed, try read_cifti_separate() instead",
        "which doesn't depend on the dense mapping metadata. For now, the medial",
        "wall mask will not be included in the \"xifti\".\n"
      ))
      medial_wall_mask <- NULL
    } else if (all(medial_wall_mask)) {
      if (ncol(cortex) > 1) {
        new_medial_wall_mask <- !apply(cortex==0 | is.na(cortex), 1, all)
        if (any(!new_medial_wall_mask)) {
          warning(paste(
            "The length of the medial wall mask from the", side, "cortex metadata",
            "was equal to the number of vertices, and it was all TRUE",
            "(indicating no medial wall).",
            "But, constant 0/NA rows were detected. Discarding the mask from",
            "the metadata and inferring from the", side, "cortex data instead.\n"
          ))
          medial_wall_mask <- new_medial_wall_mask
          cortex <- cortex[new_medial_wall_mask,, drop=FALSE]
        }
      } else {
        warning(paste(
          "The length of the medial wall mask from the", side, "cortex metadata",
          "was equal to the number of vertices, and it was all TRUE",
          "(indicating no medial wall).",
          "The", side, "medial wall mask will not be included in the \"xifti\".\n"
        ))
        medial_wall_mask <- NULL
      }
    }
    list(data = cortex, medial_wall_mask = medial_wall_mask)
  }
  if ("cortex_left" %in% brainstructures) {
    cortex <- verify_cortex_and_mwall(
      xifti_data[1:last_left,, drop=FALSE],
      "left",
      cifti_map$cortex$medial_wall_mask$left
    )
    xifti$data$cortex_left <- cortex$data
    xifti$meta$cortex$medial_wall_mask["left"] <- list(cortex$medial_wall_mask)
  }
  if ("cortex_right" %in% brainstructures) {
    cortex <- verify_cortex_and_mwall(
      xifti_data[(1+last_left):last_right,, drop=FALSE],
      "right",
      cifti_map$cortex$medial_wall_mask$right
    )
    xifti$data$cortex_right <- cortex$data
    xifti$meta$cortex$medial_wall_mask["right"] <- list(cortex$medial_wall_mask)
  }

  # Place subcortical data into the "xifti" object.
  if ("subcort" %in% brainstructures) {
    alpha_to_spatial <- order(order(cifti_map$subcort$labels))
    subcort_order <- c((1+last_right):nrow(xifti_data))[alpha_to_spatial]
    xifti$data$subcort <- xifti_data[subcort_order,, drop=FALSE]
    xifti$meta$subcort$labels <- cifti_map$subcort$labels
    # Need to crop the subcortical mask. But, there may have been extra padding
    #   in the NIFTI but absent in the mapping. So, do not fill the
    #   SUBCORT_MASK_PADDING field.
    xifti$meta$subcort$mask <- crop_vol(cifti_map$subcort$mask)$data
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