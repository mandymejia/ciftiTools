#' Remove a component from a \code{"xifti"}
#' 
#' Remove a brain structure, surface, or subcortical structure from a 
#'  \code{"xifti"}.
#' 
#' @inheritParams xifti_Param
#' @param remove A character vector containing one or more of the following:
#'  \code{"cortex_left"}, \code{"cortex_right"}, \code{"subcortical"}, 
#'  \code{"surf_left"}, and \code{"surf_right"}. Each of these components will
#'  be removed from \code{xifti}.
#' @param remove_sub A vector containing subcortical structures to be removed
#'  from \code{xifti}. Can be specified with names, or with numeric factor
#'  values: see \code{\link{substructure_table}}.
#' 
#' @return The new \code{"xifti"} with the requested component(s) removed
#' 
#' @family manipulating
#' @export
#' 
remove_xifti <- function(xifti, remove=NULL, remove_sub=NULL){
  if (!is.null(remove)) {
    remove <- match.arg(remove, c("cortex_left", "cortex_right", "subcortical", "surf_left", "surf_right"), several.ok=TRUE)
    
    if ("cortex_left" %in% remove) {
      xifti$data["cortex_left"] <- list(NULL)
      xifti$meta$cortex$medial_wall_mask["left"] <- list(NULL)
    }
    
    if ("cortex_right" %in% remove) {
      xifti$data["cortex_right"] <- list(NULL)
      xifti$meta$cortex$medial_wall_mask["right"] <- list(NULL)
    }

    if ("cortex_left" %in% remove && "cortex_right" %in% remove) {
      xifti$meta$cortex["resamp_res"] <- list(NULL)
    }

    if ("subcortical" %in% remove) {
      xifti$data["subcort"] <- list(NULL)
      xifti$meta$subcort <- template_xifti()$meta$subcort
    }

    if ("surf_left" %in% remove) {
      xifti$surf["cortex_left"] <- list(NULL)
    }
    if ("surf_right" %in% remove) {
      xifti$surf["cortex_right"] <- list(NULL)
    }
  }

  if (!is.null(remove_sub)) {
    if (is.null(xifti$data$subcort)) {
      warning("No subcortical data to remove.\n")
    } else {
      sst <- substructure_table()
      if (is.numeric(remove_sub)) {
        remove_sub <- sst$ciftiTools_Name[match(remove_sub, rownames(sst))]
      } else if (all(remove_sub == toupper(remove_sub))) {
        remove_sub <- sst$ciftiTools_Name[match(remove_sub, sst$Original_Name)]
      }
      if (any(is.na(remove_sub)) || !all(remove_sub %in% sst$ciftiTools_Name)) {
        stop("Invalid subcortical substructures in `remove_sub`.")
      }
      for (ii in seq(length(remove_sub))) {
        x_brainstemMask <- xifti$meta$subcort$labels != remove_sub[ii]

        # Take subset
        xifti$data$subcort <- xifti$data$subcort[x_brainstemMask,,drop=FALSE]
        xifti$meta$subcort$labels <- xifti$meta$subcort$labels[x_brainstemMask]
        xifti$meta$subcort$mask[xifti$meta$subcort$mask] <- x_brainstemMask
      }
    }
  }

  if (!is.xifti(xifti)) { stop("Could not make a valid \"xifti\" object.") }
  xifti
}