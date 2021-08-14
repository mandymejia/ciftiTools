#' Remove a component from a \code{"xifti"}
#' 
#' Remove a brain structure or surface from a \code{"xifti"}
#' 
#' @inheritParams xifti_Param
#' @param remove A character vector containing one or more of the following:
#'  \code{"cortex_left"}, \code{"cortex_right"}, \code{"subcortical"}, 
#'  \code{"surf_left"}, and \code{"surf_right"}. Each of these components will
#'  be removed from the \code{"xifti"}
#' 
#' @return The new \code{"xifti"} with the requested component(s) removed
#' 
#' @family manipulating
#' @export
#' 
remove_xifti <- function(xifti, remove=NULL){
  if (is.null(remove)) { stop("Must specify which components to `remove`.") }
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

  if (!is.xifti(xifti)) { stop("Could not make a valid \"xifti\" object.") }
  xifti
}