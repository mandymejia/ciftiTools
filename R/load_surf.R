#' Load a \code{"surf"} included in \code{ciftiTools}
#'
#' Load a \code{"surf"} object from one of the three 32k surface geometries included
#'  in \code{ciftiTools}.
#'
#' The surfaces are from the HCP and are included according to these data use
#'  terms: Data were provided \[in part\] by the Human Connectome Project,
#'  WU-Minn Consortium (Principal Investigators: David Van Essen and Kamil
#'  Ugurbil; 1U54MH091657) funded by the 16 NIH Institutes and Centers
#'  that support the NIH Blueprint for Neuroscience Research; and by the
#'  McDonnell Center for Systems Neuroscience at Washington University.
#' 
#' @param hemisphere \code{"left"} (default) or \code{"right"}
#' @param name The name of the surface geometry to load: \code{"inflated"}
#'  (default), \code{"very inflated"}, and \code{"midthickness"}.
#' @param resamp_res The resolution to resample the surfaces to. If \code{NULL}
#'  (default) or 32492, do not resample.
#'
#' @return The \code{"surf"} object
#' 
#' @family reading
#' @export
load_surf <- function(
  hemisphere=c("left", "right"), 
  name=c("inflated", "very inflated", "midthickness"), 
  resamp_res=NULL) {

  name <- match.arg(name, c("inflated", "very inflated", "midthickness"))
  hemisphere <- match.arg(hemisphere, c("left", "right"))
  hemi <- switch(hemisphere, left="L", right="R")

  # Load the inflated surface
  surf_fname <- paste0("extdata/S1200.", hemi, ".inflated_MSMAll.32k_fs_LR.surf.gii")
  surf <- read_surf(system.file(surf_fname, package="ciftiTools"), hemisphere)

  # Replace vertex coordinates if not inflated
  if (name != "inflated") {
    name2 <- paste0(gsub(" ", "_", name), ".", hemi)
    surf$vertices <- ciftiTools.data$surfp[[name2]]
  }

  # Resample if necessary
  if (!is.null(resamp_res) && resamp_res!=32492) {
    surf <- resample_surf(surf, resamp_res)
  }

  surf
}
