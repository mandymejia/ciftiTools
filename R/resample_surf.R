#' Resample a \code{"surf"} object
#'
#' Resample a \code{"surf"} by writing it to a GIFTI, using the Connectome
#'  Workbench to resample it, and then reading the new file. The barycentric 
#'  resampling method, which is recommended for anatomical surfaces, will be 
#'  used. 
#'
#' @param surf A \code{"surf"}
#' @param resamp_res The desired resolution
#' @param hemisphere \code{"left"} or \code{"right"}. Only used if not indicated by 
#'  \code{surf$hemisphere}. An error will be raised if it does not match the 
#'  hemisphere indicated in the intermediate written GIFTI. 
#' 
#' @return The new \code{"surf"}
#' 
#' @family surfing
#' @export
#'
#' @section Connectome Workbench:
#' This function interfaces with the \code{"-surface-resample"} Workbench command.
#' 
resample_surf <- function(
  surf, resamp_res, hemisphere=c("left", "right")){

  surf <- make_surf(surf)

  if (!is.null(surf$hemisphere)) { hemisphere <- surf$hemisphere }

  original_res <- nrow(surf$vertices)

  tdir <- tempdir()

  # Write the surface to a GIFTI.
  gii_pre <- format_path("to_resample.surf.gii", tdir, 2)
  write_surf_gifti(surf, gii_pre, hemisphere)

  # Resample.
  gii_post <- format_path("to_read.surf.gii", tdir, 2)
  # Delete because if the following command fails, the pre-existing file would be used.
  if (file.exists(gii_post)) { file.remove(gii_post) }
  resample_gifti(
    gii_pre, gii_post, hemisphere=hemisphere,
    original_res = original_res, 
    resamp_res=resamp_res, resamp_method="barycentric"
  )
  
  # Read new file.
  make_surf(gii_post, hemisphere)
}
