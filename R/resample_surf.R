#' Resample "surface" object
#'
#' Resample a "surface" object by writing it to a GIFTI, using the Connectome
#'  Workbench, and then reading it back in.
#'
#' @param surf A "surface" object
#' @param resamp_res The desired resolution
#' @param side "left" or "right"
#' @param sphere_original_fname File path of [left/right]-hemisphere spherical 
#'  GIFTI files in original resolution. The hemisphere side should match \code{side}.
#' 
#' @return The new surface
#' @export
#'
resample_surf <- function(
  surf, resamp_res, side=c("left", "right"), 
  sphere_original_fname){

  side <- match.arg(side, c("left", "right"))

  tdir <- tempdir()
  sphereL_target_fname <- format_path("sphereL.surf.gii", tdir, mode=2)
  sphereR_target_fname <- format_path("sphereR.surf.gii", tdir, mode=2)
  make_helper_spheres(sphereL_target_fname, sphereR_target_fname, resamp_res)
  sphere_target_fname <- list(left=sphereL_target_fname, right=sphereR_target_fname)[side]

  gii_pre <- format_path("to_resample.surf.gii", tdir, 2)
  gii_post <- format_path("to_read.surf.gii", tdir, 2)
  write_surf_gifti(surf, gii_pre, side)
  resample_gifti(
    gii_pre, gii_post, 
    sphere_original_fname=sphere_original_fname, 
    sphere_target_fname=sphere_target_fname
  )
  make_surface(gii_post)
}

#' @param original_fname The GIFTI file to resample.
#' @param target_fname Where to save the resampled file.
#' @param file_type \code{"metric"} or \code{"surface"}, or \code{NULL} 
#'  (default) to infer from \code{original_fname}.
#' @param ROIcortex_original_fname The name of the ROI file corresponding to 
#'  \code{original_fname}. Leave as \code{NULL} (default) if this doesn't exist
#'  or shouldn't be resampled.
#' @param validROIcortex_target_fname The name of the resampled ROI file. Only 
#'  applicable if \code{ROIcortex_original_fname} is provided.
#' @inheritParams resamp_res_Param_required
#' @param sphere_original_fname File path of [left/right]-hemisphere spherical 
#'  GIFTI files in original resolution. The hemisphere side should match that of
#'  \code{original_fname}.
#' @param sphere_target_fname File path of [left/right]-hemisphere spherical 
#'  GIFTI files in targetinal resolution. The hemisphere side should match 
#'  that of \code{target_fname}. See \code{\link{make_helper_spheres}}.