#' Resample "surface" object
#'
#' Resample a "surface" object by writing it to a GIFTI, using the Connectome
#'  Workbench to resample it, and then reading the new file.
#'
#' @param surf A "surface" object
#' @param resamp_res The desired resolution
#' @param side "left" or "right"
#' @inheritParams wb_path_Param
#' 
#' @return The new surface
#' @export
#'
resample_surf <- function(
  surf, resamp_res, side=c("left", "right"), wb_path=NULL){

  stopifnot(is.surf(surf))

  side <- match.arg(side, c("left", "right"))

  original_res <- nrow(surf$vertices)

  tdir <- tempdir()

  # Create original sphere.
  sphereL_original_fname <- format_path(paste0("sphereL_", original_res, ".surf.gii"), tdir, mode=2)
  sphereR_original_fname <- format_path(paste0("sphereR_", original_res, ".surf.gii"), tdir, mode=2)
  write_spheres(sphereL_original_fname, sphereR_original_fname, original_res)
  sphere_original_fname <- switch(side, left=sphereL_original_fname, right=sphereR_original_fname)

  # Create target sphere.
  sphereL_target_fname <- format_path(paste0("sphereL_", resamp_res, ".surf.gii"), tdir, mode=2)
  sphereR_target_fname <- format_path(paste0("sphereR_", resamp_res, ".surf.gii"), tdir, mode=2)
  write_spheres(sphereL_target_fname, sphereR_target_fname, resamp_res)
  sphere_target_fname <- switch(side, left=sphereL_target_fname, right=sphereR_target_fname)

  # Write the surface to a GIFTI.
  gii_pre <- format_path("to_resample.surf.gii", tdir, 2)
  write_surf_gifti(surf, gii_pre, side)

  # Resample.
  gii_post <- format_path("to_read.surf.gii", tdir, 2)
  resample_gifti(
    gii_pre, gii_post, resamp_res=resamp_res,
    sphere_original_fname=sphere_original_fname, 
    sphere_target_fname=sphere_target_fname,
    wb_path=wb_path
  )

  # Read new file.
  make_surf(gii_post)
}