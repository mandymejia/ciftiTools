#' Resample "surface" object
#'
#' Resample a "surface" object by writing it to a GIFTI, using the Connectome
#'  Workbench to resample it, and then reading the new file.
#'
#' @param surf A "surface" object
#' @param resamp_res The desired resolution
#' @param hemisphere "left" or "right"
#' @inheritParams wb_path_Param
#' 
#' @return The new surface
#' @export
#'
resample_surf <- function(
  surf, resamp_res, hemisphere=c("left", "right"), wb_path=NULL){

  stopifnot(is.surf(surf))

  hemisphere <- match.arg(hemisphere, c("left", "right"))

  original_res <- nrow(surf$vertices)

  tdir <- tempdir()

  # Write the surface to a GIFTI.
  gii_pre <- format_path("to_resample.surf.gii", tdir, 2)
  write_surf_gifti(surf, gii_pre, hemisphere)

  # Resample.
  gii_post <- format_path("to_read.surf.gii", tdir, 2)
  resample_gifti(
    gii_pre, gii_post, hemisphere=hemisphere,
    original_res = original_res, resamp_res=resamp_res,
    wb_path=wb_path
  )

  # Read new file.
  make_surf(gii_post)
}