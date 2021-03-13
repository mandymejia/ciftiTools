#' Rotate a \code{"surface"} object
#' 
#' Rotate a \code{"surface"}. Can be used to adjust the mesh orientation prior
#'  to \code{\link{view_xifti_surface}}.
#' 
#' @param surf The \code{"surface"} object: see \code{\link{is.surf}}.
#' @param r1,r2,r3 Angle to rotate along the first, second, and third column's
#'  axis, in \code{units} (e.g. changing \code{r1} will change the vertex positions
#'  in the second and third dimensions/columns, since the mesh is being rotated
#'  with respect to the first column's axis). Default: \code{0}.
#' 
#'  With \code{view_xifti_surface} and other mesh rendering functions that 
#'  use \code{rgl}, these rotations seem to correspond to yaw, pitch, and 
#'  roll, respectively. 
#' @param units \code{"radians"} (default) or \code{"degrees"}.
#' 
#' @return The rotated \code{"surface"}.
#' 
#' @export
rotate_surf <- function(surf, r1=0, r2=0, r3=0, units=c("radians", "degrees")) {
  units <- match.arg(units, c("radians", "degrees"))
  if (units=="degrees") {
    r1 <- r1*pi/180; r2 <- r2*pi/180; r3 <- r3*pi/180
  }
  stopifnot(is.surf(surf))

  # Rotate on the mean of the data.
  surf_colmeans <- apply(surf$vertices, 2, mean)
  surf$vertices <- t(t(surf$vertices) - surf_colmeans)

  R <- matrix(c(
    cos(r1) * cos(r2),
    sin(r1) * cos(r2),
    -sin(r2),
    cos(r1) * sin(r2) * sin(r3) - sin(r1) * cos(r3),
    sin(r1) * sin(r2) * sin(r3) + cos(r1) * cos(r3),
    cos(r2) * sin(r3),
    cos(r1) * sin(r2) * cos(r3) + sin(r1) * sin(r3),
    sin(r1) * sin(r2) * cos(r3) - cos(r1) * sin(r3),
    cos(r2) * cos(r3)
  ), nrow=3)
  
  surf$vertices <- t(R %*% t(surf$vertices) + surf_colmeans)
  
  surf
}