#' Get a \code{"surf"} object
#'
#' Coerce a file path to a surface GIFTI, a \code{"gifti"} object, a list with
#'  entries "pointset" and "triangle", or a \code{"surf"} to a 
#'  \code{"surf"}. 
#'
#' @param surf Either a file path to a surface GIFTI; a \code{"gifti"}
#'  read by \code{\link[gifti]{readgii}}; a list with entries "pointset" and 
#'  "triangle"; or, a \code{"surf"} object.
#' @param expected_hemisphere The expected hemisphere (\code{"left"} or \code{"right"})
#'  of \code{surf}. If the hemisphere indicated in the GIFTI metadata is the 
#'  opposite, an error is raised. If \code{NULL} (default), use the GIFTI 
#'  hemisphere.
#' 
#' @return The \code{"surf"}: a list with components \code{"vertices"}
#'  (3D spatial locations), \code{"faces"} (defined by three vertices), and 
#'  \code{"hemisphere"} (\code{"left"}, \code{"right"}, or \code{NULL} if 
#'  unknown).
#'
#' @importFrom gifti readgii is.gifti
#'
#' @family reading
#' @family surfing
#' @export
#' 
read_surf <- function(surf, expected_hemisphere=NULL) {

  if (!is.null(expected_hemisphere)) {
    expected_hemisphere <- match.arg(expected_hemisphere, c("left", "right"))
  }

  # File --> GIFTI.
  if (is.fname(surf)){ surf <- readgii(surf) }

  # GIFTI --> list of vertices and faces.
  if (is.gifti(surf)) {
    ## Get hemisphere.
    hemisphere <- try({
      ps_idx <- which(names(surf$data) == "pointset")[1]
      ps_meta <- surf$data_meta[[ps_idx]]
      hemisphere <- ps_meta[which(ps_meta[,1] == "AnatomicalStructurePrimary"),2]
      if (!(hemisphere %in% c("CortexLeft", "CortexRight"))) {
        stop(paste0(
          "The hemisphere metadata entry (AnatomicalStructurePrimary) was not ",
          "CortexLeft or CortexRight. Instead, it was ", hemisphere, 
          ". Discarding and leaving hemisphere entry blank."
        ))
      }
      hemisphere
    }, silent=TRUE)
    if (inherits(hemisphere, "try-error")) { 
      warning(hemisphere); hemisphere <- NULL
    } else {
      hemisphere <- switch(hemisphere, CortexLeft="left", CortexRight="right")
    }
    if (!is.null(expected_hemisphere)) {
      if (hemisphere != expected_hemisphere) {
        stop(paste(
          "The expected hemisphere was", expected_hemisphere, 
          "but the hemisphere indicated in the GIFTI was the opposite."
        ))
      }
    }
    surf <- surf$data
  } else {
    hemisphere <- NULL
  }

  if (is.list(surf) && all(c("pointset", "triangle") %in% names(surf))) {
    surf <- list(
      vertices = surf$pointset, faces = surf$triangle, hemisphere = hemisphere
    )
  } 

  if (!(is.list(surf) && all(c("vertices", "faces") %in% names(surf)))) {
    stop("The object could not be converted into a surface.")
  } 

  ## Format faces as integers starting index at 1 instead of 0
  if (min(surf$faces)==0) surf$faces <- surf$faces + 1
  mode(surf$faces) <- "integer"

  # Return cifti_surface or error.
  if (!is.surf(surf)) {
    stop("The object could not be converted into a surface.")
  }

  structure(surf, class="surf")
}

#' @rdname read_surf
#' @export
make_surf <- function(surf, expected_hemisphere=NULL){
  read_surf(surf=surf, expected_hemisphere=expected_hemisphere)
}