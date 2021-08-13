#' Smooth a metric GIFTI file
#'
#' Smooths metric GIFTI data along the cortical surface. The results are written
#'  to a new GIFTI file.
#'
#' @param original_fname The GIFTI file to smooth.
#' @param target_fname Where to save the smoothed file.
#' @param surf_fname Surface GIFTI files cortical surface along which to smooth. 
#'  If not provided, the default inflated surfaces will be used.
#' @param surf_FWHM The full width at half maximum (FWHM) parameter
#'  for the gaussian surface smoothing kernel, in mm. Default: \code{5}
#' @param hemisphere The cortex hemisphere: \code{"left"} or \code{"right"}.
#'  Only used if \code{surf_fname} is \code{NULL}.
#' @param ROI_fname The ROI to limit smoothing to, as a metric file. This is
#'  used to exclude the medial wall from smoothing. If not provided (default)
#'  all the data is smoothed across the surface.
#' @param zeroes_as_NA Should zero-values be treated as NA? Default: \code{FALSE}.
#' @return The smoothed GIFTI file name, invisibly
#'
#' @importFrom gifti readgii
#'
#' @export
#'
#' @section Connectome Workbench:
#' This function interfaces with the \code{"-metric-smoothing"} Workbench command.
#' 
smooth_gifti <- function(
  original_fname, target_fname, surf_fname=NULL, surf_FWHM=5, 
  hemisphere=c("left", "right"), ROI_fname=NULL, zeroes_as_NA=FALSE) {

  # ----------------------------------------------------------------------------
  # Check arguments. -----------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  stopifnot(file.exists(original_fname))

  # Use default surface if none provided.
  if (is.null(surf_fname)) {
    ciftiTools_warn(paste(
      "No surface provided to `smooth_gifti`,",
      "so using the surface included in `ciftiTools`."
    ))

    hemisphere <- match.arg(hemisphere, c("left", "right"))

    x_res <- nrow(readgii(original_fname)$data[[1]])
    surf_fname <- file.path(tempdir(), paste0(hemisphere, ".surf.gii"))
    surf_fname <- resample_gifti(
      ciftiTools.files()$surf[hemisphere], 
      surf_fname, hemisphere=hemisphere, file_type="surface", resamp_res=x_res
    )
  }

  stopifnot(file.exists(surf_fname))

  # ----------------------------------------------------------------------------
  # Make and run command. ------------------------------------------------------
  # ----------------------------------------------------------------------------

  cmd <- paste(
    "-metric-smoothing", 
    sys_path(surf_fname), sys_path(original_fname), 
    surf_FWHM / (2*sqrt(2*log(2))), sys_path(target_fname)
  )
  if (!is.null(ROI_fname)) { cmd <- paste(cmd, "-roi", sys_path(ROI_fname)) }
  if (zeroes_as_NA) { cmd <- paste(cmd, "-fix-zeros") }
  run_wb_cmd(cmd)

  return(invisible(target_fname))
}

#' @rdname smooth_gifti
#' @export
smoothGIfTI <- function(
  original_fname, target_fname, surf_fname, surf_FWHM=5,
  zeroes_as_NA=FALSE) {

  smooth_gifti(
    original_fname, target_fname, surf_fname, surf_FWHM, zeroes_as_NA
  )
}

#' @rdname smooth_gifti
#' @export
smoothgii <- function(
  original_fname, target_fname, surf_fname, surf_FWHM=5,
  zeroes_as_NA=FALSE) {

  smooth_gifti(
    original_fname, target_fname, surf_fname, surf_FWHM, zeroes_as_NA
  )
}