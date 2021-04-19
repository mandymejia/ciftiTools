#' Read gifti file(s) as a \code{xifti} object
#' 
#' Read in gifti metric files as a \code{xifti} object. May also include
#'  surface geometry gifti files and perform resampling. 
#' 
#' @param cortexL,cortexL_mwall Left cortex data and ROI. Each must be a path to 
#'  a GIFTI file.
#' 
#'  If \code{cortexL_mwall} is not provided, \code{cortexL} should have data for
#'  all vertices on the left cortical surface (\eqn{V_L x T} data matrix). There 
#'  will not be a mask for the medial wall. Not providing the medial wall mask 
#'  is appropriate for ".dlabels.nii" files where the medial wall may have its 
#'  own label and therefore should not be treated as missing data.
#' 
#'  If \code{cortexL_mwall} is provided, \code{cortexL} should either have data
#'  for all vertices on the left cortical surface (\eqn{V_L x T} data matrix, with
#'  filler values e.g. \code{0} or \code{NaN} for medial wall vertices), or have data 
#'  only for non-medial wall vertices (\eqn{(V_L - mwall_L) x T} data matrix).
#'  The medial wall mask will be the \code{0} values in \code{cortexL_mwall}. 
#'  The medial wall mask should be provided whenever the medial wall should be
#'  treated as missing data. 
#' 
#'  Since the unmasked cortices must have the same number of vertices,
#'  \code{V_L} should match \code{V_R}, or \code{resamp_res} must be set.
#' @param cortexR,cortexR_mwall Right cortex data and ROI. Each must be a path to 
#'  a GIFTI file.
#' 
#'  If \code{cortexR_mwall} is not provided, \code{cortexR} should have data for
#'  all vertices on the right cortical surface (\eqn{V_R x T} data mre 
#'  will not be a mask for the medial wall. Not providing the medial wall mask 
#'  is appropriate for ".dlabels.nii" files where the medial wall may have its 
#'  own label and therefore should not be treated as missing data.
#' 
#'  If \code{cortexR_mwall} is provided, \code{cortexR} should either have data
#'  for all vertices on the right cortical surface (\eqn{V_R x T} data matrix, with
#'  filler values e.g. \code{0} or \code{NaN} for medial wall vertices), or have data 
#'  only for non-medial wall vertices (\eqn{(V_R - mwall_R) x T} data matrix).
#'  The medial wall mask will be the \code{0} values in \code{cortexR_mwall}. 
#'  The medial wall mask should be provided whenever the medial wall should be
#'  treated as missing data. 
#' 
#'  Since the unmasked cortices must have the same number of vertices,
#'  \code{V_L} should match \code{V_R}, or \code{resamp_res} must be set.
#' @param mwall_values If \code{cortex[L/R]_mwall} was not provided, or if it
#'  was invalid (i.e. bad length or all \code{TRUE}), the medial wall mask will
#'  be inferred from rows in \code{cortex[L/R]} that are constantly one of these
#'  values. Default: \code{c(NA, NaN)}. If \code{NULL}, do not attempt to infer
#'  the medial wall from the data values. \code{NULL} should be used if \code{NA}
#'  or \code{NaN} are legitimate values that non-medial wall vertices might
#'  take on.
#' @param surfL,surfR (Optional) File path(s) to surface GIFTI(s) for the left 
#'  or right cortex. 
#' @param resamp_res Resolution to resample the cortical data and surface to.
#'  Default: \code{NULL} (do not resample). If provided, the original resolutions
#'  of the cortex data and surfaces may differ.
#' @param col_names Names of each measurement/column in the data. Overrides
#'  names indicated in the data components.
#' @param HCP_32k_auto_mwall If left and/or right cortex data is provided, and
#'  the number of vertices matches that of the HCP 32k mesh (29696 on left, and
#'  29716 on right), should the medial wall masks be added to the \code{"xifti"}
#'  if not provided? Default: \code{TRUE}.
#' @param read_dir (Optional) Append a directory to all file names in the
#'  arguments. If \code{NULL} (default), do not modify file names.
#' @param validate Validate that the result is a \code{"xifti"} object? Default:
#'  \code{TRUE}. If \code{FALSE}, the result may not be properly formatted
#'  if the inputs were invalid.
#' 
#' @return The \code{"xifti"} object containing all the data in the input giftis.
#' 
#' @export
#' 
read_xifti2 <- function(
  cortexL=NULL, cortexL_mwall=NULL,
  cortexR=NULL, cortexR_mwall=NULL,
  mwall_values=c(NA, NaN), surfL=NULL, surfR=NULL, 
  resamp_res=NULL, col_names=NULL, HCP_32k_auto_mwall=TRUE,
  read_dir=NULL, validate=TRUE) {

  # Check that each input file exists.
  badfile <- function(x){
    if (!is.null(read_dir)) { x <- file.path(read_dir, x) }
    (!is.null(x)) && (!file.exists(x))
  }
  if (badfile(cortexL)) { stop("Left cortex does not reference an existing file.") }
  if (badfile(cortexL_mwall)) { stop("Left cortex ROI does not reference an existing file.") }
  if (badfile(cortexR)) { stop("Right cortex does not reference an existing file.") }
  if (badfile(cortexR_mwall)) { stop("Right cortex ROI does not reference an existing file.") }

  # Resample each input file, if requested.
  if (!is.null(resamp_res)) {
    tdir <- tempdir()
    
    ## Left cortex.
    if (!is.null(cortexL)) {
      x <- resample_gifti(
        cortexL, 
        paste0("resamp", cifti_component_suffix("cortexL")),
        hemisphere="left", resamp_res=resamp_res, 
        ROIcortex_original_fname=cortexL_mwall, 
        read_dir=read_dir, write_dir=tdir
      )
      cortexL <- x[1]; if (!is.null(cortexL_mwall)) { cortexL_mwall <- x[2] }
    }

    ## Right cortex.
    if (!is.null(cortexR)) {
      x <- resample_gifti(
        cortexR, 
        paste0("resamp", cifti_component_suffix("cortexR")),
        hemisphere="right", resamp_res=resamp_res, 
        ROIcortex_original_fname=cortexR_mwall, 
        read_dir=read_dir, write_dir=tdir
      )
      cortexR <- x[1]; if (!is.null(cortexR_mwall)) { cortexR_mwall <- x[2] }
    }

    ## Left surface.
    if (!is.null(surfL)) {
      surfL <- resample_gifti(
        surfL, 
        paste0("surf_resamp", cifti_component_suffix("cortexL")),
        hemisphere="left", file_type="surf", resamp_res=resamp_res,
        read_dir=read_dir, write_dir=tdir
      )
    }

    ## Right surface.
    if (!is.null(surfR)) {
      surfR <- resample_gifti(
        surfR, 
        paste0("surf_resamp", cifti_component_suffix("cortexR")),
        hemisphere="left", file_type="surf", resamp_res=resamp_res,
        read_dir=read_dir, write_dir=tdir
      )
    }

    read_dir <- NULL
  }

  make_xifti(
    cortexL=cortexL, cortexL_mwall=cortexL_mwall,
    cortexR=cortexR, cortexR_mwall=cortexR_mwall,
    mwall_values=mwall_values,
    surfL=surfL, surfR=surfR,
    col_names=col_names, HCP_32k_auto_mwall=HCP_32k_auto_mwall,
    read_dir=read_dir, validate=validate
  )

}