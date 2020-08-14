#' Make a template "xifti" object.
#' 
#' @return An empty "xifti" object with a valid list structure
#'
#' @keywords internal
#' 
template_xifti <- function(){
  xifti <- list(
    data = list(
      cortex_left = NULL,
      cortex_right = NULL,
      subcort = NULL
    ),
    surf = list(
      cortex_left = NULL,
      cortex_right = NULL
    ),
    meta = list(
      cortex = list(
        medial_wall_mask = list(
          left = NULL,
          right = NULL
        ),
        resamp_resolution = NULL
      ),
      subcort = list(
        labels = NULL,
        mask = NULL,
        trans_mat = NULL
      ),
      cifti = list(
        intent = NULL,
        brainstructures = NULL
      )
    )
  )

  structure(xifti, class="xifti")
}