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
      cifti = list(
        intent = NULL,
        misc = NULL
      ),
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
        mask_padding = list(
          i = c(NA, NA), 
          j = c(NA, NA), 
          k = c(NA, NA)
        )
      )
    )
  )

  structure(xifti, class="xifti")
}