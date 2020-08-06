#' Make a template "xifti" object.
#' 
#' @return A "xifti" object with a valid list structure
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