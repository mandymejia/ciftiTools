#' Make a template "xifti" object.
#' 
#' @return An empty "xifti" object with a valid list structure
#'
#' @keywords internal
#' 
#'  Additional metadata depends on the type of CIFTI file:
#' 
#'  For "dtseries" files:
#'    cifti$time_start
#'    cifti$time_step
#'    cifti$time_unit
#' 
#'  For "dscalar" files:
#'    cifti$names       (Name of each data column)
#' 
#'  For "dlabels" files:
#'    cifti$labels      (L x 5 data.frame. Rows are named. Cols: KRGBA)
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
        resamp_res = NULL
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