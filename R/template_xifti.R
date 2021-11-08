#' Make a template \code{"xifti"} object
#' 
#' Make a \code{"xifti"} object with all terminal entries \code{NULL}. Useful for
#'  seeing the structure of a \code{"xifti"} object or creating a new one.
#' 
#' Additional metadata depends on the type of CIFTI file:
#' 
#'  \enumerate{
#'    \item{"dtseries"}{
#'      \enumerate{
#'        \item{time_start:}{   Start time}
#'        \item{time_step:}{   The TR}
#'        \item{time_unit:}{   Unit of time}
#'      }
#'    }
#'    \item{"dscalar"}{
#'      \enumerate{
#'        \item{names:}{   Name of each data column}
#'      }
#'    }
#'    \item{"dlabels"}{
#'      \enumerate{
#'        \item{names:}{   Name of each data column}
#'        \item{labels:}{
#'             Length \eqn{T} list of \eqn{L \times 5} data.frames. List entry 
#'          names are data column names. Row names are label names. 
#'          Column names are: Key, Red, Green, Blue, and Alpha.
#'        }
#'      }
#'    }
#'  }
#'
#' 
#' @return An empty \code{"xifti"} object with a valid list structure
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
        resamp_res = NULL
      ),
      subcort = list(
        labels = NULL,
        mask = NULL,
        trans_mat = NULL,
        trans_units = NULL
      ),
      cifti = list(
        intent = NULL,
        brainstructures = NULL
      )
    )
  )

  structure(xifti, class="xifti")
}