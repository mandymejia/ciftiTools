## usethis namespace: start
## usethis namespace: end
#' ciftiTools: Tools for Reading and Visualizing CIFTI Brain Files
#' 
#' Here are groups of commonly-used functions in \code{ciftiTools}:
#'
#' Functions for reading in CIFTI or GIFTI data:
#' 
#' \itemize{
#'  \item{\code{read_xifti}:}{   Read in a CIFTI file as a \code{"xifti"}}
#'  \item{\code{read_xifti2}:}{   Read in GIFTI files as a \code{"xifti"}}
#'  \item{\code{as.xifti}:}{   Combine numeric data to form a \code{"xifti"}}
#'  \item{\code{read_surf}:}{   Read in a surface GIFTI file as a \code{"surf"}}
#'  \item{\code{info_cifti}:}{   Read the metadata in a CIFTI file}
#'  \item{\code{load_surf}:}{   Read in a surface included in \code{ciftiTools}}
#'  \item{\code{load_parc}:}{   Read in a parcellation included in \code{ciftiTools}}
#' }
#' 
#' Functions for writing CIFTI or GIFTI data:
#' 
#' \itemize{
#'  \item{\code{write_cifti}:}{   Write a \code{"xifti"} to a CIFTI file}
#'  \item{\code{write_xifti2}:}{   Write a \code{"xifti"} to GIFTI and NIFTI files}
#'  \item{\code{write_metric_gifti}:}{   Write a numeric data matrix to a metric GIFTI file}
#'  \item{\code{write_surf_gifti}:}{   Write a \code{"surf"} to a surface GIFTI file}
#'  \item{\code{write_subcort_nifti}:}{   Write subcortical data to NIFTI files}
#'  \item{\code{separate_cifti}:}{   Separate a CIFTI file into GIFTI and NIFTI files}
#' }
#' 
#' Functions for manipulating \code{"xifti"}s:
#' 
#' \itemize{
#'  \item{\code{apply_xifti}:}{   Apply a function along the rows or columns of the \code{"xifti"} data matrix}
#'  \item{\code{combine_xifti}:}{   Combine multiple \code{"xifti"}s with non-overlapping brain structures}
#'  \item{\code{convert_xifti}:}{   Convert the intent of a \code{"xifti"}}
#'  \item{\code{merge_xifti}:}{   Concatenate data matrices from multiple \code{"xifti"}s}
#'  \item{\code{newdata_xifti}:}{   Replace the data matrix in a \code{"xifti"}}
#'  \item{\code{remove_xifti}:}{   Remove a brain structure or surface from a \code{"xifti"}}
#'  \item{\code{select_xifti}:}{   Select data matrix columns of a \code{"xifti"}}
#'  \item{\code{transform_xifti}:}{   Apply a univariate transformation to a \code{"xifti"} or pair of \code{"xifti"}s}
#'  \item{\code{add_surf}:}{   Add surfaces to a \code{"xifti"}}
#'  \item{\code{move_from_mwall}:}{   Move medial wall vertices back into the \code{"xifti"} data matrix}
#'  \item{\code{move_to_mwall}:}{   Move rows with a certain value into the \code{"xifti"} medial wall mask}
#' }
#' 
#' S3 methods for \code{"xifti"}s:
#' 
#' \itemize{
#'  \item{\code{summary} and \code{print}:}{   Summarize the contents.}
#'  \item{\code{as.matrix}:}{   Convert data to a locations by measurements numeric matrix.}
#'  \item{\code{dim}:}{    Obtain number of locations and number of measurements.}
#'  \item{\code{plot}:}{   Visualize the cortical surface and/or subcortical data.}
#'  \item{\code{+}, \code{-}, \code{*}, \code{/}, \code{^}, \code{\%\%}, \code{\%/\%}:}{    Operation between a \code{"xifti"} and a scalar, or between two \code{"xifti"}s.}
#'  \item{\code{abs}, \code{ceiling}, \code{exp}, \code{floor}, \code{log}, \code{round}, \code{sign}, and \code{sqrt}:}{    Univariate transformation of \code{"xifti"} data.}
#' }
#' 
#' Functions for working with surfaces:
#' 
#' \itemize{
#'  \item{\code{read_surf}:}{   Read in a surface GIFTI file as a \code{"surf"}}
#'  \item{\code{is.surf}:}{   Verify a \code{"surf"}}
#'  \item{\code{write_surf_gifti}:}{   Write a \code{"surf"} to a surface GIFTI file}
#'  \item{\code{view_surf}:}{   Visualize a \code{"surf"}}
#'  \item{\code{resample_surf}:}{   Resample a \code{"surf"}}
#'  \item{\code{rotate_surf}:}{   Rotate the geometry of a \code{"surf"}}
#' }
#'
#' @name ciftiTools
"_PACKAGE"
