#' ciftiTools: Tools for Reading and Visualizing CIFTI Brain Files
#'
#' Functions for reading in CIFTI or GIFTI metric data:
#' 
#' \itemize{
#'  \item{\code{read_xifti}:}{   Read in a CIFTI file as a \code{"xifti"}}
#'  \item{\code{read_xifti2}:}{   Read in GIFTI files as a \code{"xifti"}}
#'  \item{\code{as.xifti}:}{   Combine numeric data to form a \code{"xifti"}}
#'  \item{\code{read_surf}:}{   Read in a surface GIFTI file as a \code{"surf"}}
#'  \item{\code{info_cifti}:}{   Read the metadata in a CIFTI file}
#'  \item{\code{load_parc}:}{   Read in a dlabel parcellation included in \code{ciftiTools}}
#' }
#' 
#' Functions for writing CIFTI or GIFTI metric data:
#' 
#' \itemize{
#'  \item{\code{write_cifti}:}{   Write a \code{"xifti"} to a CIFTI file}
#'  \item{\code{write_metric_gifti}:}{   Write a data matrix to a metric GIFTI file}
#'  \item{\code{write_surf_gifti}:}{   Write a \code{"surf"} to a surface GIFTI file}
#'  \item{\code{write_subcort_nifti}:}{   Write subcortical data to NIFTI files}
#'  \item{\code{separate_cifti}:}{   Separate a CIFTI file into GIFTI and NIFTI files}
#' }
#' 
#' Functions for manipulating \code{"xifti"}s:
#' 
#' \itemize{
#'  \item{\code{apply_xifti}:}{   Apply a function along the rows or columns of a \code{"xifti"}}
#'  \item{\code{combine_xifti}:}{   Combine \code{"xifti"}s with non-overlapping brain structures}
#'  \item{\code{concat_xifti}:}{   Concatenate \code{"xifti"}s}
#'  \item{\code{convert_xifti}:}{   Convert the intent of a \code{"xifti"}}
#'  \item{\code{newdata_xifti}:}{   Replace the data in a \code{"xifti"}}
#'  \item{\code{remove_xifti}:}{   Remove a component from a \code{"xifti"}}
#'  \item{\code{select_xifti}:}{   Select columns of a \code{"xifti"}}
#'  \item{\code{transform_xifti}:}{   Apply a univariate transformation to a \code{"xifti"} or pair of \code{"xifti"}s}
#'  \item{\code{add_surf}:}{   Add surfaces to a \code{"xifti"}}
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
#' @docType package
#' @name ciftiTools
NULL