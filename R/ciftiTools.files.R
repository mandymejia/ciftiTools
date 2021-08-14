#' \code{ciftiTools} files
#' 
#' CIFTI and surface GIFTI files included in the \code{ciftiTools} package
#' 
#' The CIFTI files are from NITRC: cifti-2_test_data-1.2.zip at
#'  https://www.nitrc.org/frs/?group_id=454
#'
#' The surfaces are from the HCP and are included according to these data use
#'  terms: Data were provided \[in part\] by the Human Connectome Project,
#'  WU-Minn Consortium (Principal Investigators: David Van Essen and Kamil
#'  Ugurbil; 1U54MH091657) funded by the 16 NIH Institutes and Centers
#'  that support the NIH Blueprint for Neuroscience Research; and by the
#'  McDonnell Center for Systems Neuroscience at Washington University.
#'
#' Only the inflated surfaces are available as GIFTI files. To access the other
#'  surfaces included in the package (very inflated and midthickness), see
#'  \code{\link{load_surf}}.
#'
#' @return a list of file paths
#' 
#' @export
#' 
ciftiTools.files <- function() {
  list(
    cifti = unlist(list(
      dtseries = system.file(
        "extdata",
        "Conte69.MyelinAndCorrThickness.32k_fs_LR.dtseries.nii",
        package="ciftiTools"
      ),
      dscalar = system.file(
        "extdata",
        "Conte69.MyelinAndCorrThickness.6k_fs_LR.dscalar.nii",
        package="ciftiTools"
      ),
      dlabel = system.file(
        "extdata",
        "Conte69.parcellations_VGD11b.6k_fs_LR.dlabel.nii",
        package="ciftiTools"
      ),
      dscalar_ones = system.file(
        "extdata",
        "ones_1k.dscalar.nii",
        package="ciftiTools"
      )
    )),
    surf = unlist(list(
      left = system.file(
        "extdata",
        paste0("S1200.L.inflated_MSMAll.32k_fs_LR.surf.gii"),
        package="ciftiTools"
      ),
      right = system.file(
        "extdata",
        paste0("S1200.R.inflated_MSMAll.32k_fs_LR.surf.gii"),
        package="ciftiTools"
      )
    ))
  )
}