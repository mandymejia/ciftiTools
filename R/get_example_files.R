#' Get example files
#' 
#' Get the file paths of the ciftiTools example files
#' 
#' @return A list with CIFTI file names in the first entry, and surface file
#'  names in the second.
#' @keywords internal 
get_example_files <- function(){
  list(
    cifti = list(
      dtseries = system.file(
        "extdata",
        "Conte69.MyelinAndCorrThickness.32k_fs_LR.dtseries.nii",
        package="ciftiTools"
      ),
      dscalar = system.file(
        "extdata",
        "Conte69.MyelinAndCorrThickness.32k_fs_LR.dscalar.nii",
        package="ciftiTools"
      ),
      dlabel = system.file(
        "extdata",
        "Conte69.parcellations_VGD11b.32k_fs_LR.dlabel.nii",
        package="ciftiTools"
      ),
      dscalar_ones = system.file(
        "extdata",
        "ones.dscalar.nii",
        package="ciftiTools"
      )
    ),
    surf = list(
      left = system.file(
        "extdata",
        "Conte69.L.inflated.32k_fs_LR.surf.gii",
        package="ciftiTools"
      ),
      right = system.file(
        "extdata",
        "Conte69.R.inflated.32k_fs_LR.surf.gii",
        package="ciftiTools"
      )
    )
  )
}