#' Get example files
#' 
#' Get the file paths of the example CIFTI and surface GIFTI files included
#'  with \code{ciftiTools}.
#' 
#' These files are from NITRC: cifti-2_test_data-1.2.zip at 
#'  https://www.nitrc.org/frs/?group_id=454
#' 
#' @return A list with CIFTI file names in the first entry, and surface file
#'  names in the second.
#' 
#' @export
#' 
demo_files <- function(){
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
        "Conte69.L.inflated.32k_fs_LR.surf.gii",
        package="ciftiTools"
      ),
      right = system.file(
        "extdata",
        "Conte69.R.inflated.32k_fs_LR.surf.gii",
        package="ciftiTools"
      )
    ))
  )
}