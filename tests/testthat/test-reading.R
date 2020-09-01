check_wb <- function() {
  wb_path <- try(get_wb_cmd_path(ciftiTools.getOption("wb_path"), verbose=FALSE), silent=TRUE)
  if ("try-error" %in% class(wb_path)) {
    skip("Connectome Workbench is not available.")
  }
}

test_that("Reading CIFTI and GIFTI files is working", {
  check_wb()

  cii_fnames <- list(
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
    ones = system.file(
      "extdata",
      "ones.dscalar.nii",
      package="ciftiTools"
    )
  )

  for (cii_fname in cii_fnames) {
    # Read file in different ways
    cii_flat <- readcii(cii_fname, flat=TRUE)
    cii_info <- info_cifti(cii_fname)
    cii <- readcii(cii_fname, brainstructures=cii_info$cifti$brainstructures)
    expect_equal(cii_flat, ciftiTools:::flatten_xifti(cii))
    cii <- readcii(cii_fname, brainstructures=cii_info$cifti$brainstructures[1])
    cii <- readcii(cii_fname, brainstructures=cii_info$cifti$brainstructures, verbose=FALSE)

    # Test other alias
    cii <- read_cifti(cii_fname, brainstructures=cii_info$cifti$brainstructures)
  }

  # Reading surfaces
  surfL <- make_surf(
    system.file(
      "extdata",
      "Conte69.L.inflated.32k_fs_LR.surf.gii",
      package="ciftiTools"
    )
  )
  surfR <- make_surf(
    system.file(
      "extdata",
      "Conte69.R.inflated.32k_fs_LR.surf.gii",
      package="ciftiTools"
    )
  )
  cii <- add_surf(cii, surfL=surfL, surfR=surfR)
})
