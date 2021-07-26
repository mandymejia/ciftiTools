check_wb <- function() {
  if (is.null(ciftiTools.getOption("wb_path"))) {
    skip("Connectome Workbench is not available.")
  }
}

test_that("Reading CIFTI and GIFTI files is working", {
  check_wb()

  fnames <- ciftiTools:::demo_files()

  for (cii_fname in fnames$cifti) {
    cat("\n\n"); cat(cii_fname); cat("\n\n")

    # Read file in different ways
    cii_flat <- readcii(cii_fname, flat=TRUE)
    cii_info <- info_cifti(cii_fname)
    cii <- readcii(cii_fname, brainstructures=cii_info$cifti$brainstructures)
    testthat:::expect_equal(cii_flat, ciftiTools:::flatten_xifti(cii))
    cii <- readcii(cii_fname, brainstructures=cii_info$cifti$brainstructures[1])
    cii2 <- ciftiTools:::read_cifti_separate(cii_fname, brainstructures=cii_info$cifti$brainstructures[1])
    ciftiTools:::expect_equal_xifti(cii, cii2)

    cii <- readcii(cii_fname, brainstructures=cii_info$cifti$brainstructures[1], idx=1)
    cii2 <- ciftiTools:::read_cifti_separate(cii_fname, brainstructures=cii_info$cifti$brainstructures[1], idx=1)
    ciftiTools:::expect_equal_xifti(cii, cii2)

    # Basic properties
    summary(cii)
    dim(cii)

    # Test other alias
    cii <- read_cifti(cii_fname, brainstructures=cii_info$cifti$brainstructures)

    if (cii_fname == "dtseries") {
      # Reading surfaces (only test 32k dtseries)
      cii <- add_surf(fnames$cifti["dtseries"], surfL=fnames$surf["left"], surfR=fnames$surf["right"])
      cii <- readcii(fnames$cifti["dtseries"], brainstructures=cii_info$cifti$brainstructures, 
        surfL_fname=fnames$surf["left"], surfR_fname=fnames$surf["right"], verbose=FALSE
      )
      testthat::expect_error(
        readcii(
          fnames$cifti["dtseries"], brainstructures=cii_info$cifti$brainstructures, 
          surfL_fname=fnames$surf["right"], surfR_fname=fnames$surf["left"]
        )
      )
    }
  }

})
