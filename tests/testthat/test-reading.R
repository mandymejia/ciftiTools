check_wb <- function() {
  if (is.null(ciftiTools.getOption("wb_path"))) {
    skip("Connectome Workbench is not available.")
  }
}

test_that("Reading CIFTI and GIFTI files is working", {
  check_wb()

  fnames <- ciftiTools.files()

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

    # Reading surfaces
    if (!grepl("ones", cii_fname)) {
      cii <- add_surf(cii, surfL=fnames$surf["left"], surfR="midthickness")
      testthat::expect_error(
        readcii(
          cii_fname, brainstructures=cii_info$cifti$brainstructures,
          surfL_fname=fnames$surf["right"], surfR_fname=fnames$surf["left"]
        )
      )
    }
  }

  load_parc()
  load_parc("Schaefer_400")
  testthat::expect_error(load_parc("NotAValidParc"))
  stopifnot(is.surf(load_surf()))
  stopifnot(is.surf(load_surf("right", "midthickness")))
})
