check_wb <- function() {
  wb_path <- try(get_wb_cmd_path(ciftiTools.getOption("wb_path"), verbose=FALSE), silent=TRUE)
  if ("try-error" %in% class(wb_path)) {
    skip("Connectome Workbench is not available.")
  }
}

test_that("Reading CIFTI and GIFTI files is working", {
  check_wb()

  fnames <- ciftiTools:::get_example_files()

  for (cii_fname in fnames$cifti) {
    cat("\n\n"); cat(cii_fname); cat("\n\n")

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
  cii <- add_surf(cii, surfL=fnames$surf["left"], surfR=fnames$surf["right"])
})
