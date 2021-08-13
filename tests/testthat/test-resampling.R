check_wb <- function() {
  if (is.null(ciftiTools.getOption("wb_path"))) {
    skip("Connectome Workbench is not available.")
  }
}

test_that("Resampling CIFTI and GIFTI files is working", {
  check_wb()

  tdir <- tempdir()

  fnames <- ciftiTools.files()

  for (cii_fname in fnames$cifti) {
    cat("\n\n"); cat(cii_fname); cat("\n\n")

    # read_cifti with resampling
    brainstructures <- info_cifti(cii_fname)$cifti$brainstructures
    if (identical(brainstructures, "subcortical")) { next }
    cii <- readcii(
      cii_fname, brainstructures=brainstructures,
      resamp_res=5000
    )
    cii <- resample_cifti(cii, resamp_res=1000)

    if (cii_fname == "dtseries") {
      # read_cifti with resampling, with surfaces
      cii <- readcii(
        cii_fname, brainstructures=brainstructures,
        resamp_res=10000, surfL=fnames$surf["left"], surfR=fnames$surf["right"]
      )
      cii <- resample_cifti(cii, resamp_res=1000)

      # resample_surf
      surf <- resample_surf(cii$surf$cortex_left, hemisphere="left", resamp_res=3000)

      # resample_cifti
      cii2_fnames <- resample_cifti(
        cii_fname, basename(cii_fname), resamp_res=4001,
        surfL_original_fname = fnames$surf["left"],
        surfR_original_fname = fnames$surf["right"],
        surfL_target_fname = basename(fnames$surf["left"]),
        surfR_target_fname = basename(fnames$surf["right"]),
        write_dir = tdir
      )
      cii2 <- readcii(
        cii2_fnames["cifti"],
        surfL_fname=cii2_fnames["surfL"], surfR_fname=cii2_fnames["surfR"]
      )

      # resample_cifti_from_template
      cii3 <- readcii(resample_cifti_from_template(
        original=cii_fname, template=cii2_fnames["cifti"],
        target=file.path(tdir, paste0("v2_", basename(cii_fname)))
      ))

      try(testthat::expect_equal(cii2$data, cii3$data))
    }
  }
})
