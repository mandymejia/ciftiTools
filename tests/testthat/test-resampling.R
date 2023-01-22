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

    if (grepl("dtseries", cii_fname)) {

      # read_cifti with resampling, with surfaces
      cii <- readcii(
        cii_fname, brainstructures=brainstructures,
        resamp_res=10000, surfL=fnames$surf["left"], surfR=fnames$surf["right"]
      )
      cii <- resample_cifti(cii, resamp_res=1000)

      # same as above, with adaptive
      cii <- readcii(
        cii_fname, brainstructures=brainstructures,
        resamp_res=10000, resamp_method="adaptive",
        areaL_original_fname=ciftiTools.files()$surf["left"],
        areaR_original_fname=ciftiTools.files()$surf["right"],
        surfL=fnames$surf["left"], surfR=fnames$surf["right"]
      )
      cii <- remove_xifti(cii, c("cortex_right", "surf_right"))
      surfL_10k_fname <- paste0(tempfile(), "_L.surf.gii")
      write_surf(cii$surf$cortex_left, surfL_10k_fname)
      cii2 <- resample_cifti(cii, resamp_res=2000, surfL_original_fname = surfL_10k_fname)
      cii_10k_fname <- paste0(tempfile(), ".dtseries.nii")
      write_cifti(cii, cii_10k_fname)
      cii_2k_fname <- paste0(tempfile(), ".2k.dtseries.nii")
      resample_cifti(cii_10k_fname, cii_2k_fname, resamp_res=2000)

      # resample_surf
      surf <- resample_surf(cii$surf$cortex_left, hemisphere="left", resamp_res=3000)

      tdir <- tempdir()

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

  # resample_cifti_from_template, unequal res
  x <- read_cifti(fnames$cifti[1], resamp_res=2000, brainstructures="left")
  y <- read_cifti(fnames$cifti[2], resamp_res=4000, brainstructures="right")
  z <- combine_xifti(x,y)
  q <- write_cifti(z, paste0(tempfile(), ".dtseries.nii"))
  x <- resample_cifti(z, resamp_res=8000); x <- remove_xifti(x, "cortex_left")
  y <- resample_cifti(z, resamp_res=3000); y <- remove_xifti(y, "cortex_right")
  z <- combine_xifti(x,y)
  q2 <- write_cifti(z, paste0(tempfile(), ".dtseries.nii"))
  q3 <- resample_cifti_from_template(q, q2, paste0(tempfile(), ".dtseries.nii"))
  z <- read_cifti(q3)
})
