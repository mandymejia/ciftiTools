check_wb <- function() {
  wb_path <- try(get_wb_cmd_path(ciftiTools.getOption("wb_path"), verbose=FALSE), silent=TRUE)
  if ("try-error" %in% class(wb_path)) {
    skip("Connectome Workbench is not available.")
  }
}

expect_equal_cifti_data <- function(cii1, cii2) {
  expect_equal(cii1$data, cii2$data)
  expect_equal(cii1$meta$subcort$labels, cii1$meta$subcort$labels)
  expect_equal(cii1$meta$subcort$mask, cii1$meta$subcort$mask)
  expect_equal(cii1$meta$cortex$medial_wall_mask$left, cii1$meta$cortex$medial_wall_mask$left)
  expect_equal(cii1$meta$cortex$medial_wall_mask$right, cii1$meta$cortex$medial_wall_mask$right)
}

test_that("Writing CIFTI and GIFTI files is working", {
  check_wb()

  tdir <- tempdir()

  fnames <- ciftiTools:::get_example_files()

  for (cii_fname in fnames$cifti) {
    cat("\n\n"); cat(cii_fname); cat("\n\n")

    # Read the CIFTI
    cii_info <- info_cifti(cii_fname)
    brainstructures <- cii_info$cifti$brainstructures
    cii <- read_cifti(cii_fname, brainstructures=brainstructures)
    cii_fname2 <- file.path(tdir, paste0("temp.", ciftiTools:::get_cifti_extn(cii_fname)))
    if (grepl("dlabel", cii_fname2)) { next }

    # Write and read back in
    write_cifti(cii, cii_fname2)
    stopifnot(file.exists(cii_fname2))
    cii2 <- read_cifti(cii_fname2, brainstructures=brainstructures)

    # Check if same
    # (Some metadata will be different)
    expect_equal_cifti_data(cii, cii2)

    # Write separate components
    cii_sep <- separate_cifti(cii_fname, write_dir=tdir, brainstructures=brainstructures)
    parts <- list()

    # Put back together
    if ("left" %in% brainstructures) {
      parts <- c(
        parts, 
        list(
          cortexL = cii_sep["cortexL"], 
          cortexL_mwall = cii_sep["ROIcortexL"]
        )
      )
    }
    if ("right" %in% brainstructures) {
      parts <- c(
        parts, 
        list(
          cortexR = cii_sep["cortexR"], 
          cortexR_mwall = cii_sep["ROIcortexR"]
        )
      )    }
    if ("subcortical" %in% brainstructures) {
      parts <- c(
        parts, 
        list(
          subcortVol = cii_sep["subcortVol"],
          subcortLabs = cii_sep["subcortLabs"],
          subcortMask = cii_sep["ROIsubcortVol"]
        )
      )
    }
    cii2 <- do.call(ciftiTools:::make_xifti, parts)

    # Check if same
    # (Some metadata will be different)
    expect_equal_cifti_data(cii, cii2)
  }

  # Writing surfaces
  surfL <- make_surf(fnames$surf["left"])
  surfR <- make_surf(fnames$surf["right"])
  surfL_fname2 <- file.path(tdir, "temp_L.surf.gii")
  surfR_fname2 <- file.path(tdir, "temp_R.surf.gii")
  write_surf_gifti(surfL, surfL_fname2, hemisphere="left")
  write_surf_gifti(surfR, surfR_fname2, hemisphere="right")
  expect_equal(surfL, make_surf(surfL_fname2))
  expect_equal(surfR, make_surf(surfR_fname2))
})
