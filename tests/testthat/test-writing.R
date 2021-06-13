check_wb <- function() {
  if (is.null(ciftiTools.getOption("wb_path"))) {
    skip("Connectome Workbench is not available.")
  }
}

test_that("Writing CIFTI and GIFTI files is working", {
  check_wb()

  tdir <- tempdir()

  fnames <- ciftiTools:::demo_files()

  for (cii_fname in fnames$cifti) {
    cat("\n\n"); cat(cii_fname); cat("\n\n")

    # Read the CIFTI
    cii_info <- info_cifti(cii_fname)
    brainstructures <- cii_info$cifti$brainstructures
    cii <- read_cifti(cii_fname, brainstructures=brainstructures)
    cii_fname2 <- file.path(tdir, paste0("temp.", ciftiTools:::get_cifti_extn(cii_fname)))

    # Write and read back in
    write_cifti(cii, cii_fname2)
    stopifnot(file.exists(cii_fname2))
    cii2 <- read_cifti(cii_fname2, brainstructures=brainstructures)

    # Check if same
    # (Some metadata will be different)
    ciftiTools:::expect_equal_xifti(cii, cii2)

    write_cifti(cii, file.path(tdir, "temp")) # intent is not provided in name

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
    cii2 <- do.call(ciftiTools:::make_xifti, c(parts, list(mwall_values=NULL)))

    # Check if same
    # (Some metadata will be different)
    ciftiTools:::expect_equal_xifti(cii, cii2)

    # Remove subcort and test `read_xifti2`
    cii_sep <- as.list(cii_sep[!grepl("subcort", names(cii_sep))])
    names(cii_sep)[names(cii_sep) == "ROIcortexL"] <- "cortexL_mwall"
    names(cii_sep)[names(cii_sep) == "ROIcortexR"] <- "cortexR_mwall"
    cii2 <- do.call(read_xifti2, cii_sep[1])
    cii2 <- do.call(read_xifti2, cii_sep)
    cii2 <- do.call(read_xifti2, c(cii_sep[1], list(resamp_res=1000)))
    cii2 <- do.call(read_xifti2, c(cii_sep, list(resamp_res=1000, surfL=demo_files()$surf["left"])))
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
