check_wb <- function() {
  wb_path <- try(get_wb_cmd_path(ciftiTools.getOption("wb_path"), verbose=FALSE), silent=TRUE)
  if ("try-error" %in% class(wb_path)) {
    skip("Connectome Workbench is not available.")
  }
}

test_that("Miscellaneous functions are working", {
  check_wb()

  tdir <- tempdir()

  fnames <- ciftiTools:::get_example_files()

  for (cii_fname in fnames$cifti) {
    cat("\n\n"); cat(cii_fname); cat("\n\n")

    brainstructures <- info_cifti(cii_fname)$cifti$brainstructures

    # smooth_cifti
    cii <- read_cifti(
      smooth_cifti(
        cii_fname, file.path(tdir, basename(cii_fname)),
        surface_sigma=3, volume_sigma=3,
        surfL_fname=fnames$surf["left"],
        surfR_fname=fnames$surf["right"],
        subcortical_zeroes_as_NA=TRUE
      ),
      brainstructures = brainstructures
    )

    # unmask_cortex
    if ("left" %in% brainstructures) {
      cor2 <- unmask_cortex(cii$data$cortex_left, cii$meta$cortex$medial_wall_mask$left)
    }
    if ("right" %in% brainstructures) {
      cor2 <- unmask_cortex(cii$data$cortex_right, cii$meta$cortex$medial_wall_mask$right)
    }

    # unmask_vol
    if ("subcortical" %in% brainstructures) {
      vol2 <- unmask_vol(cii$data$subcort, cii$meta$subcort$mask)
      labs2 <- unmask_vol(as.numeric(cii$meta$subcort$labels), cii$meta$subcort$mask)
      sub2 <- ciftiTools:::make_subcort(vol2, labs2)
      #sub2 <- make_subcort(vol2, labs2, cii$meta$subcort$mask)
      testthat::expect_equal(sub2$data, cii$data$subcort)
      testthat::expect_equal(sub2$labels, cii$meta$subcort$labels)
    }
  }


})
