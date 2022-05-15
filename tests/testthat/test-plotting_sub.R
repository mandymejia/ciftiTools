check_wb <- function() {
  if (is.null(ciftiTools.getOption("wb_path"))) {
    skip("Connectome Workbench is not available.")
  }
}

test_that("plot_xifti_volume and related functions are working", {

  check_wb()

  fnames <- ciftiTools.files()
  cii <- read_cifti(fnames$cifti["dscalar_ones"], brainstructures="all")
  plot(cii); rgl::rgl.close()
  view_xifti_volume(
    cii, color_mode="diverging", colors="Spectral",
    zlim=c(2, 5), title="Abcd", legend_embed=FALSE
  )
  x <- convert_xifti(cii, "dlabel")
  testthat::expect_warning(view_xifti_volume(x, slices=seq(12), plane="sag"))
  # view_xifti_volume(cii, plane="sag", slices=20) fails???
  plot(cii, plane="sag", color_mode="qualitative"); rgl::rgl.close()

  cii <- read_cifti(fnames$cifti["dscalar_ones"], brainstructures="sub")
  plot(cii, convention="rad", orientation_labels=TRUE)
})
