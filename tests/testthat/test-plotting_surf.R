check_wb <- function() {
  if (is.null(ciftiTools.getOption("wb_path"))) {
    skip("Connectome Workbench is not available.")
  }
}

test_that("plot_xifti_surface and related functions are working", {

  check_wb()

  plot2 <- function(...){plot(...); rgl::rgl.close()}

  fnames <- ciftiTools.files
  for (cii_fname in fnames$cifti) {
    cii <- readcii(cii_fname)
    print(cii$meta$cifti$intent)
    plot2(cii)
    print("sequential")
    plot2(cii, color_mode="sequential")
    plot2(cii, color_mode="sequential", zlim=c(2, 5), title="", widget=FALSE)
    plot2(cii, color_mode="sequential", zlim=c(5, 2), surfR="very inflated", surfL="very inflated")
    print("diverging")
    plot2(cii, color_mode="diverging")
    #plot2(cii, color_mode="diverging", zlim=c(2, 5))
    plot2(cii, color_mode="diverging", zlim=c(5, 2))
    #plot2(cii, color_mode="diverging", zlim=c(2, 3, 5))
    plot2(cii, color_mode="diverging", zlim=c(5, 3, 2), surfL="midthickness", surfR="midthickness")
    print("qualitative")
    if (!is.null(cii$meta$cifti$intent) && cii$meta$cifti$intent != 3007) {
      cii$data$cortex_left[] <- round(cii$data$cortex_left*5)
      cii$data$cortex_right[] <- round(cii$data$cortex_right*5)
      plot2(cii, color_mode="qualitative")
      plot2(cii, color_mode="qualitative", zlim=3)
      #plot2(cii, color_mode="qualitative", zlim=30)
      cii <- convert_xifti(cii, "dlabel")
      plot2(cii)
    }
    cii <- readcii(cii_fname, brainstructures=c("left"), resamp_res=2000)
    plot2(cii, hemisphere="both")
    testthat::expect_warning(
      # warning printed, even though they do match
      plot2(cii, surfR=ciftiTools.files$surf["right"], hemisphere="both")
    )
    testthat::expect_warning(
      plot2(cii, surfR="very inflated", hemisphere="both")
    )
  }

  plot2(read_surf(ciftiTools.files$surf["left"]))
  plot2(load_surf(name="midthickness"))
  testthat::expect_warning(
    plot2(as.xifti(surfR=make_surf(ciftiTools.files$surf["right"])), hemisphere="both", title="My Awesome Surfs")
  )

})
