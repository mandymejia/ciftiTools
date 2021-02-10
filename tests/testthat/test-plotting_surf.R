check_wb <- function() {
  if (is.null(ciftiTools.getOption("wb_path"))) {
    skip("Connectome Workbench is not available.")
  }
}

test_that("plot_xifti_surface and related functions are working", {

  check_wb()


  fnames <- demo_files()
  for (cii_fname in fnames$cifti) {
    cii <- readcii(cii_fname)
    print(cii$meta$cifti$intent)
    plot(cii)
    print("sequential")
    plot(cii, color_mode="sequential")
    plot(cii, color_mode="sequential", zlim=c(2, 5), title="", widget=FALSE); rgl::rgl.close()
    plot(cii, color_mode="sequential", zlim=c(5, 2))
    print("diverging")
    plot(cii, color_mode="diverging")
    #plot(cii, color_mode="diverging", zlim=c(2, 5))
    plot(cii, color_mode="diverging", zlim=c(5, 2))
    #plot(cii, color_mode="diverging", zlim=c(2, 3, 5))
    plot(cii, color_mode="diverging", zlim=c(5, 3, 2))
    print("qualitative")
    if (!is.null(cii$meta$cifti$intent) && cii$meta$cifti$intent != 3007) {
      cii$data$cortex_left[] <- round(cii$data$cortex_left*5)
      cii$data$cortex_right[] <- round(cii$data$cortex_right*5)
      plot(cii, color_mode="qualitative")
      plot(cii, color_mode="qualitative", zlim=3)
      #plot(cii, color_mode="qualitative", zlim=30)
      cii <- convert_to_dlabel(cii)
      plot(cii)
    }
  }

  plot(make_surf(demo_files()$surf["left"]))

})
