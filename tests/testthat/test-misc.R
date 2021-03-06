check_wb <- function() {
  if (is.null(ciftiTools.getOption("wb_path"))) {
    skip("Connectome Workbench is not available.")
  }
}

test_that("Miscellaneous functions are working", {
  check_wb()

  tdir <- tempdir()

  fnames <- ciftiTools:::demo_files()

  surfL_6k_fname <- file.path(tdir, "L_6k.surf.gii")
  resample_gifti(
    fnames$surf["left"], surfL_6k_fname,
    hemisphere="left", resamp_res=6000
  )
  surfR_6k_fname <- file.path(tdir, "R_6k.surf.gii")
  resample_gifti(
    fnames$surf["right"], surfR_6k_fname,
    hemisphere="right", resamp_res=6000
  )

  surfL_1k_fname <- file.path(tdir, "L_1k.surf.gii")
  resample_gifti(
    fnames$surf["left"], surfL_1k_fname,
    hemisphere="left", resamp_res=1000
  )
  surfR_1k_fname <- file.path(tdir, "R_1k.surf.gii")
  resample_gifti(
    fnames$surf["right"], surfR_1k_fname,
    hemisphere="right", resamp_res=1000
  )

  # List Options
  ciftiTools.listOptions()

  for (cii_fname in fnames$cifti) {
    cat("\n\n"); cat(cii_fname); cat("\n\n")

    brainstructures <- info_cifti(cii_fname)$cifti$brainstructures

    surf_fnames <- switch(gsub(".nii", "", ciftiTools:::get_cifti_extn(cii_fname), fixed=TRUE),
      dscalar = list(left=surfL_6k_fname, right=surfR_6k_fname),
      dtseries = list(left=fnames$surf["left"], right=fnames$surf["right"]),
      dscalar_ones = list(left=surfL_1k_fname, right=surfR_1k_fname),
      dlabel = list(left=surfL_6k_fname, right=surfR_6k_fname)
    )

    # smooth_cifti
    # not sure why it doesn't work for ones_1k (because all data are equal?)
    if (!grepl("ones_1k", cii_fname) && !grepl("dlabel", cii_fname)) {
      cii <- read_cifti(
        smooth_cifti(
          cii_fname, file.path(tdir, basename(cii_fname)),
          surf_FWHM=3, vol_FWHM=3,
          surfL_fname=surf_fnames$left,
          surfR_fname=surf_fnames$right,
          subcortical_zeroes_as_NA=TRUE
        ),
        brainstructures = "all" #warning should happen if not all are present
      )
      cii <- smooth_cifti(
        cii, file.path(tdir, basename(cii_fname)),
        surf_FWHM=5, vol_FWHM=5,
        surfL_fname=surf_fnames$left,
        surfR_fname=surf_fnames$right,
        subcortical_zeroes_as_NA=TRUE
      )
      cii <- smooth_cifti(
        cii, file.path(tdir, basename(cii_fname)),
        surf_FWHM=7, vol_FWHM=7
      )
    }

    cii <- read_cifti(cii_fname, brainstructures = brainstructures)
    if (!is.null(cii$meta$cortex$medial_wall_mask$left)) {
      cii <- add_surf(cii, surfL=resample_surf(surf_fnames$left, resamp_res=length(cii$meta$cortex$medial_wall_mask$left)))
    }

    # remove_xifti (not exported)
    cii <- ciftiTools:::remove_xifti(cii, c("cortex_left", "sub", "surf_right"))

    # unmask_cortex
    if (!is.null(cii$data$cortex_left)) {
      cor2 <- unmask_cortex(
        cii$data$cortex_left,
        cii$meta$cortex$medial_wall_mask$left
      )
    }
    if (!is.null(cii$data$cortex_right)) {
      cor2 <- unmask_cortex(
        cii$data$cortex_right,
        cii$meta$cortex$medial_wall_mask$right
      )
    }

    # unmask_vol
    if (!is.null(cii$data$subcort)) {
      vol2 <- unmask_vol(cii$data$subcort, cii$meta$subcort$mask)
      labs2 <- unmask_vol(
        as.numeric(cii$meta$subcort$labels),
        cii$meta$subcort$mask
      )
      sub2 <- ciftiTools:::make_subcort(vol2, labs2)
      #sub2 <- make_subcort(vol2, labs2, cii$meta$subcort$mask)
      testthat::expect_equal(sub2$data, cii$data$subcort)
      testthat::expect_equal(sub2$labels, cii$meta$subcort$labels)
    }

    # Operations
    cii + cii + cii
    cii - cii / (abs(cii) + 1)
    (5*cii) %% round(cii, 1)
    if (!grepl("dlabel", cii_fname)) {
      testthat::expect_equal(exp(1)^log(cii) + 0, cii*1)
    }

    # Select
    L <- ncol(do.call(rbind, cii$data))
    if (L > 1) {
      cii <- select_xifti(cii, seq(2,1))
      # Concat
      cii <- concat_xifti(xifti_list=list(concat_xifti(cii, cii), cii))
      testthat::expect_equal(
        select_xifti(cii, rep(seq(ncol(do.call(rbind, cii$data))), 2))$data,
        concat_xifti(cii, cii)$data
      )
    }
    # [TO DO]: Test concatenating xiftis of different types
  }

})
