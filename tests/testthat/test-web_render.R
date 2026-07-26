# Tests for the web-render (Tahoe / opt-in) backend.
# Add new web-render regressions here rather than a new file per bug.
# Each test uses with_web_render() below to force the backend on regardless
# of host platform, so these run anywhere with Connectome Workbench.

check_wb <- function() {
  if (is.null(ciftiTools.getOption("wb_path"))) {
    skip("Connectome Workbench is not available.")
  }
}

# Force web-render on regardless of host platform, restore on exit.
# Also opens a null pdf() device so the categorical legend's print(cleg) call
# (base R graphics from view_xifti_surface) doesn't spawn an Rplots.pdf.
with_web_render <- function(code) {
  old_web  <- getOption("ciftiTools.web_render")
  old_null <- getOption("rgl.useNULL")
  grDevices::pdf(NULL)
  on.exit({
    # view_xifti_surface may close the pdf device itself; ignore "no device"
    tryCatch(grDevices::dev.off(), error = function(e) NULL)
    options(rgl.useNULL = old_null, ciftiTools.web_render = old_web)
  })
  options(rgl.useNULL = TRUE, ciftiTools.web_render = TRUE)
  force(code)
}

load_xii <- function(intent = "dlabel") {
  fnames <- ciftiTools.files()
  surfL <- read_surf(fnames$surf["left"])
  surfR <- read_surf(fnames$surf["right"])
  add_surf(
    read_cifti(fnames$cifti[intent], brainstructures = c("left","right")),
    surfL = surfL, surfR = surfR
  )
}

test_that("plot(), view_xifti(), view_xifti_surface() all return widget visibly on web-render", {
  check_wb()
  with_web_render({
    xii <- load_xii("dlabel")

    r1 <- withVisible(view_xifti_surface(xii, idx = 1))
    expect_true(r1$visible)
    expect_s3_class(r1$value, "htmlwidget")

    r2 <- withVisible(view_xifti(xii, idx = 1))
    expect_true(r2$visible)
    expect_s3_class(r2$value, "htmlwidget")

    r3 <- withVisible(plot(xii, idx = 1))
    expect_true(r3$visible)
    expect_s3_class(r3$value, "htmlwidget")
  })
})

test_that("Damon's dscalar plot() repro auto-prints on web-render", {
  check_wb()
  with_web_render({
    xii <- load_xii("dscalar")
    r <- withVisible(plot(xii, color_mode = "diverging"))
    expect_true(r$visible)
    expect_s3_class(r$value, "htmlwidget")
  })
})

test_that("plot() with fname=png stays invisible and writes the file (side-effect only)", {
  check_wb()
  with_web_render({
    xii <- load_xii("dlabel")
    tmp <- tempfile(fileext = ".png")
    on.exit(unlink(tmp))
    r <- withVisible(plot(xii, idx = 1, fname = tmp))
    expect_false(r$visible)
    expect_type(r$value, "character")
    expect_true(file.exists(tmp))
  })
})

test_that("plot() with explicit widget=FALSE on web-render warns and forces widget=TRUE", {
  check_wb()
  with_web_render({
    xii <- load_xii("dscalar")
    expect_warning(
      r <- withVisible(plot(xii, idx = 1, widget = FALSE)),
      "web-render"
    )
    expect_true(r$visible)
    expect_s3_class(r$value, "htmlwidget")
  })
})
