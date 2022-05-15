check_wb <- function() {
  if (is.null(ciftiTools.getOption("wb_path"))) {
    skip("Connectome Workbench is not available.")
  }
}

test_that("plot_xifti `together` argument is working", {

  check_wb()

  fnames <- ciftiTools.files()

  # CORTEX ---------------------------------------------------------------------
  x <- read_cifti(fnames$cifti["dscalar"])
  y <- transform_xifti(x, function(q){rnorm(length(q))})
  x <- merge_xifti(x, y, x+y)
  x$meta$cifti$names <- c("dat", "datB", "rand", "randB", "sum", "sumB")
  # Plots with saving
  tdir <- tempdir()
  q <- plot(x, idx=c(3,2,2,1), together="idx", together_title="hi", fname=file.path(tdir, "abc"))
  view_comp(q)
  view_comp(c(q, q), nrow=1, title_height=.05, title="Hello")
  view_comp(c(q, q, q), title="Goodbye", ncol=1, fname=paste0(tempfile(), ".png"))
  q <- plot(x, idx=c(3,2,2,1), together="idx", together_title="hi", fname=file.path(tdir, "abc.png"))
  q <- plot(x, idx=c(3,2,2,1), together="idx", together_title="hi", together_ncol=1, fname=file.path(tdir, "abc.pdf"))
  q <- plot(x, idx=c(3,1), together="idx", legend_embed=FALSE, colors="Spectral", fname=file.path(tdir, "abc"))
  q <- plot(x, idx=c(3,1), together="leg", color_mode="seq", fname=file.path(tdir, "abc"))
  q <- plot(x, idx=c(3,1), together="leg", color_mode="seq", fname=file.path(tdir, "abc.pdf"))
  q <- plot(x, idx=seq(4), together="leg", view="lateral", together_title="hello", fname=file.path(tdir, "abc.png"))
  q <- plot(x, idx=seq(3,6), together="leg", view="lateral", together_title="hello", fname=file.path(tdir, "abc.png"), fname_suffix="idx", bg="black", text_color="white")
  q <- plot(x, idx=5, together=c("idx", "leg"), view="medial", hemisphere="left", together_title="hey", fname=file.path(tdir, "qrs"))
  testthat::expect_warning(
    q <- plot(x, idx=c(6,5,1), together=c("idx", "leg"), slider_title="zzz", fname=file.path(tdir, c("a", "b", "c")))
  )
  q <- plot(x, idx=c(6,5,1), together=c("leg"), slider_title="zzz", fname=file.path(tdir, c("a", "b", "c")))

  # SUBCORTEX ------------------------------------------------------------------

  x <- read_cifti(fnames$cifti["dscalar_ones"], brainstructures="subc")
  x <- select_xifti(x, idx=c(1,1,1))
  x$meta$cifti$names <- c("ones", "rand", "sum")
  x$data$subcort[,2] <- rnorm(nrow(x$data$subcort))
  x$data$subcort[,3] <- rowSums(x$data$subcort[,seq(2)])
  # Plots in RStudio window
  plot(x, idx=c(1,1,1), together="idx", what="vol")
  plot(x, idx=seq(3), together="idx", what="vol")
  plot(x, color_mode="sequential", zlim=c(-5, 5), together=c("idx", "leg"))
  plot(x, idx=c(2,3), color_mode="sequential", zlim=c(-2, 2), together=c("idx", "leg"))
  plot(x, idx=c(2,3), color_mode="sequential", zlim=c(-2, 2), together=c("leg"))
  plot(x*100, idx=c(2,3,3,2), together_title="hello", digits=1, together=c("idx"))
  plot(x, idx=c(2,1,3,1), together=c("idx", "leg"), text_color="black", bg="white", together_title="abc")
  # Plots with saving
  tdir <- tempdir()
  q <- plot(x, idx=c(3,2,2,1), together="idx", together_title="hi", fname=file.path(tdir, "abc"))
  q <- plot(x, idx=c(3,2,2,1), together="idx", together_title="hi", fname=file.path(tdir, "abc.png"))
  q <- plot(x, idx=c(3,2,2,1), together="idx", together_title="hi", fname=file.path(tdir, "abc.pdf"))
  q <- plot(x, idx=c(3,1), together="idx", legend_embed=FALSE, colors="Spectral", fname=file.path(tdir, "abc"))
  q <- plot(x, idx=c(3,1), together="leg", color_mode="seq", plane="cor", fname=file.path(tdir, "abc"))
  q <- plot(x, idx=c(3,1), together="leg", color_mode="seq", plane="cor", fname=file.path(tdir, "abc.pdf"))
  # fname=TRUE
  olddir <- getwd()
  setwd(tdir)
  q <- plot(x, idx=c(3,2,2,1), together="idx", together_title="hi", fname=TRUE)
  q <- plot(x, idx=2, together="idx", fname=TRUE)
  q <- plot(x, idx=c(3,1), together="idx", legend_embed=FALSE, colors="Spectral", fname=TRUE)
  q <- plot(x, idx=c(3,1), together="leg", color_mode="seq", plane="cor", fname=TRUE)
  setwd(olddir)

  # QUALITATIVE
  # Plots in RStudio window
  plot(x > 1, color_mode="qual", idx=c(2,1,3,1), together=c("idx", "leg"), text_color="black", bg="white", together_title="abc")
  plot(x > 1, color_mode="qual", idx=c(2,1,3,1), together="idx", text_color="black", bg="white", together_title="abc")
  plot(x > 0, idx=c(3,2,1), together="idx", n_slices=4, plane="sag", color_mode="qual", colors=c("red", "blue"))
  plot(x > 0, idx=c(3,2,1), together=c("idx", "leg"), n_slices=4, plane="sag", color_mode="qual", colors=c("red", "blue"))
  plot(x > 0, together="idx", color_mode="qual")
  # Plots with saving
  setwd(tdir)
  q <- plot(x > 2, idx=c(2,1), together="idx", fname=TRUE, color_mode="qual")
  q <- plot(x > 2, idx=c(3,2,2,1), together="idx", fname=TRUE, color_mode="qual")
  q <- plot(x > 2, idx=c(3,2,2,1), together=c("idx", "leg"), fname=TRUE, color_mode="qual")
  q <- plot(x > 2, idx=c(3,2,1), together="leg", fname=TRUE, color_mode="qual")
  q <- plot(x > 2, idx=c(3,2,1), together="leg", fname="abc.pdf", color_mode="qual")

  # DLABEL XIFTI WITH MULTIPLE LEVELS
  setwd(olddir)
  x <- round(round(x/3)*2)
  y <- x; y$data$subcort[] <- as.numeric(y$meta$subcort$labels)
  y <- transform_xifti(y, function(q){c(rep(1, 5), rep(3, 5), rep(10, 20))[q+1]})
  x <- convert_xifti(x, "dlabel")
  y <- convert_xifti(y, "dlabel")
  plot(y, idx=c(1,1,1), together="idx", what="vol")
  plot(x, idx=seq(3), together="idx", what="vol")
  testthat::expect_warning(
    plot(x, idx=seq(3), together="idx", what="vol", colors="Spectral")
  )
  plot(x, together=c("idx", "leg"))
  plot(x, together=c("idx", "leg"), legend_alllevels=TRUE)
  plot(x, idx=c(2,3), together=c("idx", "leg"), colors=c(rep("white", 5), rep("black", 7)))
  plot(x, idx=c(2,3), together=c("leg"))
  plot(transform_xifti(x, function(q){ifelse(q %in% seq(4,8), 2, q)}), idx=c(2,3,3,2), together_title="hello", together=c("idx"))
  plot(x, idx=c(2,1,3,1), together=c("idx", "leg"), text_color="black", bg="white", together_title="abc")
})
