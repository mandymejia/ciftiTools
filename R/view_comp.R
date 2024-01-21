#' Crop image
#' 
#' Crop whitespace from image
#' 
#' @param x An image read by \code{png::readPNG}, or file path to that
#' @param dims The dimensions to crop. Default: \code{seq(2)}.
#' @return The cropped image
#' 
#' @keywords internal
#' 
crop_image <- function(x, dims=seq(2)){
  use_file <- is.character(x)
  if (use_file) {
    x_fname <- x[1]
    if (!requireNamespace("png", quietly = TRUE)) {
      stop("Package \"png\" needed to crop image. Please install it.", call. = FALSE)
    }
    x <- png::readPNG(x_fname)
  }

  # Crop
  y <- rowSums(x[,,seq(3)], dims=2) != 3
  if (!any(y)) { return(x) }
  if (1 %in% dims) {
    y1 <- which(apply(y, 1, any))
  } else {
    y1 <- c(1, nrow(y))
  }
  if (2 %in% dims) {
    y2 <- which(apply(y, 2, any))
  } else {
    y2 <- c(1, ncol(y))
  }

  x <- x[
    seq(min(y1), max(y1)),
    seq(min(y2), max(y2)),
  ]

  if (use_file) {
    png::writePNG(x, x_fname)
    return(x_fname)
  } else {
    return(x)
  }
}

#' View composite of images
#' 
#' Create a single image which displays multiple image files. Tailored to support
#'  composite layouts of plots from \code{\link{view_xifti}}.
#' 
#' Requires the following packages: \code{png}, \code{grid}, \code{gridExtra}
#' 
#'  How it works: the non-legend images (plots) are composited in a call to 
#'  \code{grid::arrangeGrob}. If a title or legend exists, it's added to the top and
#'  bottom, respectively, of the plots after with another call to \code{grid::arangeGrob}.
#' 
#' @param img Character vector of paths to images to include. They will be
#'  arranged by row.  
#' @param ncol,nrow Control the layout of the composite image. \code{NULL} (default) 
#'  will use approximately same numbers of rows and columns.
#' @param legend File path to a legend image to add, or \code{NULL} (default)
#'  to not add a legend.
#' @param title A length-one character vector to use as the title, or \code{NULL}
#'  (default) to not add a title.
#' @param legend_height,title_height Heights of the legend and title, if 
#'  applicable. Specified relative to all the plots, so \code{.1} would
#'  mean the height is a tenth of the height of all the plots. Default:
#'  \code{.1} for the title and \code{.3} for the legend.
#' @param title_fsize Multiplier for font size. Default: \code{5}
#' @param newpage Call \code{grid::grid.newpage} before rendering? 
#'  Default: \code{is.null(fname)}.
#' @param fname If \code{NULL} (default), print the result. Otherwise, save to
#'  a PNG file at this location. Will override \code{newpage} to \code{FALSE}.
#' @param ... Additional arguments to \code{gridExtra::arrangeGrob}. The
#'  arguments \code{grobs} and \code{layout_matrix} should be avoided because 
#'  they are determined based on \code{img}. adjusting \code{widths} may be useful, 
#'  e.g. to make the subcortex subplot be less wide than the cortex subplot.
#'  
#' @family visualizing
#' @export
#' 
#' @return The composite plot
view_comp <- function(
  img, ncol=NULL, nrow=NULL, 
  legend=NULL, title=NULL, 
  legend_height=.3, title_height=.1, title_fsize=5,
  newpage=is.null(fname), fname=NULL, ...) {

  # Check packages
  pkg <- vapply(c("png", "grid", "gridExtra"), requireNamespace, quietly=TRUE, FALSE)
  if (any(!pkg)) {
    stop(
      "These packages need to be installed to use `view_comp`: ", 
      paste(names(pkg)[!pkg], collapse=", "), call.=FALSE
    )
  }

  write_comp <- !is.null(fname)

  # Load images as grobs
  img <- as.character(img)
  img <- lapply(img, png::readPNG)
  img <- lapply(img, grid::rasterGrob)

  # Arrange the plots
  comp <- gridExtra::arrangeGrob(grobs=img, ncol=ncol, nrow=nrow, ...)
  
  if (write_comp) {
    comp_dim <- dim(comp)
    dtable <- expand.grid(lapply(rev(comp_dim), seq))[seq(length(img)),]
    dtable$height <- vapply(img, function(x){dim(x$raster)[1]}, 0)
    dtable$width <- vapply(img, function(x){dim(x$raster)[2]}, 0)
    row_heights <- vapply(seq(comp_dim[1]), function(x){
      max(dtable[dtable$Var2==x,"height"])}, 0
    )
    col_widths <- vapply(seq(comp_dim[2]), function(x){
      max(dtable[dtable$Var1==x,"width"])}, 0
    )
    # `img_dim`: height by width
    # `img_dim2`: is the composited images plus title and legend
    img_dim <- img_dim2 <- c(sum(row_heights), sum(col_widths))
  }

  # comp_dims <- list(
  #   height=vapply(img, function(x){dim(x)[1]}, 0),
  #   width=vapply(img, function(x){dim(x)[2]}, 0)
  # )

  # Check legend
  use_legend <- !is.null(legend)
  if (use_legend) {
    legend <- as.character(legend)[1]
    stopifnot(is.numeric(legend_height) && length(legend_height)==1)
    stopifnot(legend_height>0)
    legend <- grid::rasterGrob(crop_image(png::readPNG(legend), 2))
    if (write_comp) { img_dim2[1] <- img_dim2[1] + img_dim2[1]*legend_height }
  } else {
    legend_height <- NULL
  }

  # Check title
  use_title <- !is.null(title)
  if (use_title) { 
    title <- as.character(title)[1]
    stopifnot(
      (is.numeric(title_height) && length(title_height)==1) && title_height>0
    )
    title_size <- grid::get.gpar("fontsize")$fontsize * title_fsize
    title <- grid::textGrob(title, gp=grid::gpar(fontsize=title_size))
    if (write_comp) { img_dim2[1] <- img_dim2[1] + img_dim2[1]*title_height }
  } else {
    title_height <- NULL
  }

  # Add title and legend, if applicable
  if (use_legend || use_title) {
    grobs <- c(list(title), list(comp), list(legend))
    grobs <- grobs[!vapply(grobs, is.null, FALSE)]
    comp <- gridExtra::arrangeGrob(
      grobs=grobs, ncol=1, heights=c(title_height, 1, legend_height)
    )
  }

  if (write_comp) {
    if (newpage) { warning("Ignoring `newpage`.") }
    if (endsWith(fname, "png")) { 
      png(fname, height=img_dim2[1], width=img_dim2[2])
    } else if (endsWith(fname, "pdf")) {
      pdf(fname, height=img_dim2[1], width=img_dim2[2])
    } else {
      stop("`fname` should end with 'png' or 'pdf'.")
    }
  } else {
    if (newpage) { grid::grid.newpage() }
  }
  grid::grid.draw(comp)
  if (write_comp) { 
    dev.off()
    return(fname)
  } else {
    return(invisible(comp))
  }
}
