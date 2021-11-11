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
#' @param layout \code{"array"} (default) which is suitable for displaying
#'  multiple cortex plots or multiple subcortex plots, or \code{"pair"} which is
#'  suitable for displaying one cortex plot and one subcortex plot side-by-side.
#' @param legend File path to a legend image to add, or \code{NULL} (default)
#'  to not add a legend.
#' @param title A length-one character vector to use as the title, or \code{NULL}
#'  (default) to not add a title.
#' @param legend_height,title_height Heights of the legend and title, if 
#'  applicable. Specified relative to all the plots, so \code{.1} would
#'  mean the height is a tenth of the height of all the plots. Default:
#'  \code{.1} for the title and \code{.3} for the legend.
#' @param title_fsize Multiplier for font size. Default: \code{1.5}
#' @param newpage Call \code{grid::grid.newpage} at the start? Default: \code{TRUE}.
#' @param ... Additional arguments to \code{gridExtra::arrangeGrob}. The
#'  arguments \code{grobs}, \code{layout_matrix}, \code{nrow}, and \code{ncol}
#'  should be avoided because they are determined based on \code{img} and
#'  \code{layout}. If the \code{layout} is \code{"pair"} or \code{"pairL"},
#'  adjusting \code{widths} may be useful, e.g. to make the subcortex subplot
#'  be less wide than the cortex subplot.
#'  
#' 
#' @export
#' 
#' @return The composite plot
view_comp <- function(
  img, layout=c("array", "pair"), 
  legend=NULL, title=NULL, 
  legend_height=.3, title_height=.1, title_fsize=1.5,
  newpage=TRUE, ...) {

  # Check packages
  pkg <- vapply(c("png", "grid", "gridExtra"), requireNamespace, quietly=TRUE, FALSE)
  if (any(!pkg)) {
    stop(
      "These packages need to be installed to use `view_comp`: ", 
      paste(names(pkg)[!pkg], collapse=", "), call.=FALSE
    )
  }

  if (newpage) { grid::grid.newpage() }

  # Load images as grobs
  img <- as.character(img)
  img <- lapply(img, png::readPNG)
  img <- lapply(img, grid::rasterGrob)

  # Check for legend. If present, separate it out from the plot images
  layout <- match.arg(layout, c("array", "pair"))

  # Check legend
  use_legend <- !is.null(legend)
  if (use_legend) {
    legend <- as.character(legend)[1]
    stopifnot(is.numeric(legend_height) && length(legend_height)==1)
    stopifnot(legend_height>0)
    legend <- grid::rasterGrob(crop_image(png::readPNG(legend), 2))
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
  } else {
    title_height <- NULL
  }

  # Arrange the plots
  if (layout == "array") {
    comp <- gridExtra::arrangeGrob(grobs=img, ...)
  } else if (layout == "pair") {
    comp <- gridExtra::arrangeGrob(grobs=img, ncol=2, ...)
  } else {
    stop()
  }

  # Add title and legend, if applicable
  if (use_legend || use_title) {
    grobs <- c(list(title), list(comp), list(legend))
    grobs <- grobs[!vapply(grobs, is.null, FALSE)]
    comp <- gridExtra::arrangeGrob(
      grobs=grobs, ncol=1, heights=c(title_height, 1, legend_height)
    )
  }

  grid::grid.draw(comp)
  invisible(comp)
}
