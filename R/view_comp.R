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
#'  arranged by row. If the \code{layout} is \code{"arrayL"} or \code{"pairL"},
#'  put the legend image path last. 
#' @param layout \code{"array"} (default) which is suitable for displaying
#'  multiple cortex plots or multiple subcortex plots,; \code{"pair"} which is
#'  suitable for displaying one cortex plot and one subcortex plot side-by-side;
#'  or \code{"arrayL"} or \code{"pairL"} to add a legend to the bottom.
#' @param title A length-one character vector to use as the title, or \code{NULL}
#'  (defuault) to not add a title.
#' @param title_height,legend_height Heights of the title and legend, if 
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
  img, layout=c("array", "pair", "arrayL", "pairL"), 
  title=NULL, title_height=.1, title_fsize=1.5,
  legend_height=.3, newpage=TRUE, ...) {

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
  legend <- NULL
  layout <- match.arg(layout, c("array", "pair", "arrayL", "pairL"))
  use_legend <- endsWith(layout, "L")
  layout <- gsub("L$", "", layout)
  if (use_legend) {
    legend <- img[[length(img)]]
    img <- img[seq(length(img)-1)]
    stopifnot(
      (is.numeric(legend_height) && length(legend_height)==1) && legend_height>0
    )
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
