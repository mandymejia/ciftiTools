#' View subcortical data in a \code{"xifti"}
#'
#' Visualize the subcortical data in a \code{"xifti"} using a series of 2D
#'  slices (based on \code{\link[oro.nifti]{overlay}}) or an interactive widget
#'  (based on \code{papayar::papaya}). Note: \code{papayar} has been removed
#'  from CRAN so the widget is not available. If \code{papayar} returns to CRAN
#'  the widget will be made available again.
#'
#' Note that \code{color_mode}, \code{zlim}, and \code{colors} only affect the
#'  color scale of the data values whereas \code{structural_img_colors} only
#'  affects the color scale of the background image.
#'
#' Currently, the color-related arguments only affect the 2D slice view. The
#'  color limits and palette must be edited using the widget controls once it's
#'  rendered.
#'
#' Arguments concerning anatomical orientation assume that the subcortical data
#'  is stored in the following way: first dimension is normal to the sagittal
#'  plane, going left to right; second dimension is normal to the coronal plane,
#'  going from the front of the head (anterior) to the back (posterior); third
#'  dimension is normal to the axial plane, going from the top of the head
#'  (superior) to the neck (inferior).
#'
#' For non-interactive plots, if \code{n_slices > 1} and \code{convention="neurological"},
#'  axial slices are ordered from the neck (inferior) to the top of the head
#'  (superior), sagittal slices are ordered left to right, and coronal slices
#'  are ordered back (posterior) to front (anterior). If
#'  \code{convention="radiological"}, sagittal slices are instead ordered right
#'  to left.
#'
#' @inheritParams xifti_Param
#' @param structural_img The structural MRI image on which to overlay the
#'  subcortical plot. Can be a file name, \code{"MNI"} (default) to use
#'  the MNI T1-weighted template included in \code{ciftiTools}, or \code{NULL}
#'  to use a blank image.
#  Note that the colors are not identical.
#' @param color_mode (Optional) \code{"sequential"}, \code{"qualitative"},
#'  \code{"diverging"}, or \code{"auto"} (default). Auto mode will use the
#'  qualitative color mode if the \code{"xifti"} object represents a .dlabel
#'  CIFTI (intent 3007). Otherwise, it will use the diverging mode if the data
#'  contains both positive and negative values, and the sequential mode if the
#'  data contains >90\% positive or >90\% negative values. See
#'  \code{\link{make_color_pal}} for more details.
#' @param zlim (Optional) Controls the mapping of values to each
#'  color in \code{colors}. If the length is longer than
#'  one, using -Inf will set the value to the data minimum, and Inf will set
#'  the value to the data maximum. See \code{\link{make_color_pal}}
#'  description for more details.
#' @param colors (Optional) \code{"ROY_BIG_BL"}, vector of colors to use,
#'  the name of a ColorBrewer palette (see \code{RColorBrewer::brewer.pal.info}
#'  and colorbrewer2.org), the name of a viridisLite palette, or a data.frame
#'  with columns \code{"color"} and \code{"value"} (will override \code{zlim}).
#'  If \code{NULL}
#'  (default), will use the positive half of \code{"ROY_BIG_BL"} (sequential),
#'  \code{"Set2"} (qualitative), or the full \code{"ROY_BIG_BL"} (diverging). An
#'  exception to these defaults is if the \code{"xifti"} object represents a
#'  .dlabel CIFTI (intent 3007), in which case the colors in the label table
#'  will be used. See \code{\link{make_color_pal}} for more details.
#' @param structural_img_colors Colors to use for the background image. These
#'  will be assigned in order from lowest to highest value with equal spacing
#'  between the colors. (\code{color_mode}, \code{zlim} and \code{colors} have
#'  no bearing on the background image colors.) This argument is used as
#'  the \code{col.x} argument to \code{oro.nifti::overlay} directly. Default:
#'  \code{gray(0:255/280)}. To use the \code{oro.nifti::overlay} default instead
#'  set this argument to \code{gray(0:64/64)}.
#'
#' @param idx The time/column index of the data to display. \code{NULL} (default)
#'  will display the first column.
#'
#'  If \code{widget}, only one index at a time may be displayed.
#'
#'  If \code{!widget} and the length of \code{idx} is greater than one, a new
#'  plot will be created for each \code{idx}. These can be toggled between using
#'  the arrows at the top of the display window if working interactively in
#'  \code{RStudio}; or, these will be written to separate files if
#'  \code{!isFALSE(fname)}.
#' @param plane The plane to display for the slices:
#'  \code{"axial"} (default), \code{"sagittal"} or \code{"coronal"}.
#'  Ignored if \code{widget}.
#' @param n_slices The number of slices to display. Default: \code{9}.
#'  The slices will be selected in a way that visualizes as much of the
#'  subcortex as possible. Ignored if \code{widget}.
#' @param slices Which slices to display. If provided, this argument will
#'  override \code{n_slices}. Should be a numeric vector with integer values
#'  between one and the number of slices in \code{plane}. Ignored if \code{widget}.
#' @param convention \code{"neurological"} (default) or \code{"radiological"}.
#'  Neurological convention will display the left side of the brain on the left
#'  side of axial and coronal images, and in the first few slices of a series of
#'  sagittal images. Radiological convention will display the right side of the
#'  brain on the left side of axial and coronal images, and in the first few
#'  slices of a series of sagittal images.
#' @param together Only applies if saving image files (\code{!isFALSE(fname)}).
#'  Use this argument to create and save a composite image which combines
#'  multiple plots. \code{NULL} (default) will not combine any plots. Otherwise,
#'  this argument should be a character vector with one or more of the
#'  following entries:
#'
#'  \code{"leg"} to combine the color legend with each \code{"xifti"} data plot.
#'  Overrides/ignores \code{legend_embed}.
#'
#'  \code{"idx"} to place all the plots for the different \code{"idx"} in a grid.
#'  If the data is not qualitative, a shared color bar will be added to the bottom
#'  of the composite. If the data is qualitative, a shared color legend will be
#'  added to the bottom only if \code{"leg"} is in \code{together}.
#
#  \code{"bs"} (\code{view_xifti} only) to place the cortical and subcortical plots
#  side-by-side.
#
#'  For greater control see \code{view_comp} or \code{grid::arrangeGrob}.
#' @param together_ncol If \code{"idx" \%in\% together}, this determines the number
#'  of columns to use in the array of subplots for different indices.
#'  By default, the number of columns and rows will be determined such that they
#'  are about equal.
#' @param together_title If a composite image is made based on \code{together},
#'  use this argument to add a grand title to the composite image. Should be
#'  a length-one character vector or \code{NULL} (default) to not add a grand title.
#' @param widget Create an interactive widget using \code{papayar}? Otherwise
#'  display static 2D slices. Default: \code{FALSE}.
#'
#'  Note that the widget can only display one \code{idx} at a time.
#' 
#'  Note: \code{papayar} has been removed
#'  from CRAN so the widget is not available. If \code{papayar} returns to CRAN
#'  the widget will be made available again.
#' @param title Optional title(s) for the plot(s). It will be printed at the top.
#'
#'  Default: \code{NULL} will not use any title if \code{length(idx)==1}.
#'  Otherwise, it will use the time index (".dtseries") or name
#'  (.dscalar or .dlabel) of each data column.
#'
#'  To use a custom title(s), use a length 1 character vector (same title for
#'  each plot) or length \code{length(idx)} character vector (different title
#'  for each plot). Set to \code{NULL} or an empty character to omit the title.
#'
#'  If the title is non-empty but does not appear, try lowering \code{cex.title}.
#' @param fname,fname_suffix Save the plot(s) (and color legend if applicable)?
#'
#'  If \code{isFALSE(fname)} (default), no files will be written.
#'
#'  If \code{widget}, these arguments are ignored.
#'
#'  If neither of the cases above apply, a png image will be written for each
#'  \code{idx}. If \code{isTRUE(fname)} the files will be named by the
#'  data column names (underscores will replace spaces). Or, set \code{fname} to a
#'  length 1 character vector to name files by this suffix followed by the
#'  \code{fname_suffix}: either the data column names (\code{"names"}) or the
#'  index value (\code{"idx"}). Or, set \code{fname} to a character vector with the same
#'  length as \code{idx} to name the files exactly.
#' @param fname_sub Add "_sub" to the end of the names of the files being saved?
#'  Default: \code{FALSE}. This is useful if cortical plots of the same data are being
#'  saved too.
#' @param legend_fname Save the color legend? Since the legend is the same
#'  for each \code{idx} only one legend is written even if \code{length(idx)>1}.
#'  This argument can be \code{NULL} to not save the legend, an exact file
#'  path, or a length-one character vector with "\[fname\]" in it, which will
#'  name the legend based on \code{fname\[1\]}. For example, if \code{fname\[1\]}
#'  is \code{"plots/my_cifti.png"} and \code{legend_fname} is \code{"\[fname\]_legend"}
#'  (default), then the legend plot will be saved to \code{"plots/my_cifti_legend.png"}.
#' @param legend_ncol Number of columns in color legend. If
#'  \code{NULL} (default), use 10 entries per row. Only applies if the color
#'  legend is used (qualitative data).
#' @param legend_alllevels Show all label levels in the color legend? If
#'  \code{FALSE} (default), just show the levels present in the data being
#'  viewed. Only applies if the color legend is used (qualitative data).
#' @param legend_embed Should the colorbar be embedded in the plot?
#'  It will be positioned at the bottom. Default: \code{TRUE}.
#'  If \code{FALSE}, print/save it separately instead.
#'
#'  Only applies if the color bar is used (sequential or diverging data).
#'  The color legend (qualitative data) cannot be embedded at the moment.
#' @param digits The number of digits for the colorbar legend ticks.
#'  If \code{NULL} (default), let \code{\link{format}} decide.
#' @param cex.title Font size multiplier for the title. \code{NULL} (default)
#'  will use \code{1.2} for titles less than 20 characters long, and smaller
#'  sizes for increasingly longer titles. If saving a PNG and PDF file, the default
#'  will also scale with \code{width} relative to the default value of \code{width}.
#' @param ypos.title,xpos.title The positioning of the title can be finicky,
#'  especially when using an R Markdown document interactively in which case it
#'  appears too high in the plot. Use these arguments to nudge the title up
#'  or down (\code{ypos.title}) or left or right (\code{xpos.title}).
#' @param orientation_labels Show orientation labels at the top left and top
#'  right of the plot? These will indicate the directions along the left-right
#'  axis for each slice image. Default: \code{FALSE}. Ignored if \code{widget}.
#'  The vertical positioning is controlled by \code{ypos.title}, and the font
#'  size is controlled by \code{cex.title}.
#' @param text_color Color for text in title and colorbar legend. Default:
#'  \code{"white"}. If \code{"white"}, will use black instead for the color
#   legend and the color bar if printed separately (since those will have
#   white backgrounds). To override this behaviour use \code{"#FFFFFF"} instead.
#' @param bg Background color. \code{NULL} will use \code{"black"}. Does not affect
#'  the color legend or color bar if printed separately: those will always have
#'  white backgrounds.
#' @param width,height The dimensions of the plot, in pixels. Only affects saved
#'  images (if \code{!isFALSE(fname)}). If \code{NULL}, file dimensions will be
#'  400 x 600 pixels for PNGs and 4 x 6 in. for PDFs.
#'
#'  Currently, there is no way to control the
#'  dimensions of the plot if working interactively in RStudio or creating a knitted
#'  R Markdown document. The default appears to be a wide aspect ratio.
#' @param ... Additional arguments to pass to \code{papayar::papaya} or \code{oro.nifti::overlay}.
#'  Note that for \code{oro.nifti::overlay} the following additional arguments
#'  should not be provided since they are pre-determined inside this function
#'  or by the arguments listed above:
#'  \code{x}, \code{y}, \code{plane}, \code{col.y}, \code{col.x}, \code{zlim.y},
#'  \code{oma}, \code{plot.type}, \code{bg}.
#' @return If a png or pdf file(s) were written, the names of the files for
#'  each index (and color legend if applicable) will be returned. Otherwise,
#'  \code{NULL} is invisibly returned.
#'
#' @family common
#' @export
#' @importFrom graphics plot.new
#' @importFrom grDevices dev.list dev.off png pdf gray
#' @importFrom stats median quantile
#' @importFrom oro.nifti overlay readNIfTI as.nifti
view_xifti_volume <- function(
  xifti, structural_img="MNI",
  color_mode="auto", zlim=NULL, colors=NULL,
  structural_img_colors=gray(0:255/280), title=NULL,
  idx=NULL, plane=c("axial", "sagittal", "coronal"),
  convention=c("neurological", "radiological"),
  n_slices=9, slices=NULL,
  together=NULL, together_ncol=NULL, together_title=NULL,
  widget=FALSE,
  fname=FALSE, fname_suffix=c("names", "idx"), fname_sub=FALSE,
  legend_fname="[fname]_legend",
  legend_ncol=NULL, legend_alllevels=FALSE, legend_embed=NULL, digits=NULL,
  cex.title=NULL, ypos.title=0, xpos.title=0, orientation_labels=FALSE,
  text_color="white", bg=NULL, width=NULL, height=NULL, ...) {

  # ----------------------------------------------------------------------------
  # Check arguments ------------------------------------------------------------
  # ----------------------------------------------------------------------------

  stopifnot(is.xifti(xifti))
  if (is.null(xifti$data$subcort)) {
    stop("No subcortical data in the `xifti`.")
  }

  if (!is.null(structural_img)) {
    if (structural_img=="MNI") {
      structural_img <- system.file("extdata/MNI152_T1_2mm_crop.nii.gz", package="ciftiTools")
    } else if (!is.fname(structural_img)){
      stop(paste(
        "`structural_img` argument not one of:",
        "`NULL`, `\"MNI\"`, or an existing file."
      ))
    }
  }

  convention <- match.arg(convention, c("neurological", "radiological"))

  if (is.null(idx)) { idx <- 1 }
  if (is.null(fname)) { fname <- FALSE }
  if (is.null(legend_fname)) { legend_fname <- "[fname]_legend" }
  idx <- as.numeric(idx)
  if (length(widget) > 1) {
    warning("Using the first entry of `widget`.")
    widget <- as.logical(widget[[1]])
  }
  if (widget) { stop("The widget is not currently available due to a dependency being removed from CRAN. Please set `widget=FALSE`.") }

  makePNG <- makePDF <- FALSE

  # File saving: `together`
  if (!is.null(together)) {

    if (widget) { stop(
      "Composite images are not compatible with widget. ",
      "Set `together=NULL` or `widget=FALSE`."
    ) }

    # Check packages
    pkg <- vapply(c("png", "grid", "gridExtra"), requireNamespace, quietly=TRUE, FALSE)
    if (any(!pkg)) {
      stop(
        "These packages need to be installed to use `view_comp`: ",
        paste(names(pkg)[!pkg], collapse=", "), call.=FALSE
      )
    }

    # check `together`
    together <- match.arg(together, c("leg", "idx"), several.ok=TRUE)
    together_leg <- "leg" %in% together
    together_idx <- "idx" %in% together
    together <- TRUE

    # check `together_title`
    if (!is.null(together_title)) {
      together_title <- as.character(together_title)
      if (length(together_title) > 1) {
        warning("Using the first entry of `together_title`.\n")
        together_title <- together_title[1]
      }
    }

    if (isFALSE(fname)) {
      makePNG <- TRUE
      comp_dummy <- TRUE
    } else {
      comp_dummy <- FALSE
    }
  } else {
    together <- together_leg <- together_idx <- comp_dummy <- FALSE
  }

  comp_fname <- NULL
  legend_embed2 <- FALSE
  if (!widget) {

    # `fname`, `fname_suffix`, `legend_fname`
    if (!isFALSE(fname) || together) {

      fname_use_names <- !together_idx && !is.null(xifti$meta$cifti$names)

      # Use default file name(s) if `fname==TRUE`.
      if (isTRUE(fname) || comp_dummy) {
        if (together_idx) {
          if (isTRUE(fname)) {fname <- "xifti_subcort" }
          comp_fname <- fname
        } else if (fname_use_names && !comp_dummy) {
          fname <- gsub(" ", "_", xifti$meta$cifti$names[idx], fixed=TRUE)
          if (length(fname) != length(unique(fname))) {
            warning(
              "Non-unique names in the `xifti` ",
              "(note spaces were replaced with underscores)... proceeding anyway. ",
              "If this is not intended, set `fname` to the exact unique file ",
              "name(s) you want to use for each `idx`.\n"
            )
          }
        } else {
          fname <- "xifti_subcort"
        }
      } else {
        fname <- as.character(fname)
      }

      # Strip file extension.
      fname <- gsub(".html$", "", fname)
      fname <- gsub(".png$", "", fname)
      makePDF <- any(grepl("\\.pdf$", fname))
      makePNG <- !makePDF
      # Check length.
      if (!(length(fname) %in% c(1, length(idx)))) {
        warning("Using first entry of `fname` since its length is not 1, or the length of `idx`.\n")
        fname <- fname[1]
      }
      # Save `fname` to use for composite.
      if (together && makePDF) { fname <- gsub(".pdf$", "", fname) }
      if ((together_idx || (makePDF && together_leg)) && is.null(comp_fname)) {
        comp_fname <- fname
        if (length(comp_fname) > 1) {
          warning("Using the first entry of `fname` (since compositing, there's only one file to save).\n")
          comp_fname <- comp_fname[1]
        }
      }

      sufPNG <- ifelse(fname_sub, "_sub.png", ".png")
      sufPDF <- ifelse(fname_sub, "_sub.pdf", ".pdf")

      # Modify `fname`
      if (together) {
        legend_embed2 <- !isFALSE(legend_embed)
        legend_embed <- FALSE
      }
      if (!together && makePDF) {
        if (length(fname) > 1) {
          warning("Using first entry of `fname`, since only one pdf file is being written.\n")
          fname <- fname[1]
        }
        if (fname_sub) {
          fname <- gsub("\\.pdf$", "_sub.pdf", fname)
        }
      }
      if (together_idx || (makePDF && together_leg) || (together_leg && comp_dummy)) {
        fname <- paste0(tempfile(as.character(seq(length(idx)))), ".png")
        comp_fname <- paste0(comp_fname, ifelse(makePDF, sufPDF, sufPNG))
      } else {
        # Add suffix if multiple `idx`.
        if (length(fname) == 1 && length(idx) > 1) {
          fname_suffix <- match.arg(fname_suffix, c("names", "idx"))
          if (fname_suffix == "names" && fname_use_names) {
            fname <- paste0(fname, "_", xifti$meta$cifti$names[idx])
          } else {
            fname <- paste0(fname, "_", as.character(idx))
          }
        }
        # Add png file extension.
        if (makePNG) { fname <- paste0(fname, sufPNG) }
      }

      # Check that `fname` are unique.
      if (length(fname) != length(unique(fname))) {
        warning(
          "Non-unique file names... proceeding anyway. ",
          "If this is not intended, set `fname` to the exact unique file ",
          "name(s) you want to use for each `idx`.\n"
        )
      }

      # Check that `fname` directories exist.
      fname_dirs <- unique(dirname(fname))
      if (!all(file.exists(fname_dirs))) {
        stop(
          "These directories for `fname` do not exist: ",
          paste0(fname_dirs[!dir.exists(fname_dirs)],
          collapse=" ")
        )
      }

      # `legend_fname`
      if (together_leg) {
          legend_fname <- paste0(tempfile(), ".png")
      } else if (!isFALSE(legend_fname)) {
        if (!(length(legend_fname) == 1)) {
          warning("Using first entry of `legend_fname`.\n")
          legend_fname <- legend_fname[1]
        }
        if (grepl("\\[fname\\]", legend_fname)) {
          legend_fname <- gsub(
            "\\[fname\\]",
            gsub("\\.png|\\.html", "", ifelse(together_idx, basename(comp_fname), basename(fname[1]))),
            basename(legend_fname)
          )
          legend_fname <- file.path(
            ifelse(together_idx, dirname(comp_fname), dirname(fname[1])), legend_fname
          )
        }
        if (!endsWith(legend_fname, ".png")) { legend_fname <- paste0(legend_fname, ".png") }
      }

      # Check that `legend_fname` directory exists.
      if (!isFALSE(legend_fname)) {
        if (!dir.exists(dirname(legend_fname))) {
          stop(
            "The directory for `legend_fname` does not exist: ",
            dirname(legend_fname)
          )
        }
      }

      if (!isFALSE(fname) & !makePDF) {
        # Try to avoid this error with colorbar:
        #   Error in par(old.par) :
        #   invalid value specified for graphical parameter "pin"
        # Only do this if saving a png, because otherwise the plot won't show.
        while (!is.null(grDevices::dev.list())) { grDevices::dev.off() }
      }
    }

    # `title`
    use_title <- TRUE
    if (!is.null(title)) {
      if (length(title) == 1){
        title <- rep(title, length(idx))
      } else if (length(title) != length(idx)) {
        stop("Length of `title` must be 1 or the same as the length of `idx`.")
      }
      if (all(title == "")) { use_title <- FALSE }
    } else {
      if (length(idx)==1) { use_title <- FALSE }
    }

    # `legend_ncol`, `digits`
    if (!is.null(legend_ncol)) { legend_ncol <- as.numeric(legend_ncol) }
    if (!is.null(digits)) { digits <- as.numeric(digits) }

    # `cex.title`, `text_color`, "bg"
    if (!is.null(cex.title)) { cex.title <- as.numeric(cex.title) }
    text_color <- as.character(text_color)
    text_color2 <- ifelse(text_color=="white", "black", text_color)
    if (!is.null(bg)) { bg <- as.character(bg) }

  } else {

    # if (!requireNamespace("papayar", quietly = TRUE)) {
    #   stop("Package \"papayar\" needed for this function to work. Please install it.",
    #        call. = FALSE)
    # }

    # `fname`, `legend_fname`
    if (!isFALSE(fname)) {
      warning("Saving the subcortical widget is not supported. Setting `fname` to `FALSE`.\n")
      fname <- FALSE
    }
    legend_fname <- FALSE

    if (length(idx) > 1) {
      stop("Only one `idx` at a time is supported for the interactive subcortical widget.")
    }
  }

  # `color_mode`
  if (is.null(color_mode)) { color_mode <- "auto" }
  if (color_mode == "auto") {
    if (!is.null(xifti$meta$cifti$intent) && xifti$meta$cifti$intent==3007) {
      color_mode <- "qualitative"
    }
    # Otherwise, set after call to view_xifti_surface.mesh_val
  } else {
    color_mode <- match.arg(color_mode, c("sequential", "qualitative", "diverging"))
  }

  # `zlim`, `colors` handled later

  if (is.null(bg)) { bg <- "black" }

  close_after_save <- TRUE

  # ----------------------------------------------------------------------------
  # Get the data values. -------------------------------------------------------
  # ----------------------------------------------------------------------------

  # Get volume and labels.
  values <- xifti$data$subcort[,idx,drop=FALSE]
  vol <- unmask_subcortex(values, xifti$meta$subcort$mask, fill=NA)
  if (length(dim(vol)) == 3) {
    dim(vol) <- c(dim(vol), 1)
  }
  labs_bs <- unmask_subcortex(as.numeric(xifti$meta$subcort$labels), xifti$meta$subcort$mask, fill=0)

  # Set `color_mode` if `"auto"`; set `colors` if `NULL`
  if (color_mode == "auto" || (color_mode!="qualitative" && is.null(colors))) {
    values <- as.vector(values)

    if (color_mode == "auto") {
      if (length(zlim) == 3) {
        color_mode <- "diverging"
      } else if (is.null(values) || all(values %in% c(NA, NaN))) {
        color_mode <- "diverging"
        if (is.null(colors)) { colors <- "ROY_BIG_BL" }
      } else if (length(zlim) == 2) {
        color_mode <- ifelse(prod(zlim) >= 0, "sequential", "diverging")
      }
    }

    if (color_mode == "auto" || is.null(colors)) {
      pctile_05 <- quantile(values, .05, na.rm=TRUE)
      pctile_95 <- quantile(values, .95, na.rm=TRUE)
      pctile_05_neg <- pctile_05 < 0
      pctile_95_pos <- pctile_95 > 0

      if (color_mode == "sequential") {
        colors <- ifelse(pctile_05_neg, "ROY_BIG_BL_neg", "ROY_BIG_BL_pos")
      }

      if (!xor(pctile_05_neg, pctile_95_pos)) {
        if (color_mode == "auto") { color_mode <- "diverging" }
        if (is.null(colors)) { colors <- "ROY_BIG_BL" }
      } else if (pctile_95_pos) {
        if (color_mode == "auto") { color_mode <- "sequential" }
        if (is.null(colors)) { colors <- "ROY_BIG_BL_pos" }
      } else if (pctile_05_neg) {
        if (color_mode == "auto") { color_mode <- "sequential" }
        if (is.null(colors)) { colors <- "ROY_BIG_BL_neg" }
      } else { stop() }
    }
  }

  values[values == NaN] <- NA
  unique_vals <- NULL
  if (all(is.na(values))) {
    values <- NULL
    pal <- pal_base <- pal_even <- NULL
  } else {

    # [TO DO]: have digits control color bar tick labels?
    if (is.null(digits)) {
      signif_digits <- 3
    } else {
      signif_digits <- digits
    }
    DATA_MIN <- signif(min(values, na.rm=TRUE), signif_digits)
    DATA_MAX <- signif(max(values, na.rm=TRUE), signif_digits)

    if (is.null(zlim)) {
      if (color_mode=="qualitative") {
        if (!is.null(zlim)) { warning("`zlim` not supported for qualitative data. Ignoring.") }
        # Placeholder: the color limits will actually be 1 to the number of unique values.
        # This variable `zlim` won't be used.
        zlim <- c(0,1)
      } else {
        pctile_05 <- signif(quantile(values, .05, na.rm=TRUE), signif_digits)
        pctile_95 <- signif(quantile(values, .95, na.rm=TRUE), signif_digits)
        pctile_05_neg <- pctile_05 < 0
        pctile_95_pos <- pctile_95 > 0

        if (!pctile_05_neg) {
          if (pctile_95 == 0) { pctile_95 <- DATA_MAX }
          zlim <- c(0, pctile_95)
        } else if (!pctile_95_pos) {
          if (pctile_05 == 0) { pctile_05 <- DATA_MAX }
          zlim <- c(pctile_05, 0)
        } else {
          pctile_max <- max(abs(c(pctile_05, pctile_95)))
          if (pctile_max == 0) { pctile_max <- max(abs(c(DATA_MIN, DATA_MAX))) }
          if (color_mode=="diverging") {
            zlim <- c(-pctile_max, 0, pctile_max)
          } else {
            zlim <- c(-pctile_max, pctile_max)
          }
        }

        message(
          "`zlim` not provided: using color range ",
          as.character(min(zlim)), " - ", as.character(max(zlim)), " ",
          "(data limits: ", as.character(min(DATA_MIN)), " - ",
          as.character(max(DATA_MAX)), ")."
        )
      }
    }

    zlim[zlim==-Inf] <- DATA_MIN
    zlim[zlim==Inf] <- DATA_MAX

    # Make base palette and full palette. Use evenly-spaced colors.
    if (color_mode=="qualitative") {
      # For .dlabel files, use the included labels metadata colors.
      if ((!is.null(xifti$meta$cifti$intent) && xifti$meta$cifti$intent==3007)) {
        if (length(idx) > 1) { message("Color labels from first requested column will be used.") }
        labs <- xifti$meta$cifti$labels[[idx[1]]]
        if (is.null(colors)) {
          pal_base <- data.frame(
            color = grDevices::rgb(labs$Red, labs$Green, labs$Blue, labs$Alpha),
            value = labs$Key
          )
        } else {
          unique_vals <- sort(unique(as.vector(vol[!is.na(vol)])))
          vol[,,,] <- as.numeric(factor(vol, levels=unique_vals))
          pal_base <- make_color_pal(
            colors=colors, color_mode=color_mode, zlim=nrow(labs)
          )
        }
      # Otherwise, use the usual colors.
      } else {
        unique_vals <- sort(unique(as.vector(vol[!is.na(vol)])))
        vol[,,,] <- as.numeric(factor(vol, levels=unique_vals))
        pal_base <- make_color_pal(
          colors=colors, color_mode=color_mode, zlim=length(unique_vals)
        )
      }
      pal <- pal_even <- pal_base

      pal_even <- pal_even[seq(
        which(pal_even$value == min(vol, na.rm=TRUE)),
        which(pal_even$value == max(vol, na.rm=TRUE))
      ),]
      zlim <- c(min(pal_even$value), max(pal_even$value))

    } else {

      vol[vol < min(zlim)] <- min(zlim)
      vol[vol > max(zlim)] <- max(zlim)

      if (is.data.frame(colors)) {
        stopifnot(ncol(colors)==2 && colnames(colors)==c("color", "value"))
        pal_base <- colors
      } else {
        pal_base <- make_color_pal(colors=colors, color_mode=color_mode, zlim=zlim)
      }
      pal <- expand_color_pal(pal_base)
      if (length(unique(diff(pal$value))) > 1) {
        np <- nrow(pal)
        pal_even <- expand_color_pal(pal, np*500)
        pal_even$value <- pal_even$value - min(pal_even$value)
        pal_even$value <- pal_even$value / max(pal_even$value)
        pm <- vector("numeric", np)
        for (ii in seq(np)) { pm[ii] <- which.min(abs(pal_even$value - (ii-1)/(np-1))) }
        pal_even <- pal_even[pm,]
      } else {
        pal_even <- pal
      }
    }
  }

  # ----------------------------------------------------------------------------
  # Get the colorbar/legend arguments. ----------------------------------------
  # ----------------------------------------------------------------------------

  any_colors <- !all(is.na(values))
  use_cleg <- FALSE
  if (any_colors) {
    # Color legend
    if (color_mode == "qualitative") {
      if (isTRUE(legend_embed) && !together_leg) {
        warning(
          "`legend_embed` is `TRUE` and `color_mode` is `'qualitative'`. ",
          "However, the color legend for qualitative data cannot be embedded. ",
          "Embedding a color bar that shows the colors in order, instead of the color legend. ",
          "To view the color legend separately (as recommended) set `legend_embed` to `FALSE`.\n"
        )
        colorbar_kwargs <- view_xifti.cbar(pal_base, pal, color_mode, text_color, digits) # added?
      } else {
        legend_embed <- FALSE; use_cleg <- TRUE
        # Get the labels for the color legend list.
        if (is.null(xifti$meta$cifti$intent) || (xifti$meta$cifti$intent != "3007")) {
          if (!is.null(unique_vals)) {
            cleg_labs <- unique_vals
          } else {
            cleg_labs <- paste0("Label ", seq(nrow(pal_base)))
          }
        } else {
          cleg_labs <- rownames(xifti$meta$cifti$labels[[idx[1]]])
        }
        cleg_labs <- as.character(cleg_labs)
        cleg <- pal_base
        cleg$labels <- factor(cleg_labs, levels=unique(cleg_labs))
        if (!legend_alllevels && is.null(unique_vals)) {
          cleg <- cleg[cleg$value %in% as.vector(vol),]
        }
        # Skip if there are too many legend labels, or only one label.
        if (nrow(cleg) < 1) {
          use_cleg <- FALSE
        } else if (nrow(cleg) > 200) {
          use_cleg <- FALSE
          if (isFALSE(fname) || !isFALSE(legend_fname)) {
            warning("Too many labels (> 200) for qualitative color legend. Not rendering it.\n")
          }
        } else {
          if (!requireNamespace("ggpubr", quietly = TRUE)) {
            use_cleg <- FALSE
            warning("Package \"ggpubr\" needed to make the color legend. Please install it. Skipping the color legend for now.\n")
          } else {
            # Get the color legend list dimensions.
            if (is.null(legend_ncol)) {
              legend_ncol <- max(1, floor(.8 * sqrt(nrow(cleg))))
              colorlegend_nrow <- ceiling(nrow(cleg) / legend_ncol)
            }
            cleg <- view_xifti.cleg(
              cleg, legend_ncol, text_color2,
              title_sub=fname_sub
            )
          }
        }
      }
      colorbar_kwargs <- NULL

    # Color bar
    } else {

      if (is.null(legend_embed)) { legend_embed <- TRUE }
      if (together_leg) { legend_embed <- FALSE }
      colorbar_kwargs <- view_xifti.cbar(
        pal_base, pal, color_mode,
        ifelse(legend_embed || (!use_cleg && together && !comp_dummy), text_color, text_color2), digits
      )
    }

  } else {
    if (isTRUE(legend_embed)) {
      warning(
        "`legend_embed` is `TRUE`, but there is no data. Setting to `FALSE`.\n"
      )
    }
    if (isTRUE(together_leg)) {
      warning(
        "`together_leg` is `TRUE`, but there is no data. Setting to `FALSE`.\n"
      )
    }
    colorbar_kwargs <- NULL
    legend_embed <- together_leg <- FALSE
  }

  # ----------------------------------------------------------------------------
  # Set up the display window. -------------------------------------------------
  # ----------------------------------------------------------------------------

  flip <- (convention=="radiological" && widget) || (convention=="neurological" && !widget)
  if (flip) {
    # cat("Flipping NIFTI image.")
    vol <- vol[rev(seq(dim(vol)[1])),,,,drop=FALSE]
    xifti$meta$subcort$mask <- xifti$meta$subcort$mask[rev(seq(dim(vol)[1])),,,drop=FALSE]
    labs_bs <- labs_bs[rev(seq(dim(vol)[1])),,,drop=FALSE]
  }

  if (!widget) {
    plane <- match.arg(plane, c("axial", "sagittal", "coronal"))
    plane_dim <- switch(plane, axial=3, coronal=2, sagittal=1)
    if (is.null(slices)) {
      if (is.null(n_slices)) { warning("Using 9 slices."); n_slices <- 9 }
      n_slices <- as.numeric(n_slices)
      if (length(n_slices) > 1) { warning("Using the first entry of `slice`."); n_slices <- n_slices[1] }
      # Pick slices that are spaced out, and with many voxels.
      mask_count <- apply(xifti$meta$subcort$mask, plane_dim, sum)
      ns_all <- length(mask_count)
      slices <- seq(ns_all)
      # Remove slices with zero voxels.
      slices <- slices[mask_count != 0]
      mask_count <- mask_count[mask_count != 0]
      ns_all <- length(mask_count)
      if (n_slices > length(slices)) {
        warning(
          "`n_slices` is larger than the number of non-empty slices (",
          length(slices), "). Showing all non-empty slices."
        )
        n_slices <- length(slices)
      }
      # Remove slices with few voxels.
      if (n_slices < (ns_all / 2)) {
        slices <- slices[mask_count > quantile(mask_count, .33)]
      }
      slices <- slices[round(seq(1, length(slices), length.out=n_slices))]
    } else {
      slices <- as.numeric(slices)
      stopifnot(all(slices %in% seq(dim(xifti$meta$subcort$mask)[plane_dim])))
    }
  }

  if (!is.null(structural_img)) {
    # Need to use `oro.nifti` here instead of `RNifti` because
    #   we modify @.Data to work with `oro.nifti::overlay`.
    # `img` is +LAS (+RAS convention, but sform mult is negative for x.)
    img <- readNIfTI(structural_img)
    img[is.na(img)] <- 0
    if (flip) {
      img[] <- img[rev(seq(dim(img)[1])),,,drop=FALSE]
    }

    # Check data dimensions.
    if (!isTRUE(all.equal(dim(img), dim(vol)[seq(3)]))) {
      stop(paste0(
        "The subcortical data in the CIFTI and the `structural_img` are of ",
        "different dimensions: (", paste(dim(img), collapse=", "), ") and (",
        paste(dim(vol)[seq(3)], collapse=", "), ") respectively."
      ))
    }

    # Check data orientation alignment.
    # This uses the sform method (srow_x, srow_y, and srow_z), not qform
    #   or ANALYZE-based methods.
    # This is because the Connectome Workbench seems to export the
    #   TransformationMatrixIJKtoXYZ as the sform transformation matrix
    #   in -cifti-separate.
    img_trans_mat <- make_trans_mat(structural_img)
    xii_trans_mat <- xifti$meta$subcort$trans_mat
    if (!is.null(xii_trans_mat)) {
      if (!all(dim(img_trans_mat) == dim(xii_trans_mat))) {
        warning(paste(
          "`meta$subcort$trans_mat` has different dimensions than image",
          "trans_mat, i.e. `rbind(srow_x, srow_y, srow_z)`. This may indicate",
          "that the volumes are not aligned."
        ))
      } else {
        trans_mat_diff <- max(abs(as.numeric(img_trans_mat - xii_trans_mat)))
        if (max(trans_mat_diff > ciftiTools.getOption("EPS"))) {
          warning(paste(
            "`meta$subcort$trans_mat` has different values than the image",
            "trans_mat, i.e. `rbind(srow_x, srow_y, srow_z)`. This may indicate",
            "that the volumes are not aligned."
          ))
        }
      }
    }
    img_overlay <- img_labels <- img*0
  } else {
    img <- oro.nifti::as.nifti(vol[,,,1]*0)
    img@.Data <- xifti$meta$subcort$mask
    img_overlay <- img_labels <- img
  }

  if (widget) {
    img_overlay@.Data <- vol[,,,1]
    img_overlay@.Data[labs_bs==0] <- NA
    img_labels@.Data <- labs_bs
    img_labels@.Data[labs_bs==0] <- NA

    # return(
    #   papayar::papaya(list(img, img_labels, img_overlay), ...)
    # )
    return(NULL)
  }

  if (is.null(width)) { width <- 200 * ifelse(!together && makePDF, .02, 2) }
  if (is.null(height)) { height <- 300 * ifelse(!together && makePDF, .02, 2) }

  together_scale <- width/200

  if (!together && makePDF) { pdf(fname, width=width, height=height) }

  zlim <- sort(zlim)[c(1, length(zlim))]

  for (jj in seq(length(idx))) {
    this_idx <- idx[jj]

    if (together || makePNG) { png(fname[jj], bg=bg, width=width, height=height) }

    if (jj > 1) { img_overlay <- img_labels <- img*0 }
    img_overlay@.Data <- vol[,,,jj]
    img_overlay@.Data[labs_bs==0] <- NA
    img_labels@.Data <- labs_bs
    img_labels@.Data[labs_bs==0] <- NA

    if (plane=="axial") {
      img2 <- img[,,slices]
      img_overlay2 <- img_overlay[,,slices,drop=FALSE]
    } else if (plane=="coronal") {
      img2 <- img[,slices,]
      img_overlay2 <- img_overlay[,slices,,drop=FALSE]
    } else if (plane=="sagittal") {
      img2 <- img[slices,,]
      img_overlay2 <- img_overlay[slices,,,drop=FALSE]
    } else { stop() }

    oro.nifti::overlay(
      x=img2, y=img_overlay2, plane=plane,
      col.y=as.character(pal_even$color),
      col.x=structural_img_colors,
      zlim.y=zlim,
      oma=c(5,0,5,0),
      plot.type=ifelse(length(slices)==1, "single", "multiple"),
      bg=bg, ...
    )

    # Draw the colorbar (if applicable).
    if (any_colors && !use_cleg && legend_embed) {
      if (!requireNamespace("fields", quietly = TRUE)) {
        stop("Package \"fields\" needed to render the color bar for `view_xifti_surface`. Please install it.", call. = FALSE)
      }
      #x1 x2 for left: .06, .26
      colorbar_kwargs$smallplot <- c(.4, .6, .05, .08) # x1 x2 y1 y2
      colorbar_kwargs$axis.args$cex.axis <- .8
      colorbar_kwargs$axis.args$mgp <- c(3, .2, 0) # move axis labels closer up
      colorbar_kwargs$axis.args$tck <- -.2
      try(suppressWarnings(do.call(fields::image.plot, colorbar_kwargs)), silent=TRUE)
    }

    # Draw the title
    if (use_title) {
      if (is.null(title)) {
        title_jj <- view_xifti.title(xifti$meta, idx[jj])
      } else {
        title_jj <- title[jj]
      }
      if (title_jj != "") {

        if (is.null(cex.title)) {
          cex.title <- 1.2
          if (makePNG) {
            cex.title <- cex.title * width / 400
          } else if (!together && makePDF) {
            cex.title <- cex.title * width / 4
          }
          if (nchar(title_jj) > 20) { cex.title <- cex.title * sqrt(20 / nchar(title_jj)) }
          cex.title <- round(cex.title*100)/100
        }

        title(
          title_jj, col.main=text_color, cex.main=cex.title,
          line= 1.75 + ypos.title, # Move up
          adj=ifelse(!isFALSE(fname), ifelse(!together && makePDF, .42, .45), .465) + xpos.title # Move up and left
        )
      }
    }

    if (orientation_labels) {
      olabs <- switch(plane,
        axial = c("L", "R"),
        coronal = c("L", "R"),
        sagittal = c("P", "A")
      )
      if (convention=="radiological" && plane!="sagittal") { olabs <- c("R", "L") }
      title(
        olabs[1], col.main=text_color, cex.main=cex.title,
        line = 1.75 + ypos.title,
        adj = 0
      )
      title(
        olabs[2], col.main=text_color, cex.main=cex.title,
        line = 1.75 + ypos.title,
        adj = 1
      )
    }

    if (!together && makePDF) {
      # plot.new()
      NULL
    } else if (together || makePNG) {
      dev.off()
    }
  }

  if (!together && makePDF) { dev.off() }

  if (use_cleg && ((together && !comp_dummy) || together_leg)) {
    if (!isFALSE(fname)) {
      if (!isFALSE(legend_fname)) {
        cleg_h_per_row <- 1/3 #inches
        cleg_w_factor <- mean(nchar(cleg_labs)*1/4) + 3
        ggplot2::ggsave(
          filename = legend_fname,
          height = (2 + colorlegend_nrow) * cleg_h_per_row, # add 2 for title
          width = (legend_ncol) * cleg_h_per_row * cleg_w_factor
        )
      }
      if (close_after_save) { dev.off() }
    }
  } else if (use_cleg && !comp_dummy && !together_leg && isFALSE(fname)) {
    print(cleg)
  } else if (!use_cleg) {
    # Make (and save) the colorbar (if applicable).
    if (any_colors && !legend_embed) {
      if (!requireNamespace("fields", quietly = TRUE)) {
        stop("Package \"fields\" needed to render the color bar for `view_xifti_surface`. Please install it.", call. = FALSE)
      }
      if (!isFALSE(fname) && !isFALSE(legend_fname)) {
        lfac <- ifelse(!together && makePDF, 2/.02, 1) * width
        png(legend_fname, bg=ifelse(comp_dummy, "white", bg), width=lfac, height=ceiling(lfac*.4))
      } else {
        plot.new()
      }
      colorbar_kwargs$smallplot <- c(.15, .85, .45, .6) # x1 x2 y1 y2
      # Make labels smaller (cex.axis) and lower to compensate (mgp)
      colorbar_kwargs$axis.args$cex.axis <- colorbar_kwargs$axis.args$cex.axis * together_scale * .85
      colorbar_kwargs$axis.args$mgp <- c(3,.5+together_scale,0)

      try(suppressWarnings(do.call(fields::image.plot, colorbar_kwargs)), silent=TRUE)
      if (!isFALSE(fname) && !isFALSE(legend_fname)) { if (close_after_save) { dev.off() } }
    }
  }

  fname_all <- fname
  if (!isFALSE(legend_fname) && !legend_embed) {
    if (use_cleg || any_colors){
      fname_all <- c(fname, legend_fname)
    }
  }

  # Does this ever happen?
  if (!file.exists(as.character(legend_fname))) { legend_fname <- NULL }
  # Compositing `together`
  if (together_idx) {
    if (is.null(together_ncol)) {
      together_ncol <- ceiling(sqrt(length(idx)))
    }
    together_nrow <- ceiling(length(idx)/together_ncol)
    comp_width <- width * together_ncol
    comp_height <- height * together_nrow
    title_height <- .2/together_nrow
    leg_height <- .3/together_nrow
    comp_height_mult <- 1
    if (together_leg || !use_cleg) { comp_height_mult <- comp_height_mult + leg_height }
    if (!is.null(together_title)) { comp_height_mult <- comp_height_mult + title_height }
    if (!comp_dummy) {
      if (makePDF) {
        pdf(comp_fname, bg=bg, width=comp_width * .01, height=comp_height * comp_height_mult * .01)
      } else {
        png(comp_fname, bg=bg, width=comp_width, height=comp_height * comp_height_mult)
      }
    }
    fname_all <- comp_fname
    if ((!legend_embed2) || (!together_leg && use_cleg)) {
      fname_all <- c(fname_all, legend_fname)
      legend_fname <- NULL
    }
    view_comp(
      fname, ncol=together_ncol,
      legend=legend_fname, legend_height=leg_height,
      title=together_title, title_height=title_height,
      title_fsize=1.5 * together_scale
    )
    if (!together_leg && !use_cleg && legend_embed2 && (!is.null(legend_fname))) {
      tfile <- paste0(tempfile(), ".png")
      file.copy(legend_fname, tfile, overwrite=TRUE)
      file.remove(legend_fname)
    }
    if (!together_leg && use_cleg && !legend_embed && (!is.null(legend_fname))) {
      tfile <- paste0(tempfile(), ".png")
      file.copy(legend_fname, tfile, overwrite=TRUE)
      file.remove(legend_fname)
    }
    if (!together_leg) {
      if (use_cleg) {
        if (!comp_dummy) {
          fname_all <- c(fname_all, legend_fname)
        } else {
          print(cleg)
        }
      }
    }
    if (!comp_dummy) { dev.off() }
    # comp_fname <- crop_image(comp_fname)

  } else if (together_leg) {
    for (ff in seq(length(fname))) {
      if (!comp_dummy) {
        tfile <- tempfile()
        png(tfile, bg=bg, width=width, height=floor(height*1.3))
      }
      view_comp(fname[ff], legend=legend_fname)
      if (!comp_dummy) {
        dev.off()
        file.copy(tfile, fname[ff], overwrite=TRUE)
        file.remove(tfile)
      }
    }
    fname_all <- fname

    if (makePDF) {
      pdf(tfile, bg=bg, width=width*.01, height=floor(height*.013))
      for (ff in seq(length(fname))) {
        grid::grid.raster(png::readPNG(fname[ff]))
        if (ff < length(fname)) { plot.new() }
      }
      dev.off()
      file.copy(tfile, comp_fname, overwrite=TRUE)
      file.remove(tfile)
      fname_all <- comp_fname
    }
  }

  if ((makePNG || makePDF) && !comp_dummy) {
    return(invisible(fname_all))
  } else {
    return(invisible(NULL))
  }
}

#' @rdname view_xifti_volume
#' @export
view_cifti_volume <- function(
  xifti, structural_img="MNI",
  color_mode="auto", zlim=NULL, colors=NULL,
  structural_img_colors=gray(0:255/280), title=NULL,
  idx=NULL, plane=c("axial", "sagittal", "coronal"),
  n_slices=9, slices=NULL,
  together=NULL, together_ncol=NULL, together_title=NULL,
  widget=FALSE,
  fname=FALSE, fname_suffix=c("names", "idx"), fname_sub=FALSE,
  legend_fname="[fname]_legend",
  legend_ncol=NULL, legend_alllevels=FALSE, legend_embed=NULL, digits=NULL,
  cex.title=NULL, ypos.title=0, xpos.title=0,
  text_color="white", bg=NULL, width=NULL, height=NULL, ...) {

  view_xifti_volume(
    xifti=xifti, structural_img=structural_img,
    color_mode=color_mode, zlim=zlim, colors=colors,
    structural_img_colors=structural_img_colors, title=title,
    idx=idx, plane=plane,
    n_slices=n_slices, slices=slices,
    together=together, together_ncol=together_ncol, together_title=together_title,
    widget=widget,
    fname=fname, fname_suffix=fname_suffix, fname_sub=fname_sub,
    legend_fname=legend_fname,
    legend_ncol=legend_ncol, legend_alllevels=legend_alllevels, legend_embed=legend_embed, digits=digits,
    cex.title=cex.title, ypos.title=ypos.title, xpos.title=xpos.title,
    text_color=text_color, bg=bg, width=width, height=height, ...
  )
}

#' @rdname view_xifti_volume
#' @export
viewCIfTI_volume <- function(
  xifti, structural_img="MNI",
  color_mode="auto", zlim=NULL, colors=NULL,
  structural_img_colors=gray(0:255/280), title=NULL,
  idx=NULL, plane=c("axial", "sagittal", "coronal"),
  n_slices=9, slices=NULL,
  together=NULL, together_ncol=NULL, together_title=NULL,
  widget=FALSE,
  fname=FALSE, fname_suffix=c("names", "idx"), fname_sub=FALSE,
  legend_fname="[fname]_legend",
  legend_ncol=NULL, legend_alllevels=FALSE, legend_embed=NULL, digits=NULL,
  cex.title=NULL, ypos.title=0, xpos.title=0,
  text_color="white", bg=NULL, width=NULL, height=NULL, ...) {

  view_xifti_volume(
    xifti=xifti, structural_img=structural_img,
    color_mode=color_mode, zlim=zlim, colors=colors,
    structural_img_colors=structural_img_colors, title=title,
    idx=idx, plane=plane,
    n_slices=n_slices, slices=slices,
    together=together, together_ncol=together_ncol, together_title=together_title,
    widget=widget,
    fname=fname, fname_suffix=fname_suffix, fname_sub=fname_sub,
    legend_fname=legend_fname,
    legend_ncol=legend_ncol, legend_alllevels=legend_alllevels, legend_embed=legend_embed, digits=digits,
    cex.title=cex.title, ypos.title=ypos.title, xpos.title=xpos.title,
    text_color=text_color, bg=bg, width=width, height=height, ...
  )
}

#' @rdname view_xifti_volume
#' @export
viewcii_volume <- function(
  xifti, structural_img="MNI",
  color_mode="auto", zlim=NULL, colors=NULL,
  structural_img_colors=gray(0:255/280), title=NULL,
  idx=NULL, plane=c("axial", "sagittal", "coronal"),
  n_slices=9, slices=NULL,
  together=NULL, together_ncol=NULL, together_title=NULL,
  widget=FALSE,
  fname=FALSE, fname_suffix=c("names", "idx"), fname_sub=FALSE,
  legend_fname="[fname]_legend",
  legend_ncol=NULL, legend_alllevels=FALSE, legend_embed=NULL, digits=NULL,
  cex.title=NULL, ypos.title=0, xpos.title=0,
  text_color="white", bg=NULL, width=NULL, height=NULL, ...) {

  view_xifti_volume(
    xifti=xifti, structural_img=structural_img,
    color_mode=color_mode, zlim=zlim, colors=colors,
    structural_img_colors=structural_img_colors, title=title,
    idx=idx, plane=plane,
    n_slices=n_slices, slices=slices,
    together=together, together_ncol=together_ncol, together_title=together_title,
    widget=widget,
    fname=fname, fname_suffix=fname_suffix, fname_sub=fname_sub,
    legend_fname=legend_fname,
    legend_ncol=legend_ncol, legend_alllevels=legend_alllevels, legend_embed=legend_embed, digits=digits,
    cex.title=cex.title, ypos.title=ypos.title, xpos.title=xpos.title,
    text_color=text_color, bg=bg, width=width, height=height, ...
  )
}
