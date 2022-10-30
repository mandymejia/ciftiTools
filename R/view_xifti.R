#' Get title for \code{view_xifti_surface} or \code{view_xifti_volume}
#' 
#' Determine the title(s) for the cortical surface or subcortical volume plot,
#'  if it was not provided by the user.
#' 
#' @param title Title text or \code{NULL}
#' @param xifti_meta \code{xifti$meta}
#' @param this_idx The index
#' @param cex.title,text_color See \code{\link{view_xifti_surface}}
#' @param indiv_panel_width The width of the panel to write the title in
#' 
#' @return The RGL object ID for the title
#' 
#' @keywords internal
#'  
view_xifti.title <- function(xifti_meta, idx){

  intent <- xifti_meta$cifti$intent

  if (is.null(intent)) {
    if (!is.null(xifti_meta$cifti$names) && length(xifti_meta$cifti$names)>=idx) {
      title <- xifti_meta$cifti$names[idx]
    } else {
      title <- ""
    }

  } else if (intent == 3002) {
    title <- paste("Index", idx)
    if (!any(vapply(xifti_meta$cifti[c("time_start", "time_step", "time_unit")], is.null, FALSE))) {
      title <- paste0(
        title, " (", 
        xifti_meta$cifti$time_start+xifti_meta$cifti$time_step*idx, 
        " ", xifti_meta$cifti$time_unit, "s)"
      )
    }

  } else if (intent == 3006) {
    if (!is.null(xifti_meta$cifti$names) && length(xifti_meta$cifti$names)>=idx) {
      title <- xifti_meta$cifti$names[idx]
    } else {
      title <- ""
    }
    
  } else if (intent == 3007) {
    if (!is.null(xifti_meta$cifti$labels) && length(xifti_meta$cifti$labels)>=idx) {
      title <- names(xifti_meta$cifti$labels)[idx]
    } else {
      title <- ""
    }
  }

  title
}

#' Make the colorbar for \code{view_xifti_surface}
#' 
#' See \code{\link{view_xifti_surface}} for details.
#' 
#' @param pal_base Base palette
#' @param pal Full palette
#' @param color_mode See \code{\link{view_xifti_surface}}
#' @param text_color Color of text
#' @param digits See \code{\link{view_xifti_surface}}
#' 
#' @return A list of keyword arguments to \code{\link[fields]{image.plot}}
#' 
#' @keywords internal
#' 
view_xifti.cbar <- function(pal_base, pal, color_mode, text_color, digits, scientific=NA) {

  colorbar_breaks <- c(
    pal_base$value[1],
    pal$value[1:(length(pal$value)-1)] + diff(pal$value)/2,
    pal$value[length(pal$value)]
  )
  colorbar_breaks <- unique(colorbar_breaks)

  colorbar_labs <- switch(color_mode,
    sequential=c(
      pal_base$value[1],
      pal_base$value[nrow(pal_base)]
    ),
    qualitative=seq_len(nrow(pal_base)),
    diverging=c(
      pal_base$value[1],
      pal_base$value[as.integer(ceiling(nrow(pal_base)/2))],
      pal_base$value[nrow(pal_base)]
    )
  )

  if (length(colorbar_breaks) == 1) {
    colorbar_kwargs <- list(
      legend.only=TRUE, 
      zlim=c(1,2), 
      col=rep(pal$color[1], 2), 
      breaks=c(0, 1, 2), 
      axis.args=list(at=1, labels=colorbar_breaks)
    )
  } else {
    colorbar_kwargs <- list(
      legend.only = TRUE, 
      zlim = range(pal$value), 
      col = as.character(pal$color),
      breaks=colorbar_breaks, 
      #legend.lab=colorbar_label,
      axis.args=list(
        cex.axis=1.7, # size of labels (numeric limits)
        at=colorbar_labs,
        col=text_color, col.ticks=text_color, col.axis=text_color,
        labels=format(colorbar_labs, digits=digits, scientific=scientific)
      )
    )
  }

  colorbar_kwargs <- c(colorbar_kwargs,
    list(
      horizontal=TRUE, # horizontal legend
      legend.cex=2, # not sure what this does?
      #legend.shrink=.5, # half the width of the legend #override by smallplot
      #legend.width=1.67, # height of colorbar #override by smallplot
      legend.line=5, # height of lines between labels and colorbar
      #legend.mar=4, # legend margin #override by smallplot
      smallplot=c(.15, .5, .65, 1) # x1 x2 y1 y2
    )
  )
}

#' Draw color legend for qualitative mode
#' 
#' See \code{\link{view_xifti_surface}} for details.
#' 
#' @param pal_base Base palette + labels for each row
#' @param leg_ncol Number of columns in legend. 
#' @param text_color Color of text
#' @param scale of text
#' 
#' @return A color legend from ggplot2
#' 
#' @keywords internal
#' 
view_xifti.cleg <- function(pal_base, leg_ncol, text_color, scale=1, title_sub=FALSE){

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" needed to make the color legend. Please install it.", call. = FALSE)
  }

  if (!requireNamespace("ggpubr", quietly = TRUE)) {
    stop("Package \"ggpubr\" needed to make the color legend. Please install it.", call. = FALSE)
  }

  point_size <- 5 * scale
  legend_title_size <- 1.5 * scale
  legend_text_size <- 1.2 * scale
  if (is.null(leg_ncol)) { leg_ncol <- floor(sqrt(nrow(pal_base))) }

  colors2 <- pal_base$color; names(colors2) <- pal_base$labels

  value <- NULL
  plt <- ggplot2::ggplot(data=pal_base, ggplot2::aes(x=value, y=value, color=labels)) + 
    ggplot2::geom_point(size=point_size, shape=15) + ggplot2::theme_bw() +
    ggplot2::scale_color_manual(values=colors2, name=ifelse(title_sub, "Labels (subcortex)", "Labels")) +
    ggplot2::guides(color=ggplot2::guide_legend(label.theme=ggplot2::element_text(color=text_color), ncol=leg_ncol)) +
    ggplot2::theme(legend.title=ggplot2::element_text(
      size=ggplot2::rel(legend_title_size)), 
      legend.text=ggplot2::element_text(color=text_color, size=ggplot2::rel(legend_text_size))
    )
  leg <- ggpubr::as_ggplot(ggpubr::get_legend(plt))
}

#' View a \code{"xifti"} object
#' 
#' Displays the data in a \code{"xifti"} object using \code{\link{view_xifti_surface}} 
#'  and/or \code{\link{view_xifti_volume}}. Compared to calling these two
#'  functions separately on the same data, this function may be more convenient
#'  since the automatic choice of color mode and limits is determined across
#'  the entire data and shared between the two plots. Also, if writing files
#'  the subcortical plots will not overwrite the cortical plots. 
#'
#' @inheritParams xifti_Param
#' @param what \code{"surface"}, \code{"volume"}, or \code{"both"}. \code{NULL}
#'  will infer based on the contents of the \code{"xifti"}: if there is data, 
#'  plot the surface cortex data if present, and the volumetric subcortical data 
#'  otherwise. If there is no data, plot the surface geometry if present, and
#'  do nothing otherwise.
#' @param ... Additional arguments to pass to either view function.
#'
#' @return The return value of \code{view_xifti_surface} or
#'  \code{view_xifti_volume}.
#'
#' @export
#'
view_xifti <- function(xifti, what=NULL, ...) {
  stopifnot(is.xifti(xifti))
  
  has_left <- !is.null(xifti$data$cortex_left)
  has_right <- !is.null(xifti$data$cortex_right)
  has_sub <- !is.null(xifti$data$subcort)
  has_surfL <- !is.null(xifti$surf$cortex_left)
  has_surfR <- !is.null(xifti$surf$cortex_right)

  out <- list(surface = NULL, volume = NULL)

  if (is.null(what)) { 
    if ((has_left | has_right) & has_sub) {
      what <- "both"
    } else if (has_left | has_right) { 
      what <- "surface" 
    } else if (has_sub) {
      what <- "volume"
    } else if (has_surfL | has_surfR) {
      what <- "surface"
    } else {
      cat("Nothing to plot: the `xifti` is empty.\n")
      return(NULL)
    }
  } else {
    what <- match.arg(what, c("surface", "volume", "both"))
    if (what == "both" || what == "surface") {
      if (!any(c(has_left, has_right, has_surfL, has_surfR))) {
        stop("No cortical data nor surface geometry are present in the `xifti`, so the surface cannot be plotted.")
      }
    }
    if (what == "both" || what == "volume") {
      if (!has_sub) {
        stop("No subcortical data are present in the `xifti`, so the volume cannnot be plotted.")
      }
    }
  }

  args <- list(...)

  vxs <- function(
    xifti, args, color_mode, zlim, colors,
    fname, fname_sub,
    structural_img, plane, convention, n_slices, slices, 
    ypos.title, xpos.title, orientation_labels,
    ...
    ) { 
    view_xifti_surface(
      xifti, color_mode=args$color_mode, zlim=args$zlim, colors=args$colors, 
      fname=args[["fname"]], ...
    ) 
  }
  vxv <- function(
    xifti, args, color_mode, zlim, colors,
    fname, fname_sub, 
    hemisphere, view, slider_title, borders, alpha, 
    edge_color, vertex_color, vertex_size, material, zoom,
    ...
    ) { 
    view_xifti_volume(
      xifti, color_mode=args$color_mode, zlim=args$zlim, colors=args$colors, 
      fname=args[["fname"]], fname_sub=args[["fname_sub"]], ...
    ) 
  }

  if (length(args$widget) > 1) { 
    args$widget <- as.logical(args$widget[[1]])
  }

  # If `both`, show cortex only if making a widget, and do not write out overlapping
  # file names. ----------------------------------------------------------------
  args[["fname_sub"]] <- FALSE
  if (what == "both") {
    if (isTRUE(args$widget)) {
      warning(
        "Only one widget can be made at a time. ",
        "Plotting the cortex only. ",
        "Use a separate command to display the subcortex.\n"
      )
      args$widget <- TRUE
      what <- "surface"
    } else if (length(args$idx) > 1 && (isFALSE(args[["fname"]]) || is.null(args[["fname"]]))) {
      warning(
        "A widget is needed to display more than one column for the cortex. ",
        "But, only one widget can be made at a time. ",
        "Plotting the cortex only. ",
        "Use a separate command to display the subcortex.\n"
      )
      args$widget <- TRUE
      what <- "surface"
    }
    args[["fname_sub"]] <- TRUE
  } else if (what == "surface") {
    if (length(args$idx) > 1 && (isFALSE(args[["fname"]]) || is.null(args[["fname"]]))) {
      args$widget <- TRUE
    }
  }

  # If `both`, use the same zlim, colors, and color_mode -----------------------
  made_same <- FALSE
  if (what == "both") {

    if (is.null(args$idx)) { args$idx <- 1 }
    args$idx <- as.numeric(args$idx)

    stopifnot(all(args$idx > 0) && all(args$idx <= ncol(xifti)))

    if (is.null(args$color_mode)) { args$color_mode <- "auto" }
    if (args$color_mode == "auto") {
      if (!is.null(xifti$meta$cifti$intent) && xifti$meta$cifti$intent==3007) {
        args$color_mode <- "qualitative"
      } 
      # Otherwise, set after call to view_xifti_surface.mesh_val
    } else {
      args$color_mode <- match.arg(args$color_mode, c("sequential", "qualitative", "diverging"))
    }

    values <- as.vector(as.matrix(xifti)[,args$idx])

    if (!is.null(values)) {

      # Determine `color_mode` and `colors`
      if (args$color_mode == "sequential")
      if (args$color_mode == "auto" || is.null(args$colors)) {
        if (args$color_mode == "auto") {
          if (length(args$zlim) == 3) { 
            args$color_mode <- "diverging"
          } else if (is.null(values) || all(values %in% c(NA, NaN))) { 
            args$color_mode <- "diverging"
            if (is.null(args$colors)) { args$colors <- "ROY_BIG_BL" }
          } else if (length(args$zlim) == 2) {
            args$color_mode <- ifelse(prod(args$zlim) >= 0, "sequential", "diverging")
          } 
        }

        if (args$color_mode == "auto" || (args$color_mode!="qualitative" && is.null(args$colors))) {
          pctile_05 <- quantile(values, .05, na.rm=TRUE)
          pctile_95 <- quantile(values, .95, na.rm=TRUE)
          pctile_05_neg <- pctile_05 < 0
          pctile_95_pos <- pctile_95 > 0

          if (args$color_mode == "sequential") {
            args$colors <- ifelse(pctile_05_neg, "ROY_BIG_BL_neg", "ROY_BIG_BL_pos")
          }

          if (!xor(pctile_05_neg, pctile_95_pos)) {
            if (args$color_mode == "auto") { args$color_mode <- "diverging" }
            if (is.null(args$colors)) { args$colors <- "ROY_BIG_BL" }
          } else if (pctile_95_pos) {
            if (args$color_mode == "auto") { args$color_mode <- "sequential" }
            if (is.null(args$colors)) { args$colors <- "ROY_BIG_BL_pos" }
          } else if (pctile_05_neg) {
            if (args$color_mode == "auto") { args$color_mode <- "sequential" }
            if (is.null(args$colors)) { args$colors <- "ROY_BIG_BL_neg" }
          } else { stop() }
        }
      }

      # Determine `zlim`
      if (! args$color_mode=="qualitative") {
        
        # Use same iff not qualitative and some colors -------------------------
        made_same <- TRUE
        
        if (is.null(args$zlim)) {

          if (is.null(args$digits)) {
            signif_digits <- 3
          } else {
            signif_digits <- args$digits
          }

          DATA_MIN <- signif(min(values, na.rm=TRUE), signif_digits)
          DATA_MAX <- signif(max(values, na.rm=TRUE), signif_digits)

          pctile_05 <- signif(quantile(values, .05, na.rm=TRUE), signif_digits)
          pctile_95 <- signif(quantile(values, .95, na.rm=TRUE), signif_digits)
          pctile_05_neg <- pctile_05 < 0
          pctile_95_pos <- pctile_95 > 0

          if (!pctile_05_neg) {
            if (pctile_95 == 0) { pctile_95 <- DATA_MAX }
            args$zlim <- c(0, pctile_95)
          } else if (!pctile_95_pos) {
            if (pctile_05 == 0) { pctile_05 <- DATA_MAX }
            args$zlim <- c(pctile_05, 0)
          } else {
            pctile_max <- max(abs(c(pctile_05, pctile_95)))
            if (pctile_max == 0) { pctile_max <- max(abs(c(DATA_MIN, DATA_MAX))) }
            if (args$color_mode=="diverging") {
              args$zlim <- c(-pctile_max, 0, pctile_max)
            } else {
              args$zlim <- c(-pctile_max, pctile_max)
            }
          }

          message(
            "`zlim` not provided: using color range ", 
            as.character(min(args$zlim)), " - ", as.character(max(args$zlim)), " ",
            "(data limits: ", as.character(min(DATA_MIN)), " - ", 
            as.character(max(DATA_MAX)), ")."
          )
        }
      }
    }
  }
  # ----------------------------------------------------------------------------

  if (what == "both" | what == "surface") {
    out$surface <- vxs(xifti, args, ...)
  }
  if (what == "both" | what == "volume") {
    args2 <- args
    if (what == "both" && args$color_mode != "qualitative" && "legend_embed" %in% names(args) && args$legend_embed=="FALSE") {
      out$volume <- vxv(xifti, args2, legend_fname=tempfile(), ...)
    } else {
       out$volume <- vxv(xifti, args2, ...)
    }
  }

  out <- switch(what,
    both = out,
    surface = out$surface,
    volume = out$volume
  )

  if (isTRUE(args$widget)) {
    return(out)
  } else {
    return(invisible(out))
  }
}

#' S3 method: use \code{view_xifti} to plot a \code{"xifti"} object
#'
#' @inheritParams x_Param_xifti
#' @param ... Additional arguments to \code{\link{view_xifti}}, except
#'  \code{what}, which will be set to \code{NULL}.
#'
#' @method plot xifti
#' 
#' @export
#' 
plot.xifti <- function(x, ...){
  view_xifti(x, ...)
}

#' @rdname view_xifti
#' @export
view_cifti <- function(xifti, ...){
  view_xifti(xifti, ...)
}

#' @rdname view_xifti
#' @export
viewCIfTI <- function(xifti, ...){
  view_xifti(xifti, ...)
}

#' @rdname view_xifti
#' @export
viewcii <- function(xifti, ...){
  view_xifti(xifti, ...)
}
