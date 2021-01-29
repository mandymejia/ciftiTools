#' Sort out surface & hemispehre args for \code{view_xifti_surface}
#' 
#' See \code{\link{view_xifti_surface}} for details.
#' 
#' @param xifti The \code{"xifti"}
#' @param surfL Left surface
#' @param surfR Right surface
#' @param hemisphere Hemisphere
#' 
#' @return A list with entries "surfL", "surfR" and "hemisphere"
#' 
#' @keywords internal
#' 
view_xifti_surface.surf_hemi <- function(
  xifti, surfL, surfR, hemisphere){

  # Make hemisphere one of: "left", "right", or c("left", "right")
  if (!is.null(hemisphere)) {
    hemisphere <- unique(vapply(hemisphere, match.arg, "", c("left", "right", "both")))
    if ("both" %in% hemisphere) { hemisphere <- c("left", "right") } 
  }

  # Prioritize the surface argument, the surface in the xifti, and the example
  #   surface in that order. Only use the example surface if that cortex data
  #   is present, or if it is indicated in "hemisphere".
  use_example_surf <- c("left"=FALSE, "right"=FALSE)
  for (this_h in c("left", "right")) {
    surf <- switch(this_h, left=surfL, right=surfR)
    surf2 <- switch(this_h, left=xifti$surf$cortex_left, right=xifti$surf$cortex_right)
    surf3 <- demo_files()$surf[this_h]
    dat <- switch(this_h, left=xifti$data$cortex_left, right=xifti$data$cortex_right)

    if (!is.null(surf)) {
      surf <- make_surf(surf, this_h)
    } else if (!is.null(surf2)) {
      surf <- make_surf(surf2, this_h)
    } else if (this_h %in% hemisphere) {
      surf <- make_surf(surf3, this_h)
      use_example_surf[this_h] <- TRUE
    } else if (is.null(hemisphere)) {
      if (!is.null(dat)) { 
        surf <- make_surf(surf3, this_h) 
        use_example_surf[this_h] <- TRUE
      }
    }

    if (this_h == "left") {
      surfL <- surf
    } else {
      surfR <- surf
    }
  }

  # Check: at least one surface will be plotted. 
  if (is.null(surfL) && is.null(surfR)) {
    stop("No cortical data nor surfaces were provided.")
  }

  # Check: warn if the surfaces might not match.
  if (!is.null(surfL) && !is.null(surfR)) {
    if (all(use_example_surf == c("left"=TRUE, "right"=FALSE))) {
      warning(paste(
        "Using default surface for left hemisphere, which may not match",
        "provided surface for right hemisphere. To avoid this problem, set",
        "`hemisphere=\"right\"` to only plot the right hemisphere, or",
        "provide the matching left hemisphere.\n"
      ))
    }
    if (all(use_example_surf == c("left"=FALSE, "right"=TRUE))) {
      warning(paste(
        "Using default surface for right hemisphere, which may not match",
        "provided surface for left hemisphere. To avoid this problem, set",
        "`hemisphere=\"left\"` to only plot the left hemisphere or",
        "provide the matching right hemisphere.\n"
      ))
    }
  }

  # Check: each surface is the correct hemisphere.
  if (!is.null(surfL$hemisphere) && surfL$hemisphere == "right") { 
    warning("The left surface actually corresponds to the right cortex.\n") 
  }
  if (!is.null(surfR$hemisphere) && surfR$hemisphere == "left") { 
    warning("The right surface actually corresponds to the left cortex.\n") 
  }

  if (is.null(hemisphere)) {
    if (!is.null(surfL)) { hemisphere <- "left" }
    if (!is.null(surfR)) { hemisphere <- c(hemisphere, "right") }
  }

  if (length(hemisphere) == 2) { hemisphere <- c("left", "right") } # left first

  list(
    surfL=surfL,
    surfR=surfR,
    hemisphere=hemisphere
  )
}


#' Get the mesh(es) and data values for \code{view_xifti_surface}
#' 
#' See \code{\link{view_xifti_surface}} for details.
#' 
#' @param xifti The \code{"xifti"}
#' @param surfL Left surface
#' @param surfR Right surface
#' @param hemisphere Hemisphere
#' @param idx Index
#' 
#' @return A list with entries "mesh" and "values"
#' 
#' @keywords internal
#' 
view_xifti_surface.mesh_val <- function(xifti, surfL, surfR, hemisphere, idx) {
  
  # ----------------------------------------------------------------------------
  # Get the data values. -------------------------------------------------------
  # ----------------------------------------------------------------------------

  mesh <- list(left=NULL, right=NULL)
  values <- list(left=NULL, right=NULL)

  for (h in hemisphere) {
    surf_h <- switch(h, left=surfL, right=surfR)
    mwall_h <- xifti$meta$cortex$medial_wall_mask[[h]]
    cor_h <- switch(h, left="cortex_left", right="cortex_right")

    if (is.null(mwall_h)) {
      # [TO DO]: Think about this...
      mwall_h_len <- ifelse(
        is.null(xifti$data[[cor_h]]), 
        nrow(surf_h$vertices), 
        nrow(xifti$data[[cor_h]])
      )
      mwall_h <- rep(TRUE, mwall_h_len)
    }

    if (nrow(surf_h$vertices) != length(mwall_h)) {
      ciftiTools_msg(paste(
        "The",h,"surface does not have the same number of vertices as the data",
        "(length of medial wall mask, or rows in data if the mask is absent).",
        "Resampling the",h,"surface. (If the \"wb_path\" option has not been",
        "set an error will occur; set it or correct the surface prior to",
        "plotting.)"
      ))

      surf_h <- resample_surf(
        surf_h, length(mwall_h), hemisphere=h
      )
      if (nrow(surf_h$vertices) != length(mwall_h)) {
        stop(paste(
          "The",h,"surface could not be resampled to match the number of",
          "vertices in the data for the",h,"cortex. Check that the `xifti`",
          "dimensions are as expected?"
        ))
      }
    }

    # Get data values.
    values[[h]] <- matrix(NA, ncol=length(idx), nrow=length(mwall_h))
    if (!is.null(xifti$data[[cor_h]])) {
      if (!all(idx %in% seq_len(ncol(xifti$data[[cor_h]])))) {
        stop(paste0(
          "At least one requested index/indices was not a valid column in",
          " the xifti (between 1 and", ncol(xifti$data[[cor_h]]), ")." 
        ))
      }
      values[[h]][mwall_h,] <- xifti$data[[cor_h]][,idx, drop=FALSE]
    }

    ## Construct the mesh.
    vertices_h <- t(cbind(
      surf_h$vertices, 
      ### Add homogenous coordinates
      rep(1, nrow(surf_h$vertices))
    ))
    faces_h <- t(surf_h$faces)
    mesh[[h]] <- rgl::tmesh3d(vertices_h, faces_h, meshColor = "vertices")

    ## Add normals for smooth coloring.
    mesh[[h]] <- rgl::addNormals(mesh[[h]])
  }

  list(mesh=mesh, values=values)
}

#' Get the palettes and data color mapping for \code{view_xifti_surface}
#' 
#' See \code{\link{view_xifti_surface}} for details.
#' 
#' @param hemisphere,values,idx idx see \code{\link{view_xifti_surface}}
#' @param colors,color_mode,zlim, colorbar_digits see 
#'  \code{\link{view_xifti_surface}}
#' @param xifti_meta \code{xifti$meta}
#' @param border_color,surfL,surfR see \code{\link{view_xifti_surface}}
#' 
#' @return A list with entries "pal_base", "pal", and "color_vals"
#' 
#' @keywords internal
#' 
view_xifti_surface.color <- function(
  hemisphere, values, idx, 
  colors, color_mode, zlim, colorbar_digits,
  xifti_meta,
  border_color, surfL, surfR) {

  # Put values together (to use data bounds across all measurements/columns)
  nvertL <- ifelse("left" %in% hemisphere, nrow(values$left), 0)
  if (!is.null(values)) {
    values <- do.call(rbind, values)
    values[values == NaN] <- NA
    if (all(is.na(values))) { values <- NULL }
  }

  any_colors <- !is.null(values)
  if (any_colors) {

    if (is.null(colorbar_digits)) {
      signif_digits <- 3
    } else {
      signif_digits <- colorbar_digits
    }
    DATA_MIN <- round(min(values, na.rm=TRUE), signif_digits)
    DATA_MAX <- round(max(values, na.rm=TRUE), signif_digits)

    if (is.null(zlim)) {
      if (color_mode=="qualitative") {
        zlim <- c(0,1) # Placeholder: `zlim` is never used for qualitative!
      } else {
        pctile_05 <- round(quantile(values, .05, na.rm=TRUE), signif_digits)
        pctile_95 <- round(quantile(values, .95, na.rm=TRUE), signif_digits)
        pctile_05_neg <- pctile_05 < 0
        pctile_95_pos <- pctile_95 > 0

        if (!pctile_05_neg) {
          zlim <- c(0, pctile_95)
        } else if (!pctile_95_pos) {
          zlim <- c(pctile_05, 0)
        } else {
          pctile_max <- max(abs(c(pctile_05, pctile_95)))
          if (color_mode=="diverging") {
            zlim <- c(-pctile_max, 0, pctile_max)
          } else {
            zlim <- c(-pctile_max, pctile_max)
          }
        }

        message(
          "Since `zlim` was not provided, setting the color range to ", 
          as.character(min(zlim)), " - ", as.character(max(zlim)), "."
        )
      }
    }

    zlim[zlim==-Inf] <- DATA_MIN
    zlim[zlim==Inf] <- DATA_MAX

    # Make base palette and full palette.
    if (color_mode=="qualitative") {
      # For .dlabel files, use the included labels metadata colors.
      if ((!is.null(xifti_meta$cifti$intent) && xifti_meta$cifti$intent==3007) && is.null(colors)) {
        if (length(idx) > 1) { message("Color labels from first requested column will be used.\n") }
        labs <- xifti_meta$cifti$labels[[idx[1]]]
        N_VALUES <- length(labs$Key)
        pal_base <- data.frame(
          color = grDevices::rgb(labs$Red, labs$Green, labs$Blue, labs$Alpha),
          value = labs$Key
        )
      # Otherwise, use the usual colors.
      } else {
        unique_values <- sort(unique(as.vector(values[!is.na(values)])))
        values[,] <- as.numeric(factor(values, levels=unique_values))
        pal_base <- make_color_pal(
          colors=colors, color_mode=color_mode, zlim=length(unique_values)
        )
      }
      pal <- pal_base
    } else {
      pal_base <- make_color_pal(colors=colors,color_mode=color_mode, zlim=zlim)
      pal <- expand_color_pal(pal_base)
    }

    # Map each vertex to a color by its value.
    values[,] <- use_color_pal(as.vector(values), pal, indices=TRUE)

    # Put values back apart into left and right cortex
    if (length(hemisphere) == 2) {
      values <- list(
        left = values[1:nvertL,, drop=FALSE],
        right = values[(nvertL+1):nrow(values),, drop=FALSE]
      )
    } else if (hemisphere == "left") {
      values <- list(
        left = values,
        right = NULL
      )
    } else if (hemisphere == "right") {
      values <- list(
        left = NULL,
        right = values
      )
    }

  } else {
    pal <- pal_base <- NULL
    # Will become NA_COLOR
    values <- list(left=matrix(0), right=matrix(0))
  }

  list(pal_base=pal_base, pal=pal, color_vals=values)
}

#' Make the colorbar for \code{view_xifti_surface}
#' 
#' See \code{\link{view_xifti_surface}} for details.
#' 
#' @param pal_base Base palette
#' @param pal Full palette
#' @param color_mode See \code{\link{view_xifti_surface}}
#' @param text_color Color of text
#' @param colorbar_digits See \code{\link{view_xifti_surface}}
#' 
#' @return A list of keyword arguments to \code{\link[fields]{image.plot}}
#' 
#' @keywords internal
#' 
view_xifti_surface.cbar <- function(pal_base, pal, color_mode, text_color, colorbar_digits) {

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
        cex.axis=1.7, at=colorbar_labs,
        col=text_color, col.ticks=text_color, col.axis=text_color,
        labels=format(colorbar_labs, digits=colorbar_digits)
      )
    )
  }

  colorbar_kwargs <- c(colorbar_kwargs,
    list(
      horizontal=TRUE, # horizontal legend
      legend.cex=2, # double size of labels (numeric limits)
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
#' @param pal_base Base palette
#' @param labels Label for each color in the palette
#' @param leg_ncol Number of columns in legend. 
#' @param colorbar_digits Not used.
#' @param scale of text
#' 
#' @return A list of keyword arguments to \code{\link[fields]{image.plot}}
#' 
#' @keywords internal
#' 
view_xifti_surface.cleg <- function(pal_base, labels, leg_ncol, text_color, scale=1){

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" needed to make the color legend. Please install it.", call. = FALSE)
  }

  if (!requireNamespace("ggpubr", quietly = TRUE)) {
    stop("Package \"ggpubr\" needed to make the color legend. Please install it.", call. = FALSE)
  }

  point_size <- 5 * scale
  legend_title_size <- 1.5 * scale
  legend_text_size <- 1.2 * scale
  if (is.null(leg_ncol)) { leg_ncol <- floor(nrow(pal_base)/10) + 1 }

  pal_base$labels <- factor(labels, levels=unique(labels))
  colors2 <- pal_base$color; names(colors2) <- pal_base$labels

  value <- NULL
  plt <- ggplot2::ggplot(data=pal_base, ggplot2::aes(x=value, y=value, color=labels)) + 
    ggplot2::geom_point(size=point_size, shape=15) + ggplot2::theme_bw() +
    ggplot2::scale_color_manual(values=colors2, name="Labels") +
    ggplot2::guides(color=ggplot2::guide_legend(label.theme=ggplot2::element_text(color=text_color), ncol=leg_ncol)) +
    ggplot2::theme(legend.title=ggplot2::element_text(
      size=ggplot2::rel(legend_title_size)), 
      legend.text=ggplot2::element_text(color=text_color, size=ggplot2::rel(legend_text_size))
    )
  leg <- ggpubr::as_ggplot(ggpubr::get_legend(plt))
}

#' Draw title in RGL
#' 
#' See \code{\link{view_xifti_surface}} for details.
#' 
#' @param title Title text or \code{NULL}
#' @param xifti_meta \code{xifti$meta}
#' @param this_idx The index
#' @param cex.title,text_color See \code{\link{view_xifti_surface}}
#' 
#' @return The RGL object ID for the title
#' 
#' @keywords internal
#' 
view_xifti_surface.draw_title <- function(title, xifti_meta, this_idx, cex.title, text_color){
  if (is.null(title)) {
    intent <- xifti_meta$cifti$intent

    if (is.null(intent)) {
      if (!is.null(xifti_meta$cifti$names) && length(xifti_meta$cifti$names)>=this_idx) {
        title <- xifti_meta$cifti$names[this_idx]
      } else {
        title <- ""
      }

    } else if (intent == 3002) {
      title <- paste("Index", this_idx)
      if (!any(vapply(xifti_meta$cifti[c("time_start", "time_step", "time_unit")], is.null, FALSE))) {
        title <- paste0(
          title, " (", 
          xifti_meta$cifti$time_start+xifti_meta$cifti$time_step*this_idx, 
          " ", xifti_meta$cifti$time_unit, "s)"
        )
      }

    } else if (intent == 3006) {
      if (!is.null(xifti_meta$cifti$names) && length(xifti_meta$cifti$names)>=this_idx) {
        title <- xifti_meta$cifti$names[this_idx]
      } else {
        title <- ""
      }
      
    } else if (intent == 3007) {
      if (!is.null(xifti_meta$cifti$labels) && length(xifti_meta$cifti$labels)>=this_idx) {
        title <- names(xifti_meta$cifti$labels)[this_idx]
      } else {
        title <- ""
      }
    }
  }
  
  if (is.null(cex.title)) {
    # Default: 200% font size, but increasingly smaller for longer titles
    if (nchar(title) > 20) {
      cex.title <- 40 / nchar(title)
    } else {
      cex.title <- 2
    }
  }

  rgl::text3d(
    x=0, y=0, z=0, 
    cex=cex.title,
    adj = c(.5, .5),
    font=2, # Forget if this made a difference...
    color=text_color,
    text=title
  )
}

#' Draw brain hemisphere mesh in RGL
#' 
#' See \code{\link{view_xifti_surface}} for details.
#' 
#' @param mesh RGL brain mesh
#' @param mesh_color The color at each vertex
#' @param alpha,vertex_color,vertex_size,edge_color See 
#'  \code{\link{view_xifti_surface}}
#' 
#' @return \code{NULL}, invisibly
#' 
#' @keywords internal
#' 
view_xifti_surface.draw_mesh <- function(
  mesh, mesh_color, alpha, 
  vertex_color, vertex_size, edge_color){

  # Draw the mesh.
  mesh_col <- rgl::shade3d(
    mesh, 
    color=mesh_color, 
    specular="black", 
    alpha=alpha,
    legend=TRUE
  )

  ## Vertices.
  if (vertex_size > 0) { 
    mesh_vert <- rgl::shade3d(
      mesh, 
      color=vertex_color, size=vertex_size,
      specular="black",
      front="points", back="points", 
      legend=FALSE
    )
  } else {
    mesh_vert <- NULL
  }

  ## Edges.
  if (!is.null(edge_color)) {
    mesh_edge <- rgl::shade3d(
      mesh, 
      color=edge_color, 
      specular="black",
      front="lines", back="lines", 
      legend=FALSE
    )
  } else {
    mesh_edge <- NULL
  }

  out <- list(mesh_col=mesh_col, mesh_vert=mesh_vert, mesh_edge=mesh_edge)
  out[!vapply(out, is.null, FALSE)]
}

#' View cortical surface
#' 
#' Visualize \code{"xifti"} cortical data using an interactive Open GL window
#'  made with \code{rgl}. The \code{rgl} and \code{fields} packages are required.
#'
#' @inheritSection rgl_interactive_plots_Description Navigating and Embedding the Interactive Plots
#' @inheritSection rgl_static_plots_Description Embedding the Static Plots
#' 
#' @inheritParams xifti_Param
#' @inheritParams surface_plot_Params
#' @param idx The time/column index of the data to display. If its length is
#'  greater than one and \code{save==FALSE}, use the widget instead of the 
#'  OpenGL window to view the plot interactively, because the widget's slider
#'  will control what time/column is being displayed in the widget whereas all 
#'  meshes will be rendered on top of one another in the Open GL window.
#' @param hemisphere Which brain cortex to display: "both", "left", or "right".
#'  Each will be plotted in a separate panel column.
#' 
#'  If a brain cortex is requested but no surface is available, a default
#'  inflated surface will be used.
#' 
#'  This argument can also be \code{NULL} (default). In this case, the default
#'  inflated surface included with \code{ciftiTools} will be used for each
#'  cortex with data (i.e. if \code{xifti$data$cortex_left} and/or 
#'  \code{xifti$data$cortex_right} exist).
#' 
#'  Surfaces without data will be colored white. 
#' @param colors (Optional) "ROY_BIG_BL", vector of colors to use,
#'  the name of a ColorBrewer palette (see \code{RColorBrewer::brewer.pal.info}
#'  and colorbrewer2.org), or the name of a viridisLite palette. 
#'  Defaults are \code{"ROY_BIG_BL"} (sequential),
#'  \code{"Set2"} (qualitative), and \code{"ROY_BIG_BL"} (diverging). An exception
#'  to these defaults is if the \code{"xifti"} object represents a .dlabel CIFTI (intent 3007),
#'  then the qualitative colors in the label table will be used.
#'  See \code{\link{make_color_pal}} for more details.
#' @param color_mode (Optional) \code{"sequential"}, \code{"qualitative"},
#'  \code{"diverging"}, or \code{"auto"} (default). Auto mode will use the
#'  qualitative color mode if the \code{"xifti"} object represents a .dlabel 
#'  CIFTI (intent 3007). Otherwise, it will use the diverging mode if the data
#'  contains both positive and negative values, and the sequential mode if the
#'  data contains >90% positive or >90% negative values. See 
#'  \code{\link{make_color_pal}} for more details.
#' @param zlim (Optional) Controls the mapping of values to each
#'  color in \code{colors}. If the length is longer than
#'  one, using -Inf will set the value to the data minimum, and Inf will set
#'  the value to the data maximum. See \code{\link{make_color_pal}} 
#'  description for more details.
#' @param surfL,surfR (Optional if \code{xifti$surf$cortex_left} and
#'  \code{xifti$surf$cortex_right} are not empty) The brain surface model to use.
#'  Each can be a file path for a GIFTI, a file read by gifti::readgii,
#'  or a list with components "vertices" and "faces". If provided, they will override
#'  \code{xifti$surf$cortex_left} and \code{xifti$surf$cortex_right} if they exist.
#'  Otherwise, leave these arguments as \code{NULL} (default) to use
#'  \code{xifti$surf$cortex_left} and \code{xifti$surf$cortex_right}.
#' @param qualitative_colorlegend If \code{color_mode=="qualitative"}, should 
#'  the colorbar be replaced with a color legend? It will be printed separately 
#'  from the RGL window. Default: \code{TRUE}.
#' @param colorlegend_ncol Number of columns in color legend. If
#'  \code{NULL} (default), use 10 entries per row. Only applies if the color
#'  legend is drawn (\code{qualitative_colorlegend} is \code{TRUE} and the
#'  qualitative color mode is used).
#' @param colorbar_embedded Should the colorbar be embedded in the plot?
#'  It will be positioned in the bottom-left corner, in a separate subplot
#'  with 1/4 the height of the brain cortex subplots. Default: \code{TRUE}.
#'  If \code{FALSE}, print it separately instead. 
#' @param colorbar_digits The number of digits for the colorbar legend ticks.
#'  If \code{NULL} (default), let \code{\link{format}} decide.
#' @param border_color Only applicable if \code{color_mode} is 
#'  \code{"qualitative"}. Border vertices will be identified (those that share a
#'  faces with at least one vertex of a different value) and drawn in this color
#'  instead, overriding the color indicated by their value. If \code{NULL}, do
#'  not draw borders.
#' @param widget_idx_warn If \code{save==FALSE} or \code{close_after_save==FALSE},
#'  each mesh must be rendered in the same window. This is problematic if 
#'  \code{idx} and the mesh size are large. So, this option can be used to print 
#'  a warning when \code{length(idx)} exceeds \code{widget_idx_warn} and the 
#'  meshes are being drawn in the same window. Use \code{Inf} to never print a
#'  warning. The default, \code{NULL}, will print a warning if \code{idx} times
#'  the number of vertices exceeds 200k (approximately three 32k meshes for both
#'  the left and right hemisphere.)
#' @param slider_title Text at bottom of plot that will be added if a widget
#'  is used, to provide a title for the slider. Default: \code{"Index:"}.
#'  If \code{NULL}, no title will be added.
#' @param render_rgl Default: \code{TRUE}. If \code{FALSE}, do not render the
#'  Open GL window, widget, or image(s). Instead, return the \code{rgl} mesh
#'  objects and coloring information. This should be used by developers only.
#' @return If \code{save} and \code{close_after_save}, the name(s) of the image
#'  file(s) that were written. 
#' 
#'  Otherwise, if the length of \code{idx} is equal to one, a list of the rgl
#'  object IDs which can be used to further manipulate the Open GL window; if
#'  the length of \code{idx} is greater than one, the htmlwidget with slider
#'  to interactively control which timepoint is being displayed.
#' 
#' @importFrom grDevices dev.list dev.off rgb
#' @importFrom stats quantile
#' @export
view_xifti_surface <- function(xifti, idx=NULL,
  hemisphere=NULL, view=c("both", "lateral", "medial"),
  width=NULL, height=NULL, zoom=NULL,
  bg=NULL, title=NULL, cex.title=NULL, text_color="black",
  save=FALSE, close_after_save=TRUE, 
  colors=NULL, color_mode="auto", zlim=NULL,
  surfL=NULL, surfR=NULL,
  qualitative_colorlegend = TRUE, colorlegend_ncol=NULL,
  colorbar_embedded=TRUE, colorbar_digits=NULL,
  alpha=1.0, 
  edge_color=NULL, vertex_color=NULL, vertex_size=0, border_color=NULL,
  widget_idx_warn=NULL, slider_title="Index:", render_rgl=TRUE, mode=NULL) {

  # Check X11
  if (!capabilities("X11")) {
    ciftiTools_warn("X11 capability is needed to open the rgl window for `view_xifti_surface`.")
  }

  if (length(idx) > 1) {
    if (!requireNamespace("rmarkdown", quietly = TRUE)) {
      stop(paste(
        "Package \"rmarkdown\" will be needed by `view_xifti_surface` to",
        "display a widget. Please install it.\n"
      ))
    }
  }

  # Try to avoid this error with colorbar:
  #   Error in par(old.par) :
  #   invalid value specified for graphical parameter "pin"
  while (!is.null(grDevices::dev.list())) { grDevices::dev.off() }

  # ----------------------------------------------------------------------------
  # Check arguments ------------------------------------------------------------
  # ----------------------------------------------------------------------------

  # Check `xifti` and surfaces.
  stopifnot(is.xifti(xifti))
  T_ <- ncol(do.call(rbind, xifti$data))
  x <- view_xifti_surface.surf_hemi(xifti, surfL, surfR, hemisphere)
  surfL <- x$surfL; surfR <- x$surfR; hemisphere <- x$hemisphere

  # Check `view`.
  view <- unique(vapply(view, match.arg, "", c("lateral", "medial", "both")))
  if ("both" %in% view) { view <- c("lateral", "medial") }
  if (length(view) == 2) { view <- c("lateral", "medial") } # lateral comes first

  if (!is.null(mode)) {
    warning("`mode` is deprecated; use `save` and `close_after_save` instead.\n")
  }
  use_widget <- length(idx) > 1

  # Set `idx` if null.
  if (is.null(idx)) { idx <- 1 }

  # Check `save`
  if (is.null(save)) { save <- FALSE }
  if (identical(save, TRUE)) { save <- "xifti_surf" }
  do_save <- !isFALSE(save)
  if (do_save) {
    if (length(idx) == 1) {
      save <- as.character(save[1])
      if (!endsWith(save, ".png")) { save <- paste0(save, ".png") }
    } else {
      if (length(save)==length(idx)) {
        save <- as.character(save)
      } else {
        if (length(save) != 1) {
          warning("Length of `save` and `idx` differ. Using the first entry of `save`.\n")
        }
        save <- gsub(".png", "", save[1], fixed=TRUE)
        save <- paste0(as.character(save), "_", idx, ".png")
      }
    }
  }

  # Color mode
  if (is.null(color_mode)) { color_mode <- "auto" }
  if (color_mode == "auto") {
    if (!is.null(xifti$meta$cifti$intent) && xifti$meta$cifti$intent==3007) {
      color_mode <- "qualitative"
    } 
    # Otherwise, set after call to view_xifti_surface.mesh_val
  } else {
    color_mode <- match.arg(color_mode, c("sequential", "qualitative", "diverging"))
  }

  NA_COLOR <- "white"

  # Title
  use_title <- TRUE
  if (!is.null(title)) {
    if (length(title) == 1){
      title <- rep(title, length(idx))
    } else if (length(title) != length(idx)) { 
      stop("Length of `title` must be 1 or the same as `idx`.") 
    }
    if (all(title == "")) { use_title <- FALSE }
  }

  # Hopefully will find a better solution (controlling widget size) soon.
  if (is.null(zoom)) {
    zoom <- .6
    if (use_widget) { 
      if (length(view)==1) {
        if (length(hemisphere) == 1) {
          zoom <- .7
        } else {
          zoom <- .85
        }
      } else {
        zoom <- .65
      }
    } 
  }

  use_slider_title <- use_widget && !is.null(slider_title)

  # ----------------------------------------------------------------------------
  # Get the data values and surface models, and construct the mesh. ------------
  # ----------------------------------------------------------------------------

  x <- view_xifti_surface.mesh_val(xifti, surfL, surfR, hemisphere, idx)
  mesh <- x$mesh; values <- x$values

  # Overrides for "auto" color mode.
  if (color_mode == "auto") {
    values_vec <- as.vector(do.call(cbind, values))

    if (length(zlim) == 3) { 
      color_mode <- "diverging"
    } else if (is.null(values) || all(values_vec %in% c(NA, NaN))) { 
      color_mode <- "diverging"
    } else {
      pctile_05 <- quantile(values_vec, .05, na.rm=TRUE)
      pctile_95 <- quantile(values_vec, .95, na.rm=TRUE)
      pctile_05_neg <- pctile_05 < 0
      pctile_95_pos <- pctile_95 > 0

      if (!xor(pctile_05_neg, pctile_95_pos)) {
        color_mode <- "diverging"
        if (is.null(colors)) { colors <- "ROY_BIG_BL" }
      } else if (pctile_95_pos) {
        color_mode <- "sequential"
        if (is.null(colors)) { colors <- "ROY_BIG_BL_pos" }
      } else if (pctile_05_neg) {
        color_mode <- "sequential"
        if (is.null(colors)) { colors <- "ROY_BIG_BL_neg" }
      } else { stop() }
    }
    rm(values_vec)
  }

  if ((!do_save || !close_after_save) && !is.null(values)) {
    
    if (is.null(widget_idx_warn) && (length(idx) * nrow(do.call(cbind, values)) > 200000)) {
      print_widget_idx_warn <- TRUE
    } else if (!is.null(widget_idx_warn) && (length(idx) > widget_idx_warn)) {
      print_widget_idx_warn <- TRUE
    } else {
      print_widget_idx_warn <- FALSE
    }
    if (print_widget_idx_warn) {
      warning(paste(
        "There are many brain meshes being drawn in the same window.",
        "The render time for the htmlwidget might be slow."
      ))
    }
  }

  # ----------------------------------------------------------------------------
  # Get the palettes and vertex coloring. --------------------------------------
  # ----------------------------------------------------------------------------

  x <- view_xifti_surface.color(
    hemisphere, values, idx, 
    colors, color_mode, zlim, colorbar_digits,
    xifti$meta,
    border_color, surfL, surfR
  )
  pal_base <- x$pal_base; pal <- x$pal; color_vals <- x$color_vals
  any_colors <- !all(unlist(lapply(color_vals, dim)) == 1)

  if (!render_rgl) {
    return(
      list(mesh=mesh, values=values, pal_base=pal_base, pal=pal, color_vals=color_vals)
    )
  }

  # ----------------------------------------------------------------------------
  # Get the colorbar/legend arguments. ----------------------------------------
  # ----------------------------------------------------------------------------

  if (any_colors) {

    # Color legend
    if (color_mode == "qualitative" && qualitative_colorlegend) {
      use_cleg <- TRUE; colorbar_embedded <- FALSE
      if (is.null(xifti$meta$cifti$intent)) {
        labels <- paste0("Label ", seq(nrow(pal_base)))
      } else if (xifti$meta$cifti$intent == "3007") {
        labels <- rownames(xifti$meta$cifti$labels[[idx[1]]])
      } else {
        labels <- paste0("Label ", seq(nrow(pal_base)))
      }
      labels <- as.character(labels)
      if (is.null(colorlegend_ncol)) {
        colorlegend_ncol <- floor(nrow(pal_base)/10) + 1
      }
      if (length(labels) > 200) {
        use_cleg <- FALSE
        warning("Too many labels (> 200) for qualitative color legend. Not rendering it.")
      } else {
        cleg <- view_xifti_surface.cleg(pal_base, labels, colorlegend_ncol, text_color)
      }
    } else {
      use_cleg <- FALSE
    }

    # Color bar
    colorbar_kwargs <- view_xifti_surface.cbar(
      pal_base, pal, color_mode, text_color, colorbar_digits
    )
  }

  # ----------------------------------------------------------------------------
  # Set up the RGL window. -----------------------------------------------------
  # ----------------------------------------------------------------------------

  # Check width and height.
  brain_panels_nrow <- length(view)
  brain_panels_ncol <- length(hemisphere)

  all_panels_nrow <- brain_panels_nrow + 1*use_title + 1*colorbar_embedded + 1*use_slider_title
  all_panels_ncol <- brain_panels_ncol

  if (is.null(width) | is.null(height)) {
    DEF_ASPECT_PER_PANEL <- c(10, 7) # aspect ratio
    def_aspect <- DEF_ASPECT_PER_PANEL * c(brain_panels_ncol, brain_panels_nrow)
    DEF_MAX_SIZE <- c(1500, 700)

    if (is.null(width) & is.null(height)) {
      window_dims <- def_aspect*floor(min(DEF_MAX_SIZE/def_aspect))
    } else if (is.null(width)) {
      height <- as.integer(height)
      window_dims <- c(floor(height*def_aspect[1]/def_aspect[2]), height)
    } else if (is.null(height)) {
      width <- as.integer(width)
      window_dims <- c(width, floor(width*def_aspect[2]/def_aspect[1]))
    }
    brain_panels_width <- window_dims[1]
    brain_panels_height <- window_dims[2]
  } else {
    brain_panels_width <- as.integer(width)
    brain_panels_height <- as.integer(height)
  }

  indiv_panel_width <- brain_panels_width/brain_panels_ncol
  indiv_panel_height <- brain_panels_height/brain_panels_nrow

  TITLE_AND_LEGEND_HEIGHT_RATIO <- 1/6
  all_panels_width <- brain_panels_width
  all_panels_height <- brain_panels_height +
    (indiv_panel_height * TITLE_AND_LEGEND_HEIGHT_RATIO) *
      (all_panels_nrow - brain_panels_nrow)

  all_panels_heights <- rep.int(1, brain_panels_nrow)
  if (use_title) { all_panels_heights <- c(TITLE_AND_LEGEND_HEIGHT_RATIO, all_panels_heights) }
  if (colorbar_embedded) { all_panels_heights <- c(all_panels_heights, TITLE_AND_LEGEND_HEIGHT_RATIO) }
  if (use_slider_title) { all_panels_heights <- c(all_panels_heights, TITLE_AND_LEGEND_HEIGHT_RATIO) }

  rglIDs <- vector("list", length(idx))
  names(rglIDs) <- idx

  # For each idx...
  for (jj in seq_len(length(idx))) {
    this_idx <- idx[jj]

    # Open a new window at the start, as well as with each new image.
    if (jj == 1 || do_save) {
      rgl::open3d()
      if (is.null(bg)) { bg <- "white" }
      rgl::bg3d(color=bg)
      rgl::par3d(windowRect = c(20, 20, all_panels_width, all_panels_height))
      Sys.sleep(1) #https://stackoverflow.com/questions/58546011/how-to-draw-to-the-full-window-in-rgl
      subscenes <- rgl::layout3d(
        matrix(1:(all_panels_ncol*all_panels_nrow), nrow=all_panels_nrow, byrow=T),
        widths=rep.int(1, all_panels_ncol),
        heights=all_panels_heights,
        parent = NA, sharedMouse = TRUE
      )
    }

    # Determine the panel layout.
    brain_panels <- as.character(t(outer(view, hemisphere, paste, sep="_"))) # by row
    n_brain_panels <- length(brain_panels)
    rglIDs[[jj]] <- vector("list", length=n_brain_panels+1)
    names(rglIDs[[jj]]) <- c(brain_panels, "title")

    # --------------------------------------------------------------------------
    # Draw title, brain, and color bar. ----------------------------------------
    # --------------------------------------------------------------------------

    # Make the title (if applicable).
    if (use_title) {
      names(subscenes)[subscenes == rgl::subsceneInfo()$id] <- "title"
      rglIDs[[jj]][["title"]] <- view_xifti_surface.draw_title(
        title[jj], xifti$meta, this_idx, cex.title, text_color
      )

      rgl::next3d(current = NA, clear = FALSE, reuse = FALSE)
      if(all_panels_ncol==2){
        rgl::next3d(current = NA, clear = FALSE, reuse = FALSE)
      }
    }

    # Make the brain.
    for(ii in 1:n_brain_panels) {
      p <- brain_panels[ii]
      names(subscenes)[subscenes == rgl::subsceneInfo()$id] <- p

      # Get the hemisphere.
      if (grepl("left", p)) {
        h <- "left"; h2 <- "right"
      } else if (grepl("right", p)) {
        h <- "right"; h2 <- "left"
      } else {
        stop()
      }

      # Get the rotation.
      if (grepl("lateral", p)) {
        rot <- h
      } else if (grepl("medial", p)) {
        rot <- h2
      } else { 
        rot <- "ID"
      }
      rot <- switch(rot, 
        left = rbind( # Outer side of left surface toward viewer
          c( 0,-1, 0, 0),
          c( 0, 0, 1, 0),
          c(-1, 0, 0, 0),
          c( 0, 0, 0, 1)
        ),
        right = rbind( # Outer side of right surface toward viewer
          c( 0, 1, 0, 0),
          c( 0, 0, 1, 0),
          c( 1, 0, 0, 0),
          c( 0, 0, 0, 1)
        ),
        ID = diag(4)
      )

      # If the lateral view was already drawn, re-use that mesh.
      if ((length(view)==2 && grepl("medial", p))) {
        this_mesh <- rglIDs[[jj]][[gsub("medial", "lateral", p)]]
        rgl::addToSubscene3d(this_mesh$mesh_col)
        if (!is.null(this_mesh$mesh_vert)) { rgl::addToSubscene3d(this_mesh$mesh_vert) }
        if (!is.null(this_mesh$mesh_edge)) { rgl::addToSubscene3d(this_mesh$mesh_edge) }
        rglIDs[[jj]][[p]] <- this_mesh

      # Otherwise, draw the mesh.
      } else {
        # Get the color.
        if (any_colors) {
          mesh_color <- c(NA_COLOR, as.character(pal$color))[color_vals[[h]][,jj] + 1]
        } else {
          mesh_color <- rep(NA_COLOR, nrow(switch(h, left=surfL, right=surfR)$vertices))
        }
        # Draw border.
        if (!is.null(border_color) && color_mode=="qualitative") {
          if (h == "left") {
            #cat("\nComputing left border (takes a while).\n")
            mesh_color[parc_borders(values$left[,jj], surfL)] <- border_color
          } else if (h == "right") {
            #cat("\nComputing right border (takes a while).\n")
            mesh_color[parc_borders(values$right[,jj], surfR)] <- border_color
          }
        }
        # Draw the mesh.
        rglIDs[[jj]][[p]] <- view_xifti_surface.draw_mesh(
          mesh[[h]], mesh_color, 
          alpha, vertex_color, vertex_size, edge_color
        )
      }

      rgl::rgl.viewpoint(userMatrix=rot, fov=0, zoom=zoom) #Default: 167% size
      rgl::next3d(current = NA, clear = FALSE, reuse = FALSE)
    }

    # Make the colorbar (if applicable).
    if (any_colors) {

      if (colorbar_embedded) {
        if (do_save || jj==1) {
          names(subscenes)[subscenes == rgl::subsceneInfo()$id] <- "legend"
          if (use_widget) {
            if (length(hemisphere)==1) {
              # Somehow fix colorbar stretching. Changing x doesn't work
              invisible()
            }
          }

          if (!requireNamespace("fields", quietly = TRUE)) {
            stop("Package \"fields\" needed to use `view_xifti_surface`. Please install it.", call. = FALSE)
          }
          rgl::bgplot3d(
            # Warning: calling par(new=TRUE) with no plot
            # Error in par(old.par) :
            #   invalid value specified for graphical parameter "pin"
            try(suppressWarnings(do.call(fields::image.plot, colorbar_kwargs)), silent=TRUE),
            bg.color=bg
          )
        }

        rgl::next3d(current = NA, clear = FALSE, reuse = FALSE)
        if(all_panels_ncol==2){
          rgl::next3d(current = NA, clear = FALSE, reuse = FALSE)
        }
      } else {

        if (use_cleg) {
          print(cleg)
          if (do_save) {
            ggplot2::ggsave(filename=gsub(".png", "_legend.png", save[jj]))
            if (close_after_save) { dev.off() }
          }

        } else {
          if (!requireNamespace("fields", quietly = TRUE)) {
            stop("Package \"fields\" needed to use `view_xifti_surface`. Please install it.", call. = FALSE)
          }
          colorbar_kwargs$smallplot <- c(.15, .85, .45, .6) # x1 x2 y1 y2
          try(suppressWarnings(do.call(fields::image.plot, colorbar_kwargs)), silent=TRUE) 
        }
      }
    }

    if (use_slider_title) {
      rglIDs[[jj]][["slider_title"]] <- view_xifti_surface.draw_title(
        slider_title, xifti$meta, this_idx, cex.title, text_color
      )
      rgl::next3d(current = NA, clear = FALSE, reuse = FALSE)
      if(all_panels_ncol==2){
        rgl::next3d(current = NA, clear = FALSE, reuse = FALSE)
      }
    }

    if (do_save) {
      rgl::rgl.snapshot(save[jj])
      rgl::rgl.close()
    }
  }

  names(subscenes)[is.na(names(subscenes))] <- "Empty"

  if (do_save && close_after_save) {
    return(invisible(save))

  } else if (use_widget) {

    titleIDs <- titleScenes <- NULL
    leftIDs <- leftScenes <- rightIDs <- rightScenes <- NULL
    #return(list(subscenes=subscenes, rglIDs=rglIDs))
    controls <- vector("list", 0)

    if (use_title) {
      titleIDs <- lapply(rglIDs, `[[`, "title")
      titleIDs <- lapply(titleIDs, as.integer)
      if (length(unique(titleIDs)) > 1) {
        titleScenes <- as.integer(subscenes[names(subscenes)=="title"])
        titleControl <- rgl::subsetControl(1, subsets=titleIDs, subscenes=titleScenes)
        controls <- c(controls, list(titleControl))
      }
    }

    if ("left" %in% hemisphere) {
      leftIDs <- lapply(rglIDs, function(x){unique( lapply(x[grepl("left", names(x))], unlist) )})
      leftIDs <- lapply(leftIDs, as.integer)
      leftScenes <- as.integer(subscenes[grepl("left", names(subscenes))])
      leftControl <- rgl::subsetControl(1, subsets=leftIDs, subscenes=leftScenes)
      controls <- c(controls, list(leftControl))
    }

    if ("right" %in% hemisphere) {
      rightIDs <- lapply(rglIDs, function(x){unique( lapply(x[grepl("right", names(x))], unlist) )})
      rightIDs <- lapply(rightIDs, as.integer)
      rightScenes <- as.integer(subscenes[grepl("right", names(subscenes))])
      rightControl <- rgl::subsetControl(1, subsets=rightIDs, subscenes=rightScenes)
      controls <- c(controls, list(rightControl))
    }

    # [TO DO]: Adjust sizing
    return(
      rgl::playwidget(
        rgl::rglwidget(), 
        start=0, stop=length(idx)-1, interval=1,
        components="Slider", #height=all_panels_height, width=all_panels_width,
        controls
      )
    )
  } else {
    return(invisible(rglIDs))
  }
}

#' @rdname view_xifti_surface
#' @export
view_cifti_surface <- function(xifti, idx=NULL,
  hemisphere=NULL, view=c("both", "lateral", "medial"),
  width=NULL, height=NULL, zoom=NULL,
  bg=NULL, title=NULL, cex.title=NULL, text_color="black",
  save=FALSE, close_after_save=TRUE,
  colors=NULL, color_mode=NULL, zlim=NULL,
  surfL=NULL, surfR=NULL,
  colorbar_embedded=TRUE, colorbar_digits=NULL,
  alpha=1.0, edge_color=NULL, vertex_color=NULL, vertex_size=0, 
  widget_idx_warn=NULL, render_rgl=TRUE, mode=NULL){

  view_xifti_surface(
    xifti=xifti, idx=idx,
    hemisphere=hemisphere, view=view,
    width=width, height=height, zoom=zoom,
    bg=bg, title=title, cex.title=cex.title, text_color=text_color,
    save=save, close_after_save=close_after_save,
    colors=colors, color_mode=color_mode, zlim=zlim,
    surfL=surfL, surfR=surfR,
    colorbar_embedded=colorbar_embedded, colorbar_digits=colorbar_digits, 
    alpha=alpha, edge_color=edge_color, 
    vertex_color=vertex_color, vertex_size=vertex_size,
    widget_idx_warn=widget_idx_warn, render_rgl=render_rgl, mode=mode
  )
}

#' @rdname view_xifti_surface
#' @export
viewCIfTI_surface <- function(xifti, idx=NULL,
  hemisphere=NULL, view=c("both", "lateral", "medial"),
  width=NULL, height=NULL, zoom=NULL,
  bg=NULL, title=NULL, cex.title=NULL, text_color="black",
  save=FALSE, close_after_save=TRUE, 
  colors=NULL, color_mode=NULL, zlim=NULL,
  surfL=NULL, surfR=NULL,
  colorbar_embedded=TRUE, colorbar_digits=NULL,
  alpha=1.0, edge_color=NULL, vertex_color=NULL, vertex_size=0, 
  widget_idx_warn=NULL, render_rgl=TRUE, mode=NULL){

  view_xifti_surface(
    xifti=xifti, idx=idx,
    hemisphere=hemisphere, view=view,
    width=width, height=height, zoom=zoom,
    bg=bg, title=title, cex.title=cex.title, text_color=text_color,
    save=save, close_after_save=close_after_save, 
    colors=colors, color_mode=color_mode, zlim=zlim,
    surfL=surfL, surfR=surfR,
    colorbar_embedded=colorbar_embedded, colorbar_digits=colorbar_digits, 
    alpha=alpha, edge_color=edge_color, 
    vertex_color=vertex_color, vertex_size=vertex_size,
    widget_idx_warn=widget_idx_warn, render_rgl=render_rgl, mode=mode
  )
}

#' @rdname view_xifti_surface
#' @export
viewcii_surface <- function(xifti, idx=NULL,
  hemisphere=NULL, view=c("both", "lateral", "medial"),
  width=NULL, height=NULL, zoom=NULL,
  bg=NULL, title=NULL, cex.title=NULL, text_color="black",
  save=FALSE, close_after_save=TRUE,
  colors=NULL, color_mode=NULL, zlim=NULL,
  surfL=NULL, surfR=NULL,
  colorbar_embedded=TRUE, colorbar_digits=NULL,
  alpha=1.0, edge_color=NULL, vertex_color=NULL, vertex_size=0, 
  widget_idx_warn=NULL, render_rgl=TRUE, mode=NULL){

  view_xifti_surface(
    xifti=xifti, idx=idx,
    hemisphere=hemisphere, view=view,
    width=width, height=height, zoom=zoom,
    bg=bg, title=title, cex.title=cex.title, text_color=text_color,
    save=save, close_after_save=close_after_save,
    colors=colors, color_mode=color_mode, zlim=zlim,
    surfL=surfL, surfR=surfR,
    colorbar_embedded=colorbar_embedded, colorbar_digits=colorbar_digits, 
    alpha=alpha, edge_color=edge_color, 
    vertex_color=vertex_color, vertex_size=vertex_size,
    widget_idx_warn=widget_idx_warn, render_rgl=render_rgl, mode=mode
  )
}