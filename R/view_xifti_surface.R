#' Sort out surface & hemisphere args for \code{view_xifti_surface}
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
    surf3 <- ciftiTools.files()$surf[this_h]
    dat <- switch(this_h, left=xifti$data$cortex_left, right=xifti$data$cortex_right)

    if (!is.null(surf)) {
      surf <- read_surf(surf, this_h)
    } else if (!is.null(surf2)) {
      surf <- read_surf(surf2, this_h)
    } else if (this_h %in% hemisphere) {
      surf <- read_surf(surf3, this_h)
      use_example_surf[this_h] <- TRUE
    } else if (is.null(hemisphere)) {
      if (!is.null(dat)) {
        surf <- read_surf(surf3, this_h)
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
#' @param material \code{rgl} material properties
#'
#' @return A list with entries "mesh" and "values"
#'
#' @keywords internal
#'
view_xifti_surface.mesh_val <- function(xifti, surfL, surfR, hemisphere, idx, material=NULL) {

  # ----------------------------------------------------------------------------
  # Get the data values. -------------------------------------------------------
  # ----------------------------------------------------------------------------

  mesh <- list(left=NULL, right=NULL)
  values <- list(left=NULL, right=NULL)

  # solve for cortical resolution.
  res <- infer_resolution(xifti, surfL, surfR)

  # get the mesh and values for each hemisphere
  for (h in hemisphere) {
    h2 <- switch(h, left="right", right="left")
    surf_h <- switch(h, left=surfL, right=surfR)
    mwall_h <- xifti$meta$cortex$medial_wall_mask[[h]]
    cor_h <- switch(h, left="cortex_left", right="cortex_right")
    cor_h2 <- switch(h, left="cortex_right", right="cortex_left")
    res_h <- switch(h, left=res[1], right=res[2])

    # Use empty surface
    if (is.na(res_h)) {
      res_h <- NULL
    } else if (res_h==0) {
      res_h <- switch(h, left=res[2], right=res[1])
      mwall_h <- xifti$meta$cortex$medial_wall_mask[[h2]]
      if (is.na(res_h) || res_h==0) { res_h <- NULL }
    }

    if (is.null(mwall_h)) {
      if (is.null(res_h) || (!is.null(xifti$data[[cor_h]]) && (nrow(xifti$data[[cor_h]]) != res_h))) {
        stop(
          "Cannot infer medial wall locations on ", h,
          " cortex. Please provide $meta$cortex$medial_wall_mask$", cor_h
        )
      } else {
        mwall_h <- rep(TRUE, res_h)
      }
    }

    if (nrow(surf_h$vertices) != res_h) {
      ciftiTools_msg(paste(
        "The",h,"surface is not in the inferred resolution,", res_h, ".",
        "Resampling the",h,"surface. (If the \"wb_path\" option has not been",
        "set an error will occur; set it or correct the surface prior to",
        "plotting.)"
      ))

      surf_h <- resample_surf(surf_h, res_h, hemisphere=h)

      if (nrow(surf_h$vertices) != res_h) {
        stop(paste(
          "The",h,"surface could not be resampled to match the inferred resolution,",
          res_h, ". Check that the `xifti` dimensions are as expected?",
          "If no surface was provided, you can also try adding one to the `xifti` object",
          "with `add_surf`."
        ))
      }
    }

    # Get data values.
    values[[h]] <- matrix(NA, ncol=length(idx), nrow=res_h)
    if (!is.null(xifti$data[[cor_h]])) {
      if (ncol(xifti$data[[cor_h]]) == 0) {
        stop("There are zero data columns in the `xifti`.")
      } else if (!all(idx %in% seq_len(ncol(xifti$data[[cor_h]])))) {
        stop(paste0(
          "At least one requested index/indices was not a valid column in",
          " the xifti (between 1 and ", ncol(xifti$data[[cor_h]]), ")."
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
    mesh[[h]] <- rgl::tmesh3d(
      vertices_h, faces_h, meshColor = "vertices",
      material=material
    )
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
#' @param colors,color_mode,zlim, digits see
#'  \code{\link{view_xifti_surface}}
#' @param xifti_meta \code{xifti$meta}
#' @param surfL,surfR see \code{\link{view_xifti_surface}}
#'
#' @return A list with entries "pal_base", "pal", "color_vals", and "unique_vals"
#'
#' @keywords internal
#'
view_xifti_surface.color <- function(
  hemisphere, values, idx,
  colors, color_mode, zlim, digits,
  xifti_meta,
  surfL, surfR) {

  # For qualitative color legend if xifti was not dlabel
  unique_vals <- NULL

  # Put values together (to use data bounds across all measurements/columns)
  nvertL <- ifelse("left" %in% hemisphere, nrow(values$left), 0)
  if (!is.null(values)) {
    values <- do.call(rbind, values)
    values[values == NaN] <- NA
    if (all(is.na(values))) { values <- NULL }
  }

  any_colors <- !is.null(values)
  if (any_colors) {

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

    # Make base palette and full palette.
    if (color_mode=="qualitative") {
      # For .dlabel files, use the included labels metadata colors.
      if ((!is.null(xifti_meta$cifti$intent) && xifti_meta$cifti$intent==3007)) {
        if (length(idx) > 1) { message("Color labels from first requested column will be used.") }
        labs <- xifti_meta$cifti$labels[[idx[1]]]
        if (is.null(colors)) {
          pal_base <- data.frame(
            color = grDevices::rgb(labs$Red, labs$Green, labs$Blue, labs$Alpha),
            value = labs$Key
          )
        } else {
          unique_vals <- sort(unique(as.vector(values[!is.na(values)])))
          values[,] <- as.numeric(factor(values, levels=unique_vals))
          pal_base <- make_color_pal(
            colors=colors, color_mode=color_mode, zlim=nrow(labs)
          )
        }
      # Otherwise, use the usual colors.
      } else {
        unique_vals <- sort(unique(as.vector(values[!is.na(values)])))
        values[,] <- as.numeric(factor(values, levels=unique_vals))
        pal_base <- make_color_pal(
          colors=colors, color_mode=color_mode, zlim=length(unique_vals)
        )
      }
      pal <- pal_base
    } else {
      if (is.data.frame(colors)) {
        stopifnot(ncol(colors)==2 && colnames(colors)==c("color", "value"))
        pal_base <- colors
      } else {
        pal_base <- make_color_pal(colors=colors, color_mode=color_mode, zlim=zlim)
      }
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

  list(pal_base=pal_base, pal=pal, color_vals=values, unique_vals=unique_vals)
}

#' Draw title in RGL
#'
#' See \code{\link{view_xifti_surface}} for details.
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
view_xifti_surface.draw_title <- function(title, xifti_meta, this_idx, cex.title, text_color, indiv_panel_width){

  if (is.null(title)) {
    title <- view_xifti.title(xifti_meta, this_idx)
  }

  if (is.null(cex.title)) {
    cex.title <- indiv_panel_width / 250
    if (nchar(title) > 20) { cex.title <- cex.title * sqrt(20 / nchar(title)) }
    cex.title <- round(cex.title*100)/100
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
  vertex_color, vertex_size, edge_color, material){

  specular <- "black"
  if (!is.null(material) && "specular" %in% names(material)) {
    specular <- material[["specular"]]
  }

  # Draw the mesh.
  mesh_col <- rgl::shade3d(
    mesh,
    color=mesh_color,
    specular=specular,
    alpha=alpha,
    legend=TRUE
  )

  ## Vertices.
  if (vertex_size > 0) {
    mesh_vert <- rgl::shade3d(
      mesh,
      color=vertex_color, size=vertex_size,
      specular=specular,
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
      specular=specular,
      front="lines", back="lines",
      legend=FALSE
    )
  } else {
    mesh_edge <- NULL
  }

  out <- list(mesh_col=mesh_col, mesh_vert=mesh_vert, mesh_edge=mesh_edge)
  out[!vapply(out, is.null, FALSE)]
}

#' View cortical surface data in a \code{"xifti"}
#'
#' Visualize \code{"xifti"} cortical data using an interactive Open GL window
#'  or htmlwidget made with \code{rgl}. The \code{rmarkdown} package is
#'  required for the htmlwidget functionality.
#'
#' @inheritSection rgl_interactive_plots_Description Navigating and Embedding the Interactive Plots
#' @inheritSection rgl_static_plots_Description Embedding the Static Plots
#'
#' @inheritParams xifti_Param
#' @param surfL,surfR (Optional) The brain surface model to use. Each can be a
#'  \code{"surf"} object, any valid argument to \code{\link{read_surf}} , or one
#'  of \code{"very inflated"}, \code{"inflated"}, or \code{"midthickness"}. If
#'  provided, it will override \code{xifti$surf$cortex_left} or
#'  \code{xifti$surf$cortex_right} if it exists. Leave as \code{NULL} (default)
#'  to use \code{xifti$surf$cortex_left} or \code{xifti$surf$cortex_right} if it
#'  exists, or the default inflated surfaces if it does not exist.
#'
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
#' @param idx The time/column index of the data to display. \code{NULL} (default)
#'  will display the first column.
#'
#'  If its length is greater than one, and \code{isFALSE(fname)},
#'  a widget must be used since a single OpenGL window cannot show multiple
#'  indexes. A slider will be added to the widget to control what time/column
#'  is being displayed.
#' @param hemisphere Which brain cortex to display: "both" (default), "left",
#'  or "right". Each will be plotted in a separate panel column.
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
#' @inheritParams surface_plot_Params
#' @param slider_title Text at bottom of plot that will be added if a slider
#'  is used, to provide a title for it. Default: \code{"Index"}.
#'  If \code{NULL} or an empty character, no title will be added.
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
#'  It will be positioned in the bottom-left corner, in a separate subplot
#'  with 1/4 the height of the brain cortex subplots. Default: \code{TRUE}.
#'  If \code{FALSE}, print/save it separately instead.
#'
#'  Only applies if the color bar is used (sequential or diverging data)
#'  or if \code{"leg" \%in\% together}. Otherwise the color legend
#'  (qualitative data) cannot be embedded at the moment.
#' @param digits The number of digits for the colorbar legend ticks.
#'  If \code{NULL} (default), let \code{\link{format}} decide.
#' @param scientific Use scientific notation? If \code{NA} (default), let
#'  \code{\link{format}} decide.
#' @param borders Only applicable if \code{color_mode} is \code{"qualitative"}.
#'  Border vertices will be identified (those that share a face with at least
#'  one vertex of a different value) and colored over. If this argument is
#'  \code{TRUE} borders will be colored in black; provide the name of a different
#'  color to use that instead. If \code{FALSE} or \code{NULL} (default), do
#'  not draw borders.
#' @return If a png or html file(s) were written, the names of the files for
#'  each index (and color legend if applicable) will be returned. Otherwise,
#'  the widget itself is returned if a widget was used, and the rgl object IDs
#'  are returned if an Open GL window was used. The rgl object IDs are useful
#'  for further programmatic manipulation of the Open GL window.
#'
#' @importFrom grDevices dev.list dev.off png rgb
#' @importFrom stats quantile
#' @family common
#' @export
view_xifti_surface <- function(
  xifti=NULL, surfL=NULL, surfR=NULL,
  color_mode="auto", zlim=NULL, colors=NULL,
  idx=NULL, hemisphere=NULL,
  together=NULL, together_ncol=NULL, together_title=NULL,
  view=c("both", "lateral", "medial"), widget=NULL,
  title=NULL, slider_title="Index",
  fname=FALSE, fname_suffix=c("names", "idx"), legend_fname="[fname]_legend",
  legend_ncol=NULL, legend_alllevels=FALSE, legend_embed=NULL, 
  digits=NULL, scientific=NA,
  cex.title=NULL, text_color="black", bg=NULL,
  borders=FALSE, alpha=1.0, edge_color=NULL, vertex_color=NULL, vertex_size=0,
  material=NULL,
  width=NULL, height=NULL, zoom=NULL
  ) {

  # Try to avoid this error with colorbar:
  #   Error in par(old.par) :
  #   invalid value specified for graphical parameter "pin"
  while (!is.null(grDevices::dev.list())) { grDevices::dev.off() }

  # ----------------------------------------------------------------------------
  # Check arguments ------------------------------------------------------------
  # ----------------------------------------------------------------------------

  # Determine output type.
  # Based on: `idx`, `widget`, and `fname`.
  if (is.null(idx)) { idx <- 1 }
  if (is.null(fname)) { fname <- FALSE }
  if (is.null(legend_fname)) { legend_fname <- "[fname]_legend" }
  idx <- as.numeric(idx)
  if (length(widget) > 1) {
    warning("Using the first entry of `widget`.")
    widget <- as.logical(widget[[1]])
  }
  if (rgl::rgl.useNULL()) {
    if (isFALSE(widget)) {
      warning("`rgl.useNULL` is `TRUE`, and the null device cannot render the Open GL window. Using a widget instead.\n")
    }
    if (length(fname) > 1) {
      warning("Using first entry of `fname`, since only one html file is being written.\n")
      fname <- fname[1]
    }
    if (is.character(fname)) {
      if (endsWith(fname, ".png")) {
        warning("`rgl.useNULL` is `TRUE`, and the null device cannot render the Open GL window to create the pngs. Using an html file instead.\n")
      }
    }
    widget <- TRUE
  } else {
    if (isFALSE(fname)) {
      if (isFALSE(widget) && (length(idx) > 1)) {
        warning(
          "`widget` is `FALSE` but `length(idx) > 1`. ",
          "This is not permissible since the OpenGL window can only show one measurement at a time. ",
          "Setting `widget` to `TRUE`. ",
          "To view multiple measurements without a widget, set `fname` to save .png files or an .html file. ",
          "Or, select a single `idx` to view in the OpenGL window.\n"
        )
        widget <- TRUE
      }
      if (is.null(widget)) { widget <- length(idx) > 1 }
    } else {
      if (is.character(fname)) {
        fname_dirs <- unique(dirname(fname))
        if (!all(dir.exists(fname_dirs))) { stop("`fname` directory does not exist.") }
        if (any(grepl("html", fname))) {
          if (length(fname) > 1) { warning("Using the first entry of `fname` with `'html'` in it.\n") }
          if (!endsWith(fname, ".html")) { warning("fname has `html` in its name aside from the file extension.\n"); fname <- paste0(fname, ".html") }
          fname <- fname[grepl("html", fname)][1]
          if (isFALSE(widget)) { warning("Saving an .html file requires rendering a widget. Setting `widget=TRUE`.\n") }
          widget <- TRUE
        } else {
          if (isTRUE(widget)) {
            warning(
              "`fname` is not `FALSE` but `widget` is `TRUE`. ",
              "`view_xifti_surface` assumes the user wants to save a .png file(s), but these can only be rendered using the Open GL window. ",
              "Setting `widget` to `FALSE` in order to render .png file(s) using OpenGL. ",
              "To save an html file instead, append `'.html'` to `fname`. ",
              "Or, to view a widget rather than writing any files, set `fname` to `FALSE`.\n"
            )
          }
          widget <- FALSE
        }
      } else if (isTRUE(fname)) {
        if (isTRUE(widget)) {
          warning(
            "`fname` is not `FALSE` but `widget` is `TRUE`. ",
            "`view_xifti_surface` assumes the user wants to save a .png file(s), but these can only be rendered using the Open GL window. ",
            "Setting `widget` to `FALSE` in order to render .png file(s) using OpenGL. ",
            "To save an html file instead, append `'.html'` to `fname`. ",
            "Or, to view a widget rather than writing any files, set `fname` to `FALSE`.\n"
          )
        }
        widget <- FALSE
      }
    }
  } # `widget` should be `TRUE` or `FALSE` now.

  saving_file <- !isFALSE(fname)

  # File saving: `together`
  if (!is.null(together)) {

    # TO DO: allow display of static composites in the widget?

    if (widget) { stop(
      "Composite images are not compatible with widget. ",
      "Set `together=NULL` or `widget=FALSE`."
    ) }

    if (!saving_file) { stop(
      "Composite images are not compatible with OpenGL window. ",
      "Set `together=NULL` or file path(s) for `fname`."
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
  } else {
    together <- together_leg <- together_idx <- FALSE
  }

  # File saving: `fname`
  comp_fname <- NULL
  legend_embed2 <- FALSE
  if (saving_file) {

    fname_use_names <- !together_idx && !is.null(xifti$meta$cifti$names)

    # Use default file name(s) if `fname==TRUE`.
    if (isTRUE(fname)) {
      if (together_idx) {
        fname <- "xifti_surf"
        comp_fname <- fname
      } else if (fname_use_names) {
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
        fname <- "xifti_surf"
      }
    } else {
      fname <- as.character(fname)
    }

    # `fname` if saving htmlwidget.
    if (any(grepl("\\.html$", fname)) || rgl::rgl.useNULL()) {
      if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
        stop(
          "Package \"htmlwidgets\" will be needed by `view_xifti_surface` to ",
          "write the widget to an html file. Please install it.\n"
        )
      }
      if (length(fname) > 1) {
        warning("Using first entry of `fname`, since only one html file is being written.\n")
        fname <- fname[1]
      }
      if (!grepl("\\.html$", fname)) {
        fname <- paste0(fname, ".html")[1]
      }

    # `fname` if saving png.
    } else {
      # Strip file extension.
      fname <- gsub(".pdf$", "", fname)
      fname <- gsub(".png$", "", fname)
      # Check length.
      if (!(length(fname) %in% c(1, length(idx)))) {
        warning("Using first entry of `fname` since its length is not 1, or the length of `idx`.\n")
        fname <- fname[1]
      }
      # Save `fname` to use for composite.
      if (together_idx && is.null(comp_fname)) {
        comp_fname <- fname
        if (length(comp_fname) > 1) {
          warning("Using the first entry of `fname` (since compositing, there's only one file to save).\n")
          comp_fname <- comp_fname[1]
        }
      }

      # Modify `fname`
      if (together) {
        legend_embed2 <- !isFALSE(legend_embed)
        legend_embed <- FALSE
      }
      if (together_idx) {
        fname <- paste0(tempfile(as.character(seq(length(idx)))), ".png")
        comp_fname <- paste0(comp_fname, ".png")
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
        fname <- paste0(fname, ".png")
      }

      # Check that `fname` are unique.
      if (length(fname) != length(unique(fname))) {
        warning(
          "Non-unique file names... proceeding anyway. ",
          "If this is not intended, set `fname` to the exact unique file ",
          "name(s) you want to use for each `idx`.\n"
        )
      }
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
  } else {
    legend_fname <- FALSE
  }

  # Check `rmarkdown`
  if (widget && !requireNamespace("rmarkdown", quietly = TRUE)) {
    stop(paste(
      "Package \"rmarkdown\" will be needed by `view_xifti_surface` to",
      "display a widget. Please install it.\n"
    ))
  }

  # `xifti`, `surfL`, `surfR`
  if (is.character(surfL) && length(surfL)==1 && surfL %in% c("inflated", "very inflated", "midthickness")) {
    surfL <- load_surf("left", surfL)
  }
  if (is.character(surfR) && length(surfR)==1 && surfR %in% c("inflated", "very inflated", "midthickness")) {
    surfR <- load_surf("right", surfR)
  }
  if (is.null(xifti)) {
    if (is.null(surfL) && is.null(surfR)) {
      warning("Nothing to plot in `view_xifti_surface`.\n")
      return(NULL)
    } else {
      if (!is.null(idx) && (length(idx)>1 || idx!=1)) {
        warning("Ignoring `idx` argument, since there is no data to plot.\n")
      }
      return(
        view_xifti_surface(
          xifti=make_xifti(surfL=surfL, surfR=surfR),
          colors=colors, hemisphere=hemisphere, view=view, widget=widget,
          title=title, slider_title=slider_title,
          fname=fname, fname_suffix=fname_suffix,
          legend_ncol=legend_ncol, legend_embed=legend_embed, digits=digits,
          cex.title=cex.title, text_color=text_color, bg=bg,
          borders=borders, alpha=alpha, edge_color=edge_color,
          vertex_color=vertex_color, vertex_size=vertex_size,
          width=width, height=height, zoom=zoom
        )
      )
    }
  }
  stopifnot(is.xifti(xifti))
  x <- view_xifti_surface.surf_hemi(xifti, surfL, surfR, hemisphere)
  surfL <- x$surfL; surfR <- x$surfR; hemisphere <- x$hemisphere

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

  # `hemisphere`
  hemisphere <- as.character(hemisphere)
  hemisphere <- unique(vapply(hemisphere, match.arg, "", c("left", "right", "both")))
  if ("both" %in% hemisphere) { hemisphere <- c("left", "right") }
  if (length(hemisphere) > 1) { hemisphere <- c("left", "right") } # left first

  # `view`
  view <- as.character(view)
  view <- unique(vapply(view, match.arg, "", c("lateral", "medial", "both")))
  if ("both" %in% view) { view <- c("lateral", "medial") }
  if (length(view) > 1) { view <- c("lateral", "medial") } # lateral first

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

  # `slider_title`
  use_slider_title <- widget && length(idx)>1 && !is.null(slider_title)
  if (use_slider_title) {
    slider_title <- as.character(slider_title)[1]
    use_slider_title <- widget && (slider_title != "")
  }

  # `legend_ncol`, `digits`
  if (!is.null(legend_ncol)) { legend_ncol <- as.numeric(legend_ncol) }
  if (!is.null(digits)) { digits <- as.numeric(digits) }
  # handle `legend_embed` later

  # `cex.title`, `text_color`, "bg"
  if (!is.null(cex.title)) { cex.title <- as.numeric(cex.title) }
  text_color <- as.character(text_color)
  if (!is.null(bg)) { bg <- as.character(bg) }

  # `borders`, `alpha`, `edge_color`, `vertex_color`, `vertex_size`
  if (!is.null(borders)) {
    if (length(borders) > 1) { warning("Using first entry of `borders` only.\n") }
    borders <- borders[1]
    if (isFALSE(borders)) {
      borders <- NULL
    } else if (isTRUE(borders)) {
      borders <- "black"
    } else {
      borders <- as.character(borders)
    }
  }
  alpha <- as.numeric(alpha)
  if (!is.null(edge_color)) { edge_color <- as.character(edge_color) }
  if (!is.null(vertex_color)) { vertex_color <- as.character(vertex_color) }
  if (!is.null(vertex_size)) { vertex_size <- as.numeric(vertex_size) }

  # `width`, `height`, `zoom`
  # [TO DO]: Improve this?
  if (is.null(zoom)) {
    if (widget) { zoom <- .67 } else { zoom <- .6 }
  }
  if (!is.null(width)) { width <- as.numeric(width) }
  if (!is.null(height)) { height <- as.numeric(height) }

  # Former arguments that are now internal
  render_rgl <- TRUE # for debugging
  NA_COLOR <- "white" # surfaces without data will be white
  close_after_save <- TRUE # always close after saving
  widget_idx_warn <- TRUE # always warn if widget might take a while to load

  # ----------------------------------------------------------------------------
  # Get the data values and surface models, and construct the mesh. ------------
  # ----------------------------------------------------------------------------

  x <- view_xifti_surface.mesh_val(xifti, surfL, surfR, hemisphere, idx, material)
  mesh <- x$mesh; values <- x$values

  # Set `color_mode` if `"auto"`; set `colors` if `NULL`
  if (color_mode == "auto" || (color_mode!="qualitative" && is.null(colors))) {
    values_vec <- do.call(c, values)

    if (color_mode == "auto") {
      if (length(zlim) == 3) {
        color_mode <- "diverging"
      } else if (is.null(values_vec) || all(values_vec %in% c(NA, NaN))) {
        color_mode <- "diverging"
        if (is.null(colors)) { colors <- "ROY_BIG_BL" }
      } else if (length(zlim) == 2) {
        color_mode <- ifelse(prod(zlim) >= 0, "sequential", "diverging")
      }
    }

    if (color_mode == "auto" || is.null(colors)) {
      pctile_05 <- quantile(values_vec, .05, na.rm=TRUE)
      pctile_95 <- quantile(values_vec, .95, na.rm=TRUE)
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

    rm(values_vec)
  }

  # Warn user if widget rendering may take a while.
  if (widget && !is.null(values)) {
    if (widget_idx_warn && length(idx) * max(vapply(values, length, 0)) > 200000) {
      warning(paste(
        "There are many brain meshes being drawn.",
        "The render time for the htmlwidget might be slow.\n"
      ))
    }
  }

  # ----------------------------------------------------------------------------
  # Get the palettes and vertex coloring. --------------------------------------
  # ----------------------------------------------------------------------------

  x <- view_xifti_surface.color(
    hemisphere, values, idx,
    colors, color_mode, zlim, digits,
    xifti$meta, surfL, surfR
  )
  pal_base <- x$pal_base; pal <- x$pal
  color_vals <- x$color_vals; unique_vals <- x$unique_vals
  any_colors <- !all(unlist(lapply(color_vals, dim)) == 1)

  # Return `rgl` input info instead of rendering mesh (for debugging).
  if (!render_rgl) {
    return(
      list(mesh=mesh, values=values, pal_base=pal_base, pal=pal, color_vals=color_vals)
    )
  }

  # ----------------------------------------------------------------------------
  # Get the colorbar/legend arguments. ----------------------------------------
  # ----------------------------------------------------------------------------

  use_cleg <- FALSE
  if (any_colors) {
    # Use color legend (except in the weird case).
    if (color_mode == "qualitative") {
      if (isTRUE(legend_embed) && !together_leg) {
        warning(
          "`legend_embed` is `TRUE` and `color_mode` is `'qualitative'`. ",
          "However, the color legend for qualitative data cannot be embedded. ",
          "Embedding a color bar that shows the colors in order, instead of the color legend. ",
          "To view the color legend separately (as recommended) set `legend_embed` to `FALSE`.\n"
        )
        colorbar_kwargs <- view_xifti.cbar(pal_base, pal, color_mode, text_color, digits, scientific=scientific) # added?
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
        if ((!legend_alllevels) && (is.null(unique_vals))) {
          cleg <- cleg[cleg$value %in% as.vector(do.call(c, values)),]
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
            cleg <- view_xifti.cleg(cleg, legend_ncol, text_color)
          }
        }
      }
      colorbar_kwargs <- NULL

    # Color bar
    } else {
      if (is.null(legend_embed)) { legend_embed <- TRUE }
      if (together_leg) { legend_embed <- FALSE }
      colorbar_kwargs <- view_xifti.cbar(pal_base, pal, color_mode, text_color, digits, scientific=scientific)
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
  # Set up the RGL window. -----------------------------------------------------
  # ----------------------------------------------------------------------------

  # Check width and height.
  brain_panels_nrow <- length(view)
  brain_panels_ncol <- length(hemisphere)

  all_panels_nrow <- brain_panels_nrow + 1*use_title + 1*legend_embed + 1*use_slider_title
  all_panels_ncol <- brain_panels_ncol

  if (is.null(width) || is.null(height)) {
    DEF_ASPECT_PER_BRAIN_PANEL <- c(10, 7) # aspect ratio
    def_aspect <- DEF_ASPECT_PER_BRAIN_PANEL * c(brain_panels_ncol, brain_panels_nrow)
    if (widget) { DEF_MAX_SIZE <- c(600, 700) } else { DEF_MAX_SIZE <- c(1500, 700) }

    if (is.null(width) & is.null(height)) {
      brain_panels_dims <- def_aspect*floor(min(DEF_MAX_SIZE/def_aspect))
    } else if (is.null(width)) {
      height <- round(height)
      brain_panels_dims <- c(floor(height*def_aspect[1]/def_aspect[2]), height)
    } else if (is.null(height)) {
      width <- round(width)
      brain_panels_dims <- c(width, floor(width*def_aspect[2]/def_aspect[1]))
    }
    brain_panels_dims <- as.integer(brain_panels_dims)
  } else {
    brain_panels_dims <- as.integer(round(c(width, height)))
  }

  together_scale <- brain_panels_dims[1]/300

  indiv_panel_width <- brain_panels_dims[1]/brain_panels_ncol
  indiv_panel_height <- brain_panels_dims[2]/brain_panels_nrow

  TITLE_AND_LEGEND_HEIGHT_RATIO <- 1/5 # formerly 1/6
  all_panels_width <- brain_panels_dims[1]
  # Add to height to account for title and slider title.
  all_panels_height <- brain_panels_dims[2] +
    (indiv_panel_height * TITLE_AND_LEGEND_HEIGHT_RATIO) * (all_panels_nrow - brain_panels_nrow)

  panels_rel_heights <- rep.int(1, brain_panels_nrow)
  if (use_title) { panels_rel_heights <- c(TITLE_AND_LEGEND_HEIGHT_RATIO, panels_rel_heights) }
  if (legend_embed) { panels_rel_heights <- c(panels_rel_heights, TITLE_AND_LEGEND_HEIGHT_RATIO) }
  if (use_slider_title) { panels_rel_heights <- c(panels_rel_heights, TITLE_AND_LEGEND_HEIGHT_RATIO) }

  rglIDs <- vector("list", length(idx))
  names(rglIDs) <- idx

  # For each idx...
  for (jj in seq_len(length(idx))) {
    this_idx <- idx[jj]

    # Open a new window at the start, as well as with each new image.
    if (jj == 1 || (saving_file && !widget)) {
      rgl::open3d()
      if (is.null(bg)) { bg <- "white" }
      rgl::bg3d(color=bg)
      rgl::par3d(windowRect = 40 + c(0, 0, round(all_panels_width), round(all_panels_height)))
      Sys.sleep(1) #https://stackoverflow.com/questions/58546011/how-to-draw-to-the-full-window-in-rgl
      if (use_title && length(hemisphere) > 1) {
        layout_matrix <- matrix(c(1, seq(all_panels_ncol*all_panels_nrow - 1)), nrow=all_panels_nrow, byrow=T)
      } else {
        layout_matrix <- matrix(seq(all_panels_ncol*all_panels_nrow), nrow=all_panels_nrow, byrow=T)
      }
      subscenes <- rgl::layout3d(
        layout_matrix,
        widths=rep.int(1, all_panels_ncol),
        heights=panels_rel_heights,
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
        title[jj], xifti$meta, this_idx, cex.title, text_color, indiv_panel_width
      )

      rgl::next3d(current = NA, clear = FALSE, reuse = FALSE)
      # if(all_panels_ncol==2){
      #   rgl::next3d(current = NA, clear = FALSE, reuse = FALSE)
      # }
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
        if (!is.null(borders) && color_mode=="qualitative") {
          if (h == "left") {
            mesh_color[parc_borders(values$left[,jj])] <- borders
          } else if (h == "right") {
            mesh_color[parc_borders(values$right[,jj])] <- borders
          }
        }
        # Draw the mesh.
        rglIDs[[jj]][[p]] <- view_xifti_surface.draw_mesh(
          mesh[[h]], mesh_color,
          alpha, vertex_color, vertex_size, edge_color, material
        )
      }

      rgl::view3d(userMatrix=rot, fov=0, zoom=zoom)
      rgl::next3d(current = NA, clear = FALSE, reuse = FALSE)
    }

    # Make (and save) the colorbar (if applicable).
    if (any_colors) {

      if (legend_embed) {
        if (jj == 1 || (saving_file && !widget)) {
          names(subscenes)[subscenes == rgl::subsceneInfo()$id] <- "legend"
          if (widget) {
            if (length(hemisphere)==1) {
              # [TO DO] Somehow fix colorbar stretching. Changing x doesn't work
              invisible()
            }
          }

          if (!requireNamespace("fields", quietly = TRUE)) {
            stop("Package \"fields\" needed to render the color bar for `view_xifti_surface`. Please install it.", call. = FALSE)
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
        if (jj == 1) {
          if (use_cleg) {
            print(cleg)
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

          } else {
            if (!requireNamespace("fields", quietly = TRUE)) {
              stop("Package \"fields\" needed to render the color bar for `view_xifti_surface`. Please install it.", call. = FALSE)
            }
            if (!isFALSE(fname) && !isFALSE(legend_fname)) {
              png(
                legend_fname, bg=bg,
                width=brain_panels_dims[1], height=ceiling(brain_panels_dims[1]*.4)
              )
            }
            colorbar_kwargs$smallplot <- c(.15, .85, .7, .9) # x1 x2 y1 y2
            # Make labels smaller (cex.axis) and lower to compensate (mgp)
            colorbar_kwargs$axis.args$cex.axis <- colorbar_kwargs$axis.args$cex.axis * together_scale * .85
            colorbar_kwargs$axis.args$mgp <- c(3,.5+together_scale,0)

            try(suppressWarnings(do.call(fields::image.plot, colorbar_kwargs)), silent=TRUE)
            if (!isFALSE(fname) && !isFALSE(legend_fname)) { if (close_after_save) { dev.off() } }
          }
        }
      }
    }

    if (use_slider_title) {
      if (jj==1) {
        rglIDs[[jj]][["slider_title"]] <- view_xifti_surface.draw_title(
          slider_title, xifti$meta, this_idx, cex.title, text_color, widget
        )
      }
      rgl::next3d(current = NA, clear = FALSE, reuse = FALSE)
      if(all_panels_ncol==2){
        rgl::next3d(current = NA, clear = FALSE, reuse = FALSE)
      }
    }

    if (!widget && saving_file) {
      rgl::rgl.snapshot(fname[jj])
      rgl::close3d()
    }
  }

  names(subscenes)[is.na(names(subscenes))] <- "Empty"

  fname_all <- fname
  if (!isFALSE(legend_fname) && !legend_embed) {
    if (use_cleg || any_colors){ fname_all <- c(fname, legend_fname) }
  }

  if (widget) {

    titleIDs <- titleScenes <- NULL
    leftIDs <- leftScenes <- rightIDs <- rightScenes <- NULL
    controls <- vector("list", 0)

    if (use_title) {
      titleIDs <- lapply(rglIDs, `[[`, "title")
      titleIDs <- unlist(titleIDs)
      if (length(unique(titleIDs)) > 1) {
        titleScenes <- as.integer(subscenes[names(subscenes)=="title"])
        titleControl <- rgl::subsetControl(1, subsets=titleIDs, subscenes=titleScenes)
        controls <- c(controls, list(titleControl))
      }
    }

    if ("left" %in% hemisphere) {
      leftIDs <- lapply(rglIDs, function(x){unique( lapply(x[grepl("left", names(x))], unlist) )})
      leftIDs <- unlist(leftIDs)
      leftScenes <- as.integer(subscenes[grepl("left", names(subscenes))])
      leftControl <- rgl::subsetControl(1, subsets=leftIDs, subscenes=leftScenes)
      controls <- c(controls, list(leftControl))
    }

    if ("right" %in% hemisphere) {
      rightIDs <- lapply(rglIDs, function(x){unique( lapply(x[grepl("right", names(x))], unlist) )})
      rightIDs <- unlist(rightIDs)
      rightScenes <- as.integer(subscenes[grepl("right", names(subscenes))])
      rightControl <- rgl::subsetControl(1, subsets=rightIDs, subscenes=rightScenes)
      controls <- c(controls, list(rightControl))
    }

    # [TO DO]: Adjust sizing
    # Looked into htmlwidgets::sizingPolicy but it doesn't affect how the legend
    #   scales with the plot width, which can't be controlled within RStudio
    #   pane.
    out <- rgl::rglwidget(
      height=round(all_panels_height/1.1),
      width=round(all_panels_width/1.1)
    )
    if (length(idx) > 1) {
      out <- rgl::playwidget(
        out, start=0, stop=length(idx)-1, interval=1,
        components="Slider", controls
      )
    }
    rgl::close3d()
  }

  # Does this ever happen?
  if (!file.exists(as.character(legend_fname))) { legend_fname <- NULL }
  # Compositing `together`
  if (together_idx) {
    if (is.null(together_ncol)) {
      together_ncol <- ceiling(sqrt(length(idx)))
    }
    together_nrow <- ceiling(length(idx)/together_ncol)
    comp_width <- all_panels_width * together_ncol
    comp_height <- all_panels_height * together_nrow
    title_height <- .2/together_nrow
    leg_height <- .3/together_nrow
    comp_height_mult <- 1
    if (together_leg || !use_cleg) { comp_height_mult <- comp_height_mult + leg_height }
    if (!is.null(together_title)) { comp_height_mult <- comp_height_mult + title_height }
    png(comp_fname, bg=bg, width=comp_width, height=comp_height * comp_height_mult)
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
    dev.off()
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
    # comp_fname <- crop_image(comp_fname)
  } else if (together_leg) {
    for (ff in seq(length(fname))) {
      tfile <- tempfile()
      png(tfile, bg=bg, width=all_panels_width, height=floor(all_panels_height*1.3))
      view_comp(fname[ff], legend=legend_fname)
      dev.off()
      file.copy(tfile, fname[ff], overwrite=TRUE)
      file.remove(tfile)
    }
    fname_all <- fname
  }


  # Return values.
  if (widget) {
    if (saving_file) {
      htmlwidgets::saveWidget(out, fname, background=bg, title="ciftiTools surface plot")
      return(invisible(fname))
    } else {
      return(out)
    }
  } else if (saving_file) {
    return(invisible(fname_all))
  } else {
    return(invisible(rglIDs))
  }
}

#' @rdname view_xifti_surface
#' @export
view_cifti_surface <- function(
  xifti=NULL, surfL=NULL, surfR=NULL,
  color_mode="auto", zlim=NULL, colors=NULL,
  idx=NULL, hemisphere=NULL,
  together=NULL, together_ncol=NULL, together_title=NULL,
  view=c("both", "lateral", "medial"), widget=NULL,
  title=NULL, slider_title="Index",
  fname=FALSE, fname_suffix=c("names", "idx"), legend_fname="[fname]_legend",
  legend_ncol=NULL, legend_alllevels=FALSE, legend_embed=NULL, 
  digits=NULL, scientific=NA,
  cex.title=NULL, text_color="black", bg=NULL,
  borders=FALSE, alpha=1.0, edge_color=NULL, vertex_color=NULL, vertex_size=0,
  width=NULL, height=NULL, zoom=NULL){

  view_xifti_surface(
    xifti=xifti, surfL=surfL, surfR=surfR,
    color_mode=color_mode, zlim=zlim, colors=colors,
    idx=idx, hemisphere=hemisphere,
    together=together, together_ncol=together_ncol, together_title=together_title,
    view=view, widget=widget,
    title=title, slider_title=slider_title,
    fname=fname, fname_suffix=fname_suffix, legend_fname=legend_fname,
    legend_ncol=legend_ncol, legend_alllevels=legend_alllevels, legend_embed=legend_embed, 
    digits=digits, scientific=scientific,
    cex.title=cex.title, text_color=text_color, bg=bg,
    borders=borders, alpha=alpha, edge_color=edge_color, vertex_color=vertex_color, vertex_size=vertex_size,
    width=width, height=height, zoom=zoom
  )
}

#' @rdname view_xifti_surface
#' @export
viewCIfTI_surface <- function(
  xifti=NULL, surfL=NULL, surfR=NULL,
  color_mode="auto", zlim=NULL, colors=NULL,
  idx=NULL, hemisphere=NULL,
  together=NULL, together_ncol=together_ncol, together_title=NULL,
  view=c("both", "lateral", "medial"), widget=NULL,
  title=NULL, slider_title="Index",
  fname=FALSE, fname_suffix=c("names", "idx"), legend_fname="[fname]_legend",
  legend_ncol=NULL, legend_alllevels=FALSE, legend_embed=NULL, 
  digits=NULL, scientific=NA,
  cex.title=NULL, text_color="black", bg=NULL,
  borders=FALSE, alpha=1.0, edge_color=NULL, vertex_color=NULL, vertex_size=0,
  width=NULL, height=NULL, zoom=NULL){

  view_xifti_surface(
    xifti=xifti, surfL=surfL, surfR=surfR,
    color_mode=color_mode, zlim=zlim, colors=colors,
    idx=idx, hemisphere=hemisphere,
    together=together, together_ncol=together_ncol, together_title=together_title,
    view=view, widget=widget,
    title=title, slider_title=slider_title,
    fname=fname, fname_suffix=fname_suffix, legend_fname=legend_fname,
    legend_ncol=legend_ncol, legend_alllevels=legend_alllevels, legend_embed=legend_embed, 
    digits=digits, scientific=scientific,
    cex.title=cex.title, text_color=text_color, bg=bg,
    borders=borders, alpha=alpha, edge_color=edge_color, vertex_color=vertex_color, vertex_size=vertex_size,
    width=width, height=height, zoom=zoom
  )
}

#' @rdname view_xifti_surface
#' @export
viewcii_surface <- function(
  xifti=NULL, surfL=NULL, surfR=NULL,
  color_mode="auto", zlim=NULL, colors=NULL,
  idx=NULL, hemisphere=NULL,
  together=NULL, together_ncol=together_ncol, together_title=NULL,
  view=c("both", "lateral", "medial"), widget=NULL,
  title=NULL, slider_title="Index",
  fname=FALSE, fname_suffix=c("names", "idx"), legend_fname="[fname]_legend",
  legend_ncol=NULL, legend_alllevels=FALSE, legend_embed=NULL, 
  digits=NULL, scientific=NA,
  cex.title=NULL, text_color="black", bg=NULL,
  borders=FALSE, alpha=1.0, edge_color=NULL, vertex_color=NULL, vertex_size=0,
  width=NULL, height=NULL, zoom=NULL){

  view_xifti_surface(
    xifti=xifti, surfL=surfL, surfR=surfR,
    color_mode=color_mode, zlim=zlim, colors=colors,
    idx=idx, hemisphere=hemisphere,
    together=together, together_ncol=together_ncol, together_title=together_title,
    view=view, widget=widget,
    title=title, slider_title=slider_title,
    fname=fname, fname_suffix=fname_suffix, legend_fname=legend_fname,
    legend_ncol=legend_ncol, legend_alllevels=legend_alllevels, legend_embed=legend_embed, 
    digits=digits, scientific=scientific,
    cex.title=cex.title, text_color=text_color, bg=bg,
    borders=borders, alpha=alpha, edge_color=edge_color, vertex_color=vertex_color, vertex_size=vertex_size,
    width=width, height=height, zoom=zoom
  )
}
