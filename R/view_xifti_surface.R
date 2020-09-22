
#' Sort out surface & hemispehre args for \code{view_xifti_surface}
#' 
#' See \code{\link{view_xifti_surface}} for details.
#' 
#' @param xifti The xifti
#' @param surfL Left surface
#' @param surfR Right surface
#' @param hemisphere Hemisphere
#' 
#' @return A list with entries "surfL", "surfR" and "hemisphere"
#' 
#' @keywords internal
#' 
view_xifti_surface.args_surf_hemi <- function(
  xifti, surfL, surfR, hemisphere
){

  # Make hemisphere one of: "left", "right", or c("left", "right")
  if (!is.null(hemisphere)) {
    hemisphere <- unique(sapply(hemisphere, match.arg, c("left", "right", "both")))
    if ("both" %in% hemisphere) { hemisphere <- c("left", "right") } 
  }

  # Prioritize the surface argument, the surface in the xifti, and the example
  #   surface in that order. Only use the example surface if that cortex data
  #   is present, or if it is indicated in "hemisphere".
  use_example_surf <- c("left"=FALSE, "right"=FALSE)
  for (this_h in c("left", "right")) {
    surf <- switch(this_h, left=surfL, right=surfR)
    surf2 <- switch(this_h, left=xifti$surf$cortex_left, right=xifti$surf$cortex_right)
    surf3 <- get_example_files()$surf[this_h]
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

  list(
    surfL=surfL,
    surfR=surfR,
    hemisphere=hemisphere
  )
}

#' View cortical surface
#' 
#' Visualize \code{"xifti"} cortical data. The \code{rgl} and \code{fields} 
#'  packages are required.
#'
#' @inheritParams xifti_Param
#' @param idx The time/column index of the xifti data to plot.
#' 
#'  If \code{mode} is \code{"widget"} or \code{"image"}, this should be a single
#'  number. If \code{is.null(idx)}, the first column will be used.
#' 
#'  If \code{mode} is \code{"video"}, this should be a vector of numbers. If
#'  \code{is.null(idx)}, all columns will be used. 
#' @param hemisphere Which brain cortex to display: "both", "left", or "right".
#' 
#'  If a brain cortex is requested but no surface is available, a default
#'  inflated surface will be used.
#' 
#'  This argument can also be null \code{NULL} (default). In this case, a
#'  default inflated surface will be used for each cortex with available data 
#'  (i.e. if \code{xifti$data$cortex_left} and/or \code{xifti$data$cortex_right} 
#'  exist).
#' 
#'  Surfaces without data will still be displayed, colored white.
#'  
#'  Each cortex will be shown in a separate panel column within the RGL window.
#' @param colors (Optional) "ROY_BIG_BL", vector of colors to use,
#'  OR the name of a ColorBrewer palette (see RColorBrewer::brewer.pal.info
#'  and colorbrewer2.org). Defaults are \code{"ROY_BIG_BL"} (sequential),
#'  \code{"Set2"} (qualitative), and \code{"ROY_BIG_BL"} (diverging). An exception
#'  to these defaults is if the \code{"xifti"} object represents a .dlabel CIFTI (intent 3007),
#'  then the qualitative colors in the label table will be used.
#'  See the \code{ciftiTools::make_color_pal()} description for more details.
#' @param color_mode (Optional) \code{"sequential"}, \code{"qualitative"},
#'  or \code{"diverging"}. \code{NULL} will use the qualitative color mode if
#'  the \code{"xifti"} object represents a .dlabel CIFTI (intent 3007), and the sequential
#'  color mode otherwise. See the \code{ciftiTools::make_color_pal()}
#'  description for more details.
#' @param zlim (Optional) Controls the mapping of values to each
#'  color in \code{colors}. If the length is longer than
#'  one, using -Inf will set the value to \code{DATA_MIN}, and Inf will set
#'  the value to \code{DATA_MAX}. See the
#'  \code{ciftiTools::make_color_pal()} description for more details.
#' @param surfL,surfR (Optional if \code{xifti$surf$cortex_left} and
#'  \code{xifti$surf$cortex_right} are not empty) The brain surface model to use.
#'  Each can be a file path for a GIFTI, a file read by gifti::readgii,
#'  or a list with components "vertices" and "faces". If provided, they will override
#'  \code{xifti$surf$cortex_left} and \code{xifti$surf$cortex_right} if they exist.
#'  Otherwise, leave these arguments as \code{NULL} (default) to use
#'  \code{xifti$surf$cortex_left} and \code{xifti$surf$cortex_right}.
#' @param colorbar_embedded Should the colorbar be embedded in the RGL window?
#'  It will be positioned in the bottom-left corner, in a separate subplot
#'  with 1/4 the height of the brain cortex subplots. Default: \code{TRUE}.
#'  If \code{FALSE}, print it separately instead.
#' @param colorbar_digits The number of digits for the colorbar legend ticks.
#'  If \code{NULL} (default), let \code{\link{format}} decide.
#' @inheritParams surface_plot_Params
#'
#' @export
#' @importFrom grDevices dev.list dev.off rgb
view_xifti_surface <- function(xifti, idx=NULL,
  hemisphere=NULL, view=c("both", "lateral", "medial"),
  mode=c("widget", "image", "video"), width=NULL, height=NULL, zoom=.6,
  bg=NULL, title=NULL, cex.title=NULL, text_color="black",
  fname="xifti",
  colors=NULL, color_mode=NULL, zlim=NULL,
  surfL=NULL, surfR=NULL,
  colorbar_embedded=TRUE, colorbar_digits=NULL,
  alpha=1.0,
  edge_color=NULL, vertex_color=NULL, vertex_size=0) {

  # Check required packages and X11
  if (!requireNamespace("rgl", quietly = TRUE)) {
    stop("Package \"rgl\" needed to use `view_xifti_surface`. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("fields", quietly = TRUE)) {
    stop("Package \"fields\" needed to use `view_xifti_surface`. Please install it.", call. = FALSE)
  }
  if (!capabilities("X11")) {
    ciftiTools_warn("X11 capability is needed to open the rgl window for `view_xifti_surface`.")
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
  x <- view_xifti_surface.args_surf_hemi(xifti, surfL, surfR, hemisphere)
  surfL <- x$surfL; surfR <- x$surfR; hemisphere <- x$hemisphere

  # Check `view`.
  view <- unique(sapply(view, match.arg, c("lateral", "medial", "both")))
  if ("both" %in% view) { view <- c("lateral", "medial") }

  # Check `mode` compatibility with `idx` and `fname`.
  mode <- match.arg(mode, c("widget", "image", "video"))

  if (mode == "video") {
    if (is.null(idx)) {
      idx <- 1:T_
    } else {
      stopifnot(all(idx %in% 1:T_))
    }
  } else {
    if (is.null(idx)) {
      idx <- 1
    } else {
      if (length(idx) > 1) stop("For widget and image modes, only one time/column index is supported right now.")
    }
  }

  if (mode=="image") {
    fname <- as.character(fname[1])
    if (!endsWith(fname, ".png")) { fname <- paste0(fname, ".png") }
  } else if (mode=="video") {
    if (length(fname)==length(idx)) {
      fname <- as.character(fname)
    } else {
      fname <- gsub(".png", "", fname[1], fixed=TRUE)
      fname <- paste0(as.character(fname), "_", idx, ".png")
    }
  }

  print(fname)

  # Color mode
  if (is.null(color_mode)) {
    if (!is.null(xifti$meta$cifti$intent) && xifti$meta$cifti$intent==3007) {
      color_mode <- "qualitative"
    } else {
      color_mode <- "sequential"
    }
  } else {
    color_mode <- match.arg(color_mode, c("sequential", "qualitative", "diverging"))
  }

  # Check width and height.
  brain_panels_nrow <- length(view)
  brain_panels_ncol <- length(hemisphere)

  if (is.null(title)) {
    no_title = FALSE
  } else {
    no_title = title == ""
  }
  all_panels_nrow <- brain_panels_nrow + 1*(!no_title) + 1*colorbar_embedded
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


  NA_COLOR <- "white"

  # ----------------------------------------------------------------------------
  # Get the data values and surface models, and construct the mesh. ------------
  # ----------------------------------------------------------------------------

  mesh =  list(left=NULL, right=NULL)
  values = list(left=NULL, right=NULL)

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
    }

    # Get data values.
    values[[h]] <- matrix(NA, ncol=length(idx), nrow=length(mwall_h))
    if (!is.null(xifti$data[[cor_h]])) {
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

  # Put values together (to use data bounds across all measurements/columns)
  nvertL <- ifelse(is.null(values$left), 0, nrow(values$left))
  if (!is.null(values)) {
    values <- do.call(rbind, values)
    values[values == NaN] <- NA
    if (all(is.na(values))) { values <- NULL }
  }

  any_colors <- !is.null(values)

  if (any_colors) {
    # --------------------------------------------------------------------------
    # Assign colors to vertices based on intensity. ----------------------------
    # --------------------------------------------------------------------------

    # Get the base palette.
    if (color_mode=="qualitative") {
      # For .dlabel files, use the included labels metadata colors.
      if (!is.null(xifti$meta$cifti$intent) && xifti$meta$cifti$intent==3007) {
        labs <- xifti$meta$cifti$labels[[idx]]
        N_VALUES <- length(labs$Key)
        pal_base <- data.frame(
          color = grDevices::rgb(labs$Red, labs$Green, labs$Blue, labs$Alpha),
          value = labs$Key
        )
      # Otherwise, use the usual colors.
      } else {
        unique_values <- sort(unique(as.vector(values[!is.na(values)])))
        values[,] <- as.numeric(factor(values, levels=unique_values))
        N_VALUES <- length(unique_values)
        pal_base <- make_color_pal(
          colors=colors, color_mode=color_mode, zlim=zlim,
          DATA_MIN=1, DATA_MAX=N_VALUES
        )
      }
    } else {
      pal_base <- make_color_pal(
        colors=colors, color_mode=color_mode, zlim=zlim,
        DATA_MIN=min(as.vector(values), na.rm=TRUE), 
        DATA_MAX=max(as.vector(values), na.rm=TRUE)
      )
    }

    # Interpolate colors in the base palette for higher color resolution.
    if (color_mode %in% c("sequential", "diverging")) {
      pal <- expand_color_pal(pal_base)
    } else {
      pal <- pal_base
    }

    # Map each vertex to a color by its value.
    values[,] <- use_color_pal(as.vector(values), pal, indices=TRUE)

    # Put values back apart into left and right cortex
    if (length(hemisphere)==2) {
      values <- list(
        left = values[1:nvertL,, drop=FALSE],
        right = values[(nvertL+1):nrow(values),, drop=FALSE]
      )
    } else if (hemisphere=="left") {
      values <- list(
        left = values,
        right = NULL
      )
    } else if (hemisphere=="right") {
      values <- list(
        left = NULL,
        right = values
      )
    }

    # ----------------------------------------------------------------------------
    # Make the colorbar ----------------------------------------------------------
    # ----------------------------------------------------------------------------

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
      qualitative=1:N_VALUES,
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

  } else {
    # Will become NA_COLOR
    values <- list(left=matrix(0), right=matrix(0))
  }

  # ----------------------------------------------------------------------------
  # Color and arrange the meshes according to the layout. ----------------------
  # ----------------------------------------------------------------------------

  # Should only loop once unless mode=="video"

  for (jj in 1:length(idx)) {
    this_idx <- idx[jj]

    # Open a new RGL window.
    rgl::open3d()
    if (is.null(bg)) { bg <- "white" }
    rgl::bg3d(color=bg)
    rgl::par3d(windowRect = c(20, 20, all_panels_width, all_panels_height))
    Sys.sleep(1) #https://stackoverflow.com/questions/58546011/how-to-draw-to-the-full-window-in-rgl

    all_panels_heights <- rep.int(1, brain_panels_nrow)
    if (!no_title) {all_panels_heights <- c(TITLE_AND_LEGEND_HEIGHT_RATIO, all_panels_heights) }
    if (colorbar_embedded) {all_panels_heights <- c(all_panels_heights, TITLE_AND_LEGEND_HEIGHT_RATIO) }

    # Determine the panel layout.
    rgl::layout3d(
      matrix(1:(all_panels_ncol*all_panels_nrow), nrow=all_panels_nrow, byrow=T),
      widths=rep.int(1, all_panels_ncol),
      heights=all_panels_heights,
      parent = NA, sharedMouse = TRUE
    )
    brain_panels <- as.character(t(outer(view, hemisphere, paste0))) # by row
    n_brain_panels <- length(brain_panels)

    if (!no_title) {
      if (is.null(title)) {
        intent <- xifti$meta$cifti$intent
        if (is.null(intent)) {
          this_title <- ""
        } else if (intent == 3002) {
          this_title <- paste("Index", this_idx)
          if (!any(sapply(xifti$meta$cifti[c("time_start", "time_step", "time_unit")], is.null))) {
            this_title <- paste0(
              this_title, " (", 
              xifti$meta$cifti$time_start+xifti$meta$cifti$time_step*this_idx, 
              " ", xifti$meta$cifti$time_unit, "s)"
            )
          }
        } else if (intent == 3006) {
          if (!is.null(xifti$meta$cifti$names) && length(xifti$meta$cifti$names)>=this_idx) {
            this_title <- xifti$meta$cifti$names[this_idx]
          } else {
            this_title <- ""
          }
        } else if (intent == 3007) {
          if (!is.null(xifti$meta$cifti$labels) && length(xifti$meta$cifti$labels)>=this_idx) {
            this_title <- names(xifti$meta$cifti$labels)[this_idx]
          } else {
            this_title <- ""
          }
        }
      }
      if (is.null(cex.title)) {
        # Default: 200% font size, but increasingly smaller for longer titles
        if (nchar(this_title) > 20) {
          cex.title <- 40 / nchar(this_title)
        } else {
          cex.title <- 2
        }
      }
      rgl::text3d(x=0, y=0, z=0, #These values don't seem to do anything...
                  cex=cex.title,
                  adj=c(.5,.5), #replace with adj(c(0, .5)) when coords are moved
                  font=2, # Forget if this made a difference...
                  color=text_color,
                  text=this_title
      )
      rgl::next3d(current = NA, clear = FALSE, reuse = FALSE)

      if(all_panels_ncol==2){
        rgl::next3d(current = NA, clear = FALSE, reuse = FALSE)
      }
    }

    # Rotation matrices to orient meshes.
    rot <- list(
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

    # Populate the RGL window.
    for(ii in 1:n_brain_panels) {
      p <- brain_panels[ii]

      # Get the hemisphere.
      if (grepl("left", p)) {
        h <- "left"; h2 <- "right"
      } else if (grepl("right", p)) {
        h <- "right"; h2 <- "left"
      } else {
        h <- "neither"; h2 <- "neither"
      }

      # Get the rotation.
      if (grepl("lateral", p)) {
        this_rot <- rot[[h]]
      } else if (grepl("medial", p)) {
        this_rot <- rot[[h2]]
      } else { this_rot <- rot$ID }

      # Get the color.
      if (any_colors) {
        this_vals <- values[[h]][,this_idx]
        color_jj <- c(NA_COLOR, as.character(pal$color))[this_vals + 1]
      } else {
        color_jj <- NA_COLOR
      }

      # Draw the mesh.
      rgl::shade3d(
        mesh[[h]], 
        color=color_jj, 
        specular="black", 
        alpha=alpha,
        legend=TRUE
      )

      ## Vertices.
      if (vertex_size > 0) { 
        rgl::shade3d(
          mesh[[h]], 
          color=vertex_color, size=vertex_size,
          specular="black",
          front="points", back="points", 
          legend=FALSE
        )
      }

      ## Edges.
      if (!is.null(edge_color)) {
        rgl::shade3d(
          mesh[[h]], 
          color=edge_color, 
          specular="black",
          front="lines", back="lines", 
          legend=FALSE
        )
      }

      ## shift brains to left to make room for legend on right
      #displacement <- .25 * diff(range(this_surf$vertices[,2]))
      #if (grepl("lateral", p)) { displacement <- -displacement }
      #if (grepl("left", p)) { displacement <- -displacement }
      #this_trans <- t(rgl::translationMatrix(0, displacement, 0))
      this_trans <- diag(4)

      this_mat <- this_rot %*% this_trans
      rgl::rgl.viewpoint(userMatrix=this_mat, fov=0, zoom=zoom) #Default: 167% size
      rgl::next3d(current = NA, clear = FALSE, reuse = FALSE)
    }

    if (any_colors) {
      if (colorbar_embedded) {
        rgl::bgplot3d(
          # Warning: calling par(new=TRUE) with no plot
          # Error in par(old.par) :
          #   invalid value specified for graphical parameter "pin"
          try(suppressWarnings(do.call(fields::image.plot, colorbar_kwargs)), silent=TRUE),
          bg.color=bg
        )
      } else {
        colorbar_kwargs$smallplot=c(.15, .85, .45, .6) # x1 x2 y1 y2
        try(suppressWarnings(do.call(fields::image.plot, colorbar_kwargs)), silent=TRUE)
      }
    }

    rgl::next3d(current = NA, clear = FALSE, reuse = FALSE)
    if(all_panels_ncol==2){
      rgl::next3d(current = NA, clear = FALSE, reuse = FALSE)
    }

    if (mode %in% c("image", "video")) {
      rgl::rgl.snapshot(fname[jj])
      rgl::rgl.close()
    }
  }

  return(invisible(fname))
}

#' @rdname view_xifti_surface
#' @export
view_cifti_surface <- function(xifti, idx=NULL,
  hemisphere=NULL, view=c("both", "lateral", "medial"),
  mode=c("widget", "image", "video"), width=NULL, height=NULL, zoom=.6,
  bg=NULL, title=NULL, cex.title=NULL, text_color="black",
  fname="xifti",
  colors=NULL, color_mode=NULL, zlim=NULL,
  surfL=NULL, surfR=NULL,
  colorbar_embedded=TRUE, colorbar_digits=NULL,
  alpha=1.0,
  edge_color=NULL, vertex_color=NULL, vertex_size=0){

  view_xifti_surface(
    xifti, idx,
    hemisphere, view,
    mode, width, height, zoom,
    bg, title, cex.title, text_color,
    fname,
    colors, color_mode, zlim,
    surfL, surfR,
    colorbar_embedded,
    colorbar_digits, alpha,
    edge_color, vertex_color, vertex_size
  )
}

#' @rdname view_xifti_surface
#' @export
viewCIfTI_surface <- function(xifti, idx=NULL,
  hemisphere=NULL, view=c("both", "lateral", "medial"),
  mode=c("widget", "image", "video"), width=NULL, height=NULL, zoom=.6,
  bg=NULL, title=NULL, cex.title=NULL, text_color="black",
  fname="xifti",
  colors=NULL, color_mode=NULL, zlim=NULL,
  surfL=NULL, surfR=NULL,
  colorbar_embedded=TRUE, colorbar_digits=NULL,
  alpha=1.0,
  edge_color=NULL, vertex_color=NULL, vertex_size=0){

  view_xifti_surface(
    xifti=xifti, idx=idx,
    hemisphere=hemisphere, view=view,
    mode=mode, width=width, height=height, zoom=zoom,
    bg=bg, title=title, cex.title=cex.title, text_color=text_color,
    fname=fname,
    colors=colors, color_mode=color_mode, zlim=zlim,
    surfL=surfL, surfR=surfR,
    colorbar_embedded=colorbar_embedded,
    colorbar_digits=colorbar_digits, alpha=alpha,
    edge_color=edge_color, vertex_color=vertex_color, vertex_size=vertex_size
  )
}

#' @rdname view_xifti_surface
#' @export
viewcii_surface <- function(xifti, idx=NULL,
  hemisphere=NULL, view=c("both", "lateral", "medial"),
  mode=c("widget", "image", "video"), width=NULL, height=NULL, zoom=.6,
  bg=NULL, title=NULL, cex.title=NULL, text_color="black",
  fname="xifti",
  colors=NULL, color_mode=NULL, zlim=NULL,
  surfL=NULL, surfR=NULL,
  colorbar_embedded=TRUE, colorbar_digits=NULL,
  alpha=1.0,
  edge_color=NULL, vertex_color=NULL, vertex_size=0){

  view_xifti_surface(
    xifti=xifti, idx=idx,
    hemisphere=hemisphere, view=view,
    mode=mode, width=width, height=height, zoom=zoom,
    bg=bg, title=title, cex.title=cex.title, text_color=text_color,
    fname=fname,
    colors=colors, color_mode=color_mode, zlim=zlim,
    surfL=surfL, surfR=surfR,
    colorbar_embedded=colorbar_embedded,
    colorbar_digits=colorbar_digits, alpha=alpha,
    edge_color=edge_color, vertex_color=vertex_color, vertex_size=vertex_size
  )
}