#' Visualize "xifti" cortical data. The \code{rgl} package is required.
#'
#' @inheritParams xifti_Param
#' @param idx The time/column index of the xifti data to plot.
#'  Currently one a single time point is supported. Default: the first index.
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
#'  to these defaults is if the "xifti" represents a .dlabel CIFTI (intent 3007),
#'  then the qualitative colors in the label table will be used.
#'  See the \code{ciftiTools::make_color_pal()} description for more details.
#' @param color_mode (Optional) \code{"sequential"}, \code{"qualitative"},
#'  or \code{"diverging"}. \code{NULL} will use the qualitative color mode if
#'  the "xifti" represents a .dlabel CIFTI (intent 3007), and the sequential
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
view_xifti_surface <- function(xifti, idx=1,
  hemisphere=NULL, view=c("both", "lateral", "medial"),
  mode=c("widget", "image", "video"), width=NULL, height=NULL, zoom=.6,
  bg=NULL, title=NULL, cex.title=NULL, text_color="black",
  fname="xifti", write_dir=NULL,
  colors=NULL, color_mode=NULL, zlim=NULL,
  surfL=NULL, surfR=NULL,
  colorbar_embedded=TRUE, colorbar_digits=NULL,
  alpha=1.0,
  edge_color=NULL, vertex_color=NULL, vertex_size=0) {

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
  while (!is.null(dev.list()))  dev.off()

  # ----------------------------------------------------------------------------
  # Check arguments ------------------------------------------------------------
  # ----------------------------------------------------------------------------

  # Check xifti, idx, and surfaces.
  stopifnot(is.xifti(xifti))
  if (length(idx) > 1) stop("Only one time/column index is supported right now.")

  if (!is.null(surfL)) {
    surfL <- make_surf(surfL, "left")
  } else if (!is.null(xifti$surf$cortex_left)) {
    surfL <- make_surf(xifti$surf$cortex_left, "right")
  }

  if (!is.null(surfR)) {
    surfR <- make_surf(surfR, "right")
  } else if (!is.null(xifti$surf$cortex_right)) {
    surfR <- make_surf(xifti$surf$cortex_right, "right")
  }

  if (!is.null(hemisphere)) {
    if (length(hemisphere)==2) {
      if (all(hemisphere %in% c("left", "right"))) {
        hemisphere = "both"
      } else {
        stop("`hemisphere` should be \"left\", \"right\", or \"both\".")
      }
    }
    if (hemisphere %in% c("both", "left")) {
      if (is.null(surfL)) {
        surfL <- make_surf(get_example_files()$surf["left"], "left")
      }
    }
    if (hemisphere %in% c("both", "right")) {
      if (is.null(surfR)) {
        surfR <- make_surf(get_example_files()$surf["right"], "right")
      }
    }
  } else {
    if (is.null(surfL) && is.null(surfR)) {
      if (is.null(xifti$data$cortex_left) && is.null(xifti$data$cortex_right)) {
        stop("No cortical data nor surfaces were provided.")
      } 
      if (!is.null(xifti$data$cortex_left)) {
        surfL <- make_surf(get_example_files()$surf["left"], "left")
      }
      if (!is.null(xifti$data$cortex_right)) {
        surfR <- make_surf(get_example_files()$surf["right"], "right")
      }
    } else if (is.null(surfL)) {
      if (!is.null(xifti$data$cortex_left)) {
        warning(paste(
          "Using default surface for left hemisphere, which may not match",
          "provided surface for right hemisphere. To avoid this problem, set",
          "`hemisphere=\"right\"` to only plot the right hemisphere or",
          "provide the matching left hemisphere."
        ))
        surfL <- make_surf(get_example_files()$surf["left"], "left")
      }
    } else if (is.null(surfR)) {
      if (!is.null(xifti$data$cortex_right)) {
        warning(paste(
          "Using default surface for right hemisphere, which may not match",
          "provided surface for right hemisphere. To avoid this problem, set",
          "`hemisphere=\"left\"` to only plot the left hemisphere or",
          "provide the matching right hemisphere."
        ))
        surfR <- make_surf(get_example_files()$surf["right"], "right")
      }
    }
  }

  if (!is.null(surfL) && !is.null(surfL$hemisphere)) {
    if (surfL$hemisphere == "right") { 
      warning("The left surface actually corresponds to the right cortex.\n") 
    }
  }
  if (!is.null(surfR) && !is.null(surfR$hemisphere)) {
    if (surfR$hemisphere == "left") { 
      warning("The right surface actually corresponds to the left cortex.\n") 
    }
  }

  # Check hemisphere and view.
  if (is.null(hemisphere)) {
    hemisphere <- c("left", "right", "both")[1*(!is.null(surfL)) + 2*(!is.null(surfR))]
  }
  if (hemisphere=="both") { hemisphere=c("left", "right") } # reformat

  view <- match.arg(view, c("both", "lateral", "medial"))
  if (view=="both") { view=c("lateral", "medial") } # reformat
  
  brain_panels_nrow <- length(view)
  brain_panels_ncol <- length(hemisphere)

  if (is.null(title)) {
    no_title = FALSE
  } else {
    no_title = title == ""
  }
  all_panels_nrow <- brain_panels_nrow + 1*(!no_title) + 1*colorbar_embedded
  all_panels_ncol <- brain_panels_ncol

  # Check other arguments.
  mode <- match.arg(mode, c("widget", "image", "video"))
  if (mode == "video") { stop("The video mode is not yet supported.") }

  if (mode=="image") {
    if (!endsWith(fname, ".png")) { fname <- paste0(fname, ".png") }
    img_fname <- format_path(fname, write_dir, mode=2)
  }

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

  # ----------------------------------------------------------------------------
  # Get the data values and surface models, and construct the mesh. ------------
  # ----------------------------------------------------------------------------

  to_rgl <- list(
    left <-  list(mesh=NULL, values=NULL, colors=NULL),
    right <- list(mesh=NULL, values=NULL, colors=NULL)
  )

  for (h in hemisphere) {
    surf_h <- switch(h, left=surfL, right=surfR)
    mwall_h <- xifti$meta$cortex$medial_wall_mask[[h]]
    cor_h <- switch(h, left="cortex_left", right="cortex_right")
    val_h <- xifti$data[[cor_h]]

    # Check for surface data.
    if (is.null(surf_h)) {
      stop(paste0(
        "The ",h," hemisphere was requested, but no surface data ",
        "(xifti$surf",cor_h,") or the ", switch(h, "surfL", "surfR"), 
        " argument to view_xifti) was provided."
      ))
    }

    if (is.null(mwall_h)) {
      if (is.null(val_h)) {
        mwall_h <- rep(TRUE, nrow(surf_h$vertices))
      } else {
        mwall_h <- rep(TRUE, nrow(val_h))
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
      }
    }

    # Get data values.
    to_rgl[[h]]$values <- matrix(
      NA, ncol=length(idx), nrow=nrow(surf_h$vertices)
    )
    if (!is.null(val_h)) {
      to_rgl[[h]]$values[mwall_h,] <- val_h[,idx, drop=FALSE]
    }

    ## Construct the mesh.
    vertices_h <- t(cbind(
      surf_h$vertices, 
      ### Add homogenous coordinates
      rep(1, nrow(surf_h$vertices))
    ))
    faces_h <- t(surf_h$faces)
    to_rgl[[h]]$mesh <- rgl::tmesh3d(
      vertices_h, faces_h, meshColor = "vertices"
    )
    ## Add normals for smooth coloring.
    to_rgl[[h]]$mesh <- rgl::addNormals(to_rgl[[h]]$mesh)
  }

  values <- c(to_rgl$left$values, to_rgl$right$values)
  if (all(is.na(values))) { values <- NULL }

  if (!is.null(values)) {

    # ----------------------------------------------------------------------------
    # Assign colors to vertices based on intensity. ------------------------------
    # ----------------------------------------------------------------------------

    # Get the base palette.
    if (color_mode=="qualitative") {
      # For .dlabel files, use the included labels metadata colors.
      if (!is.null(xifti$meta$cifti$intent) && xifti$meta$cifti$intent==3007) {
        labs <- xifti$meta$cifti$labels[[idx]]
        N_VALUES <- length(labs$Key)
        pal_base <- data.frame(
          color = rgb(labs$Red, labs$Green, labs$Blue, labs$Alpha),
          value = labs$Key
        )
      # Otherwise, use the usual colors.
      } else {
        values <- as.numeric(factor(values, levels=sort(unique(values[!is.na(values)]))))
        N_VALUES <- length(unique(values[!is.na(values)]))
        pal_base <- make_color_pal(
          colors=colors, color_mode=color_mode, zlim=zlim,
          DATA_MIN=1, DATA_MAX=N_VALUES
        )
      }
    } else {
      pal_base <- make_color_pal(
        colors=colors, color_mode=color_mode, zlim=zlim,
        DATA_MIN=min(values, na.rm=TRUE), DATA_MAX=max(values, na.rm=TRUE)
      )
    }

    # Interpolate colors in the base palette for higher color resolution.
    if (color_mode %in% c("sequential", "diverging")) {
      pal <- expand_color_pal(pal_base)
    } else {
      pal <- pal_base
    }

    # Map each vertex to a color by its value.
    if (nrow(pal)==1) {
      cols <- ifelse(is.na(values), "white", pal$color[1])
    } else {
      cols <- use_color_pal(values, pal) # color_NA?
    }
    if (length(hemisphere)==2) {
      nvoxL <- length(to_rgl$left$values)
      to_rgl$left$colors <- cols[1:nvoxL]
      to_rgl$right$colors <- cols[(nvoxL+1):length(cols)]
    } else if (hemisphere=="left") {
      to_rgl$left$colors <- cols
    } else if (hemisphere=="right") {
      to_rgl$right$colors <- cols
    }
    rm(cols)

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
    to_rgl$left$colors <- "white"
    to_rgl$right$colors <- "white"
  }

  # ----------------------------------------------------------------------------
  # Color and arrange the meshes according to the layout. ----------------------
  # ----------------------------------------------------------------------------

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
        title <- ""
      } else if (intent == 3002) {
        title <- paste("Index", idx)
        if (!any(sapply(xifti$meta$cifti[c("time_start", "time_step", "time_unit")], is.null))) {
          title <- paste0(title, " (", xifti$meta$cifti$time_start+xifti$meta$cifti$time_step*idx, " ", xifti$meta$cifti$time_unit, "s)")
        }
      } else if (intent == 3006) {
        if (!is.null(xifti$meta$cifti$names) && length(xifti$meta$cifti$names)>=idx) {
          title <- xifti$meta$cifti$names[idx]
        } else {
          title <- ""
        }
      } else if (intent == 3007) {
        if (!is.null(xifti$meta$cifti$labels) && length(xifti$meta$cifti$labels)>=idx) {
          title <- names(xifti$meta$cifti$labels)[idx]
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
    rgl::text3d(x=0, y=0, z=0, #These values don't seem to do anything...
                cex=cex.title,
                adj=c(.5,.5), #replace with adj(c(0, .5)) when coords are moved
                font=2, # Forget if this made a difference...
                color=text_color,
                text=title
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

    # Draw the mesh.
    rgl::shade3d(
      to_rgl[[h]]$mesh, 
      color=to_rgl[[h]]$colors, 
      specular="black", 
      alpha=alpha,
      legend=TRUE
    )

    ## Vertices.
    if (vertex_size > 0) { 
      rgl::shade3d(
        to_rgl[[h]]$mesh, 
        color=vertex_color, size=vertex_size,
        specular="black",
        front="points", back="points", 
        legend=FALSE
      )
    }

    ## Edges.
    if (!is.null(edge_color)) {
      rgl::shade3d(
        to_rgl[[h]]$mesh, 
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

  if (!is.null(values)) {
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

  if (mode=="image") {
    rgl::rgl.snapshot(img_fname)
    rgl::rgl.close()
    return(img_fname)
  } else {
    return(invisible())
  }
}

#' Visualize xifti brain data
#'
#' @inheritParams xifti_Param
#' @param structural_img The structural MRI image on which to overlay the
#'  subcortical values. Can be a file name, \code{"MNI"} (default) to use
#'  the MNI T1-weighted template, or \code{NULL} to use a blank image.
#' @param idx The time/column index of the xifti data to plot.
#' @param plane If use_papaya=FALSE, the plane to display.
#'  Default: \code{"axial"}. Other options are \code{"sagittal"} and 
#'  \code{"coronal"}.
#' @param num.slices If use_papaya=FALSE, the number of slices to display.
#'  Default: \code{9}.
#' @param use_papaya use_papaya=TRUE will use papayar to allows for interactive visualization.
#' @param z_min Floor value.
#' @param z_max Ceiling value.
#' @inheritParams verbose_Param_TRUE
#' @param ... Additional arguments to pass to \code{papayar::papaya} or \code{oro.nifti::overlay}
#'
#' @export
#' @importFrom oro.nifti overlay readNIfTI as.nifti
view_xifti_volume <- function(
  xifti, structural_img="MNI", idx=1, plane=c("axial", "sagittal", "coronal"),
  num.slices=9, use_papaya=FALSE, z_min=NULL, z_max=NULL,
  verbose=TRUE, ...) {

  if (use_papaya) {
    if (!requireNamespace("papayar", quietly = TRUE)) {
      stop("Package \"papayar\" needed for this function to work. Please install it.",
           call. = FALSE)
    }
  }

  #stop("Does not work.")

  stopifnot(is.xifti(xifti))

  # Get volume and labels.
  values <- xifti$data$subcort[,idx]
  vol <- unmask_vol(values, xifti$meta$subcort$mask, fill=NA)
  labs <- unmask_vol(as.numeric(xifti$meta$subcort$labels), xifti$meta$subcort$mask, fill=0)

  # Pick slices with a lot of subcortical voxels.
  if (!use_papaya) {
    plane <- match.arg(plane, c("axial", "sagittal", "coronal"))
    if (plane=="axial") mask_count <- apply(xifti$meta$subcort$mask, 3, sum)
    if (plane=="coronal") mask_count <- apply(xifti$meta$subcort$mask, 2, sum)
    if (plane=="sagittal") mask_count <- apply(xifti$meta$subcort$mask, 1, sum)

    slices <- which(mask_count > max(mask_count)/2)
    inds <- round(seq(1,length(slices), length.out=num.slices))
    slices <- slices[inds]
  }

  if (!is.null(z_min)) values[values < z_min] <- z_min
  if (!is.null(z_max)) values[values > z_max] <- z_max
  if (verbose) {
    cat(paste0(
      "Values to be plotted range from ",
      min(xifti$data$subcort[,idx])," to ",
      max(xifti$data$subcort[,idx]), ".\n"
    ))
  }

  if (!is.null(structural_img)) {
    if (structural_img=="MNI") {
      img_fname <- system.file("extdata/MNI152_T1_2mm.nii.gz", package="ciftiTools")
    } else if (is.fname(structural_img)){
      img_fname <- structural_img
    } else {
      stop(paste(
        "`structural_img` argument not one of:",
        "`NULL`, `\"MNI\"`, or an existing file."
      ))
    }
    img <- readNIfTI(img_fname, reorient=FALSE)

    # Check data dimensions.
    if (!all.equal(dim(img), dim(vol))) {
      stop(paste0(
        "The subcortical data in the CIFTI and the `structural_img` are of ",
        "different dimensions: (", paste(dim(img), collapse=", "), ") and (",
        paste(dim(vol), collapse=", "), ") respectively."
      ))
    }

    # Check data orientation alignment.
    # This uses the sform method (srow_x, srow_y, and srow_z), not qform
    #   or ANALYZE-based methods.
    # This is because the Connectime Workbench seems to export the 
    #   TransformationMatrixIJKtoXYZ as the sform transformation matrix
    #   in -cifti-separate. 
    img_trans_mat <- make_trans_mat(img_fname)
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

    img_overlay <- img*0
    img_labels <- img*0
  } else {
    img <- oro.nifti::as.nifti(vol*0)
    img@.Data <- xifti$meta$subcort$mask
    img_overlay <- img_labels <- img
  }

  img_overlay@.Data <- vol
  img_overlay@.Data[labs==0] <- NA
  # Patch: if img_overlay@.Data is int, an error occurs.
  img_overlay@.Data <- img_overlay@.Data * 1.0
  img_labels@.Data <- labs
  img_labels@.Data[labs==0] <- NA

  if (!use_papaya) {
    if (plane=="axial") {
      img <- img[,,slices]
      img_overlay <- img_overlay[,,slices]
    } else if (plane=="coronal") {
      img <- img[,slices,]
      img_overlay <- img_overlay[,slices,]
    } else if (plane=="sagittal") {
      img <- img[slices,,]
      img_overlay <- img_overlay[slices,,]
    }
    oro.nifti::overlay(x=img, y=img_overlay, plane=plane, ...)
  } else {
    papayar::papaya(list(img, img_overlay, img_labels), ...)
  }
}

#' Switch for \code{\link{view_xifti_surface}} or \code{\link{view_xifti_volume}}
#'
#' @inheritParams xifti_Param
#' @param what Either "surface" or "volume". If NULL (default), view the surface if present in the xifti file, and
#'  volume otherwise
#' @param ... Additional arguments to pass to either view function.
#'
#' @return The return value of \code{view_xifti_surface} or
#'  \code{view_xifti_volume}.
#'
#' @export
#'
view_xifti <- function(xifti, what=NULL, ...) {
  stopifnot(is.xifti(xifti))
  if (is.null(what)) {
    can_do_left <- !is.null(xifti$data$cortex_left)
    can_do_right <- !is.null(xifti$data$cortex_right)
    can_do_sub <- !is.null(xifti$data$subcort)
    what <- ifelse(
      can_do_left || can_do_right,
      "surface",
      ifelse(can_do_sub, "volume", "error")
    )
  }
  if (what == "surface") {
    return(view_xifti_surface(xifti, ...))
  } else if (what == "volume") {
    return(view_xifti_volume(xifti, ...))
  } else {
    stop(paste(
      "No valid cortical surface, and no valid subcortical data.",
      "Did you forget to provide surfL/surfR?",
      "Or, did you forget to read in the subcortical data too?"
    ))
  }
}

#' S3 method: use view_xifti to plot a xifti
#'
#' @inheritParams x_Param_xifti
#' @param ... Additional arguments to \code{\link{view_xifti}}, except
#'  \code{what}, which will be set to \code{NULL}.
#'
#' @method plot xifti
#' @export
plot.xifti <- function(x, ...){
  view_xifti(x, what=NULL, ...)
}

#' @rdname view_xifti
#' @export
view_cifti <- function(xifti, what=NULL, ...){
  view_xifti(xifti, what=what, ...)
}

#' @rdname view_xifti
#' @export
viewCIfTI <- function(xifti, what=NULL, ...){
  view_xifti(xifti, what=what, ...)
}

#' @rdname view_xifti
#' @export
viewcii <- function(xifti, what=NULL, ...){
  view_xifti(xifti, what=what, ...)
}

#' @rdname view_xifti_surface
#' @export
view_cifti_surface <- function(xifti, idx=1,
  hemisphere=NULL, view=c("both", "lateral", "medial"),
  mode=c("widget", "image", "video"), width=NULL, height=NULL, zoom=.6,
  bg=NULL, title=NULL, cex.title=NULL, text_color="black",
  fname="xifti", write_dir=NULL,
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
    fname, write_dir,
    colors, color_mode, zlim,
    surfL, surfR,
    colorbar_embedded,
    colorbar_digits, alpha,
    edge_color, vertex_color, vertex_size
  )
}

#' @rdname view_xifti_surface
#' @export
viewCIfTI_surface <- function(xifti, idx=1,
  hemisphere=NULL, view=c("both", "lateral", "medial"),
  mode=c("widget", "image", "video"), width=NULL, height=NULL, zoom=.6,
  bg=NULL, title=NULL, cex.title=NULL, text_color="black",
  fname="xifti", write_dir=NULL,
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
    fname, write_dir,
    colors, color_mode, zlim,
    surfL, surfR,
    colorbar_embedded,
    colorbar_digits, alpha,
    edge_color, vertex_color, vertex_size
  )
}

#' @rdname view_xifti_surface
#' @export
viewcii_surface <- function(xifti, idx=1,
  hemisphere=NULL, view=c("both", "lateral", "medial"),
  mode=c("widget", "image", "video"), width=NULL, height=NULL, zoom=.6,
  bg=NULL, title=NULL, cex.title=NULL, text_color="black",
  fname="xifti", write_dir=NULL,
  colors=NULL, color_mode=NULL, zlim=NULL,
  surfL=NULL, surfR=NULL,
  colorbar_embedded=TRUE, colorbar_digits=NULL,
  alpha=1.0,
  edge_color=NULL, vertex_color=NULL, vertex_size=0){

  viewcii_surface(
    xifti, idx,
    hemisphere, view,
    mode, width, height, zoom,
    bg, title, cex.title, text_color,
    fname, write_dir,
    colors, color_mode, zlim,
    surfL, surfR,
    colorbar_embedded,
    colorbar_digits, alpha,
    edge_color, vertex_color, vertex_size
  )
}

#' @rdname view_xifti_volume
#' @export
view_cifti_volume <- function(
  xifti, structural_img="MNI", idx=1, plane=c("axial", "sagittal", "coronal"),
  num.slices=9, use_papaya=FALSE, z_min=NULL, z_max=NULL,
  verbose=TRUE, ...) {

  view_xifti_volume(
    xifti=xifti,
    structural_img=structural_img,
    idx=idx, plane=plane,
    num.slices=num.slices,
    use_papaya=use_papaya,
    z_min=z_min, z_max=z_max,
    verbose=verbose, ...
  )
}

#' @rdname view_xifti_volume
#' @export
viewCIfTI_volume <- function(
  xifti, structural_img="MNI", idx=1, plane=c("axial", "sagittal", "coronal"),
  num.slices=9, use_papaya=FALSE, z_min=NULL, z_max=NULL,
  verbose=TRUE, ...) {

  view_xifti_volume(
    xifti=xifti,
    structural_img=structural_img,
    idx=idx, plane=plane,
    num.slices=num.slices,
    use_papaya=use_papaya,
    z_min=z_min, z_max=z_max,
    verbose=verbose, ...
  )
}

#' @rdname view_xifti_volume
#' @export
viewcii_volume <- function(
  xifti, structural_img="MNI", idx=1, plane=c("axial", "sagittal", "coronal"),
  num.slices=9, use_papaya=FALSE, z_min=NULL, z_max=NULL,
  verbose=TRUE, ...) {

  view_xifti_volume(
    xifti=xifti,
    structural_img=structural_img,
    idx=idx, plane=plane,
    num.slices=num.slices,
    use_papaya=use_papaya,
    z_min=z_min, z_max=z_max,
    verbose=verbose, ...
  )
}