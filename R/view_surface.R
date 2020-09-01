#' Visualize a "surface" object
#'
#' @param surf The surface
#' @param hemisphere "left" or "right". Used to orient the cortical surface toward the
#'  viewer. If \code{NULL} (default), do not rotate the mesh.
#' @param mode One of "widget" (Default) or "image":
#' 
#'  "widget" will open an interactive RGL window. Left click and drag to rotate.
#'  Use the scroll wheel to zoom. Run the R function 
#'  \code{rgl::snapshot("my_file.png")} to save the RGL window as a png. 
#'  See \code{\link[rgl]{snapshot}} for more information.
#'  
#'  "image" will open the RGL window, take a screenshot using
#'  \code{\link[rgl]{snapshot}}, and close it. The screenshot will be saved
#'  as a png in \code{write_dir} and its name will be \code{fname}.
#' @param width,height The dimensions of the RGL window, in pixels. If both are 
#'  \code{NULL} (default), the dimensions will be set to 
#'  1000 (width) x 700 (height) for 1x1 and 2x2 subplots,
#'  1500 x 525 for 2x1 subplots, and
#'  500 x 700 for 1x2 subplots. These defaults are chosen to fit comfortably
#'  within a 1600 x 900 screen. Specyfing only one will set the other to maintain
#'  the same aspect ratio. Both could be specified to set the dimensions exactly.
#' @param col The color of the brain mesh. Default: \code{"white"}
#' @param zoom Adjustment to size of brain meshes. Default: \code{3/5} 
#'  (100\% + 3/5*100\% = 160\% the original size). If \code{hemisphere} is not specified,
#'  this number is multiplied by 1.5 to account for the mesh image being vertically
#'  tall (the view is the top of the brain).
#' @param bg Background color. \code{NULL} will not color the background (white).
#' @param title Optional title for the plot. Set to an empty string \code{""}
#'  (default) to omit the title. If the title is non-empty but does not
#'  appear, \code{cex.title} may need to be lowered.
#' @param cex.title Font size multiplier for the title. \code{NULL} (default)
#'  will use \code{2.5} for titles less than 20 characters long, and smaller
#'  sizes for increasingly longer titles.
#' @param text_color Color for text in title and colorbar legend. Default: 
#'  "black".
#' @param fname An identifier to use for naming the saved images 
#'  ("[fname].png") and video frames ("[fname]_1.png", "[fname]_2.png", ...).
#'  Default: "surface". 
#' @param write_dir Where should any output images be written. NULL (default) 
#'  will write them to the current working directory. 
#' 
#'  \code{write_dir} must already exist, or an error will occur.
#'
#' @export
#' @importFrom grDevices dev.list dev.off
view_surf <- function(surf, hemisphere=NULL, mode=c("widget", "image"),
  width=NULL, height=NULL, col="white", zoom=.6,
  bg=NULL, title="", cex.title=NULL, text_color="black",
  fname="surface", write_dir=NULL) {

  if (!requireNamespace("rgl", quietly = TRUE)) {
    stop("Package \"rgl\" needed to use `view_xifti_surface`. Please install it.", call. = FALSE)
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
  # [TO DO]: Check input surface matches the cortex!
  stopifnot(is.surf(surf))
  brain_panels_nrow <- 1
  brain_panels_ncol <- 1

  if (is.null(hemisphere)) {
    hemisphere <- "front"
  } else {
    hemisphere <- match.arg(hemisphere, c("left", "right"))
  }

  view <- "lateral"

  if (is.null(title)) {
    no_title = FALSE
  } else {
    no_title = title == ""
  }
  all_panels_nrow <- brain_panels_nrow + 1*(!no_title)
  all_panels_ncol <- brain_panels_ncol
  
  # Check other arguments.
  mode <- match.arg(mode, c("widget", "image"))

  if (mode=="image") {
    if (!endsWith(fname, ".png")) { fname <- paste0(fname, ".png") }
    img_fname <- format_path(fname, write_dir, mode=2)
  }

  # Check width and height.
  if (is.null(width) | is.null(height)) {
    if (hemisphere == "front") {
      DEF_ASPECT_PER_PANEL <- c(1, 1) # aspect ratio
      def_aspect <- DEF_ASPECT_PER_PANEL * c(brain_panels_ncol, brain_panels_nrow)
      DEF_MAX_SIZE <- c(1200, 1200)
      zoom <- zoom * 1.5 # Top of brain view--tall
    } else {
      DEF_ASPECT_PER_PANEL <- c(10, 7) # aspect ratio
      def_aspect <- DEF_ASPECT_PER_PANEL * c(brain_panels_ncol, brain_panels_nrow)
      DEF_MAX_SIZE <- c(1500, 700)
    }

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

  ## Construct the mesh.
  mesh <- rgl::tmesh3d(t(cbind(surf$vertices,
                                rep(1, nrow(surf$vertices)))), # add homogenous coordinate
                              t(surf$faces),
                              meshColor = "vertices")
  mesh <- rgl::addNormals(mesh) # for smooth coloring

  # Open a new RGL window.
  rgl::open3d()
  if (is.null(bg)) { bg <- "white" }
  rgl::bg3d(color=bg)
  rgl::par3d(windowRect = c(20, 20, all_panels_width, all_panels_height))
  Sys.sleep(1) #https://stackoverflow.com/questions/58546011/how-to-draw-to-the-full-window-in-rgl
  
  all_panels_heights <- rep.int(1, brain_panels_nrow)
  if (!no_title) {all_panels_heights <- c(TITLE_AND_LEGEND_HEIGHT_RATIO, all_panels_heights) }

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
      title = ""
    }
    if (is.null(cex.title)) {
      # Default: 250% font size, but increasingly smaller for longer titles
      if (nchar(title) > 20) {
        cex.title <- 50 / nchar(title)
      } else {
        cex.title <- 2.5
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
    left = rbind( # Outer hemisphere of left surface toward viewer
      c( 0,-1, 0, 0),
      c( 0, 0, 1, 0),
      c(-1, 0, 0, 0),
      c( 0, 0, 0, 1)),
    right = rbind( # Outer hemisphere of right surface toward viewer
      c( 0, 1, 0, 0),
      c( 0, 0, 1, 0),
      c( 1, 0, 0, 0),
      c( 0, 0, 0, 1)),
    ID = diag(4)
  )

  # Populate the RGL window.
  for(ii in 1:n_brain_panels) {
    p <- brain_panels[ii]

    # Select the mesh for this panel, and determine the orientation.
    rgl::shade3d(mesh, col=col, specular="black", legend=TRUE)
    if (grepl("left", p)) {
      this_rot <- rot$left
    } else if (grepl("right", p)) {
      this_rot <- rot$right
    } else {
      this_rot <- rot$ID
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

#' S3 method: plot surface
#'
#' Visualize a surface with view_surf
#' @param x A "surface" object
#' @param ... Additional arguments to \code{\link{view_surf}}
#'
#' @method plot surface
#' @export
plot.surface <- function(x, ...){
  view_surf(x, ...)
}