#' Summarize a \code{"surf"} object
#'
#' Summary method for class "surf"
#'
#' @param object Object of class "surf". 
#'  See \code{\link{is.surf}} and \code{\link{make_surf}}.
#' @param ... further arguments passed to or from other methods.
#' 
#' @export
#' 
#' @method summary surf
summary.surf <- function(object, ...) {
  out <- list(
    vertices = nrow(object$vertices),
    faces = nrow(object$faces),
    hemisphere = object$hemisphere
  )
  class(out) <- "summary.surf"
  return(out)
}

#' @rdname summary.surf
#' @export
#' 
#' @param x bject of class "surf". 
#' 
#' @method print summary.surf
#' 
print.summary.surf <- function(x, ...) {
  cat("Vertices:   ", x$vertices, "\n")
  cat("Faces:      ", x$faces, "\n")
  if (!is.null(x$hemisphere)) { cat("Hemisphere: ", x$hemisphere, "\n") }
}

#' @rdname summary.surf
#' @export
#' 
#' @method print surf
#' 
print.surf <- function(x, ...) {
  print.summary.surf(summary(x))
}

#' Distance from mask on surface
#'
#' Identify the vertices within \code{boundary_width} edges of a vertex in the 
#'  input mask on a triangular mesh. Returns the number of edges a vertex is
#'  away from the closest mask vertex. 
#' 
#' @inheritParams faces_Param
#' @inheritParams mask_Param_vertices
#' @param boundary_width A positive integer representing the width of the 
#'  boundary to compute. The furthest vertices from the input mask will be this
#'  number of edges away from the closest vertex in the input mask. Default: 
#'  \code{10}.
#' 
#' @return A length-V numeric vector. Each entry corresponds to the vertex
#'  with the same index. For vertices within the boundary, the value will be the
#'  number of vertices away from the closest vertex in the input mask.
#'  Vertices inside the input mask but at the edge of it (i.e. the vertices
#'  that define the boundary) will have value 0. Then, all other vertices will 
#'  have value -1.
#'
#' @keywords internal 
surf_dist_from_mask <- function(faces, mask, boundary_width=10){
  s <- ncol(faces)
  v <- max(faces)
  # For quads, surf_dist_from_mask() would count opposite vertices on a face as
  #   adjacent--that's probably not desired.
  stopifnot(s == 3)

  stopifnot(boundary_width > 0)

  verts_mask <- which(mask)
  b_layers <- rep(-1, v)
  for (ii in seq(1, boundary_width)) {
    # Identify vertices that share a face with in-mask vertices, or vertices
    #   in lower layers. Those faces occupy the next boundary layer.
    ## Vertices inside the mask, or a lower layer
    vert_maskorlower <- unique(c(verts_mask, which(b_layers > 0)))
    ## The number of "in-mask or lower-layer" vertices in each face
    face_n_maskorlower <- rowSums(matrix(faces %in% vert_maskorlower, ncol=s))
    ## Faces with a mix of "in-mask or lower-layer" vertices, and new vertices
    faces_adj_mask <- face_n_maskorlower > 0 & face_n_maskorlower < s
    if (!any(faces_adj_mask)) { break }
    ## Vertices in faces with a mix
    verts_adj <- unique(as.vector(faces[faces_adj_mask,]))

    ## For the first layer, in-mask vertices that are part of a mixed face
    ##  are "layer 0"
    if (ii == 1) {
      b_layers[verts_adj[verts_adj %in% which(mask)]] <- 0
    }

    ## The new vertices that are part of the mixed faces make up this layer.
    verts_adj <- verts_adj[!(verts_adj %in% vert_maskorlower)]
    b_layers[verts_adj] <- ii
  }

  b_layers
}

#' Boundary region of a mask
#' 
#' Identify the vertices within \code{boundary_width} edges of a vertex in the 
#'  input mask on a triangular mesh. Returns a logical indicating if a vertex
#'  is within \code{boundary_width} edges of the mask. 
#' 
#' @inheritParams faces_Param
#' @inheritParams mask_Param_vertices
#' @param boundary_width A positive integer representing the width of the 
#'  boundary to compute. The furthest vertices from the input mask will be this
#'  number of edges away from the closest vertex in the input mask. Default: 
#'  \code{10}.
#' 
#' @return A length-V logical vector. Each entry corresponds to the vertex
#'  with the same index. The value is true if a vertex is within 
#'  \code{boundary_width} edges of a vertex in the mesh, but is not within the
#'  mesh itself. 
#'
#' @export
boundary_mask <- function(faces, mask, boundary_width=10){
  b_layers <- surf_dist_from_mask(
    faces=faces, mask=mask, boundary_width=boundary_width
  )

  b_layers > 0
}

#' Vertex Adjacency Matrix
#'
#' Make adjacency matrix between two sets of vertices.
#'
#' @inheritParams faces_Param
#' @param v1,v2 The first and second set of vertices. These are logical vectors
#'  the same length as \code{vertices} indicating the vertices in each set.
#'  If \code{v2} is \code{NULL} (default), set \code{v2} to \code{v1}. Can
#'  alternatively be a vector if integers corresponding to vertex indices.
#'
#' @return Adjacency matrix
#' 
#' @keywords internal 
vert_adjacency <- function(faces, v1, v2=NULL){
  v_all <- unique(as.vector(faces))
  # Arguments.
  if (is.logical(v1)) { v1 <- which(v1) }
  v1 <- sort(unique(v1))
  stopifnot(all(v1 %in% v_all))
  if (is.null(v2)) {
    v2 <- v1
  } else {
    if (is.logical(v2)) { v2 <- which(v2) }
    v2 <- sort(unique(v2))
    stopifnot(all(v2 %in% v_all))
  }

  # # Faces with at least one vertex in v1, and at least one vertex in v2.
  # #   This step reduces the number of faces we check in the next step.
  # #   It also used the subset index.
  # f_btwn <- apply(matrix(faces %in% v1, ncol=3), 1, any)
  # f_btwn_mask <- f_btwn & apply(matrix(faces %in% v2, ncol=3), 1, any)
  # f_btwn <- faces[f_btwn_mask,]
  v_reidx <- rep(NA, max(as.numeric(faces)))
  v_reidx[v1] <- 1:length(v1)
  v_reidx[v2] <- 1:length(v2)
  f_reidx <- matrix(v_reidx[as.vector(faces)], ncol=ncol(faces))

  # Check each pair of vertices in each face.
  adj <- matrix(FALSE, nrow=length(v1), ncol=length(v2))
  for (ii in 1:3) {
    for (jj in 1:3) {
      if (ii == jj) { next }
      # Mark adjacency betwen (v1, v2) pairs sharing a face.
      v_pairs <- f_reidx[(faces[,ii] %in% v1) & (faces[,jj] %in% v2), c(ii,jj)]
      adj[v_pairs] <- TRUE
    }
  }

  # Add "v" to row/colnames to not confuse with numeric index.
  rownames(adj) <- paste("v", v1); colnames(adj) <- paste("v", v2)
  adj
}


#' Order Vertices on Circular Manifold
#'
#' Order vertices on circular manifold by radians (after 2D CMDS projection).
#'
#' @inheritParams vertices_Param
#' 
#' @return Index ordering of \code{vertices}
#' 
#' @importFrom stats cmdscale dist
#' @keywords internal 
radial_order <- function(vertices){
  # Use CMDS to project onto 2-dimensional subspace. Scale each dimension.
  x <- scale(cmdscale(dist(vertices)))
  # Remove zero-values to stay in the domain of the trig functions.
  x <- matrix(ifelse(abs(x) < 1e-8, sign(x)*1e-8, x), ncol=ncol(x))
  # Order by the radians counter-clockwise from positive x-axis.
  # https://stackoverflow.com/questions/37345185/r-converting-cartesian-to-polar-and-sorting
  order(order(ifelse(
    x[,1] < 0,
    atan(x[,2] / x[,1]) + pi,
    ifelse(x[,2] < 0 , atan(x[,2] / x[,1]) + 2*pi, atan(x[,2] / x[,1]))
  )))
}

#' Apply Mask With Boundary To Mesh
#'
#' Make a boundary around a mask with two levels of decimation, and apply to a mask.
#'
#' The boundary consists of a \code{width1}-vertex-wide middle region and a
#'  \code{width2}-vertex-wide outer region, for a total of \code{width1 + width2} layers
#'  of vertices surrounding the input mask. In the first layer, every \code{k1}
#'  vertex within every \code{k1} layer (beginning with the innermost
#'  layer) is retained; the rest are discarded. In the second layer, every
#'  \code{k2} vertex within every \code{k2} layer (beginning with the innermost
#'  layer) is retained; the rest are discarded. It is recommended to make \code{width1}
#'  a multiple of \code{k1} and \code{width2} a multiple of \code{k2}.
#'
#' Default boundary: a 4-vertex wide middle region with triangles twice as long,
#'  and a 6-vertex wide outer region with triangles three times as long.
#'
#' @inheritParams vertices_Param
#' @inheritParams faces_Param
#' @inheritParams mask_Param_vertices
#' @param width1,width2 the width of the middle/outer region. All vertices in the middle/outer region
#'  are between 1 and \code{width1} edges away from the closest vertex in \code{mask}/middle region.
#' @param k1,k2 roughly, the triangle size multiplier. Every \code{k1}/\code{k2} vertex within
#'  every \code{k1}/\code{k2} layer (beginning with the innermost layer) will be retained;
#'  the rest will be discarded. If the mesh originally has triangles of regular
#'  size, the sides of the triangles in the middle/outer region will be about
#'  \code{k1}/\code{k2} as long.
#'
#' @return A new mesh (list with components vertices and faces)
#' 
#' @importFrom stats quantile
#'
#' @keywords internal 
mask_with_boundary <- function(vertices, faces, mask, width1=4, k1=2, width2=6, k2=3){

  # ----------------------------------------------------------------------------
  # Check arguments ------------------------------------------------------------
  # ----------------------------------------------------------------------------
  width <- width1 + width2
  V_ <- nrow(vertices)
  F_ <- nrow(faces)
  s <- ncol(faces)
  stopifnot(s==3)

  faces2 <- list(
    rm = rep(FALSE, F_),
    add = NULL
  )

  # ----------------------------------------------------------------------------
  # Pre-compute layers and vertex adjacency matrix between neighbor layers. ----
  # ----------------------------------------------------------------------------
  b_layers <- surf_dist_from_mask(faces, mask, width)
  b_adjies <- vector("list", width)
  for (ii in 1:length(b_adjies)){
    b_adjies[[ii]] <- vert_adjacency(
      faces,
      v1 = which(b_layers == ii-1),
      v2 = which(b_layers == ii)
    )
  }

  # ----------------------------------------------------------------------------
  # Working outward from the mask, collect info on each layer (and previous), --
  # ----------------------------------------------------------------------------
  lay_idxs <- c(0, seq(1, width1, k1))
  if (width2 != 0) { lay_idxs <- c(lay_idxs, seq(width1+1, width1+width2, k2)) }
  lay_k <- c(1, rep(k1, width1), rep(k2, width2))

  # At each iteration, we will calcuate these "layer facts":
  # Note: verts_pre must be in radial order: lay$verts[lay$rad_order],]
  get_layer_facts <- function(vertices, faces, b_layers, lay_idx, k, verts_pre=NULL){
    lay <- list(idx = lay_idx)
    ## The vertices in the layer
    lay$verts <- which(b_layers == lay$idx)
    ## The number of vertices
    lay$V1 <- length(lay$verts)
    ## Faces whose vertices are entirely in the layer
    lay$faces_complete <- apply(matrix(faces %in% lay$verts, ncol=s), 1, all)
    ## The radial ordering of the vertices in the layer
    lay$rad_order <- radial_order(vertices[lay$verts,])
    ## The vertices in radial order
    lay$verts_rad <- vertices[lay$verts[order(lay$rad_order)],]
    if (!is.null(verts_pre)) {
      ## Adjust radial ordering so first vertex in this layer is closest to
      ##  first vertex in pre layer.
      rad_first <- which.min(
        apply(t(lay$verts_rad) - verts_pre[1,], 2, norm)
      )
      lay$rad_order <- ((lay$rad_order - rad_first) %% length(lay$rad_order)) + 1
      lay$verts_rad <- lay$verts_rad[c(
        seq(rad_first, nrow(lay$verts_rad)),
        seq(1, rad_first-1)
      ),]
      ## Flip direction of radial ordering (clockwise/counter-clockwise, or rather
      ##  left/right to match the pre layer)
      even_sample <- function(X, s){ X[as.integer(floor(quantile(1:nrow(X), probs=seq(1,s)/s))),] }
      pre_samp <- even_sample(verts_pre, 12)
      lay_samp <- even_sample(lay$verts_rad, 12)
      flip <- mean(apply(pre_samp - lay_samp, 1, norm)) > mean(apply(pre_samp[nrow(pre_samp):1,] - lay_samp, 1, norm))
      if (flip) {
        lay$rad_order <- (length(lay$rad_order) - lay$rad_order + 2) %% length(lay$rad_order)
        lay$rad_order[lay$rad_order==0] <- length(lay$rad_order)
        lay$verts_rad <- lay$verts_rad[nrow(lay$verts_rad):1,]
      }
    }
    ## Only keep every kth
    lay$verts_rad <- lay$verts_rad[seq(1, nrow(lay$verts_rad), k),]
    lay$V2 <- nrow(lay$verts_rad)
    lay
  }

  lay_ii <- get_layer_facts(
    vertices, faces, b_layers,
    lay_idxs[1], lay_k[1],
  )

  for (ii in 1:(length(lay_idxs)-1)) {
    # Calculate layer facts for this layer (and keep the facts for the previous one).
    lay_pre <- lay_ii
    lay_ii <- get_layer_facts(
      vertices, faces, b_layers,
      lay_idxs[ii+1], lay_k[ii+1],
      lay_pre$verts_rad
    )
    # plot_3d(lay_ii$verts_rad, 1:nrow(lay_ii$verts_rad))
    # plot_3d(vertices[lay_ii$verts,], lay_ii$rad_order)

    # --------------------------------------------------------------------------
    # Remove faces between the layers. -----------------------------------------
    # --------------------------------------------------------------------------
    faces_btwn <- apply(
      matrix(faces %in% which(b_layers %in% lay_pre$idx:lay_ii$idx), ncol=s),
      1, all
    )
    # Do not count faces that are all made of pre-layer vertices, or
    #   all post-layer vertices.
    faces_btwn <- faces_btwn & (!(lay_pre$faces_complete)) & (!(lay_ii$faces_complete))
    faces2$rm[faces_btwn] <- TRUE

    # --------------------------------------------------------------------------
    # Make new faces. ----------------------------------------------------------
    # Strategy:
    #   * Start with an arbitrary vertex from layer A.
    #   * Make a face between that vertex, the closest (first) in layer B, and
    #     the second in layer A next in radial order).
    #   * Then, make a face between the second in layer A, the first in layer B,
    #   * and the second in layer B. These two faces are a square with a dividing
    #   * diagonal.
    #   * Continue until looped around.
    #   * Add triangles evenly distributed to account for different number
    #   * of vertices.
    # --------------------------------------------------------------------------

    # Multiply adjacency matrices between all in-between layers to get
    #   pseudo-adjacency matrix between vertices in the post- and pre- layers.
    adj <- b_adjies[lay_pre$idx:lay_ii$idx]
    adj <- Reduce("%*%", adj)
    # ??? Not used yet.

    V_max <- max(lay_ii$V2, lay_pre$V2)
    v_ii <- lay_ii$verts[order(lay_ii$rad_order)][seq(1, lay_ii$V1, lay_k[ii+1])]
    v_ii <- v_ii[as.integer(ceiling(seq(1, V_max) * (lay_ii$V2 / V_max)))]
    v_pre <- lay_pre$verts[order(lay_pre$rad_order)][seq(1, lay_pre$V1, lay_k[ii])]
    v_pre <- v_pre[as.integer(ceiling(seq(1, V_max) * (lay_pre$V2 / V_max)))]

    one_back <- function(x){c(x[2:length(x)], x[1])}
    cost_a <- mean(apply(vertices[v_ii,] - vertices[one_back(v_pre),], 1, norm))
    cost_b <- mean(apply(vertices[one_back(v_ii),] - vertices[v_pre,], 1, norm))
    # Note: will remove degenerate faces after loop (v_ii/v_pre have repeats)
    if (cost_a < cost_b) {
      faces2$new <- rbind(faces2$new, cbind(v_ii, one_back(v_ii), v_pre))
      faces2$new <- rbind(faces2$new, cbind(one_back(v_ii), v_pre, one_back(v_pre)))
    } else {
      faces2$new <- rbind(faces2$new, cbind(v_pre, one_back(v_pre), v_ii))
      faces2$new <- rbind(faces2$new, cbind(one_back(v_pre), v_ii, one_back(v_ii)))
    }
  }

  # ----------------------------------------------------------------------------
  # Arrange the results. -------------------------------------------------------
  # ----------------------------------------------------------------------------

  # Remove degenerate faces.
  faces2$degen <- (faces2$new[,1] == faces2$new[,2])
  faces2$degen <- faces2$degen | (faces2$new[,1] == faces2$new[,3])
  faces2$degen <- faces2$degen | (faces2$new[,2] == faces2$new[,3])
  faces2$new <- faces2$new[!faces2$degen,]

  # Construct and return new mesh.
  #faces2$new <- rbind(faces[!faces2$rm,], faces2$new)
  verts_remaining <- unique(as.vector(faces2$new))
  verts2 <- rep(NA, V_)
  verts2[verts_remaining] <- 1:length(verts_remaining)
  faces2$new <- matrix(verts2[as.vector(faces2$new)], ncol=s)
  list(vertices=vertices[verts_remaining,], faces=faces2$new)
}

#' Apply Mask to Vertices and Faces
#'
#' Apply a binary mask to a set of vertices and faces.  Vertices not in the mask are removed,
#' and faces (triangles) with any vertices outside of the mask are removed.  Finally,
#' vertex numbering in the masked faces matrix is corrected.
#'
#' @inheritParams vertices_Param
#' @inheritParams faces_Param
#' @inheritParams mask_Param_vertices
#'
#' @return List containing masked vertices and faces matrices
#' 
#' @export
mask_vertices_faces <- function(vertices, faces, mask){

  # Number of vertices
  V <- nrow(vertices)

  # Check index of faces
  if(min(faces) == 0){
    faces <- faces + 1
  }

  mask <- as.numeric(mask)
  if(length(mask) != V | !is.vector(mask)){
    stop("Mask should be a vector of length V")
  }
  # Check only 0s and 1s
  values <- sort(unique(mask))
  if(! (min(values %in% 0:1)) ) stop("Mask should be composed of only 0s and 1s")

  inmask <- which(mask==1)

  # Apply mask to vertices
  vertices_new <- vertices[inmask,]

  ### Apply mask to faces (triangles)

  # Identify triangles where any vertex is outside of the mask
  faces <- faces[(faces[,1] %in% inmask) & (faces[,2] %in% inmask) & (faces[,3] %in% inmask),]

  # Re-number faces
  faces_new <- faces*0
  for(ii in 1:nrow(faces)){
    faces_new[ii,1] <- which(inmask == faces[ii,1])
    faces_new[ii,2] <- which(inmask == faces[ii,2])
    faces_new[ii,3] <- which(inmask == faces[ii,3])
  }

  # Return updated vertices and faces
  result <- list(vertices=vertices_new, faces=faces_new)
  return(result)
}

#' Create a mask based on vertices that are invalid
#'
#' @param data A list of sessions, where each session is a list with elements
#'  BOLD, design and nuisance. See \code{?create.session} and \code{?is.session}
#'  for more details.
#' @param meanTol,varTol Tolerance for mean and variance of each data location. Locations which
#'  do not meet these thresholds are masked out of the analysis. Defaults: \code{1e-6}.
#' @param verbose Print messages counting how many locations are removed?
#'
#' @return A logical vector indicating valid vertices
#'
#' @importFrom stats var
#' 
#' @export
make_mask <- function(data, meanTol=1e-6, varTol=1e-6, verbose=TRUE){

  # For each BOLD data matrix,
  mask_na <- mask_mean <- mask_var <- rep(TRUE, ncol(data[[1]]$BOLD))
  for (ss in seq(length(data))) {
    dss <- data[[ss]]$BOLD
    # Mark columns with any NA or NaN values for removal.
    dss_na <- is.na(dss) | is.nan(dss)
    mask_na[apply(dss_na, 2, any)] <- FALSE
    # Calculate means and variances of columns, except those with any NA or NaN.
    # Mark columns with mean/var falling under the thresholds for removal.
    mask_mean[mask_na][colMeans(dss[,mask_na,drop=FALSE]) < meanTol] <- FALSE
    mask_var[mask_na][apply(dss[,mask_na,drop=FALSE], 2, var) < varTol] <- FALSE
  }

  # Print counts of locations removed, for each reason.
  if (verbose) {
    warn_part1 <- if (any(!mask_na)) { "additional locations" } else { "locations" }
    warn_part2 <- if (length(data) > 1) { " in at least one scan.\n" } else { ".\n" }
    if (any(!mask_na)) {
      cat("\t", sum(!mask_na), paste0("locations removed due to NA or NaN values", warn_part2))
    }
    # Do not include NA locations in count.
    mask_mean2 <- mask_mean | (!mask_na)
    if (any(!mask_mean2)) {
      cat("\t", sum(!mask_mean2), warn_part1, paste0("removed due to low mean", warn_part2))
    }
    # Do not include NA or low-mean locations in count.
    mask_var2 <- mask_var | (!mask_mean) | (!mask_na)
    if (any(!mask_var2)) {
      cat("\t", sum(!mask_var2), warn_part1, paste0("removed due to low variance", warn_part2))
    }
  }

  # Return composite mask.
  mask_na & mask_mean & mask_var
}