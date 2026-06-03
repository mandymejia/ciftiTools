#' Impute \code{"xifti"} data
#'
#' Impute locations using the values of neighboring locations.
#'
#' Cortex vertices will be imputed using the five or six other vertices which
#'  share a face. The surface geometry must be present in the \code{"xifti"}.
#'
#' Subcortex voxels will be imputed using the six immediate neighbors (ignoring
#'  any out-of-mask location): above, below, left, right, forward, and back.
#'
#' Note that during imputation, locations in \code{mask}, as well as the medial
#'  wall for the cortex, are temporarily set to \code{NA}.
#'
#' Note that handling of \code{NA} values and the mask slightly differs from
#'  the cortex and subcortex. \code{impute_FUN} like \code{mean} will behave
#'  similarly, but functions which change depending on the amount of neighbor
#'  locations with \code{NA} values may differ.
#'  \code{impute_FUN} should handle \code{NA} values accordingly.
#'  For most use cases, it will make sense to pass \code{na.rm=TRUE} to
#'  \code{...} if \code{impute_FUN} is a summary function like \code{mean}.
#'
#' @param xifti A \code{"xifti"} object. The corresponding surface must be
#'  included for each cortex with data. \code{\link{add_surf}} can be used to
#'  add HCP fs_LR surfaces.
#' @param mask A logical vector whose length matches the number of rows in
#'  \code{xifti}, indicating which locations in \code{xifti} to impute.
#'  (Locations that are \code{TRUE} will be imputed.)
#'
#'  If \code{NULL} (default), will use the mask of locations with at least one
#'  \code{NA} and \code{NaN} value across the columns of \code{xifti}. The
#'  \code{NA} and \code{NaN} locations will be replaced with numeric values
#'  (except in the case of any voxels with no immediate neighbors).
#'
#'  On the other hand, if \code{mask} is provided, the \code{NA} and \code{NaN}
#'  values originally in \code{xifti}, and not in \code{mask}, will be left
#'  alone. Only locations in \code{mask} will be imputed.
#' @param impute_FUN The function to use to impute the values. It should accept
#'  a vector of numeric values (the values of neighboring locations) and return
#'  a single numeric value (the value to assign). Default: \code{mean(..., na.rm=TRUE)}.
#' @param smooth Smooth the imputed values? Smoothing will be calculated using
#'  the original and imputed data together, but only at imputed locations will
#'  the data be replaced with the smoothed values. Default: \code{TRUE}.
#' @param smooth_args List of arguments to \code{\link{smooth_cifti}}. Ignored
#'  if \code{!smooth}. \code{x} and \code{cifti_target_fname} should not be
#'  provided: \code{x} will be set to \code{xifti}, and \code{cifti_taget_fname}
#'  will remain \code{NULL}.
#' @param ... Additional arguments to \code{impute_FUN}.
#'
#' @return The input \code{xifti} with imputed data values.
#'
#' @family manipulating xifti
#'
#' @importFrom Matrix sparseMatrix rowSums Diagonal
#'
#' @export
impute_xifti <- function(
  xifti, mask=NULL, impute_FUN=function(x){mean(x, na.rm=TRUE)},
  smooth=TRUE, smooth_args=NULL,
  ...) {

  stopifnot(is.xifti(xifti))
  if (!is.null(xifti$meta$cifti$intent))

  nR <- nrow(xifti)
  nC <- ncol(xifti)

  keepNA <- !is.null(mask)
  if (keepNA) {
    which_NA <- lapply(xifti$data, function(x){which(is.na(x))})
  }

  if (is.null(mask)) {
    mask <- apply(is.na(as.matrix(xifti)), 1, any)
  }

  stopifnot(is.logical(mask))
  if (length(mask) != nR) {
    stop("The length of `mask` should match the number of rows in `xifti`.")
  }

  if (smooth && !is.null(smooth_args)) {
    stopifnot(is.list(smooth_args))
    stopifnot(!("cifti_target_fname" %in% names(smooth_args)))
  }

  # Split `mask` by brain structure. -------------------------------------------
  mask_bs <- list(
    cortex_left = NULL,
    cortex_right = NULL,
    subcort = NULL
  )
  mask2 <- mask
  for (bs in c("cortex_left", "cortex_right", "subcort")) {
    if (!is.null(xifti$data[[bs]])) {
      mask_bs[[bs]] <- mask2[seq(nrow(xifti$data[[bs]]))]
      mask2 <- mask2[seq(nrow(xifti$data[[bs]])+1, length(mask2))]
    }
  }

  mask_no_mwall <- mask # used if smoothing

  # Handle medial wall. --------------------------------------------------------
  if (!is.null(xifti$data$cortex_left) || !is.null(xifti$data$cortex_right)) {
    mwall_og <- xifti$meta$cortex$medial_wall_mask
    if (!is.null(xifti$data$cortex_left) && is.null(mwall_og$left)) {
      mwall_og$left <- rep(TRUE, nrow(xifti$data$cortex_left))
    }
    if (!is.null(xifti$data$cortex_right) && is.null(mwall_og$right)) {
      mwall_og$right <- rep(TRUE, nrow(xifti$data$cortex_right))
    }
    # Unapply the medial wall mask to the input mask.
    mask2c <- as.logical(do.call(c, mwall_og))
    mask2c[mask2c] <- c(mask_bs$cortex_left, mask_bs$cortex_right)
    mask <- c(mask2c, mask_bs$subcort)
    rm(mask2c)
    # Set mwall values to `NA` for now.
    xifti <- move_from_mwall(xifti)
  }

  # Split mask by brain structure again, after medial wall insertion.
  mask2 <- mask
  for (bs in c("cortex_left", "cortex_right", "subcort")) {
    if (!is.null(xifti$data[[bs]])) {
      mask_bs[[bs]] <- mask2[seq(nrow(xifti$data[[bs]]))]
      mask2 <- mask2[seq(nrow(xifti$data[[bs]])+1, length(mask2))]
    }
  }

  # Cortex. --------------------------------------------------------------------
  for (hemi in c("left", "right")) {
    c_hemi <- paste0("cortex_", hemi)
    if (is.null(xifti$data[[c_hemi]])) { next }
    if (!any(mask_bs[[c_hemi]])) { next }
    if (is.null(xifti$surf[[c_hemi]])) {
      stop("The ", hemi, " surface is needed for imputing ", hemi , " cortex data.")
    }

    ## Impute (Laplacian smoothing). -------------------------------------------
    dat_h  <- xifti$data[[c_hemi]]
    mask_h <- mask_bs[[c_hemi]]

    # Make adjacency list.
    faces <- xifti$surf[[c_hemi]]$faces
    nV <- nrow(xifti$surf[[c_hemi]]$vertices)
    edges <- rbind(faces[, c(1,2)], faces[, c(2,3)], faces[, c(1,3)])
    edges <- rbind(edges, edges[, c(2,1)]) # make symmetric.
    edges <- unique(edges)

    A <- Matrix::sparseMatrix(i=edges[,1], j=edges[,2], x=1, dims=c(nV, nV))
    deg <- Matrix::rowSums(A)
    W <- A / deg
    idx_imp <- which( mask_h & mwall_og[[hemi]])
    idx_val <- which(!mask_h & mwall_og[[hemi]])

    idx_valid <- which(mwall_og[[hemi]])   # all valid verts (imp + val)
    A_valid <- A[idx_valid, idx_valid]     # restrict to medial-wall-excluded verts
    deg_valid <- Matrix::rowSums(A_valid)  # degree within valid subgraph

    # Local indices within idx_valid
    is_imp <- mask_h[idx_valid]
    loc_imp <- which( is_imp)
    loc_val <- which(!is_imp)

    W_uu <- A_valid[loc_imp, loc_imp] / deg_valid[loc_imp]
    W_uk <- A_valid[loc_imp, loc_val] / deg_valid[loc_imp]

    LHS <- Matrix::Diagonal(length(loc_imp)) - W_uu
    f_k <- dat_h[idx_val, , drop=FALSE]
    RHS <- W_uk %*% f_k

    f_u <- Matrix::solve(LHS, RHS)
    xifti$data[[c_hemi]][idx_imp, ] <- as.matrix(f_u)
  }

  for (hemi in c("left", "right")) {
    c_hemi <- paste0("cortex_", hemi)
    if (is.null(xifti$data[[c_hemi]])) { next }

    # Put the medial wall back.
    if (!is.null(mwall_og[[hemi]])) {
      xifti$data[[c_hemi]] <- xifti$data[[c_hemi]][mwall_og[[hemi]],,drop=FALSE]
      xifti$meta$cortex$medial_wall_mask[[hemi]] <- mwall_og[[hemi]]
    }

    # Put original `NA` values back if applicable.
    if (keepNA) {
      xifti$data[[c_hemi]][which_NA[[c_hemi]]] <- NA
    }
  }

  # Subcortex. -----------------------------------------------------------------
  if (!is.null(xifti$data$subcort) && any(mask_bs$subcort)) {

    ## Precomputes. ------------------------------------------------------------
    # Get the six neighbors for each voxel.
    sdim <- dim(xifti$meta$subcort$mask)
    # Get the index of each voxel in both vector and array form.
    # `ind_arr2vox` converts from the latter to the former.
    ind_arr2vox <- function(arr) {
      arr[1,] + (arr[2,]-1)*sdim[1] + (arr[3,]-1)*sdim[2]*sdim[1]
    }
    ind_arr <- t(which(xifti$meta$subcort$mask, arr.ind=TRUE))
    ind_vox <- which(xifti$meta$subcort$mask)
    stopifnot(all(ind_arr2vox(ind_arr) == ind_vox)) # check
    # Jitter each voxel's array location by one step each direction (six total)
    #   and match to the vector location index. `NA`: out-of-bounds of the mask.
    ind_nbr <- cbind(
      match(ind_arr2vox(ind_arr+c(-1,0,0)), ind_vox, NA),
      match(ind_arr2vox(ind_arr+c(1,0,0)), ind_vox, NA),
      match(ind_arr2vox(ind_arr+c(0,-1,0)), ind_vox, NA),
      match(ind_arr2vox(ind_arr+c(0,1,0)), ind_vox, NA),
      match(ind_arr2vox(ind_arr+c(0,0,-1)), ind_vox, NA),
      match(ind_arr2vox(ind_arr+c(0,0,1)), ind_vox, NA)
    )
    # Takes vector of values and impute with the six neighbors.
    imp_vox <- function(vals_h, mask_imp) {
      # `vals_nbr`: `NA` values can be either in-mask `NA` values or out-of-mask
      vals_nbr <- matrix(vals_h[ind_nbr], nrow=nrow(ind_nbr))
      # `mimp_nbr`: TRUE if the neighbor voxel is a voxel being imputed
      mimp_nbr <- matrix(mask_imp[ind_nbr], nrow=nrow(ind_nbr))
      mimp_nbr[is.na(mimp_nbr)] <- FALSE
      # `z`: vector of new imputed values.
      z <- rep(NA, sum(mask_imp))
      for (rr in seq(sum(mask_imp))) {
        rm <- which(mask_imp)[rr]
        valid_vox <- !is.na(ind_nbr[rm,]) & !mimp_nbr[rm,] # rm out-of-mask and is-being-imputed
        z[rr] <- impute_FUN(vals_nbr[rm,][valid_vox], ...)
      }
      z
    }

    ## Impute loop. ------------------------------------------------------------
    dat_h <- xifti$data$subcort
    mask_h <- mask_bs$subcort
    has_changed_count <- Inf
    for (rr in seq(nR)) {
      ### Compute the imputed values. ------------------------------------------
      v_impv <- apply(dat_h, 2, imp_vox, mask_h)
      ### Update. --------------------------------------------------------------
      # Check which voxels have been updated.
      has_changed <- (v_impv - dat_h[mask_h,]) != 0
      has_changed[] <- ifelse(
        is.na(has_changed),
        xor(is.na(v_impv[]), is.na(dat_h[mask_h,][])),
        has_changed
      )

      # Set imputed values.
      dat_h[mask_h,] <- v_impv

      # Check for change.
      if (!any(has_changed)) { break }
      has_changed <- apply(has_changed, 1, all)

      # Remove updated verts from mask of verts to impute.
      mask_h[mask_h][has_changed] <- FALSE
    }

    xifti$data$subcort <- dat_h
    rm(dat_h)
    rm(mask_h)

    # Put original `NA` values back if applicable.
    if (keepNA) { xifti$data$subcort[which_NA$subcort] <- NA }
  }

  # Smooth, if applicable. -----------------------------------------------------
  if (smooth) {
    # Do smoothing.
    xifti_sm <- do.call(smooth_xifti, c(list(x=xifti), smooth_args))
    # Only replace imputed locations.
    xifti_out_mat <- as.matrix(xifti)
    xifti_out_mat[mask_no_mwall,] <- as.matrix(xifti_sm)[mask_no_mwall,]
    xifti <- newdata_xifti(xifti, xifti_out_mat)
  }

  xifti
}
