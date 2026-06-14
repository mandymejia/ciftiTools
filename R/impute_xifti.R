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
#'  the cortex and subcortex. \code{layerwise_FUN} like \code{mean} will behave
#'  similarly, but functions which change depending on the amount of neighbor
#'  locations with \code{NA} values may differ.
#'  \code{layerwise_FUN} should handle \code{NA} values accordingly.
#'  For most use cases, it will make sense to pass \code{na.rm=TRUE} to
#'  \code{...} if \code{layerwise_FUN} is a summary function like \code{mean}.
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
#' @param method The imputation method, applied to both the cortex and subcortex.
#'  \code{"laplacian"} (default) solves a sparse linear system (harmonic 
#'  interpolation) to impute all masked locations simultaneously, whereas 
#'  \code{"layerwise"} iteratively fills in boundary vertices/voxels one layer 
#'  at a time using \code{layerwise_FUN} applied to neighboring values.
#' 
#'  For high-resolution cortex data or the subcortex, the Laplacian method may
#'  run out of memory, in which case the layerwise method is an alternative. 
#' @param layerwise_FUN The function to use to impute the values if
#'  \code{method=="layerwise"}. It should accept a vector of numeric values
#'  (the values of neighboring locations) and return a single numeric value (the
#'  value to assign). Default: \code{mean(..., na.rm=TRUE)}. Not used if
#'  \code{method="laplacian"}.
#' @param smooth Smooth the imputed values? Smoothing will be calculated using
#'  the original and imputed data together, but only at imputed locations will
#'  the data be replaced with the smoothed values. If \code{NULL} (default),
#'  smooth if \code{method=="layerwise"} and not if \code{method=="laplacian"},
#'  which already produces very smooth results.
#' @param smooth_args List of arguments to \code{\link{smooth_cifti}}. Ignored
#'  if \code{!smooth}. \code{x} and \code{cifti_target_fname} should not be
#'  provided: \code{x} will be set to \code{xifti}, and \code{cifti_taget_fname}
#'  will remain \code{NULL}.
#' @param ... Additional arguments to \code{layerwise_FUN}.
#'
#' @return The input \code{xifti} with imputed data values.
#'
#' @family manipulating xifti
#'
#' @export
impute_xifti <- function(
  xifti, mask=NULL, method=c("laplacian", "layerwise"),
  layerwise_FUN=function(x){mean(x, na.rm=TRUE)},
  smooth=NULL, smooth_args=NULL,
  ...) {

  stopifnot(is.xifti(xifti))
  if (!is.null(xifti$meta$cifti$intent) && xifti$meta$cifti$intent==3007) {
    warning("`impute_xifti` is being applied to a `xifti` with label data.")
  }

  method <- match.arg(method)

  if (method == "laplacian") {
    if (!requireNamespace("Matrix", quietly = TRUE)) {
      stop("Package \"Matrix\" needed for Laplacian imputation. Please install or set `method` to `'layerwise'` instead.")
    }
  }

  if (is.null(smooth)) { smooth <- method=="layerwise" }
  stopifnot(isTRUE(smooth) || isFALSE(smooth))

  nR <- nrow(xifti)

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
  mwall_og <- list(left=NULL, right=NULL)
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

    dat_now <- xifti$data[[c_hemi]]
    mask_now <- mask_bs[[c_hemi]]

    ## Impute loop (layerwise). ------------------------------------------------
    if (method == "layerwise") {
      for (rr in seq(nR)) { # really a while(TRUE) but I figured this is safer.
        ### Compute the imputed values. ----------------------------------------
        # `i_mask`: verts to impute bordering at least one vert not being imputed
        i_mask <- boundary_mask_surf(
          xifti$surf[[c_hemi]]$faces,
          (!mask_now) & (mwall_og[[hemi]]),
          1
        ) & (mwall_og[[hemi]]) & mask_now
        if (sum(i_mask) < 1) { break }
        # `j_mask`: non-imputing verts bordering at least one vert being imputed
        j_mask <- boundary_mask_surf(xifti$surf[[c_hemi]]$faces, mask_now, 1) & mwall_og[[hemi]] & !mask_now
        # `v_adj`: adjacency matrix between `i_mask` and `j_mask`
        v_adj <- vert_adjacency(xifti$surf[[c_hemi]]$faces, i_mask, j_mask)
        stopifnot(all(rowSums(v_adj) > 0))
        # `v_vals`: for each vert to impute, the values from which to impute.
        #   All vertices in `v_vals` should have at least one value, but there may
        #   be `NA` vals from the medial wall or if the original data was `NA`.
        v_vals <- apply(v_adj, 1, function(x){dat_now[which(j_mask)[x],,drop=FALSE]}, simplify=FALSE)
        # `v_impv`: The imputed values.
        v_impv <- lapply(v_vals, function(x){apply(x, 2, layerwise_FUN, ...)})
        v_impv <- do.call(rbind, v_impv)

        ### Update. ------------------------------------------------------------
        # Check which vertices have been updated.
        has_changed <- (v_impv - dat_now[i_mask,]) != 0
        has_changed[] <- ifelse(
          is.na(has_changed),
          xor(is.na(v_impv[]), is.na(dat_now[i_mask,][])),
          has_changed
        )

        # Set imputed values.
        dat_now[i_mask,] <- v_impv

        # Check for change.
        if (!any(has_changed)) { break }
        has_changed <- apply(has_changed, 1, all)

        # Remove updated verts from mask of verts to impute.
        mask_now[i_mask][has_changed] <- FALSE
      }

      xifti$data[[c_hemi]][mask_bs[[c_hemi]], ] <- dat_now[mask_bs[[c_hemi]],,drop=FALSE]

    ## Impute, Laplacian. ------------------------------------------------------
    ## Begin: written by Claude! -----------------------------------------------
    } else {
      # Build sparse adjacency matrix from surface faces.
      faces <- xifti$surf[[c_hemi]]$faces
      nV <- nrow(xifti$surf[[c_hemi]]$vertices)
      edges <- rbind(faces[, c(1,2)], faces[, c(2,3)], faces[, c(1,3)])
      edges <- rbind(edges, edges[, c(2,1)]) # make symmetric.
      edges <- unique(edges)

      A <- Matrix::sparseMatrix(i=edges[,1], j=edges[,2], x=1, dims=c(nV, nV))
      idx_valid <- which(mwall_og[[hemi]])   # all valid verts (imp + val)
      A_valid <- A[idx_valid, idx_valid]     # restrict to medial-wall-excluded verts
      deg_valid <- Matrix::rowSums(A_valid)  # degree within valid subgraph

      # Local indices within idx_valid.
      is_imp <- mask_now[idx_valid]
      loc_imp <- which(is_imp)
      loc_val <- which(!is_imp)

      idx_imp <- which(mask_now & mwall_og[[hemi]])
      idx_val <- which(!mask_now & mwall_og[[hemi]])

      # Return `NaN` instead of `0` if no non-NA to impute from.
      if (length(loc_val) == 0) {
        xifti$data[[c_hemi]][idx_imp,] <- NaN
        rm(dat_now)
        rm(mask_now)
        next
      }

      W_uu <- A_valid[loc_imp, loc_imp] / deg_valid[loc_imp]
      W_uk <- A_valid[loc_imp, loc_val] / deg_valid[loc_imp]

      LHS <- Matrix::Diagonal(length(loc_imp)) - W_uu
      f_k <- dat_now[idx_val,,drop=FALSE]
      RHS <- W_uk %*% f_k

      f_u <- Matrix::solve(LHS, RHS)
      xifti$data[[c_hemi]][idx_imp,] <- as.matrix(f_u)
    }

    rm(dat_now)
    rm(mask_now)
  }
    ## End: written by Claude! -------------------------------------------------

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

    dat_now <- xifti$data$subcort
    mask_now <- mask_bs$subcort

    ## Impute loop (layerwise). ------------------------------------------------
    if (method == "layerwise") {
      imp_vox <- function(vals_now, mask_imp) {
        vals_nbr <- matrix(vals_now[ind_nbr], nrow=nrow(ind_nbr))
        mimp_nbr <- matrix(mask_imp[ind_nbr], nrow=nrow(ind_nbr))
        mimp_nbr[is.na(mimp_nbr)] <- FALSE
        # `z`: vector of new imputed values.
        z <- rep(NA, sum(mask_imp))
        for (rr in seq(sum(mask_imp))) {
          rm <- which(mask_imp)[rr]
          valid_vox <- !is.na(ind_nbr[rm,]) & !mimp_nbr[rm,] # rm out-of-mask and is-being-imputed
          z[rr] <- layerwise_FUN(vals_nbr[rm,][valid_vox], ...)
        }
        z
      }

      for (rr in seq(nR)) {
        v_impv <- apply(dat_now, 2, imp_vox, mask_now)
        has_changed <- (v_impv - dat_now[mask_now,]) != 0
        has_changed[] <- ifelse(
          is.na(has_changed),
          xor(is.na(v_impv[]), is.na(dat_now[mask_now,][])),
          has_changed
        )
        dat_now[mask_now,] <- v_impv
        if (!any(has_changed)) { break }
        has_changed <- apply(has_changed, 1, all)
        mask_now[mask_now][has_changed] <- FALSE
      }

    ## Impute (Laplacian). -----------------------------------------------------
    ## Begin: written by Claude! -----------------------------------------------
    } else {
      nV <- length(ind_vox)
      # Build sparse adjacency matrix from ind_nbr (ignoring out-of-mask NAs).
      row_idx <- rep(seq(nV), times=6)
      col_idx <- as.vector(ind_nbr)
      keep    <- !is.na(col_idx)
      A <- Matrix::sparseMatrix(
        i=row_idx[keep], j=col_idx[keep], x=1,
        dims=c(nV, nV)
      )
      deg <- Matrix::rowSums(A)
      # Local indices for imputed vs. known voxels.
      loc_imp <- which( mask_now)
      loc_val <- which(!mask_now)

      if (length(loc_val) == 0) {
        # Return `NaN` instead of `0` if no non-NA to impute from.
        dat_now[loc_imp,] <- NaN
      } else {
        W_uu <- A[loc_imp, loc_imp] / deg[loc_imp]
        W_uk <- A[loc_imp, loc_val] / deg[loc_imp]
        LHS  <- Matrix::Diagonal(length(loc_imp)) - W_uu
        f_k  <- dat_now[loc_val,,drop=FALSE]
        RHS  <- W_uk %*% f_k
        f_u  <- Matrix::solve(LHS, RHS)
        dat_now[loc_imp,] <- as.matrix(f_u)
      }
    }
    ## End: written by Claude! -------------------------------------------------

    xifti$data$subcort <- dat_now
    rm(dat_now)
    rm(mask_now)

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
