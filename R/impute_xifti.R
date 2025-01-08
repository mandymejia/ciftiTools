#' Impute \code{"xifti"} data
#'
#' Impute masked values using the values of adjacent, non-masked locations.
#'
# \code{NA} values not included in \code{mask} will be ignored, but an error
#  will be raised if a vertex in \code{mask} is surrounded by \code{NA} values
#  not included in \code{mask}.
#'
#' For the purpose of imputation, medial wall vertices will be temporarily set
#'  to \code{NA}.
#'
#' @param xifti A \code{"xifti"} object. The corresponding surface must be
#'  included for each cortex with data. \code{\link{add_surf}} can be used to
#'  add HCP S1200 surfaces.
#' @param mask A logical vector whose length matches the number of rows in
#'  \code{xifti}, indicating which vertices in \code{xifti} to impute. If
#'  \code{NULL} (default), will use the mask of vertices with at least one
#'  \code{NA} and \code{NaN} value across the columns of \code{xifti}.
#' @param impute_FUN The function to use to impute the values. It should accept
#'  a vector of numeric values (the values of a vertex's neighbors) and return
#'  a single numeric value (the value to assign). Default: \code{mean}.
#' @param ... Additional arguments to \code{impute_FUN}.
#'
#' @return The input \code{xifti} with imputed data values.
#'
#' @keywords internal
#'
impute_xifti <- function(xifti, mask=NULL, impute_FUN=mean, ...) {
  stopifnot(is.xifti(xifti))
  if (!is.null(xifti$meta$cifti$intent))

  nR <- nrow(xifti)
  nC <- ncol(xifti)

  if (is.null(mask)) {
    mask <- apply(is.na(as.matrix(xifti)), 1, any)
  }

  stopifnot(is.logical(mask))
  if (length(mask) != nR) {
    stop("The length of `mask` should match the number of rows in `xifti`.")
  }

  max_width <- Inf

  # Handle medial wall. --------------------------------------------------------
  mwall_og <- xifti$meta$cortex$medial_wall_mask
  # Unapply the medial wall mask to the input mask.
  mask2 <- as.logical(do.call(c, mwall_og))
  mask2[mask2] <- mask
  mask <- mask2
  rm(mask2)
  # Set mwall values to `NA` for now.
  xifti <- move_from_mwall(xifti)

  # Split `mask` by brain structure. -------------------------------------------
  mask_bs <- list(
    cortex_left = NULL,
    cortex_right = NULL,
    subcort = NULL
  )
  for (bs in c("cortex_left", "cortex_right", "subcort")) {
    if (!is.null(xifti$data[[bs]])) {
      mask_bs[[bs]] <- mask[seq(nrow(xifti$data[[bs]]))]
      mask <- mask[seq(nrow(xifti$data[[bs]])+1, length(mask))]
    }
  }

  # Cortex. --------------------------------------------------------------------
  for (hemi in c("left", "right")) {
    c_hemi <- paste0("cortex_", hemi)
    if (is.null(xifti$data[[c_hemi]])) { next }
    if (is.null(xifti$surf[[c_hemi]])) {
      stop("The ", hemi, " surface is needed for imputing ", hemi , " cortex data.")
    }

    dat_now <- xifti$data[[c_hemi]]
    mask_now <- mask_bs[[c_hemi]]
    for (rr in seq(nR)) { # really a while(TRUE) but I figured this is safer.
      ## Compute the imputed values. ---------------
      # `i_mask`: verts to impute bordering at least one vert not being imputed
      i_mask <- boundary_mask_surf(xifti$surf[[c_hemi]]$faces, !mask_now, 1)
      if (sum(i_mask) < 1) { break }
      # verbose <- TRUE
      # if (verbose) { cat("Imputing", sum(i_mask), hemi, "cortex values.\n") }
      # `j_mask`: verts not being imputed bordering at least one vert being imputed
      j_mask <- boundary_mask_surf(xifti$surf[[c_hemi]]$faces, mask_now, 1)
      # `v_adj`: adjacency matrix between `i_mask` and `j_mask`
      v_adj <- vert_adjacency(xifti$surf[[c_hemi]]$faces, i_mask, j_mask)
      stopifnot(all(rowSums(v_adj) > 0))
      # `v_vals`: for each vert to impute, the values from which to impute.
      #   All vertices in `v_vals` should have at least one value, but there may
      #   be `NA` vals from the medial wall or if the original data was `NA`.
      v_vals <- apply(v_adj, 1, function(x){dat_now[which(j_mask)[x],,drop=FALSE]}, simplify=FALSE)
      # `v_impv`: The imputed values.
      v_impv <- lapply(v_vals, function(x){apply(x, 2, impute_FUN, ...)})
      v_impv <- do.call(rbind, v_impv)

      # ## Check for values that repeatedly could not be imputed. ----------
      # # `i_stillNA`: vertices that were attempted to impute, but are `NA`.
      # i_stillNA <- which(mask_bs[[c_hemi]])[i_mask][!apply(is.na(v_impv), 1, any)]
      # i_stillNA_again <- i_stillNA[i_stillNA %in% i_wasNA]
      # if (length(i_stillNA_again) > 0) {
      #   warning("Some vertices could not be imputed for the ", hemi,
      #     " cortex. Returning the indices of these vertices. Please remove ",
      #     "them from the mask of vertices to impute, or modify `impute_FUN`.")
      #   return(i_stillNA_again)
      # }
      # i_wasNA <- i_stillNA

      ## Update. -------
      # Set imputed values.
      dat_now[i_mask,] <- v_impv
      # Remove imputed verts from mask of verts to impute.
      mask_now[i_mask] <- FALSE
    }

    xifti$data[[c_hemi]][mask_bs[[c_hemi]],] <- dat_now[mask_bs[[c_hemi]],]
    rm(dat_now)
    rm(mask_now)

    # Put the medial wall back.
    if (!is.null(mwall_og[[hemi]])) {
      xifti$data[[c_hemi]] <- xifti$data[[c_hemi]][mwall_og[[hemi]],,drop=FALSE]
      xifti$meta$cortex$medial_wall_mask[[hemi]] <- mwall_og[[hemi]]
    }
  }

  # Subcortex. -----------------------------------------------------------------
  if (!is.null(xifti$data$subcort)) { stop("Subcortex data imputation not implemented yet.") }

  xifti
}
