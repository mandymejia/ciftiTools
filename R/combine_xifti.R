#' Combine \code{"xifti"}s with non-overlapping brain structures
#' 
#' Combine \code{"xifti"}s with non-overlapping brain structures into
#'  a single \code{"xifti"}. The names and intent of the first will be used (if 
#'  present).
#' 
#' @param ... The \code{"xifti"} objects
#' @param xii_list Alternatively, a list of \code{"xifti"} objects. If specified,
#'  will ignore \code{...}
#' @param meta \code{"first"} (default) to just use the metadata from the first
#'  argument, or \code{"all"} to include the other metadata in a list.
#' @return A \code{"xifti"} with data from the inputs
#' 
#' @family manipulating
#' @export
#' 
combine_xifti <- function(..., xii_list=NULL, meta=c("first", "all")) {
  meta <- match.arg(meta, c("first", "all"))
  if (!is.null(xii_list)) { xiis <- xii_list } else { xiis <- list(...) }

  # Check validity of inputs. --------------------------------------------------
  if (length(xiis) > 3) { 
    stop(
      "Inputs must be `xifti` objects with non-overlapping brain structures.", 
      "So, there should be 3 or less inputs."
    )
  }
  # Each input must be a \code{"xifti"}.
  bad_xiis <- !vapply(xiis, is.xifti, FALSE, messages=FALSE)
  if (any(bad_xiis)) {
    is.xifti(xiis[[which(bad_xiis)[1]]])
    stop_msg <- paste0(
      "Arguments at these indices were not `xifti` objects: ", 
      paste(which(bad_xiis), collapse=", "), "."
    )
    if (sum(bad_xiis) >= 1) {
      stop_msg <- paste(
        stop_msg, 
        "The error message for the first invalid xifti is printed above."
      )
    }
    stop(stop_msg)
  }

  # Combine. -------------------------------------------------------------------
  out <- xiis[[1]]
  # Handle `dlabel`. 
  dlabel_out <- (!is.null(out$meta$cifti$intent)) && (out$meta$cifti$intent == 3007)
  if (dlabel_out) {
    dlabel_vals <- unique(do.call(c, lapply(out$meta$cifti$labels, function(x){x$Key})))
  }
  T_ <- ncol_xifti(out)
  for (ii in seq(2, length(xiis))) {
    # More validity checks.
    if (ncol_xifti(xiis[[ii]]) != T_) {
      stop(
        "The input at index ", ii, " had ", ncol_xifti(xiis[[ii]]), 
        " columns, whereas the first input had ", T_, " columns."
      )
    }
    bsidx_out <- !vapply(out$data, is.null, FALSE)
    bsidx_ii <- !vapply(xiis[[ii]]$data, is.null, FALSE)
    if (any(bsidx_out & bsidx_ii)) {
      bs_both <- c("left cortex", "right cortex", "subcortex")[bsidx_out & bsidx_ii]
      stop("These brain structures were present in more than one argument: ", paste(bs_both, collapse=", "), ".")
    }

    # Intent.
    dlabel_ii <- (!is.null(xiis[[ii]]$meta$cifti$intent)) && (xiis[[ii]]$meta$cifti$intent == 3007)
    if (dlabel_out && (!dlabel_ii)) {
      xiis[[ii]] <- convert_to_dlabel(xiis[[ii]], values=dlabel_vals)
    }

    # Combine data.
    if (!is.null(xiis[[ii]]$data$cortex_left)) {
      out$data$cortex_left <- xiis[[ii]]$data$cortex_left
      if (!is.null(xiis[[ii]]$meta$cortex$medial_wall_mask$left)) {
        out$meta$cortex$medial_wall_mask$left <- xiis[[ii]]$meta$cortex$medial_wall_mask$left
      }
    }
    if (!is.null(xiis[[ii]]$data$cortex_right)) {
      out$data$cortex_right <- xiis[[ii]]$data$cortex_right
      if (!is.null(xiis[[ii]]$meta$cortex$medial_wall_mask$right)) {
        out$meta$cortex$medial_wall_mask$right <- xiis[[ii]]$meta$cortex$medial_wall_mask$right
      }
    }
    if (!is.null(xiis[[ii]]$data$subcort)) {
      out$data$subcort <- xiis[[ii]]$data$subcort
      out$meta$subcort <- xiis[[ii]]$meta$subcort
    }
  }

  # Combine metadata.
  if (meta == "all") {
    if (!is.null(out$meta$cifti$misc$combined)) {
      warning("`$meta$cifti$misc$combined` will be overwritten.")
    }
    out$meta$cifti$misc$combined <- lapply(xiis, function(x){ x$meta$cifti })
  }

  stopifnot(is.xifti(out))

  out
}