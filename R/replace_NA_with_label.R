#' Replace \code{NA} values in dlabel \code{"xifti"} with a new label.
#' 
#' \code{NaN} values will be converted to \code{NA} and also replaced.
#' 
#' @param xifti The \code{"xifti"}
#' @param NA_value The new label value for \code{NA} values. If \code{NULL}
#'  (default), use -1 if all existing label values are non-negative, otherwise
#'  use one less than the minimum existing label value. 
#' @param NA_label The new label name for \code{NA} values. Default: \code{"NA"}.
#' @param NA_color The new label color for \code{NA} values, as a hex code with
#'  alpha. Default: \code{"#FFFFFFFF"}.
#' @param idx Numeric vector indicating the \code{"xifti"} columns to add 
#'  \code{NA} labels for. Default: \code{NULL} (add to all columns).
#' 
#' @return \code{xifti} with a new label value for \code{NA} values
#' @keywords internal
replace_NA_with_label <- function(
  xifti, NA_value=NULL, NA_label="NA", NA_color="#FFFFFFFF", idx=NULL) {

  stopifnot(is.xifti(xifti))
  nC <- ncol(xifti)

  if (is.null(NA_value)) {
    labVals <- unique(do.call(c, lapply(xifti$meta$cifti$labels, '[[', "Key")))
    NA_value <- if (all(labVals >= 0)) { -1 } else { min(labVals) - 1 }
  }

  # Make new row for NA label
  col_table <- col2rgb(NA_color, alpha=TRUE)/255
  col_table <- rbind(NA_value, col_table)
  rownames(col_table) <- c("Key", "Red", "Green", "Blue", "Alpha")
  col_table <- as.data.frame(t(col_table))
  rownames(col_table) <- NA_label

  # Add this row to each label table
  if (is.null(idx)) { idx <- seq(nC) }
  for (cc in idx) {
    if(NA_value %in% c(xifti$meta$cifti$labels[[cc]]$Key)) {
      stop("`NA_value`, ", NA_value, " already exists for column ", cc, ".")
    }
    xifti$meta$cifti$labels[[cc]] <- rbind(col_table, xifti$meta$cifti$labels[[cc]])
  }

  # Replace NA values with the new value
  q <- q2 <- as.matrix(xifti)
  q[is.na(q)] <- NA_value
  q2[,idx] <- q[,idx]
  xifti <- newdata_xifti(xifti, q2)

  stopifnot(is.xifti(xifti))
  xifti
}