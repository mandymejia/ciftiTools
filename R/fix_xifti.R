#' Fix a \code{"xifti"}
#' 
#' Make adjustments to a putative \code{"xifti"} so that it is valid. Each
#'  adjustment is reported.
#' 
#' Right now it: coerces the data to numeric matrices; adds the
#'  "Other" level to the subcortex labels.
#' 
#' @inheritParams xifti_Param
#' @param verbose Report each adjustment? Default: \code{TRUE}
#' @return The fixed \code{"xifti"}
#' 
#' @export
#' 
fix_xifti <- function(xifti, verbose=TRUE) {
  
  # Coerce the data to numeric matrices. 
  bs <- names(xifti$data)[!vapply(xifti$data, is.null, FALSE)]
  for (b in bs) {
    if (!is.matrix(xifti$data[[b]])) {
      cat("Coercing", b, "data to a matrix with one column\n")
      xifti$data[[b]] <- as.matrix(xifti$data[[b]])
    }
    if (!is.numeric(xifti$data[[b]])) {
      cat("Coercing", b, "data to numeric.\n")
      class(xifti$data[[b]]) <- "numeric"
    }
  }

  # Add "Other" level to subcortex labels. 
  if (!is.null(xifti$data$subcort)) {
    sub_levs <- levels(xifti$meta$subcort$labels)
    if (length(sub_levs) == 21 && all(sub_levs == substructure_table()$ciftiTools_Name[seq(21)])) {
      xifti$meta$subcort$labels <- factor(
        xifti$meta$subcort$labels,
        levels = substructure_table()$ciftiTools_Name
      )
    }
  }

  xifti
}