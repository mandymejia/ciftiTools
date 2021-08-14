#' Fix GIFTI medial wall
#' 
#' Replace implicit medial wall values (not indicated in ROI, but are in 
#'  \code{mwall_values}) with explicit medial wall values (indicated in ROI)
#'  in a metric GIFTI file.
#' 
#' @param metric_fname File path to the data GIFTI
#' @param fixed_metric_fname File path to the revised data GIFTI
#' @param ROI_fname File path to the ROI GIFTI
#' @param fixed_ROI_fname File path to the revised ROI GIFTI
#' @param mwall_values The values to use to infer the medial wall. Default: 
#'  \code{c(NA, NaN)}.
#' 
#' @return \code{NULL}, invisibly
#' 
#' @importFrom gifti readgii writegii
#' 
#' @keywords internal
#' 
fix_gifti_mwall <- function(
  metric_fname, fixed_metric_fname, 
  ROI_fname, fixed_ROI_fname, mwall_values=c(NA, NaN)){
  
  stopifnot(length(mwall_values) > 0)

  gii_ROIcortex <- readgii(ROI_fname)
  if (all(as.logical(gii_ROIcortex$data[[1]]))) {
    # Infer a new mwall if the current is empty.
    warn_msg <- "The ROI mask is empty (all `TRUE`) indicating no medial wall."
    gii_cortex <- readgii(metric_fname)
    new_mwall <- !apply(
      matrix(
        do.call(cbind, gii_cortex$data) %in% mwall_values, 
        ncol=length(gii_cortex$data)
      ), 
      1, all
    )

    # If the new mwall is not empty...
    if (!all(new_mwall)) {
      warn_msg <- paste(warn_msg, "A new medial wall mask will be inferred from the vertices with values matching `mwall_values`.")
      # Revise ROI
      gii_ROIcortex$data$normal[,] <- as.numeric(new_mwall)
      writegii(gii_ROIcortex, fixed_ROI_fname)
      ## Set mwall values to zero. Not sure if this is necessary.
      for (ii in seq_len(length(gii_cortex$data))) {
        gii_cortex$data[[ii]][,][!new_mwall] <- 0
      }
      writegii(gii_cortex, fixed_metric_fname)
    } else {
      warn_msg <- paste(warn_msg, "The data did not have any vertices with values constantly in `mwall_values`. Leaving the ROI/medial wall mask empty.")
    }
    ciftiTools_warn(warn_msg)
  }

  invisible(NULL) 
}