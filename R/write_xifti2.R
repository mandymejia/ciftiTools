#' Write a \code{"xifti"} object to GIFTI and NIFTI files
#'
#' Write metric or label GIFTIs for the cortical surface data and NIFTIs for the
#'  subcortical labels and mask in a \code{"xifti"} object. Each present
#'  brainstructure will be written; if a brainstructure is absent the
#'  corresponding file is not written.
#'
#' @inheritParams xifti_Param
#' @inheritParams separate_cifti_files
#' @inheritParams verbose_Param_FALSE
#'
#' @return List of written files
#' @importFrom RNifti writeNifti
#'
#' @export
#'
write_xifti2 <- function(
  xifti,
  brainstructures=NULL,
  cortexL_fname=NULL, cortexR_fname=NULL,
  subcortVol_fname=NULL, subcortLabs_fname=NULL,
  ROI_brainstructures="all",
  ROIcortexL_fname=NULL, ROIcortexR_fname=NULL,
  ROIsubcortVol_fname=NULL, write_dir=NULL,
  verbose=FALSE) {

  # [TO DO] write label GIFTI from dlabel CIFTI?

  # Check arguments.
  stopifnot(is.xifti(xifti))
  bs_present <- c("left", "right", "subcortical")[!vapply(xifti$data, is.null, FALSE)]
  if (is.null(brainstructures)) {
    brainstructures <- bs_present
  }
  brainstructures <- match_input(
    brainstructures, c("left","right","subcortical","all"),
    user_value_label="brainstructures"
  )
  if ("all" %in% brainstructures) {
    brainstructures <- c("left","right","subcortical")
  }

  # Write scalar files if intent is not specified.
  if (is.null(xifti$meta$cifti$intent)) { xifti$meta$cifti$intent <- 3006 }

  # Get output files: which ones to write, and their names
  x <- separate_cifti_files(
    intent = xifti$meta$cifti$intent,
    brainstructures=brainstructures,
    cortexL_fname=cortexL_fname, cortexR_fname=cortexR_fname,
    subcortVol_fname=subcortVol_fname, subcortLabs_fname=subcortLabs_fname,
    ROI_brainstructures=ROI_brainstructures,
    ROIcortexL_fname=ROIcortexL_fname, ROIcortexR_fname=ROIcortexR_fname,
    ROIsubcortVol_fname=ROIsubcortVol_fname, write_dir=write_dir
  )
  do <- x$do; ROI_do <- x$ROI_do; sep_fnames <- x$sep_fnames; rm(x)

  # Check brainstructures
  if (!all(brainstructures %in% bs_present)) {
    warning(paste0(
      "Only the following brainstructures are present in the CIFTI file: ",
      paste(bs_present, collapse=", "), "\n"
    ))
    brainstructures <- bs_present
  }
  do <- c("left","right","subcortical") %in% brainstructures
  names(do) <- c("left", "right", "sub")

  if (!is.null(xifti$meta$cifti$intent) && xifti$meta$cifti$intent == 3007) {
    intent <- "label"
    data_type <- "INT32"
    label_table <- xifti$meta$cifti$labels
    # [TO DO] check if this is actually necessary
    if (length(label_table) > 1) {
      if (length(unique(label_table)) > 1) {
        warning(paste(
          "CIFTI files support a different label table for each column,",
          "but GIFTI & NIFTI files only support a single label table. Writing",
          "the `xifti` requires exporting to GIFTI & NIFTI files.",
          "Using the label table for the first column."
        ))
      }
    }
    label_table <- label_table[[1]]
    col_names <- names(xifti$meta$cifti$labels)
  } else if (!is.null(xifti$meta$cifti$intent) && xifti$meta$cifti$intent == 3006) {
    col_names <- xifti$meta$cifti$names
    intent <- data_type <- label_table <- NULL
  } else {
    intent <- data_type <- label_table <- col_names <- NULL
  }

  # Left cortex
  if (do["left"]){
    if (verbose) {cat("Writing left cortex.\n")}
    # Add back medial wall.
    if (is.null(xifti$meta$cortex$medial_wall_mask$left)) {
      mwall <- rep(TRUE, nrow(xifti$data$cortex_left))
    } else {
      mwall <- xifti$meta$cortex$medial_wall_mask$left
    }
    cdat <- unmask_cortex(xifti$data$cortex_left, mwall)
    # `NA` values create another variable level without a Key--bad!
    if (!is.null(intent) && intent=="label" && ROI_do["left"]) {
      cdat[!mwall] <- cdat[mwall][1]
    }
    # Write data and ROI.
    write_metric_gifti(
      cdat, sep_fnames["cortexL"], "left", data_type = data_type,
      intent=intent, label_table = label_table, col_names = col_names
    )
    if (ROI_do["left"]) {
      write_metric_gifti(
        as.numeric(mwall), sep_fnames["ROIcortexL"],
        "left", data_type = "FLOAT32"
      )
    }
  } else {
    sep_fnames <- sep_fnames[!grepl("cortexL", names(sep_fnames))]
  }

  # Right cortex
  if (do["right"]){
    if (verbose) {cat("Writing right cortex.\n")}
    # Add back medial wall.
    if (is.null(xifti$meta$cortex$medial_wall_mask$right)) {
      mwall <- rep(TRUE, nrow(xifti$data$cortex_right))
    } else {
      mwall <- xifti$meta$cortex$medial_wall_mask$right
    }
    cdat <- unmask_cortex(xifti$data$cortex_right, mwall)
    # `NA` values create another variable level without a Key--bad!
    if (!is.null(intent) && intent=="label" && ROI_do["right"]) {
      cdat[!mwall] <- cdat[mwall][1]
    }

    # Write data and ROI.
    write_metric_gifti(
      cdat, sep_fnames["cortexR"], "right", data_type = data_type,
      intent=intent, label_table = label_table, col_names = col_names
    )
    if (ROI_do["right"]) {
      write_metric_gifti(
        as.numeric(mwall), sep_fnames["ROIcortexR"],
        "right", data_type = "FLOAT32"
      )
    }
  } else {
    sep_fnames <- sep_fnames[!grepl("cortexR", names(sep_fnames))]
  }

  ## Subcortex: unmask to get volumetric array.
  if (do["sub"]) {
    if (verbose) {cat("Writing subcortical data and labels.\n")}
    write_subcort_nifti(
      xifti$data$subcort,
      xifti$meta$subcort$labels,
      xifti$meta$subcort$mask,
      xifti$meta$subcort$trans_mat,
      xifti$meta$subcort$trans_units,
      xifti$meta$cifti$names,
      label_table,
      sep_fnames["subcortVol"],
      sep_fnames["subcortLabs"],
      sep_fnames["ROIsubcortVol"],
      fill=0
    )
  } else {
    sep_fnames <- sep_fnames[!grepl("subcort", names(sep_fnames))]
  }

  invisible(sep_fnames)
}
