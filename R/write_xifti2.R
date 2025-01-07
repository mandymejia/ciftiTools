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
#' @family writing
#' @export
#'
write_xifti2 <- function(
  xifti,
  brainstructures="existing",
  cortexL_fname=NULL, cortexR_fname=NULL,
  subcortVol_fname=NULL, subcortLabs_fname=NULL,
  ROI_brainstructures="all",
  ROIcortexL_fname=NULL, ROIcortexR_fname=NULL,
  ROIsubcortVol_fname=NULL, write_dir=NULL,
  verbose=FALSE) {

  # [DEV NOTE] This function is written in a similar way to `separate_cifti`.
  # If making changes here, consider making parallel changes there.

  # [TO DO] write label GIFTI from dlabel CIFTI?

  # Check `xifti`.
  stopifnot(is.xifti(xifti))

  # Get numerical intent. Use scalar file if no intent is present.
  if (is.null(xifti$meta$cifti$intent)) { xifti$meta$cifti$intent <- 3006 }
  intent_cifti <- xifti$meta$cifti$intent

  # Check which brainstructures are present.
  bs_present <- c("left", "right", "subcortical")[!vapply(xifti$data, is.null, FALSE)]

  # Reconcile `brainstructures` with `bs_present`.
  # Get output files: which ones to write, and their names.
  x <- separate_cifti_files(
    bs_present=bs_present,
    intent = intent_cifti,
    brainstructures=brainstructures,
    cortexL_fname=cortexL_fname, cortexR_fname=cortexR_fname,
    subcortVol_fname=subcortVol_fname, subcortLabs_fname=subcortLabs_fname,
    ROI_brainstructures=ROI_brainstructures,
    ROIcortexL_fname=ROIcortexL_fname, ROIcortexR_fname=ROIcortexR_fname,
    ROIsubcortVol_fname=ROIsubcortVol_fname,
    write_dir=write_dir
  )
  do <- x$do; ROI_do <- x$ROI_do; sep_fnames <- x$sep_fnames; rm(x)

  if (all(!do)) { message("Nothing to write."); return(invisible(NULL)) }

  # Check for and modify multiple columns with different label tables.
  if (intent_cifti == 3007) {
    intent_gii <- "label"
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
  } else if (intent_cifti == 3006) {
    col_names <- xifti$meta$cifti$names
    intent_gii <- data_type <- label_table <- NULL
  } else {
    intent_gii <- data_type <- label_table <- col_names <- NULL
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
    if (!is.null(intent_gii) && intent_gii=="label" && ROI_do["left"]) {
      cdat[!mwall] <- cdat[mwall][1]
    }
    # Write data and ROI.
    write_metric_gifti(
      cdat, sep_fnames["cortexL"], "left", data_type = data_type,
      intent=intent_gii, label_table = label_table, col_names = col_names
    )
    if (ROI_do["left"]) {
      write_metric_gifti(
        as.numeric(mwall), sep_fnames["ROIcortexL"],
        "left", data_type = "FLOAT32"
      )
    }
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
    if (!is.null(intent_gii) && intent_gii=="label" && ROI_do["right"]) {
      cdat[!mwall] <- cdat[mwall][1]
    }

    # Write data and ROI.
    write_metric_gifti(
      cdat, sep_fnames["cortexR"], "right", data_type = data_type,
      intent=intent_gii, label_table = label_table, col_names = col_names
    )
    if (ROI_do["right"]) {
      write_metric_gifti(
        as.numeric(mwall), sep_fnames["ROIcortexR"],
        "right", data_type = "FLOAT32"
      )
    }
  }

  ## Subcortex: unmask to get volumetric array.
  if (do["subVol"] | do["subLab"]) {
    if (verbose) {cat("Writing subcortical data and labels.\n")}

    # Handle case where just one of these files are requested.
    sep_fnames2 <- sep_fnames
    if (is.na(sep_fnames2["subcortVol"])) {
      sep_fnames2["subcortVol"] <- tempfile(fileext=".subVol.nii")
    }
    if (is.na(sep_fnames2["subcortLabs"])) {
      sep_fnames2["subcortLabs"] <- tempfile(fileext=".subLab.nii")
    }

    write_subcort_nifti(
      xifti$data$subcort,
      xifti$meta$subcort$labels,
      xifti$meta$subcort$mask,
      xifti$meta$subcort$trans_mat,
      xifti$meta$subcort$trans_units,
      xifti$meta$cifti$names,
      label_table,
      sep_fnames2["subcortVol"],
      sep_fnames2["subcortLabs"],
      sep_fnames["ROIsubcortVol"],
      fill=0
    )
  }

  invisible(sep_fnames)
}
