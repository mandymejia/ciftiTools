#' Make parcellation matrix
#'
#' From a single-column "dlabel" \code{"xifti"} object, make a \eqn{K \times V}
#'  matrix where row \eqn{k} is a vector corresponding to the kth Key value,
#'  with \eqn{1/k_i} at locations with that key value, and \eqn{0} elsewhere.
#'  \eqn{k_i} is the total number of locations inside Key \eqn{k}.
#'  So, the sum of each row will be \eqn{1}, and each column will only
#'  have exactly one non-zero value (because each location only has one Key
#'  value) except medial wall locations will have all zeroes.
#'
#'  Multiplying the result of \code{parc_matrix} with a \eqn{V \times T} matrix
#'  of timeseries data, e.g. from \code{as.matrix(dlabel_xii)}, will yield the
#'  average value for each Key.
#'
#' @param parc A single-column "dlabel" \code{"xifti"} object.
#' @return The parcellation matrix
#' @export
#'
parc_matrix <- function(parc){

  # `parc` should be a `dlabel` CIFTI with one column.
  stopifnot(is.xifti(parc))
  stopifnot(!is.null(parc$meta$cifti$intent))
  stopifnot(parc$meta$cifti$intent == 3007)
  if (ncol(parc) > 1) {
    warning("Using first column of `parc`.")
    parc <- select_xifti(parc, idx=1)
  }

  parc_cols <- parc$meta$cifti$labels[[1]]
  parc_labs <- rownames(parc_cols)

  nV <- nrow(parc)
  nK <- nrow(parc_cols)

  # Begin to make `parc_mat`: vertices along rows, parcels along columns
  pmat <- matrix(0, nrow=nK, ncol=nV)
  pmat[c(1+as.matrix(parc)) + seq(0, nV-1)*(nK)] <- 1
  # table(colSums(pmat)) # all ones

  # By dividing each row by its sum, we can use matrix multiplication to compute the mean fMRI signal
  pmat / rowSums(pmat)
}

#' Add subcortex to cortical parcellation
#'
#' Add the subcortex, with each brain structure as a separate parcel, to
#'  a "dlabel" cortical parcellation.
#'
#' @param parc A single-column "dlabel" \code{"xifti"} object without 
#'  subcortical data.
#' @param parc_sub A single-column \code{"xifti"} object with only
#'  subcortical data. Or, \code{"MNI"} (default) to read in and use the MNI
#'  subcortex included in \code{ciftiTools}. (The Connectome Workbench is 
#'  required.)
#' @return The new parcellation with added subcortical data and labels.
#' @export
#'
parc_add_subcortex <- function(parc, parc_sub="MNI"){
  # `parc` should be a `dlabel` CIFTI with one column, without subcortical data.
  stopifnot(is.xifti(parc))
  stopifnot(!is.null(parc$meta$cifti$intent))
  stopifnot(parc$meta$cifti$intent == 3007)
  if (ncol(parc) > 1) {
    warning("Using first column of `parc`.")
    parc <- select_xifti(parc, idx=1)
  }
  stopifnot(is.null(parc$data$subcort))
  labtab <- parc$meta$cifti$labels[[1]]

  if (identical(parc_sub, "MNI")) {
    parc_sub <- load_sub_parc()
  } else {
    stopifnot(is.xifti(parc_sub))
    stopifnot(is.null(parc_sub$data$cortex_left))
    stopifnot(is.null(parc_sub$data$cortex_right))
  }
  labtab_sub <- parc_sub$meta$cifti$labels[[1]]

  # Add new labels for subcortex keys to cortex parcellation
  last_cortex_key <- max(labtab$Key)
  stopifnot(min(labtab_sub$Key)>0)
  parc <- convert_xifti(
    parc,
    "dlabel",
    levels_old = c(
      labtab$Key,
      last_cortex_key + labtab_sub$Key
    ),
    labels = c(
      rownames(labtab),
      rownames(labtab_sub)
    ),
    colors = c(
      rgb(labtab$Red, labtab$Green, labtab$Blue),
      rgb(labtab_sub$Red, labtab_sub$Green, labtab_sub$Blue)
    ),
    add_white=FALSE
  )

  # Add subcortex to cortex parcellation
  parc_sub$meta$cifti$labels[[1]] <- parc$meta$cifti$labels[[1]]
  parc_sub <- parc_sub + last_cortex_key
  parc <- combine_xifti(parc, parc_sub)
}

#' Load subcortical parcellation
#'
#' Load the MNI subcortical parcellation included in \code{ciftiTools} as a
#'  "dlabel" \code{"xifti"} object.
#'
#' Colors are based on https://surfer.nmr.mgh.harvard.edu/fswiki/FsTutorial/AnatomicalROI/FreeSurferColorLUT
#'
#' @return The subcortical parcellation
#' @keywords internal
load_sub_parc <- function(){
  # Table of colors for each subcortical brain structure
  # Source: https://surfer.nmr.mgh.harvard.edu/fswiki/FsTutorial/AnatomicalROI/FreeSurferColorLUT
  # Modification: cerebellum was lightened and thalamus was darkened, to enhance contrast between the two
  SLabs <- c(
    ACCUMBENS_LEFT = "#007b7d",
    ACCUMBENS_RIGHT = "#007b7d",
    AMYGDALA_LEFT = "#dcd814",
    AMYGDALA_RIGHT = "#dcd814",
    BRAIN_STEM = "#ee9349",
    CAUDATE_LEFT = "#30998f",
    CAUDATE_RIGHT = "#30998f",
    CEREBELLUM_LEFT = "#d27eb9",
    CEREBELLUM_RIGHT = "#d27eb9",
    DIENCEPHALON_VENTRAL_LEFT = "#f4be91",
    DIENCEPHALON_VENTRAL_RIGHT = "#f4be91",
    HIPPOCAMPUS_LEFT = "#fefb7d",
    HIPPOCAMPUS_RIGHT = "#fefb7d",
    PALLIDUM_LEFT = "#7fd4d6",
    PALLIDUM_RIGHT = "#7fd4d6",
    PUTAMEN_LEFT = "#adf4fe",
    PUTAMEN_RIGHT = "#adf4fe",
    THALAMUS_LEFT = "#6e74c9",
    THALAMUS_RIGHT = "#6e74c9"
  )
  # Rename the brain structures
  SLabs2 <- substructure_table()
  names(SLabs) <- SLabs2[match(names(SLabs), SLabs2$Original_Name), "ciftiTools_Name"]

  # Load the subcortex `xifti` (it's a dscalar)
  xii_sub <- read_cifti(
    ciftiTools.files()$cifti["dscalar_ones"], brainstructures="sub"
  )

  # Convert to dlabel and return.
  keys_sub <- xii_sub$meta$subcort$labels
  xii_sub$data$subcort[] <- as.numeric(keys_sub)
  stopifnot(identical(levels(keys_sub), c("Cortex-L", "Cortex-R", names(SLabs))))
  convert_to_dlabel(
    xii_sub,
    levels = seq(length(SLabs)),
    labels = names(SLabs),
    colors = as.character(SLabs),
    add_white = FALSE
  )
}
