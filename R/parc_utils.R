#' Apply parcellation to CIFTI data
#' 
#' Applies a function (default: mean) to a \code{"xifti"} object for each parcel
#'  separately.
#' 
#' @param dat Am \code{"xifti"} object.
#' @param parc A single-column "dlabel" \code{"xifti"} object in-register with
#'  \code{dat}.
#' @param FUN The function to apply. It must return a single value. Default: 
#'  \code{mean}. 
#' @param return_as \code{"table"} (default) or \code{"xifti"}.
#' @return If \code{return_as} is \code{"table"}: A two-column matrix with the 
#'  parcel keys in the first column, and the corresponding value in the second. 
#'  Row names are the parcel labels.
#' 
#'  If \code{return_as} is \code{"xifti"}: A \code{"xifti"} in-register with
#'  \code{dat} in which the value at each location is the value of the
#'  corresponding parcel.
#' @export
parc_apply <- function(dat, parc, FUN=mean, return_as=c("table", "xifti")){
  # Check arguments.
  stopifnot(is.xifti(dat))
  parc <- assure_parc(parc)
  stopifnot(is.function(FUN))
  return_as <- match.arg(return_as, c("table", "xifti"))

  # `dat` and `parc` must be in-register.
  # Note: this assumes a cortex is aligned if it has the same number of 
  #   vertices, and that the subcortex is aligned as long as it has the same 
  #   number of voxels.
  stopifnot(identical(
    vapply(dat$data, nrow, 0), vapply(parc$data, nrow, 0)
  ))

  dat <- as.matrix(dat)
  parc_cols <- parc$meta$cifti$labels[[1]]
  parc_vec <- c(as.matrix(parc))

  # Quicker way, if calculating the mean
  if (identical(FUN, mean)) {
    vals <- parc_mean_mat(parc) %*% dat
  } else {
    vals <- vector("numeric", nrow(parc_cols))
    for (vv in seq(length(vals))) {
      key_vv <- parc_cols$Key[vv]
      vals[vv] <- FUN(dat[parc_vec == key_vv,])
    }
  }

  out <- cbind(parc_cols$Key, vals)
  rownames(out) <- rownames(parc_cols)
  out
}

#' Convert parcellation values to \code{"xifti"}
#' 
#' From a parcellation and a corresponding value vector, make a \code{"xifti"}
#'  object that has the values of each parcel across its locations.
#' 
#' @param parc A single-column "dlabel" \code{"xifti"} object.
#' @param vals A numeric vector. Each element should correspond to the row in
#'  the color table of \code{parc} at the same index. 
#' @return A \code{"xifti"} object
#' @export 
#' 
parc_vals_to_xifti <- function(parc, vals){
  parc <- assure_parc(parc)
  stopifnot(is.numeric(vals))
  stopifnot(nrow(parc) == length(vals))

  parc_cols <- parc$meta$cifti$labels[[1]]
  parc_vec <- c(as.matrix(parc))

  out <- convert_xifti(parc, "dscalar")
  out_vec <- parc_vec

  for (vv in seq(nrow(parc_cols))) {
    key_vv <- parc_cols$Key[vv]
    out_vec[parc_vec == key_vv] <- vals[vv]
  }

  newdata_xifti(out, out_vec)
}

#' Make parcellation mean matrix
#' 
#' Create a matrix that compute the average value for each parcel. 
#'
#' From a single-column "dlabel" \code{"xifti"} object, make a \eqn{K \times V}
#'  matrix where row \eqn{k} is a vector corresponding to the kth Key value,
#'  with \eqn{1/k_i} at locations with that key value, and \eqn{0} elsewhere.
#'  \eqn{k_i} is the total number of locations inside Key \eqn{k}.
#'  So, the sum of each row will be \eqn{1}, and each column will only
#'  have exactly one non-zero value (because each location only has one Key
#'  value) except medial wall locations will have all zeroes.
#'
#'  Multiplying the result of \code{parc_mean_mat} with a \eqn{V \times T} matrix
#'  of timeseries data, e.g. from \code{as.matrix(dlabel_xii)}, will yield the
#'  average value for each Key.
#'
#' @param parc A single-column "dlabel" \code{"xifti"} object.
#' @return The parcellation matrix
#' @export
#'
parc_mean_mat <- function(parc){
  parc <- assure_parc(parc)

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
  parc <- assure_parc(parc)
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

#' Assure this is a parcellation
#' 
#' Assure an input \code{"xifti"} object represents a parcellation. Keep only
#'  the first column if multiple columns are present.
#' 
#' @param parc The putative parcellation.
#' @return \code{parc}, if it's a parcellation.
#' @export
assure_parc <- function(parc){
  stopifnot(is.xifti(parc))
  stopifnot(!is.null(parc$meta$cifti$intent))
  stopifnot(parc$meta$cifti$intent == 3007)
  if (ncol(parc) > 1) {
    warning("Using first column of `parc`.")
    parc <- select_xifti(parc, idx=1)
  }
  parc
}

#' Parcellation borders
#' 
#' Identify vertices which lie on the border of different parcels.
#' 
#' @param parc Integer vector the same length as the number of vertices. Each
#'  entry indicates the parcel that vertex belongs to.
#' @param surf The surface which the vertices belong to, or just the \code{"faces"}
#'  component (\eqn{F \times 3} matrix where each row indicates the vertices which
#'  comprise a face). If not provided, the (resampled) default \code{hemisphere} 
#'  surface included with \code{ciftiTools} will be used.
#' @param hemisphere Only used to choose which default surface to use if 
#'  \code{is.null(surf)}. Should be \code{"left"} (default) or \code{"right"}.
#' 
#' @return Logical vector the same length as \code{parc} indicating if the
#'  vertex lies on a border.
#' 
#' @export
parc_borders <- function(parc, surf=NULL, hemisphere=c("left", "right")) {
  stopifnot(is.vector(parc))
  parc <- as.numeric(as.factor(parc))

  if (is.null(surf)) {
    hemisphere <- match.arg(hemisphere, c("left", "right"))
    surf <- load_surf(hemisphere)
    if (nrow(surf$vertices) != length(parc)) {
      surf <- resample_surf(surf, length(parc))
      if (nrow(surf$vertices) != length(parc)) {
        stop(paste(
          "`parc` has", length(parc), "vertices; the surface could not be resampled to match."
        ))
      }
    }
  }

  x <- matrix(parc[as.vector(surf$faces)], ncol=ncol(surf$faces))
  x <- (x[,1] != x[,2]) | (x[,2] != x[,3])
  x <- unique(as.vector(surf$faces[x,]))
  isBorder <- rep(FALSE, length(parc))
  isBorder[x] <- TRUE
  isBorder
}