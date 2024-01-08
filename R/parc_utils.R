#' Apply function over locations in each parcel
#'
#' Apply a function across all locations in each parcel, for a pair of data and
#'  parcellation \code{"xifti"} objects that are in registration with one
#'  another. By default, the mean value in each parcel is calculated.
#'
#' @param xii The \code{"xifti"} data to apply the function over, within each
#'  parcel.
#' @param parc The \code{"xifti"} "dlabel" parcellation. Each parcel is defined
#'  by a unique key in the label table. If there are multiple columns, only the
#'  first column will be used. Alternatively, \code{parc} can just be a vector
#'  of keys whose length is the number of data locations in \code{"xii"}.
#' @param FUN A function that takes as input an \eqn{M \times N} matrix (\eqn{M}
#'  locations in a given parcel, and \eqn{N} measurements/columns in \code{xii})
#'  and outputs a constant-sized (\eqn{Q}) numeric vector. Default: \code{mean}.
#' @param mwall_value If there is a medial wall in \code{xii}, what should value
#'  should medial wall locations be replaced with prior to calculation?
#'  Default: \code{NA}.
#' @param return_as \code{"matrix"} (default) where each row corresponds to a
#'  parcel, or a \code{"xifti"} object where each location's value is the value
#'  of its corresponding parcel?
#' @param ... Additional arguments to \code{FUN}, e.g. \code{na.rm=TRUE}.
#'  Ignored if \code{FUN=="quick_mean"}.
#'
#' @return A \eqn{P \times Q} matrix, where \eqn{P} is the number of parcels and
#'  \eqn{Q} is the length of the output of \code{FUN}. (For \code{mean},
#'  \eqn{Q = 1}).
#'
#' @export
#'
apply_parc <- function(xii, parc, FUN=mean, mwall_value=NA,
  return_as=c("matrix", "xifti"), ...){

  # Arg checks.
  stopifnot(is.xifti(xii))
  parc <- assure_parc(parc)
  return_as <- match.arg(return_as, c("matrix", "xifti"))

  # Replace medial wall.
  xii <- move_from_mwall(xii, value=mwall_value)

  # Convert `xifti` to matrix.
  if (nrow(xii) != nrow(parc)) {
    stop(
      "`xii` has ", nrow(xii), " locations (including any medial wall), but ",
      "`parc` has ", nrow(parc), " locations. They need to have the same resolution."
    )
  }
  stopifnot(identical(
    vapply(xii$data[!vapply(xii$data, is.null, FALSE)], nrow, 0),
    vapply(parc$data[!vapply(parc$data, is.null, FALSE)], nrow, 0)
  ))
  xii <- as.matrix(xii)

  # Convert `parc` to vector.
  parc_names <- rownames(parc$meta$cifti$labels[[1]])
  parc_keys <- parc$meta$cifti$labels[[1]]$Key
  parc_vec <- c(as.matrix(parc))

  nP <- length(parc_keys)
  nV <- nrow(xii)
  nT <- ncol(xii)

  # Do `FUN`.
  stopifnot(is.function(FUN))
  out <- vector("list", nP)
  names(out) <- parc_names
  for (pp in seq(nP)) {
    out[pp] <- FUN(xii[parc_vec==parc_keys[pp],], ...)
  }

  # Check that the output length is the same for each parcel.
  stopifnot(length(unique(lapply(out, dim)))==1)

  out <- do.call(rbind, out)

  # Convert is applicable.
  if (return_as=="xifti") { out <- parc_vals_to_xifti(parc, out) }

  out
}

#' Convert parcellation values to \code{"xifti"}
#'
#' From a parcellation and a corresponding value matrix, make a \code{"xifti"}
#'  object that has the value vector of each parcel across its locations.
#'
#' @param parc A single-column "dlabel" \code{"xifti"} object.
#' @param vals A numeric matrix. Rows should correspond to rows in
#'  the color table of \code{parc}. Columns will become columns in the output
#'  \code{"xifti"} object.
#' @return A \code{"xifti"} object
#' @export
#'
parc_vals_to_xifti <- function(parc, vals){
  parc <- assure_parc(parc)
  stopifnot(is.numeric(vals))
  if (is.vector(vals)) { vals <- as.matrix(vals) }

  parc_cols <- parc$meta$cifti$labels[[1]]
  parc_vec <- c(as.matrix(parc))
  parc_vec2 <- as.numeric(factor(parc_vec, levels=parc_cols$Key))

  stopifnot(nrow(parc_cols) == nrow(vals))

  rownames(vals) <- rownames(parc_cols)

  out <- vals[parc_vec2,]
  newdata_xifti(convert_xifti(parc, "dscalar"), out)
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
#' @keywords internal
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
