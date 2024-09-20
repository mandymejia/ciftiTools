#' Move data locations to the subcortex mask
#'
#' Move subcortex data locations with a specific value(s) to the subcortex mask.
#'
#' @inheritParams xifti_Param
#' @param values Values to mask out. Default: \code{NA} and \code{NaN}. Data
#'  locations in the subcortex that are one of these values (across all columns)
#'  will be moved to the subcortex mask in the metadata.
#' @param drop Only used if the \code{"xifti"} has the dlabel intent. Drop the
#'  key(s) in \code{values} from the label tables, for columns in which they no
#'  longer exist? Default: \code{FALSE}.
#'
#'
#' @return The \code{"xifti"} with re-organized data and subcortex masks
#'
#' @family manipulating xifti
#'
#' @export
#'
#' @seealso move_from_submask
move_to_submask <- function(xifti, values=c(NA, NaN), drop=FALSE){
  stopifnot(is.xifti(xifti))
  values <- as.numeric(values)

  updated <- FALSE

  mask2 <- !apply(
    matrix(xifti$data$subcort %in% values, ncol=ncol(xifti)),
    1, all
  )

  if (!all(mask2)) {
    updated <- TRUE
    if (all(!mask2)) { ciftiTools_warn(paste0("All locations are being removed in the subcortex: errors in other functions may occur.")) }
    # Update metadata.
    xifti$meta$subcort$mask[xifti$meta$subcort$mask] <- mask2
    xifti$meta$subcort$labels <- xifti$meta$subcort$labels[mask2]
    # Remove from data.
    xifti$data$subcort <- xifti$data$subcort[mask2,,drop=FALSE]
  }

  if (!updated) { ciftiTools_warn("No data locations moved."); return(xifti) }

  if (drop) {
    if (!is.null(xifti$meta$cifti$intent) && xifti$meta$cifti$intent == "3007") {
      for (ii in seq(ncol(xifti))) {
        if (any(as.matrix(xifti)[,ii] %in% values)) { next }
        labtab <- xifti$meta$cifti$labels[[ii]]
        xifti$meta$cifti$labels[[ii]] <- labtab[!(labtab$Key %in% values),]
      }
    } else {
      ciftiTools_warn("Cannot `drop` labels, because the xifti does not have the dlabel intent.")
    }
  }

  if (!is.xifti(xifti)) { stop("Could not make a valid \"xifti\" object.") }
  xifti
}

#' Move data locations from subcortex mask
#'
#' Move subcortex mask locations into the subcortex data matrix by assigning
#'  them a specific value (e.g. NA).
#'
#' @inheritParams xifti_Param
#' @param new_mask The new mask, of which the current mask should be a subset.
#'  (\code{new_mask} should have more \code{TRUE} values.) The new \code{TRUE}
#'  values will be moved to the subcortex data.
#' @param value The value to assign the new locations. Default: \code{NA}.
#' @param label_value The label value to assign the new locations. Default:
#'  \code{"Other"}.
#' @param name,RGBA Only used if the \code{"xifti"} has the dlabel intent and
#'  \code{value} is not an already-existing Key. This is the name to assign to
#'  the new key for the new locations, as well as a length-four numeric
#'  vector indicating the red, green, blue, and alpha values for the color to
#'  assign to the new key. These will be reflected in the updated label table.
#'  Note that RGBA values must all be in \[0, 1\].
#'
#'  Currently, only one name and set of RGBA values are supported, meaning that
#'  the out-of-mask subcortex locations will have the same Key, name, and color
#'  across all data columns in the \code{"xifti"}. An error will occur if the 
#'  Key already exists for some columns but not others.
#'
#'  Defaults: \code{"Other"} for \code{"name"} and white with 0 alpha for
#'  \code{RGBA}.
#'
#' @return The \code{"xifti"} with re-organized data and subcortex masks
#'
#' @export
#'
#' @seealso move_to_submask
#' @seealso unmask_cortex
move_from_submask <- function(xifti, new_mask, value=NA, label_value="Other", name="Other", RGBA=c(1,1,1,0)){
  stopifnot(is.xifti(xifti))
  value <- as.numeric(value)
  if (length(value) > 1) {
    warning("Using the first entry of `value`.")
    value <- value[1]
  }

  updated <- FALSE

  old_submask <- xifti$meta$subcort$mask
  stopifnot(all(new_mask[old_submask])) # `new_mask` should contain all values in `old_submask`
  mask2 <- old_submask[new_mask]
  if (any(!mask2)) {
    updated <- TRUE
    xifti$data$subcort <- unmask_cortex(xifti$data$subcort, mask2, mwall_fill=value)
    # Update metadata
    xifti$meta$subcort$mask <- new_mask
    stopifnot(label_value %in% levels(xifti$meta$subcort$labels))
    q <- factor(
      rep(label_value, sum(new_mask)),
      levels=levels(xifti$meta$subcort$labels)
    )
    q[mask2] <- xifti$meta$subcort$labels
    xifti$meta$subcort$labels <- q
  }

  if (!updated) { ciftiTools_warn("No data locations moved."); return(xifti) }

  if (!is.null(xifti$meta$cifti$intent) && xifti$meta$cifti$intent == 3007) {
    new_key <- vapply(
      xifti$meta$cifti$labels,
      function(x){ !(value %in% x$Key) },
      FALSE
    )
    if (any(new_key)) {
      if (!all(new_key)) {
        stop(
          "The replacement `value` is an existing key for labels in some ",
          "columns, but not others. This is not yet supported. Try choosing ",
          "a `value` which is not any existing key."
        )
      }

      name <- as.character(name)
      if (length(name) > 1) {
        warning("Using the first entry of `name`.");
        name <- name[1]
      }
      RGBA <- as.numeric(RGBA)
      if (length(RGBA) != 4) {
        stop("RGBA must be a length-4 numeric vector indicating the values for red, green, blue, and alpha.")
      }
      if (any(RGBA < 0) || any(RGBA > 1)) {
        stop("RGBA values must be in [0, 1]. (Not between 0 and 255.)")
      }

      # Add a row to the label tabel for the new key.
      for (ii in seq(ncol(xifti))) {
        labtab <- xifti$meta$cifti$labels[[ii]]
        labtab <- rbind(labtab, c(value, RGBA))
        rownames(labtab)[nrow(labtab)] <- name
        labtab <- labtab[order(labtab$Key),]
        xifti$meta$cifti$labels[[ii]] <- labtab
      }
    }
  }

  if (!is.xifti(xifti)) { stop("Could not make a valid \"xifti\" object.") }
  xifti
}
