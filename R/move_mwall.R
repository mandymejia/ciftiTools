#' Move to medial wall
#' 
#' Move cortical data locations with a specific value(s) to the medial wall mask.
#'  For example, dlabel files often have a Key value for the medial wall and no
#'  medial wall mask. This function can move data locations with that key from
#'  the data matrix to the medial wall mask metadata.
#' 
#' @inheritParams xifti_Param
#' @param values Medial wall values. Default: \code{NA} and \code{NaN}. Data
#'  locations in the left and right cortex that are one of these values (across 
#'  all columns) will be moved to the medial wall mask in the metadata.
#' @param drop Only used if the \code{"xifti"} has the dlabel intent. Drop the
#'  Key(s) in \code{values} from the label tables, for columns in which they no
#'  longer exist? Default: \code{FALSE}.
#'  
#' 
#' @return The \code{"xifti"} with re-organized data and medial wall
#' 
#' @export
#' 
#' @seealso move_from_mwall
move_to_mwall <- function(xifti, values=c(NA, NaN), drop=FALSE){
  stopifnot(is.xifti(xifti))
  values <- as.numeric(values)

  updated <- FALSE

  for (h in c("left", "right")) {
    ch <- paste("cortex", h, sep="_")

    if (!is.null(xifti$data[[ch]])) {
      # Get the new medial wall mask.
      new_mwall <- !apply(
        matrix(xifti$data[[ch]] %in% values, ncol=ncol(xifti)), 
        1, all
      )

      if (!all(new_mwall)) {
        updated <- TRUE
        # Update medial wall.
        old_mwall <- xifti$meta$cortex$medial_wall_mask[[h]]
        if (!is.null(old_mwall)) {
          xifti$meta$cortex$medial_wall_mask[[h]][old_mwall] <- new_mwall
        } else {
          xifti$meta$cortex$medial_wall_mask[[h]] <- new_mwall
        }
        # Remove from data.
        xifti$data[[ch]] <- xifti$data[[ch]][new_mwall,,drop=FALSE]
      }
    }
  }

  if (!updated) { ciftiTools_warn("No data locations moved."); return(xifti) }

  if (drop) {
    if (xifti$meta$cifti$intent == "3007") {
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

#' Move from medial wall
#' 
#' Move all medial wall locations into the cortical data by assigning them a
#'  specific value (e.g. NA).
#' 
#' @inheritParams xifti_Param
#' @param value The value to assign the medial wall locations. Default: \code{NA}.
#' @param name,RGBA Only used if the \code{"xifti"} has the dlabel intent and
#'  \code{value} is not an already-existing Key. This is the name to assign to
#'  the new key for the medial wall locations, as well as a length-four numeric
#'  vector indicating the red, green, blue, and alpha values for the color to
#'  assign to the new key. These will be reflected in the updated label table.
#'  Note that RGBA values must all be in \[0, 1\].
#' 
#'  Currently, only one name and set of RGBA values are supported, meaning that
#'  the medial wall locations will have the same Key, name, and color across
#'  all data columns in the \code{"xifti"}. An error will occur if the Key
#'  already exists for some columns but not others.
#' 
#'  Defaults: \code{"Medial_Wall"} for \code{"name"} and white with 0 alpha for
#'  \code{RGBA}.
#' 
#' @return The \code{"xifti"} with re-organized data and medial wall
#' 
#' @export
#' 
#' @seealso move_to_mwall
#' @seealso unmask_cortex
move_from_mwall <- function(xifti, value=NA, name="Medial_Wall", RGBA=c(1,1,1,0)){
  stopifnot(is.xifti(xifti))
  value <- as.numeric(value)
  if (length(value) > 1) {
    warning("Using the first entry of `value`.")
    value <- value[1]
  }

  updated <- FALSE

  for (h in c("left", "right")) {
    ch <- paste("cortex", h, sep="_")

    old_mwall <- xifti$meta$cortex$medial_wall_mask[[h]]
    if (!is.null(old_mwall)) {
      updated <- TRUE
      old_data <- xifti$data[[ch]]
      xifti$data[[ch]] <- unmask_cortex(xifti$data[[ch]], old_mwall, mwall_fill=value)
      xifti$meta$cortex$medial_wall_mask[h] <- list(NULL)
    }

  }

  if (!updated) { ciftiTools_warn("No data locations moved."); return(xifti) }

  if (xifti$meta$cifti$intent == 3007) {
    new_key <- lapply(xifti$meta$cifti$labels, function(x){ value %in% x$Key })
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