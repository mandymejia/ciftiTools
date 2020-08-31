#' Convert to metric GIFTI
#'
#' Convert a V x T data matrix (V vertices, T measurements) or a length-T list 
#'  of length-V vectors to a metric "gifti" object.
#'
#' @param data A V x T data matrix or length-T list of length-V vectors
#' @param side "left" (default) or "right".
#' @param intent "NIFTI_INTENT_*". Default: "NONE". See https://nifti.nimh.nih.gov/nifti-1/documentation/nifti1fields/nifti1fields_pages/group__NIFTI1__INTENT__CODES.html/document_view
#' @param data_type the type of \code{data}:
#'  "NIFTI_TYPE_INT32" or "NIFTI_TYPE_FLOAT32". If \code{NULL} (default), the 
#'  data_type will be inferred. 
#' @return The metric "gifti"
#'
#' @export
as.metric_gifti <- function(
  data, side=c("left", "right"), 
  intent="NONE", data_type=NULL){
  
  # Get side.
  side <- match.arg(side, c("left", "right"))

  # Get intent.
  intent <- toupper(intent)
  if (!(intent %in% c("NONE", "NORMAL"))) {
    intent_short <- tolower(intent)
    warning(paste0(
      "Names of data columns will be", intent_short, 
      ". This has not been verified to match the standard column names."
    ))
  } else {
    intent_short <- list(NONE="unknown", NORMAL="normal")[[intent]]
  }

  # If already a "gifti", use the $data only.
  if (is.gifti(data)) {
    side_idx <- names(data$file_meta) == "AnatomicalStructurePrimary"
    side_idx <- which(side_idx)[1]
    other_side <- list(right="left", left="right")[side]
    meta_side <- gsub("cortex", "", tolower(data$file_meta[[side_idx]]))
    if (grepl(other_side, meta_side)) {
      stop(paste0(
        "The requested side, ", side, 
        ", was opposite the side in the metadata, ", 
        meta_side, "."
      ))
    }
    warning("Already a \"gifti\". Using the $data element and discarding metadata.\n")
    data <- data$data
    names(data) <- rep(intent_short, length(data))
  }

  # Format data as a list.
  if (suppressMessages(is.nummat(data))) {
    data <- split(t(data), seq(ncol(data)))
    names(data) = rep(intent_short, length(data))
  }
  if (!is.list(data)) {
    stop("data must be a numeric matrix or a list of numeric vectors.")
  }
  stopifnot(length(unique(sapply(data, length))) == 1)

  # Get data type.
  if (is.null(data_type)) {
    data_type <- ifelse(
      all_integers(do.call(cbind, data)),
      "NIFTI_TYPE_INT32",
      "NIFTI_TYPE_FLOAT32"
    )
  }
  stopifnot(length(data_type) == 1)
  stopifnot(data_type %in% c("NIFTI_TYPE_INT32", "NIFTI_TYPE_FLOAT32"))

  # Name the entries in data by intent.
  if (!is.null(names(data))) {
    if (!all(unique(names(data)) == intent_short)) {
      warning(paste0(
        "Names of `data` must all match the intent: ", intent_short, ". Overwriting."
      ))
    }
  }
  names(data) <- rep(intent_short, length(data))

  # Form the "gifti".
  gii <- gifti_metric_template # from ciftiTools sysdata
  gii$data <- data
  side_idx <- which(names(gii$file_meta)=="AnatomicalStructurePrimary")[1]
  gii$file_meta[side_idx] <- list(left="CortexLeft", right="CortexRight")[side]
  gii$data_meta <- gii$data_meta[rep(1, length(data))]
  gii$data_info$Intent <- intent
  gii$data_info$DataType <- data_type
  gii$data_info$n <- length(data[[1]])
  gii$data_info$Dim0 <- length(data[[1]])
  gii$data_info <- gii$data_info[rep(1, length(data)),]

  stopifnot(is.gifti(gii))
  gii
}

#' Convert to surface GIFTI
#'
#' Convert a "surface" object or a list with elements "pointset" and 
#'  "triangle" to a surface "gifti" object.
#'
#' @param surf A "surface" object or a list with elements "pointset" and 
#'  "triangle"
#' @param side "left" (default) or "right".
#' @return The surface "gifti"
#'
#' @export
as.surf_gifti <- function(
  surf, side=c("left", "right")){

  # Get side.
  side <- match.arg(side, c("left", "right"))

  # If already a "gifti", use the $data only.
  if (is.gifti(surf)) {
    side_idx <- surf$data_meta[[1]][,"names"] == "AnatomicalStructurePrimary"
    side_idx <- which(side_idx)[1]
    other_side <- list(right="left", left="right")[side]
    meta_side <- gsub("cortex", "", tolower(surf$data_meta[[1]][side_idx, "vals"]))
    if (grepl(other_side, meta_side)) {
      stop(paste0(
        "The requested side, ", side, 
        ", was opposite the side in the metadata, ", 
        meta_side, "."
      ))
    }
    warning("Already a \"gifti\". Using the $data element and discarding metadata.\n")
    surf <- surf$data
  }

  # Format data as a list with elements "pointset" and "triangle"
  if (suppressMessages(is.surf(surf))) {
    # start indexing at 0 instead of 1
    surf <- list(pointset = surf$vertices, triangle = surf$faces)
  }
  if (!( length(surf)==2 || all(names(surf) %in% c("pointset", "triangle") ))) {
    stop("`surf` must be a \"surface\" object or a list with elements \"pointset\" and \"triangle\".")
  }
  if (min(surf$triangle)==1) { surf$triangle <- surf$triangle - 1 }

  # Form the "gifti".
  gii <- gifti_surf_template # from ciftiTools sysdata
  gii$data <- surf
  mode(gii$data$triangle) <- "integer"

  side_idx <- gii$data_meta[[1]][,"names"] == "AnatomicalStructurePrimary"
  gii$data_meta[[1]][side_idx, "vals"] <- list(left="CortexLeft", right="CortexRight")[[side]]
  gii$data_info$Dim0 <- c(nrow(surf$pointset), nrow(surf$triangle))
  gii$data_info$n <- 3 * gii$data_info$Dim0

  stopifnot(is.gifti(gii))
  gii
}