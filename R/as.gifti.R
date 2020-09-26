#' Format metric data as a \code{"gifti"} object
#'
#' Format a \eqn{V x T} numeric matrix (V vertices, T measurements) or a length-T list 
#'  of length-V numeric vectors as a \code{"gifti"} object using a template 
#'  \code{"gifti"}. The brain hemisphere (left or right) must be indicated.
#' 
#'  If \code{data} is already a \code{"gifti"} object, \code{data$data} will be
#'  used and any metadata will be overwritten by the template or discarded.
#'
#' @param data \eqn{V x T} numeric matrix, or length \eqn{T} list of length
#'  \eqn{V} numeric vectors
#' @param hemisphere The side of the brain the data represents: \code{"left"} 
#'  (default) or \code{"right"}. Used to fill the "AnatomicalStructurePrimary"
#'  metadata field.
#' @param intent The NIFTI intent, with prefix "NIFTI_INTENT_*". Default: 
#'  \code{"NONE"}. For a list of intents see 
#'  https://nifti.nimh.nih.gov/nifti-1/documentation/nifti1fields/nifti1fields_pages/group__NIFTI1__INTENT__CODES.html/document_view
#' @param data_type The NIFTI type of \code{data}: \code{"NIFTI_TYPE_INT32"} 
#'  or \code{"NIFTI_TYPE_FLOAT32"}. Can also be \code{NULL} (default), in which
#'  case the NIFTI type will be inferred from \code{data}.
#' 
#' @return The \code{"gifti"} object
#'
#' @keywords internal
#' 
as.metric_gifti <- function(
  data, hemisphere=c("left", "right"), 
  intent="NONE", data_type=NULL){

  # Get hemisphere.
  hemisphere <- match.arg(hemisphere, c("left", "right"))

  # Get intent.
  intent <- toupper(intent)
  if (startsWith(intent, "NIFTI_INTENT_")) { 
    intent <- gsub("NIFTI_INTENT_", "", intent)
  }
  if (!(intent %in% c("NONE", "NORMAL", "LABEL"))) {
    intent_short <- tolower(intent)
    ciftiTools_warn(paste0(
      "Names of data entries in \"gifti\" object will be \"", intent_short, 
      "\". This has not been verified to correspond with a NIFTI intent."
    ))
  } else {
    intent_short <- list(NONE = "unknown", NORMAL = "normal", LABEL="labels")[[intent]]
  }

  # If already a "gifti", use the $data only.
  if (is.gifti(data)) {
    hemi_idx <- names(data$file_meta) == "AnatomicalStructurePrimary"
    hemi_idx <- which(hemi_idx)[1]
    other_hemisphere <- switch(hemisphere, left="right", right="left")
    meta_hemisphere <- gsub("cortex", "", tolower(data$file_meta[[hemi_idx]]))
    if (grepl(other_hemisphere, meta_hemisphere)) {
      warning(paste0(
        "The requested hemisphere, \"", hemisphere, 
        "\", was opposite the hemisphere in the existing metadata, \"", 
        meta_hemisphere, "\"."
      ))
    }
    ciftiTools_warn("Already a \"gifti\". Using the $data element and discarding metadata.\n")
    data <- data$data
    names(data) <- rep(intent_short, length(data))
  }
  
  # Format data as a list.
  if (is.vector(data)) { data <- matrix(data, ncol=1) }
  if (suppressMessages(is.nummat(data))) {
    data <- split(t(data), seq(ncol(data)))
    data <- lapply(data, function(x){matrix(x, ncol=1)})
    names(data) <- rep(intent_short, length(data))
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
  if (!startsWith(data_type, "NIFTI_TYPE_")) { 
    data_type <- paste0("NIFTI_TYPE_", data_type)
  }
  stopifnot(data_type %in% c("NIFTI_TYPE_INT32", "NIFTI_TYPE_FLOAT32"))

  # Make data integers
  if (data_type == "NIFTI_TYPE_INT32") {
    for (ii in 1:length(data)) {
      mode(data[[ii]]) <- "integer"
    }
  }

  # Name the entries in the data by the intent.
  if (!is.null(names(data))) {
    if (!all(unique(names(data)) == intent_short)) {
      ciftiTools_warn(paste0(
        "Names of `data` must all match the intent: \"", 
        intent_short, "\". Overwriting."
      ))
    }
  }
  names(data) <- rep(intent_short, length(data))

  # Form the "gifti".
  gii <- gifti_metric_template # from ciftiTools R/sysdata.rda
  gii$data <- data
  hemi_idx <- which(names(gii$file_meta) == "AnatomicalStructurePrimary")[1]
  gii$file_meta[hemi_idx] <- switch(hemisphere, 
    left = "CortexLeft", 
    right = "CortexRight"
  )
  gii$data_meta <- gii$data_meta[rep(1, length(data))]
  gii$data_info$Intent <- intent
  gii$data_info$DataType <- data_type
  gii$data_info$n <- length(data[[1]])
  gii$data_info$Dim0 <- length(data[[1]])
  gii$data_info <- gii$data_info[rep(1, length(data)),]

  stopifnot(is.gifti(gii))
  gii
}

#' Format surface data as a \code{"gifti"} object
#'
#' Format a \code{"surface"} object or a list with elements \code{"pointset"}
#'  and \code{"triangle"} as a \code{"gifti"} object using a template 
#'  \code{"gifti"}. The brain hemisphere (left or right) must be indicated.
#'
#' @param surf A \code{"surface"} object or a list with elements 
#'  \code{"pointset"} and \code{"triangle"}
#' @param hemisphere The side of the brain the surface represents: \code{"left"} 
#'  (default) or \code{"right"}. Used to fill the "AnatomicalStructurePrimary"
#'  metadata field. Only used if \code{surf$hemisphere} is \code{NULL}.
#' 
#' @return The \code{"gifti"} object
#'
#' @keywords internal
#' 
as.surf_gifti <- function(
  surf, hemisphere=c("left", "right")){

  # --> "surface"
  surf <- make_surf(surf)
  if (is.null(surf$hemisphere)) { 
    surf$hemisphere <- match.arg(hemisphere, c("left", "right"))
  }

  # "surface" --> "gifti"
  gii <- gifti_surf_template # from ciftiTools R/sysdata.rda
  gii$data <- list(
    pointset = surf$vertices,
    triangle = surf$faces - 1
  )
  mode(gii$data$triangle) <- "integer"

  hemi_idx <- gii$data_meta[[1]][, "names"] == "AnatomicalStructurePrimary"
  gii$data_meta[[1]][hemi_idx, "vals"] <- switch(surf$hemisphere, 
    left = "CortexLeft", 
    right = "CortexRight"
  )
  gii$data_info$Dim0 <- c(nrow(gii$data$pointset), nrow(gii$data$triangle))
  gii$data_info$n <- 3 * gii$data_info$Dim0

  stopifnot(is.gifti(gii))
  gii
}