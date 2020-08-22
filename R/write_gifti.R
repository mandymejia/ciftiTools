#' Write CIFTI Cortex Data to GIFTI
#' 
#' Write the data for the left or right cortex to a metric GIFTI file.
#'
#' @param gii A V x T data matrix (V vertices, T measurements). This can also 
#'  be an object from \code{gifti::readgii}, or a length-T list of length-V 
#'  vectors. 
#' @param gifti_fname Where to write the GIFTI file.
#' @param side "left" (default) or "right". Ignored if \code{data} is already
#'  a "gifti" object.
#' @param intent "NIFTI_INTENT_*". \code{NULL} (default) will use
#'  metadata if \code{data} is a "gifti" object, or "NONE" if it cannot be 
#'  inferred. If not \code{NULL} and \code{data} is a "gifti" object, it will
#'  overwrite the existing intent. See 
#'  https://nifti.nimh.nih.gov/nifti-1/documentation/nifti1fields/nifti1fields_pages/group__NIFTI1__INTENT__CODES.html/document_view .
#' @param data_type the type of \code{data}:
#'  "NIFTI_TYPE_*" where * is "INT32" or "FLOAT32". If \code{NULL} (default), the 
#'  data type will be inferred. If not \code{NULL} and \code{data} is a "gifti" 
#'  object, it will overwrite the existing data type.
#' @param encoding One of "ASCII", "Base64Binary", or "GZipBase64Binary". If 
#'  \code{NULL} (default), will use the metadata if \code{data} is a GIFTI object, or
#'  "ASCII" if the \code{data_type} is "NIFTI_TYPE_INT32" and
#'  "GZipBase64Binary" if the \code{data_type} is "NIFTI_TYPE_FLOAT32". If not 
#'  \code{NULL} and \code{data} is a "gifti" object, it will overwrite the 
#'  existing data type.
#' @param endian "LittleEndian" (default) or "BigEndian". If \code{data} is a 
#'  "gifti" object, it will overwrite the existing endian.
#'
#' @return Whether the GIFTI was successfully written
#'
#' @importFrom gifti write_gifti
#' @export
write_metric_gifti <- function(
  gii, gifti_fname, side=c("left", "right"),
  intent=NULL, data_type=NULL, encoding=NULL, endian=c("LittleEndian", "BigEndian")){

  # Match args.
  side <- match.arg(side, c("left", "right"))
  endian <- match.arg(endian, c("LittleEndian", "BigEndian"))

  # If gii is a "gifti", use its metadata to determine unspecified options.
  if (is.gifti(gii)) { 
    if (is.null(intent)) { intent <- gii$data_info$Intent }
    if (is.null(data_type)) { data_type <- gii$data_info$DataType } 
    if (is.null(encoding)) { encoding <- gii$data_info$Encoding }

  # If gii is not a "gifti", convert it to a GIFTI and use default options
  #   where unspecified.
  } else {
    gii <- as.metric_gifti(gii)

    if (is.null(intent)) { 
      intent <- "NONE" 
    }
    if (is.null(data_type)) { 
      data_type <- ifelse(all_integers(do.call(cbind, gii$data)), "INT32", "FLOAT32")
    }
    if (is.null(encoding)) { 
      encoding <- ifelse(grepl("INT", data_type), "ASCII", "GZipBase64Binary") 
    }
  }

  # Format options
  gii$data_info$Intent <- paste0("NIFTI_INTENT_", gsub("NIFTI_INTENT_", "", toupper(intent)))
  gii$data_info$DataType <- paste0("NIFTI_TYPE_", gsub("NIFTI_TYPE_", "", toupper(data_type)))
  gii$data_info$Encoding <- encoding
  gii$data_info$Endian <- endian
  side_idx <- which(names(gii$file_meta)=="AnatomicalStructurePrimary")[1]
  gii$file_meta[side_idx] <- list(left="CortexLeft", right="CortexRight")[side]

  write_gifti(gii, gifti_fname, use_parsed_transformations=TRUE)
}

#' Write CIFTI Surface Data to GIFTI
#' 
#' Write the data for the left or right surface to a surface GIFTI file.
#'
#' @param gii A "surface" object, an object from \code{gifti::readgii}, or a 
#'  list with elements "pointset" and "triangle".
#' @param gifti_fname Where to write the GIFTI file.
#' @param side "left" (default) or "right". Ignored if \code{data} is already
#'  a "gifti" object.
#' @param encoding A length-2 vector with elements chosen among "ASCII", 
#'  "Base64Binary", and "GZipBase64Binary". If \code{NULL} (default), will use 
#'  the metadata if \code{data} is a "gifti" object, or "GZipBase64Binary" for the
#'  "pointset" and "ASCII" for the "traingles" if \code{data} is not already
#'  a GIFTI.
#' @param endian "LittleEndian" (default) or "BigEndian".
#'
#' @return Whether the GIFTI was successfully written
#'
#' @importFrom gifti write_gifti
#' @export
write_surf_gifti <- function(
  gii, gifti_fname, side=c("left", "right"),
  encoding=NULL, endian=c("LittleEndian", "BigEndian")){

  # Match args.
  side <- match.arg(side, c("left", "right"))
  endian <- match.arg(endian, c("LittleEndian", "BigEndian"))

  # If gii is a "gifti", use its metadata to determine unspecified options.
  if (is.gifti(gii)) { 
    if (is.null(encoding)) { encoding <- gii$data_info$Encoding }

  # If gii is not a "gifti", convert it to a GIFTI and use default options
  #   where unspecified.
  } else {
    gii <- as.surf_gifti(gii, side=side)
    if (is.null(encoding)) { 
      encoding <- as.character(list(pointset="GZipBase64Binary", triangle="ASCII")[names(gii$data)])
    }
  }

  # Format options
  gii$data_info$Endian <- endian
  gii$data_info$Encoding <- encoding

  # Issue #5
  tri_enc <- gii$data_info$Encoding[names(gii$data) == "triangle"]
  if (tri_enc != "ASCII") {
    warning(paste(
      "The encoding for the triangle component was", tri_enc, 
      "but only ASCII is supported for integer data types.",
      "Overwriting.\n"
    ))
    gii$data_info$Encoding[names(gii$data) == "triangle"] <- "ASCII"
  }

  write_gifti(gii, gifti_fname, use_parsed_transformations=TRUE)
}