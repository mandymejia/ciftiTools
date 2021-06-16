#' Write CIFTI cortex data to GIFTI
#' 
#' Write the data for the left or right cortex to a metric GIFTI file.
#'
#' @param x A \eqn{V x T} data matrix (V vertices, T measurements). This can also 
#'  be an object from \code{gifti::readgii}, or a length \eqn{T} list of length 
#'  \eqn{V} vectors. 
#' @param gifti_fname Where to write the GIFTI file.
#' @param hemisphere \code{"left"} (default) or \code{"right"}. Ignored if 
#'  \code{data} is already a \code{"gifti"} object.
#' @param intent "NIFTI_INTENT_*". \code{NULL} (default) will use
#'  metadata if \code{data} is a \code{"gifti"} object, or "NONE" if it cannot be 
#'  inferred. If not \code{NULL} and \code{data} is a \code{"gifti"} object, it will
#'  overwrite the existing intent. See 
#'  https://nifti.nimh.nih.gov/nifti-1/documentation/nifti1fields/nifti1fields_pages/group__NIFTI1__INTENT__CODES.html/document_view .
#' @param data_type the type of \code{data}:
#'  "NIFTI_TYPE_*" where * is "INT32" or "FLOAT32". If \code{NULL} (default), the 
#'  data type will be inferred. If not \code{NULL} and \code{data} is a 
#'  \code{"gifti"} object, it will overwrite the existing data type.
#' @param encoding One of "ASCII", "Base64Binary", or "GZipBase64Binary". If 
#'  \code{NULL} (default), will use the metadata if \code{data} is a GIFTI object,
#'  or "ASCII" if the \code{data_type} is "NIFTI_TYPE_INT32" and
#'  "GZipBase64Binary" if the \code{data_type} is "NIFTI_TYPE_FLOAT32". If not 
#'  \code{NULL} and \code{data} is a \code{"gifti"} object, it will overwrite the 
#'  existing data type.
#' @param endian "LittleEndian" (default) or "BigEndian". If \code{data} is a 
#'  \code{"gifti"} object, it will overwrite the existing endian.
#' @param col_names The names of each data column in \code{gii} (or entries in 
#'  \code{gii$data}).
#' @param label_table A data.frame with labels along rows. The row names should
#'  be the label names. The column names should be among: "Key", "Red", "Green", 
#'  "Blue", and "Alpha". The "Key" column is required whereas the others are 
#'  optional (but very often included). Values in the "Key" column should be 
#'  non-negative integers, typically beginning with 0. The other columns should
#'  be floating-point numbers between 0 and 1.
#' 
#'  Although CIFTI files support a different label table for each data column,
#'  GIFTI files only support a single label table. So this label table should be
#'  applicable to each data column.
#' 
#' @return Whether the GIFTI was successfully written
#'
#' @importFrom gifti writegii
#' @export
write_metric_gifti <- function(
  x, gifti_fname, hemisphere=c("left", "right"),
  intent=NULL, data_type=NULL, encoding=NULL, endian=c("LittleEndian", "BigEndian"),
  col_names=NULL, label_table=NULL){

  # Match args.
  hemisphere <- match.arg(hemisphere, c("left", "right"))
  endian <- match.arg(endian, c("LittleEndian", "BigEndian"))

  # If gii is a "gifti", use its metadata to determine unspecified options.
  if (is.gifti(x)) { 
    if (is.null(intent)) { intent <- x$data_info$Intent }
    if (is.null(data_type)) { data_type <- x$data_info$DataType } 
    if (is.null(encoding)) { encoding <- x$data_info$Encoding }

  # If x is not a "gifti", convert it to a GIFTI and use default options
  #   where unspecified.
  } else {

    if (is.null(intent)) { intent <- "NONE" }
    
    x <- as.metric_gifti(x, intent=intent)

    if (is.null(data_type)) { 
      data_type <- ifelse(all_integers(do.call(cbind, x$data)), "INT32", "FLOAT32")
    }
    if (is.null(encoding)) { 
      encoding <- ifelse(grepl("INT", data_type), "ASCII", "GZipBase64Binary") 
    }
  }

  T_ <- length(x$data)

  if (data_type=="INT32") {
    for (ii in 1:T_) { mode(x$data[[ii]]) <- "integer" }
  }
  
  # Format options
  x$data_info$Intent <- paste0("NIFTI_INTENT_", gsub("NIFTI_INTENT_", "", toupper(intent)))
  x$data_info$DataType <- paste0("NIFTI_TYPE_", gsub("NIFTI_TYPE_", "", toupper(data_type)))
  x$data_info$Encoding <- encoding
  x$data_info$Endian <- endian
  hemisphere_idx <- which(names(x$file_meta)=="AnatomicalStructurePrimary")[1]
  x$file_meta[hemisphere_idx] <- list(left="CortexLeft", right="CortexRight")[hemisphere]

  # Column Names
  if (!is.null(col_names)) {
    col_names <- as.character(col_names)
    if (length(col_names) != T_) {
      stop("The length of the data `col_names` must be the same length as the data (number of columns).")
    }

    for (ii in 1:T_) {
      if (length(x$data_meta) < T_) {break}
      stopifnot(is.matrix(x$data_meta[[ii]]))
      if (ncol(x$data_meta[[ii]]) == 2 && all(sort(colnames(x$data_meta[[ii]])) == sort(c("names", "vals")))) {
        md_names <- x$data_meta[[ii]][,colnames(x$data_meta[[ii]]) == "names"]
        if ("Name" %in% md_names) {
          if (x$data_meta[[ii]][which(md_names=="Name")[1],2] != "") {
            ciftiTools_warn(paste0("Replacing the existing data column name for column ", ii))
          }
          x$data_meta[[ii]][which(md_names=="Name")[1],2] <- col_names[ii]
        } else {
          x$data_meta[[ii]] <- rbind(x$data_meta[[ii]], c("names"="Name", "vals"=x$data_meta[[ii]]))
        }
      } else {
        ciftiTools_warn(paste0("Data meta entry for data column ", ii, "did not have the expected columns `names` and `vals`. Overwriting."))
        x$data_meta[[ii]] <- matrix(c("Name", col_names[ii]), nrow=1)
        colnames(x$data_meta[[ii]]) <- c("names", "vals")
      }
    }
  }

  # Label Table
  if (!is.null(label_table)) {
    ## Must be a matrix or data.frame
    stopifnot(is.matrix(label_table) || is.data.frame(label_table))

    ## Column names
    if (length(unique(colnames(label_table))) != length(colnames(label_table))) {
      stop("Label table column names must be unique.")
    }
    if (!all(colnames(label_table) %in% c("Key", "Red", "Green", "Blue", "Alpha"))) {
      stop("Label table columns must be among: `Key` (required), `Red`, `Green`, `Blue`, and `Alpha`.")
    }
    if (!("Key" %in% colnames(label_table))) { stop("`Key` column is required in the label table.") }

    ## Data type and values
    if (data_type != "INT32") {
      warning("The data type was not INT32, yet there is a label table (with integer keys). Writing the GIFTI anyway.\n")
    } else {
      label_vals <- as.numeric(label_table[,colnames(label_table) == "Key"])
      data_vals <- unique(as.vector(do.call(cbind, x$data)))
      if (!all(data_vals %in% c(NA, label_vals))) {
        stop(paste0("These data values were not in the label table:", paste(data_vals[!(data_vals %in% label_vals)], collapse=", ")))
      }
    }

    label_table[,] <- as.matrix(apply(label_table, 2, as.character))
    x$label <- label_table
  }

  writegii(x, gifti_fname, use_parsed_transformations=TRUE)
}

#' Write CIFTI surface data to GIFTI
#' 
#' Write the data for the left or right surface to a surface GIFTI file.
#'
#' @param x A \code{"surf"} object, an object from \code{gifti::readgii}, or a 
#'  list with elements "pointset" and "triangle".
#' @param gifti_fname Where to write the GIFTI file.
#' @param hemisphere "left" (default) or "right". Ignored if \code{data} is already
#'  a "gifti" object, or if it is a \code{"surf"} object with the hemisphere metadata
#'  already specified.
#' @param encoding A length-2 vector with elements chosen among "ASCII", 
#'  "Base64Binary", and "GZipBase64Binary". If \code{NULL} (default), will use 
#'  the metadata if \code{data} is a "gifti" object, or "GZipBase64Binary" for the
#'  "pointset" and "ASCII" for the "triangles" if \code{data} is not already
#'  a GIFTI.
#' @param endian "LittleEndian" (default) or "BigEndian".
#'
#' @return Whether the GIFTI was successfully written
#'
#' @importFrom gifti writegii
#' @export
write_surf_gifti <- function(
  x, gifti_fname, hemisphere=c("left", "right"),
  encoding=NULL, endian=c("LittleEndian", "BigEndian")){

  # Match args.
  hemisphere <- match.arg(hemisphere, c("left", "right"))
  endian <- match.arg(endian, c("LittleEndian", "BigEndian"))

  # If gii is a "gifti", use its metadata to determine unspecified options.
  if (is.gifti(x)) { 
    if (is.null(encoding)) { encoding <- x$data_info$Encoding }

  # If gii is not a "gifti", convert it to a GIFTI and use default options
  #   where unspecified.
  } else {
    x <- as.surf_gifti(x, hemisphere=hemisphere)
    if (is.null(encoding)) { 
      encoding <- as.character(list(pointset="GZipBase64Binary", triangle="ASCII")[names(x$data)])
    }
  }

  # Format options
  x$data_info$Endian <- endian
  x$data_info$Encoding <- encoding

  # Issue #5
  tri_enc <- x$data_info$Encoding[names(x$data) == "triangle"]
  if (tri_enc != "ASCII") {
    ciftiTools_warn(paste(
      "The encoding for the triangle component was", tri_enc, 
      "but only ASCII is supported for integer data types.",
      "Overwriting.\n"
    ))
    x$data_info$Encoding[names(x$data) == "triangle"] <- "ASCII"
  }

  writegii(x, gifti_fname, use_parsed_transformations=TRUE)
}