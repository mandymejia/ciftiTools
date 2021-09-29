#' The NIFTI intents supported by \code{ciftiTools}
#'
#' Table of CIFTI file types (NIFTI intents) supported by \code{ciftiTools}.
#'
#' See https://www.nitrc.org/forum/attachment.php?attachid=334&group_id=454&forum_id=1955
#'  for information about the different NIFTI intents.
#'
#' @return A \code{data.frame} with each supported file type along the rows, and
#'  column names "extension", "intent_code", "value", and "intent_name"
#'
#' @export
#'
supported_intents <- function(){
  df <- data.frame(rbind(
    c("dtseries.nii", "NIFTI_INTENT_CONNECTIVITY_DENSE_SERIES",   3002, "ConnDenseSeries"),
    c("dscalar.nii",  "NIFTI_INTENT_CONNECTIVITY_DENSE_SCALARS",  3006, "ConnDenseScalar"),
    c("dlabel.nii",   "NIFTI_INTENT_CONNECTIVITY_DENSE_LABELS",   3007, "ConnDenseLabel")
  ))
  colnames(df) <- c("extension", "intent_code", "value", "intent_name")
  df
}

#' Substructure table
#'
#' Table of labels for cortex hemispheres (left and right) and subcortical
#'  substructures. The names used by the CIFTI format and the names used by
#'  \code{ciftiTools} are given.
#'
#' The names used by \code{ciftiTools} are based on those in
#'  \code{FT_READ_CIFTI} from the FieldTrip MATLAB toolbox.
#'
#' @return A data.frame with each substructure along the rows. The first
#'  column gives the CIFTI format name and the second column gives the
#'  \code{ciftiTools} name.
#'
#' @export
#'
substructure_table <- function(){
  table <- data.frame(rbind(
    c("CORTEX_LEFT",                "Cortex-L"),
    c("CORTEX_RIGHT",               "Cortex-R"),
    c("ACCUMBENS_LEFT",             "Accumbens-L"),
    c("ACCUMBENS_RIGHT",            "Accumbens-R"),
    c("AMYGDALA_LEFT",              "Amygdala-L"),
    c("AMYGDALA_RIGHT",             "Amygdala-R"),
    c("BRAIN_STEM",                 "Brain Stem"),
    c("CAUDATE_LEFT",               "Caudate-L"),
    c("CAUDATE_RIGHT",              "Caudate-R"),
    c("CEREBELLUM_LEFT",            "Cerebellum-L"),
    c("CEREBELLUM_RIGHT",           "Cerebellum-R"),
    c("DIENCEPHALON_VENTRAL_LEFT",  "Diencephalon-L"),
    c("DIENCEPHALON_VENTRAL_RIGHT", "Diencephalon-R"),
    c("HIPPOCAMPUS_LEFT",           "Hippocampus-L"),
    c("HIPPOCAMPUS_RIGHT",          "Hippocampus-R"),
    c("PALLIDUM_LEFT",              "Pallidum-L"),
    c("PALLIDUM_RIGHT",             "Pallidum-R"),
    c("PUTAMEN_LEFT",               "Putamen-L"),
    c("PUTAMEN_RIGHT",              "Putamen-R"),
    c("THALAMUS_LEFT",              "Thalamus-L"),
    c("THALAMUS_RIGHT",             "Thalamus-R")
  ))
  colnames(table) <- c("Original_Name", "ciftiTools_Name")
  table$Index <- seq_len(nrow(table))
  table
}

#' Check CIFTI type
#'
#' Check that a CIFTI's NIFTI intent matches its file name extension.
#'
#' @param intent The NIFTI \code{intent_code}, as a numeric integer
#' @param extn The file name extension, e.g. "dtseries.nii"
#'
#' @return If the intent is supported, returns \code{TRUE}.
#'  If the intent is not supported, an error is raised.
#'
#' @keywords internal
#'
check_cifti_type <- function(intent, extn){
  intent_idx <- which(supported_intents()$value == intent)
  extn_idx <- which(supported_intents()$extension == extn)
  if (length(extn_idx) < 1) {
    if (length(intent_idx) < 1){
      stop(paste(
        "This CIFTI file has intent code", intent, "and extension", extn,
        "neither of which is supported by ciftiTools (yet).",
        "Only the following types are:\n\t",
        paste(supported_intents()$value, collapse="\t "),
        "\nRespectively, they correspond to these file extensions:\n\t",
        paste(supported_intents()$extension, collapse="\t ")
      ))
    } else {
      warning(paste(
        "This CIFTI file has extension", extn, "which is not yet supported by ciftiTools.",
        "Only the following types are:\t",
        paste(supported_intents()$extension, collapse="\t "),
        "\nThe intent code", intent, "is supported but does not match the extension.",
        "Was the file named incorrectly?",
        "Continuing anyway with the intent code", intent, "and correct extension",
        supported_intents()$extension[intent_idx]
      ))
    }
  } else {
    if (length(intent_idx) < 1){
      stop(paste(
        "This CIFTI file has intent code", intent, "which is not yet supported by ciftiTools.",
        "Only the following types are:\n\t",
        paste(supported_intents()$value, collapse="\n\t "),
        "\nThe extension", extn, "is supported but does not match the intent code.",
        "Was the file named incorrectly?"
      ))
    } else {
      if (intent_idx != extn_idx) {
        warning(paste(
          "This CIFTI file has intent code", intent, "and extension", extn,
          "which do not match. Was the file named incorrectly?",
          "Continuing anyway with the intent code", intent, "and correct extension",
          supported_intents()$extension[intent_idx]
        ))
      }
    }
  }
  TRUE #list(intent=intent, extn=supported_intents()extension[intent_idx])
}


#' Extract misc metadata from CIFTI
#'
#' Extract misc metadata from CIFTI header XML ("Metadata" entry)
#'
#' @param xml List representing "Metadata" entry XML
#'  (\code{xifti$CIFTI$Matrix$MetaData})
#' @param intent The CIFTI's NIFTI intent code. Not used right now, but may be later.
#'  Default: \code{3000} (NIFTI_INTENT_UNKNOWN)
#'
#' @return The metadata, a list
#'
#' @keywords internal
#'
get_misc_meta_from_cifti_xml <- function(xml, intent=3000) {
  stopifnot(is.list(xml))
  xml <- do.call(rbind, xml)
  meta <- lapply(xml[,2], function(x){x[[1]]})
  names(meta) <- lapply(xml[,1], function(x){x[[1]]})
  meta
}

#' Extract intent-specific metadata from CIFTI
#'
#' Extract intent-specific Metadata from CIFTI header XML (first "MatrixIndicesMap" entry)
#'
#' @param x List representing "MatrixIndicesMap" entry XML
#'  (\code{xifti$CIFTI$Matrix[[2]]})
#' @param intent The CIFTI's NIFTI intent code
#'
#' @return The metadata, a list
#'
#' @keywords internal
#'
get_intn_meta_from_cifti_xml <- function(xml, intent=3000) {
  stopifnot(is.list(xml))
  if (intent == 3002) {
    if ("SeriesStart" %in% attributes(xml)) {
      tnames <- c("SeriesStart", "SeriesStep", "SeriesUnit")
    } else {
      tnames <- c("TimeStart", "TimeStep", "TimeStepUnits")
    }
    meta <- list(
      time_start=as.numeric(attributes(xml)[[tnames[1]]]),
      time_step=as.numeric(attributes(xml)[[tnames[2]]]),
      time_unit=tolower(attributes(xml)[[tnames[3]]])
    )
    if (meta$time_unit == "nifti_units_sec") { meta$time_unit <- "second" }
  } else if (intent == 3006) {
    meta <- list(
      names=as.character(
        vapply(
          xml,
          function(x){ ifelse(length(x$MapName) < 1, "", x$MapName[[1]]) },
          ""
        )
      )
    )
  } else if (intent == 3007) {
    xml <- xml[names(xml) == "NamedMap"]
    labs <- vector("list", length(xml))
    for (ii in seq_len(length(xml))) {
      xml_ii <- xml[[ii]]
      labs[[ii]] <- do.call(rbind, lapply(xml_ii$LabelTable, function(x){as.numeric(attributes(x))}))
      rownames(labs[[ii]]) <- as.character(vapply(xml_ii$LabelTable, function(x){x[[1]]}, ""))
      colnames(labs[[ii]]) <- c("Key", "Red", "Green", "Blue", "Alpha")
      labs[[ii]] <- as.data.frame(labs[[ii]])
    }
    names(labs) <- vapply(xml, function(x){x$MapName[[1]]}, "")
    meta <- list(names=names(labs), labels = labs)
  } else { stop("Internal error: CIFTI intent not supported.") }
  meta
}

#' Extract data-related metadata from CIFTI
#'
#' Extract data-related metadata from CIFTI header XML (second "MatrixIndicesMap" entry)
#'
#' @param x List representing "MatrixIndicesMap" entry XML
#'  (\code{xifti$CIFTI$Matrix[[3]]})
#' @param intent The CIFTI's NIFTI intent code. Not used right now, but might be
#'  used later. Default: \code{3000} (NIFTI_INTENT_UNKNOWN)
#'
#' @return The metadata, a list
#'
#' @keywords internal
#'
get_data_meta_from_cifti_xml <- function(xml, intent=3000) {

  stopifnot(is.list(xml))

  bs_names <- as.character(lapply(xml, function(x){attr(x, "BrainStructure")}))
  bs_names <- gsub("CIFTI_STRUCTURE_", "", bs_names)

  meta <- list(
    brainstructures=NULL,
    cortex_left_mwall=NULL,
    cortex_right_mwall=NULL,
    subcort_trans_mat=NULL,
    subcort_labs=NULL,
    subcort_dims=NULL
  )

  # Left Cortex
  if ("CORTEX_LEFT" %in% bs_names) {
    meta$brainstructures <- c(meta$brainstructures, "left")
    c_idx <- xml[[which(bs_names=="CORTEX_LEFT")]]
    # MSC data uses "NodeIndices" instead of the standard "VertexIndices"
    ind <- ifelse("VertexIndices" %in% names(c_idx), "VertexIndices", "NodeIndices")
    snum <- paste0("SurfaceNumberOf", ifelse(ind=="VertexIndices", "Vertices", "Nodes"))
    v <- strsplit(gsub("\n", "", c_idx[[ind]][[1]]), " ")[[1]]
    v <- 1 + as.numeric(v[v != ""])
    meta$cortex_left_mwall <- seq_len(attr(c_idx, snum)) %in% v
  }

  # Right Cortex
  if ("CORTEX_RIGHT" %in% bs_names) {
    meta$brainstructures <- c(meta$brainstructures, "right")
    c_idx <- xml[[which(bs_names=="CORTEX_RIGHT")]]
    ind <- ifelse("VertexIndices" %in% names(c_idx), "VertexIndices", "NodeIndices")
    snum <- paste0("SurfaceNumberOf", ifelse(ind=="VertexIndices", "Vertices", "Nodes"))
    v <- strsplit(gsub("\n", "", c_idx[[ind]][[1]]), " ")[[1]]
    v <- 1 + as.numeric(v[v != ""])
    meta$cortex_right_mwall <- seq_len(attr(c_idx, snum)) %in% v
  }

  # Subcortical Transformation Matrix
  if ("Volume" %in% names(xml)) {
    meta$brainstructures <- c(meta$brainstructures, "subcortical")
    vol_tmat <- strsplit(xml$Volume$TransformationMatrixVoxelIndicesIJKtoXYZ[[1]], "\n")[[1]]
    vol_tmat <- vol_tmat[vol_tmat != ""]
    vol_tmat <- do.call(rbind, lapply(vol_tmat, function(x){as.numeric(strsplit(x, " ")[[1]])}))
    meta$subcort_trans_mat <- vol_tmat

    meta$subcort_dims <- as.numeric(strsplit(attr(xml$Volume, "VolumeDimensions"), ",")[[1]])
  }

  # Subcortical
  sub_names <- substructure_table()$Original_Name[3:nrow(substructure_table())]
  bs_subcort <- bs_names %in% sub_names
  if (any(bs_subcort)) {
    subcort_dat <- vector("list", length(sub_names))
    for (ii in seq_len(length(sub_names))) {
      sub_name <- sub_names[ii]
      xml_idx <- which(bs_names == sub_name)
      # stopifnot(length(xml_idx) > 1) # By CIFTI definition, only one match should be present.
      if (length(xml_idx) > 0) {
        subcort_dat[[ii]] <- cbind(do.call(
          rbind,
          lapply(strsplit(
            strsplit(xml[[xml_idx]]$VoxelIndicesIJK[[1]], "\n")[[1]], " "
          ), as.numeric)
          # Add two to skip cortex L/R. Line after already adds the other 1.
        ), ii+1)
      }
    }
    # Add one to start indexing at 1 instead of zero
    subcort_dat <- do.call(rbind, subcort_dat) + 1
    meta$subcort_labs <- coordlist_to_vol(subcort_dat, fill=0)
  }

  # Unsupported
  unsupported_bs <- c(
    "ALL_WHITE_MATTER", "ALL_GREY_MATTER",
    "CEREBELLAR_WHITE_MATTER_LEFT", "CEREBELLAR_WHITE_MATTER_RIGHT",
    "CEREBELLUM", "CORTEX",
    "OTHER", "OTHER_GREY_MATTER", "OTHER_WHITE_MATTER"
  )
  if (any(bs_names %in% unsupported_bs)) {
    ciftiTools_warn(paste(
      "Some brainstructures present in the CIFTI file are not supported by ciftiTools:",
      paste(bs_names[bs_names %in% unsupported_bs], collapse=" ")
    ))
  }

  meta
}

#' Get NIFTI header (of a CIFTI)
#'
#' Wrapper for Connectome Workbench command
#'  \code{-nifti-information [fname] -print-header}
#'
#' @inheritParams cifti_fname_Param
#'
#' @return The header, as a character vector
#'
#' @keywords internal
#'
header_cifti <- function(cifti_fname){
  cmd <- paste("-nifti-information", sys_path(cifti_fname), "-print-header")
  out <- run_wb_cmd(cmd, intern=TRUE, ignore.stdout=FALSE, ignore.stderr=TRUE)
  if (length(out) == 0) {
    stop(
      "Workbench command '", paste(sys_path(ciftiTools.getOption("wb_path")), cmd), 
      "' failed."
    )
  }
  out
}

#' Get XML of a CIFTI
#'
#' Wrapper for Connectome Workbench command
#'  \code{-nifti-information [fname] -print-xml}
#'
#' @inheritParams cifti_fname_Param
#'
#' @return The XML as a list
#'
#' @importFrom xml2 read_xml as_list
#'
#' @keywords internal
#'
xml_cifti <- function(cifti_fname){
  cmd <- paste("-nifti-information", sys_path(cifti_fname), "-print-xml")
  out <- run_wb_cmd(cmd, intern=TRUE, ignore.stdout=FALSE, ignore.stderr=TRUE)
  if (length(out) == 0) {
    stop(
      "Workbench command '", paste(sys_path(ciftiTools.getOption("wb_path")), cmd), 
      "' failed."
    )
  }
  as_list(read_xml(paste(out, collapse="\n")))
}

#' Get CIFTI metadata
#'
#' Get CIFTI metadata from the NIFTI header and XML using the Connectome
#'  Workbench command \code{-nifti-information}. The information is formatted as
#'  the \code{meta} component in a \code{"xifti"} object
#'  (see \code{\link{template_xifti}}), and includes:
#'  \enumerate{
#'    \item medial wall masks for the left and right cortex
#'    \item the subcortical labels (ordered spatially)
#'    \item the subcortical mask
#'    \item other NIFTI intent-specific metadata
#'  }
#'
#' Additional metadata depends on the type of CIFTI file:
#'
#'  \enumerate{
#'    \item{"dtseries"}{
#'      \enumerate{
#'        \item{time_start:}{   Start time}
#'        \item{time_step:}{   The TR}
#'        \item{time_unit:}{   Unit of time}
#'      }
#'    }
#'    \item{"dscalar"}{
#'      \enumerate{
#'        \item{names:}{   Name of each data column}
#'      }
#'    }
#'    \item{"dlabels"}{
#'      \enumerate{
#'        \item{names:}{(   Names of each data column.)}
#'        \item{labels:}{(   List of \eqn{L x 5} data.frames. Row names are the label names. Column names are Key, Red, Green, Blue, and Alpha. List entry names are the names of each data column.)}
#'      }
#'    }
#'  }
#'
#' @inheritSection labels_Description Label Levels
#'
#' @inheritParams cifti_fname_Param
#'
#' @return The metadata component of a \code{"xifti"} for the input CIFTI file
#'
#' @family reading
#' @export
#'
#' @section Connectome Workbench:
#' This function interfaces with the \code{"-nifti-information"} Workbench command.
#' 
info_cifti <- function(cifti_fname){

  # Check if this CIFTI is supported.
  cifti_fname <- format_path(cifti_fname, mode=4)
  if (!is.fname(cifti_fname)) { 
    stop("`", cifti_fname, "` is not an existing file.") 
  }

  extn <- get_cifti_extn(cifti_fname)

  # ----------------------------------------------------------------------------
  # -nifti-information ---------------------------------------------------------
  # ----------------------------------------------------------------------------

  cif_head <- header_cifti(cifti_fname)
  cif_xml <- xml_cifti(cifti_fname)

  # ----------------------------------------------------------------------------
  # Parsing --------------------------------------------------------------------
  # ----------------------------------------------------------------------------

  # Header
  ## sform and qform not typically used; instead, subcortical orientation
  ## is given by the TransformationMatrixIJKtoXYZ element in the XML
  #sform <- as.numeric(gsub("sform_code:", "", cif_head[grepl("sform_code", cif_head)]))
  #qform <- as.numeric(gsub("qform_code:", "", cif_head[grepl("qform_code", cif_head)]))
  intent <- as.numeric(gsub("intent_code:", "", cif_head[grepl("intent_code", cif_head)]))
  ## Validate intent with file extension
  check_cifti_type(intent, extn)

  # XML
  Matrix_xml <- names(cif_xml$CIFTI$Matrix)
  ## General CIFTI / Misc. metadata
  if ("MetaData" %in% Matrix_xml) {
    misc <- get_misc_meta_from_cifti_xml(cif_xml$CIFTI$Matrix$MetaData, intent)
  } else {
    misc <- NULL
  }
  ## Extension-specific metadata
  ii <- which(Matrix_xml == "MatrixIndicesMap")[1]
  intn <- get_intn_meta_from_cifti_xml(cif_xml$CIFTI$Matrix[[ii]], intent)
  ## Data
  ii <- which(Matrix_xml == "MatrixIndicesMap")[2]
  ### For MSC data
  if ("Volume" %in% Matrix_xml) {
    cif_xml$CIFTI$Matrix[[ii]] <- c(cif_xml$CIFTI$Matrix["Volume"], cif_xml$CIFTI$Matrix[[ii]])
  }
  data <- get_data_meta_from_cifti_xml(cif_xml$CIFTI$Matrix[[ii]], intent)




  # ----------------------------------------------------------------------------
  # Formatting -----------------------------------------------------------------
  # ----------------------------------------------------------------------------

  meta <- template_xifti()$meta

  meta$cortex$medial_wall_mask["left"] <- list(data$cortex_left_mwall)
  meta$cortex$medial_wall_mask["right"] <- list(data$cortex_right_mwall)
  if (!is.null(data$subcort_labs)) {
    meta$subcort$mask <- data$subcort_labs != 0
    # Add back slices so subcortical dimensions match NIFTI header
    mask_pad <- cbind(0, data$subcort_dims-dim(meta$subcort$mask))
    meta$subcort <- list(
      labels = factor(
        data$subcort_labs[meta$subcort$mask],
        levels=seq_len(nrow(substructure_table())),
        labels=substructure_table()$ciftiTools_Name
      ),
      mask = pad_vol(meta$subcort$mask, mask_pad, fill=FALSE),
      trans_mat = data$subcort_trans_mat
    )
  }

  meta$cifti$intent <- intent
  meta$cifti$brainstructures <- data$brainstructures
  meta$cifti <- c(meta$cifti, intn, list(misc=misc))

  meta
}

#' @rdname info_cifti
#' @export
infoCIfTI <- function(cifti_fname){
  info_cifti(cifti_fname)
}

#' @rdname info_cifti
#' @export
infocii <- function(cifti_fname){
  info_cifti(cifti_fname)
}
