#' Table of CIFTI File Types (Intents) Supported By ciftiTools
#' 
#' See https://www.nitrc.org/forum/attachment.php?attachid=334&group_id=454&forum_id=1955
#' 
#' @return A data.frame with each supported file type along the rows.
#' 
#' @keywords internal
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

#' Substructure Table
#' 
#' Table of labels with original names from the CIFTI file, and new names
#'  based on ft_read_cifti.
#' 
#' @return The substructure table.
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
  table$Index <- 1:nrow(table)
  table
}

#' Check CIFTI type
#' 
#' Validate the file name extension and intent code.
#' 
#' @param intent The NIFTI intent_code, as a numeric integer
#' @param extn The file name extension, e.g. "dtseries.nii"
#' 
#' @return If the intent is supported, \code{TRUE}.
#'  If the intent is not supported, an error is raised.
check_cifti_type <- function(intent, extn){
  intent_idx <- which(supported_intents()$value == intent)
  extn_idx <- which(supported_intents()$extension == extn)
  if (length(extn_idx) < 1) {
    if (length(intent_idx) < 1){
      stop(paste(
        "This CIFTI file has intent code", intent, "and extension", extn, 
        "neither of which is supported by ciftiTools (yet).",
        "Only the following types are:\n\t", 
        paste(supported_intents()$value, collapse="\n\t "),
        "\nRespectively, they correspond to these file extensions:\n\t",
        paste(supported_intents()$extension, collapse="\n\t ") 
      ))
    } else {
      warning(paste(
        "This CIFTI file has extension", extn, "which is not yet supported by ciftiTools.",
        "Only the following types are:\n\t", 
        paste(supported_intents()$extension, collapse="\n\t "),
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


#' Extract Misc Metadata from CIFTI Header
#' 
#' Extract Misc Metadata from CIFTI Header ("Metadata" entry)
#' 
#' @param xml List representing "Metadata" entry XML (xii$CIFTI$Matrix$MetaData)
#' @param intent The CIFTI/NIFTI intent code. Not used right now, but may be later.
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

#' Extract Intent-specific Metadata from CIFTI Header
#' 
#' Extract Intent-specific Metadata from CIFTI Header (first "MatrixIndicesMap" entry)
#' 
#' @param x List representing "MatrixIndicesMap" entry XML (xii$CIFTI$Matrix[[2]])
#' @param intent The CIFTI/NIFTI intent code
#' 
#' @return The metadata, a list
#' 
#' @keywords internal
#' 
get_intn_meta_from_cifti_xml <- function(xml, intent=3000) {
  stopifnot(is.list(xml))
  if (intent == 3002) {
    meta <- list(
      time_start=as.numeric(attributes(xml)$SeriesStart), 
      time_step=as.numeric(attributes(xml)$SeriesStep), 
      time_unit=tolower(attributes(xml)$SeriesUnit)
    )
  } else if (intent == 3006) {
    meta <- list(
      names=as.character(sapply(xml, function(x){x$MapName[[1]]}))
    )
  } else if (intent == 3007) {
    xml <- xml[names(xml) == "NamedMap"]
    labs <- vector("list", length(xml))
    for (ii in 1:length(xml)) {
      xml_ii <- xml[[ii]]
      labs[[ii]] <- do.call(rbind, lapply(xml_ii$LabelTable, function(x){as.numeric(attributes(x))}))
      rownames(labs[[ii]]) <- as.character(sapply(xml_ii$LabelTable, function(x){x[[1]]}))
      colnames(labs[[ii]]) <- c("Key", "Red", "Green", "Blue", "Alpha")
      labs[[ii]] <- as.data.frame(labs[[ii]])
    }
    names(labs) <- sapply(xml, function(x){x$MapName[[1]]})
    meta <- list(labels = labs)
  } else { stop("Internal error: CIFTI intent not supported.") }
  meta
}

#' Extract Data-related Metadata from CIFTI Header
#' 
#' Extract Data-related Metadata from CIFTI Header (second "MatrixIndicesMap" entry)
#' 
#' @param x List representing "MatrixIndicesMap" entry XML (xii$CIFTI$Matrix[[3]])
#' @param intent The CIFTI/NIFTI intent code. Not used right now, but may be later.
#'  Default: \code{3000} (NIFTI_INTENT_UNKNOWN)
#' 
#' @return The metadata, a list
#' 
#' @keywords internal
#' 
get_data_meta_from_cifti_xml <- function(xml, intent=3000) {
  stopifnot(is.list(xml))

  bs_names <- as.character(sapply(xml, function(x){attr(x, "BrainStructure")}))
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
    meta$cortex_left_mwall <- (1:attr(c_idx, "SurfaceNumberOfVertices")) %in% (
      1+as.numeric(strsplit(c_idx$VertexIndices[[1]], " ")[[1]]))
  }

  # Right Cortex
  if ("CORTEX_RIGHT" %in% bs_names) { 
    meta$brainstructures <- c(meta$brainstructures, "right")
    c_idx <- xml[[which(bs_names=="CORTEX_RIGHT")]] 
    meta$cortex_right_mwall <- (1:attr(c_idx, "SurfaceNumberOfVertices")) %in% (
      1+as.numeric(strsplit(c_idx$VertexIndices[[1]], " ")[[1]]))
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
    for (ii in 1:length(sub_names)) {
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

#' Get CIFTI Metadata Information
#'
#' Wrapper for -nifti-information
#'
#' @inheritParams cifti_fname_Param
#' @param what "header" or "xml"
#' @inheritParams wb_path_Param
#'
#' @return The information in a list
#'
#' @importFrom xml2 read_xml as_list
#'
#' @keywords internal
#'
info_cifti_raw <- function(cifti_fname, what=c("header", "xml"), wb_path=NULL){
  what <- match.arg(what, c("header", "xml"))
  what <- paste0("-print-", what)
  out <- run_wb_cmd(
    paste("-nifti-information", sys_path(cifti_fname), what),
    wb_path, intern=TRUE
  )
  if (what=="-print-xml"){ out <- as_list(read_xml(paste(out, collapse="\n"))) }
  out
}

#' Get CIFTI Header
#'
#' Wrapper for the Connectome Workbench command 
#'  \code{-nifti-information}. It parses the CIFTI header XML to obtain:
#' 
#'  medial wall masks for the left and right cortex,
#'  the subcortical labels (ordered spatially),
#'  the subcortical mask,
#'  and other metadata.
#'
#' @inheritParams cifti_fname_Param
#' @inheritParams wb_path_Param
#'
#' @return The metadata component of a "xifti"
#' 
#'  Additional metadata depends on the type of CIFTI file:
#' 
#'  For "dtseries" files:
#'    cifti$time_start
#'    cifti$time_step
#'    cifti$time_unit
#' 
#'  For "dscalar" files:
#'    cifti$names       (Name of each data column)
#' 
#'  For "dlabels" files:
#'    cifti$labels      (L x 5 data.frame. Rows are named. Cols: KRGBA)
#' 
#' @inheritSection labels_Description Label Levels
#' 
#' @keywords internal
#' 
info_cifti <- function(cifti_fname, wb_path=NULL){

  # Check if this CIFTI is supported.
  cifti_fname <- format_path(cifti_fname, mode=4)
  extn <- get_cifti_extn(cifti_fname)

  # ----------------------------------------------------------------------------
  # -nifti-information ---------------------------------------------------------
  # ----------------------------------------------------------------------------

  cif_head <- info_cifti_raw(cifti_fname, wb_path, what="header")
  cif_xml <- info_cifti_raw(cifti_fname, wb_path, what="xml")

  # ----------------------------------------------------------------------------
  # Parsing --------------------------------------------------------------------
  # ----------------------------------------------------------------------------

  # Header
  sform <- as.numeric(gsub("sform_code:", "", cif_head[grepl("sform_code", cif_head)]))
  qform <- as.numeric(gsub("qform_code:", "", cif_head[grepl("qform_code", cif_head)]))
  intent <- as.numeric(gsub("intent_code:", "", cif_head[grepl("intent_code", cif_head)]))
  ## Validate intent with file extension
  check_cifti_type(intent, extn)

  # XML
  ## General CIFTI / Misc. metadata
  misc <- get_misc_meta_from_cifti_xml(cif_xml$CIFTI$Matrix$MetaData, intent)
  ## Extension-specific metadata
  intn <- get_intn_meta_from_cifti_xml(cif_xml$CIFTI$Matrix[[2]], intent)
  ## Data
  data <- get_data_meta_from_cifti_xml(cif_xml$CIFTI$Matrix[[3]], intent)

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
        levels=1:nrow(substructure_table()),
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
infoCIfTI <- infocii <- function(cifti_fname, wb_path=NULL){
  info_cifti(cifti_fname, wb_path=NULL)
}
