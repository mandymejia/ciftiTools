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
  intent_idx <- which(supported_intents()$intent_code == intent)
  extn_idx <- which(supported_intents()$extension == extn)
  if (length(extn_idx) < 1) {
    if (length(intent_idx) < 1){
      stop(paste(
        "This CIFTI file has intent code", intent, "and extension", extn, 
        "neither of which is supported by ciftiTools (yet).",
        "Only the following types are:\n\t", 
        paste(supported_intents()$intent_code, collapse="\n\t"),
        "\nRespectively, they correspond to these file extensions:\n\t",
        paste(supported_intents()$extension, collapse="\n\t") 
      ))
    } else {
      warning(paste(
        "This CIFTI file has extension", extn, "which is not yet supported by ciftiTools.",
        "Only the following types are:\n\t", 
        paste(supported_intents()$extension, collapse="\n\t"),
        "\nThe intent code", intent, "is supported but does not match the extension.",
        "Was the file named incorrectly?"
        "Continuing anyway with the intent code", intent, "and correct extension", 
        supported_intents()extension[intent_idx]
      ))
    }
  } else {
    if (length(intent_idx) < 1){
      stop(paste(
        "This CIFTI file has intent code", intent, "which is not yet supported by ciftiTools.",
        "Only the following types are:\n\t", 
        paste(supported_intents()$intent_code, collapse="\n\t"),
        "\nThe extension", extn, "is supported but does not match the intent code.",
        "Was the file named incorrectly?"
      ))
    } else {
      if (intent_idx != extn_idx) {
      warning(paste(
        "This CIFTI file has intent code", intent, "and extension", extn, 
        "which do not match. Was the file named incorrectly?"
        "Continuing anyway with the intent code", intent, "and correct extension", 
        supported_intents()extension[intent_idx]
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
#' @param intent The CIFTI/NIFTI intent code
#' 
#' @return The metadata, a list
#' 
#' @keywords internal
#' 
get_misc_meta_from_cifti_xml <- function(xml, intent) {
  stopifnot(is.list(xml))
  xml <- do.call(rbind, xml)
  meta <- lapply(meta[,2], function(x){x[[1]]})
  names(meta) <- lapply(meta[,1], function(x){x[[1]]})
  meta
}

#' Extract Extension-specific Metadata from CIFTI Header
#' 
#' Extract Extension-specific Metadata from CIFTI Header (first "MatrixIndicesMap" entry)
#' 
#' @param x List representing "MatrixIndicesMap" entry XML (xii$CIFTI$Matrix[[2]])
#' @param extn The CIFTI/NIFTI intent code
#' 
#' @return The metadata, a list
#' 
#' @keywords internal
#' 
get_extn_meta_from_cifti_xml <- function(xml, extn) {
  stopifnot(is.list(xml))
  if (extn == "dtseries.nii") {
    meta <- list(
      time_start=attributes(xml)$SeriesStart, 
      time_step=attributes(xml)$SeriesStep, 
      time_unit=attributes(xml)$SeriesUnit
    )
  } else if (extn == "dscalar.nii") {
    meta <- list(
      names=as.character(sapply(attributes(xml), function(x){x$MapName[[1]]}))
    )
  } else if (extn == "dlabel.nii") {
    labs <- do.call(rbind, lapply(xml$NamedMap$LabelTable, function(x){as.character(attributes(x))}))
    rownames(labs) <- as.character(sapply(xml$NamedMap$LabelTable, function(x){x[[1]]}))
    colnames(labs) <- c("Key", "Red", "Green", "Blue", "Alpha")
    meta <- list(
      labels=labs
    )
  } else { stop("Internal error: CIFTI extension not recognized.") }
  meta
}

#' Extract Data-related Metadata from CIFTI Header
#' 
#' Extract Data-related Metadata from CIFTI Header (second "MatrixIndicesMap" entry)
#' 
#' @param x List representing "MatrixIndicesMap" entry XML (xii$CIFTI$Matrix[[3]])
#' @param extn The CIFTI/NIFTI intent code
#' 
#' @return The metadata, a list
#' 
#' @keywords internal
#' 
get_data_meta_from_cifti_xml <- function(xml, extn) {
  stopifnot(is.list(xml))

  bs_names <- as.character(sapply(xml, function(x){attr(x, "BrainStructure")}))
  bs_names <- gsub("CIFTI_STRUCTURE_", "", bs_names)

  meta <- list(
    cortex_left_mwall=NULL,
    cortex_right_mwall=NULL,
    subcort_trans_mat=NULL,
    subcort_labs=NULL
  )

  # Subcortical Transformation Matrix
  if ("Volume" %in% names(xml)) {
    vol_tmat <- strsplit(xml$Volume$TransformationMatrixVoxelIndicesIJKtoXYZ[[1]], "\n")[[1]]
    vol_tmat <- vol_tmat[vol_tmat != ""]
    vol_tmat <- do.call(rbind, lapply(vol_tmat, function(x){as.numeric(strsplit(x, " ")[[1]])}))
    meta$subcort_trans_mat <- vol_tmat
  }
  
  # Left Cortex
  if ("CIFTI_STRUCTURE_CORTEX_LEFT" %in% bs_names) { 
    cortexL <- xml[[which(bs_names=="CIFTI_STRUCTURE_CORTEX_LEFT")]] 
    meta$cortex_left_mwall <- (1:attr(cortexL, "SurfaceNumberOfVertices")) %in% (1+as.numeric(strsplit(cortexL$VertexIndices[[1]], " ")[[1]]))
  }

  # Right Cortex
  if ("CIFTI_STRUCTURE_CORTEX_RIGHT" %in% bs_names) { 
    cortexL <- xml[[which(bs_names=="CIFTI_STRUCTURE_CORTEX_RIGHT")]] 
    meta$cortex_right_mwall <- (1:attr(cortexL, "SurfaceNumberOfVertices")) %in% (1+as.numeric(strsplit(cortexL$VertexIndices[[1]], " ")[[1]]))
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
          rbind, lapply(strsplit(strsplit(xml[[xml_idx]]$VoxelIndicesIJK[[1]], "\n")[[1]], " "), as.numeric)
        ), ii+2)
      }
    }
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
    warning(paste(
      "These brainstructures are not supported by ciftiTools:",
      paste(bs_names[bs_names %in% unsupported_bs], collapse=" ")
    ))
  }

  meta
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
#' @inheritParams verbose_Param_FALSE
#'
#' @return A list with the following metadata:
#'  
#'  cifti$index_type
#'  cortex$medial_wall_mask$left,
#'  cortex$medial_wall_mask$right,
#'  subcort$labels,
#'  subcort$mask
#' 
#'  Additional metadata depends on cifti$index_type:
#' 
#'  For "series" files:
#'    cifti$time_start
#'    cifti$time_step
#'    cifti$time_unit
#' 
#'  For "scalar" files:
#'    cifti$names       (Name of each data column)
#' 
#'  For "labels" files:
#'    cifti$labels      (L x 5 data.frame. Rows are named. Cols: KRGBA)
#' 
#'  For "parcels" files:
#'    NOT SUPPORTED
#' 
#'  For "brain_models" files:
#'    NOT SUPPORTED
#' 
#' @inheritSection labels_Description Label Levels
#'
#' @importFrom xml2 read_xml as_list
#' 
#' @keywords internal
#' 
header_cifti <- function(cifti_fname, wb_path=NULL, verbose=FALSE){

  if (verbose) { exec_time <- Sys.time() }

  # Check if this CIFTI is supported.
  cifti_fname <- format_path(cifti_fname, mode=4)
  extn <- get_cifti_extn(cifti_fname)

  # ----------------------------------------------------------------------------
  # -nifti-information ---------------------------------------------------------
  # ----------------------------------------------------------------------------

  head <- run_wb_cmd(
    paste("-nifti-information", sys_path(cifti_fnames), "-print-header"),
    wb_path, intern=TRUE
  )

  cif_xml <- run_wb_cmd(
    paste("-nifti-information", sys_path(cifti_fname), "-print-xml"),
    wb_path, intern=TRUE
  )
  cif_xml <- as_list(read_xml(paste(cif_xml, collapse="\n")))

  # ----------------------------------------------------------------------------
  # Parsing --------------------------------------------------------------------
  # ----------------------------------------------------------------------------

  # Header
  sform <- as.numeric(gsub("sform_code:", "", head[grepl("sform_code", head)]))
  qform <- as.numeric(gsub("qform_code:", "", head[grepl("qform_code", head)]))
  intent <- as.numeric(gsub("intent_code:", "", head[grepl("intent_code", head)]))
  ## Validate intent with file extension
  check_cifti_type(intent, extn)

  # XML
  ## General CIFTI / Misc. metadata
  misc <- get_misc_meta_from_cifti_xml(cif_xml$CIFTI$Matrix$MetaData, intent)
  ## Extension-specific metadata
  extn <- get_extn_meta_from_cifti_xml(cif_xml$CIFTI$Matrix[[2]], intent)
  ## Data
  data <- get_data_meta_from_cifti_xml(cif_xml$CIFTI$Matrix[[3]], intent)

  # ----------------------------------------------------------------------------
  # Formatting -----------------------------------------------------------------
  # ----------------------------------------------------------------------------

  meta <- template_xifti()$meta

  meta$cortex$medial_wall_mask$left <- data$cortex_left_mwall
  meta$cortex$medial_wall_mask$right <- data$cortex_right_mwall
  if (!is.null(data$subcort_labs)) {
    sub_mask <- data$subcort_labs != 0
    sub_mask_padded <- crop_vol(sub_mask)
    meta$subcort <- list(
      labels = factor(
        data$subcort_labs[sub_mask],
        levels=1:nrow(substructure_table()),
        labels=substructure_table()$ciftiTools_Name
      ),
      mask = sub_mask_padded$mask,
      mask_padding = sub_mask_padded$padding,
      trans_mat = data$subcort$trans_mat
    )
    rm(sub_mask, sub_mask_padded)
  }

  meta$cifti$intent <- intent
  meta$cifti$misc <- misc
  meta$cifti <- c(meta$cifti, extn)

  meta
}

#' @rdname header_cifti
#' @export
headerCIfTI <- headercii <- function(cifti_fname, wb_path=NULL){
  header_cifti(cifti_fname, wb_path=NULL)
}
