#' Substructure Table
#' 
#' Table of names with original(-like) names from the CIFTI file, and new names
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

#' Map CIFTI Brainordinates
#'
#' Wrapper for the Connectome Workbench command 
#'  \code{-cifti-export-dense-mapping}. For each brainordinate in a CIFTI, gives
#'  the brainstructure (left cortex, right cortex, or subcortical) and
#'  substructure (left/right cortex or medial wall; or, one of the 19
#'  subcortical structures). It also obtains the spatial coordinates of the
#'  subcortical voxels.
#'
#' @inheritParams cifti_fname_Param
#' @inheritParams wb_path_Param
#' @inheritParams verbose_Param_FALSE
#'
#' @importFrom utils read.csv
#'
#' @return A list with the following metadata: 
#'  cortex$medial_wall_mask$left,
#'  cortex$medial_wall_mask$right,
#'  subcort$labels,
#'  subcort$mask
#' @export
#' 
#' @inheritSection labels_Description Label Levels
#' 
map_cifti <- function(cifti_fname, wb_path=NULL, verbose=FALSE){
  wb_cmd <- get_wb_cmd_path(wb_path)

  cifti_fname <- format_path(cifti_fname)
  if (!file.exists(cifti_fname)) {
    stop(paste("cifti_fname", cifti_fname, "does not exist."))
  }

  if (verbose) { exec_time <- Sys.time() }

  # ----------------------------------------------------------------------------
  # -cifti-export-dense-mapping ------------------------------------------------
  # ----------------------------------------------------------------------------

  if (verbose) { cat("Writing and reading mapping files.\n") }

  # File paths.
  write_dir <- file.path(
    tempdir(), paste0(basename(cifti_fname), "-dense-mapping")
  )
  if (!dir.exists(write_dir)) { dir.create(write_dir) }
  cortexL_fname <- file.path(write_dir, "cortexL.txt")
  cortexR_fname <- file.path(write_dir, "cortexR.txt")
  subcort_fname <- file.path(write_dir, "subcort.txt")

  # Workbench command.
  cmd <- paste(
    wb_cmd,
    "-cifti-export-dense-mapping",
      sys_path(cifti_fname),
      "COLUMN",
      "-volume-all",
        sys_path(subcort_fname),
        "-structure",
      "-surface",
        "CORTEX_LEFT",
        sys_path(cortexL_fname),
      "-surface",
        "CORTEX_RIGHT",
        sys_path(cortexR_fname)
  )
  cmd_code <- system(cmd)
  if (cmd_code != 0) {
    stop(paste0("The Connectome Workbench command failed with code ", cmd_code, 
      ". The command was:\n", cmd))
  }

  # Read in files.
  cortexL <- read.csv(cortexL_fname, sep=" ", header=FALSE)
  cortexR <- read.csv(cortexR_fname, sep=" ", header=FALSE)
  subcort <- read.csv(subcort_fname, sep=" ", header=FALSE)

  if (verbose) {
    print(Sys.time() - exec_time)
    exec_time <- Sys.time()
  }

  # ----------------------------------------------------------------------------
  # Format the mapping. --------------------------------------------------------
  # ----------------------------------------------------------------------------

  if (verbose) { cat("Formatting.\n") }

  # Name the columns.
  colnames(cortexL) <- c("idx_flat", "idx_unflat")
  colnames(cortexR) <- c("idx_flat", "idx_unflat")
  colnames(subcort) <- c("idx_flat", "label", "i", "j", "k")
  
  # Make indices start at one.
  cortexL$idx_flat <- cortexL$idx_flat + 1
  cortexL$idx_unflat <- cortexL$idx_unflat + 1
  cortexR$idx_flat <- cortexR$idx_flat - nrow(cortexL) + 1
  cortexR$idx_unflat <- cortexR$idx_unflat + 1
  subcort$idx_flat <- subcort$idx_flat - nrow(cortexL) - nrow(cortexR) + 1
  subcort$i <- subcort$i + 1
  subcort$j <- subcort$j + 1
  subcort$k <- subcort$k + 1

  ## Get the vectorized spatial index for the subcortical data.
  #subcort$idx_spatial <- order(
  #  subcort$i + 
  #  (1+max(subcort$i))*subcort$j + 
  #  (1+(1+max(subcort$i)*max(subcort$j)))*subcort$k
  #)

  # Re-level the subcortical labels, and then temporarily use integers.
  labels_original <- substructure_table()$Original_Name
  labels_ciftiTools <- substructure_table()$ciftiTools_Name
  subcort$label <- as.numeric(factor(
    subcort$label, 
    levels=labels_original
  ))

  # Assemble the subcortical mask, and use it to order the labels spatially.
  subcort2 <- coordlist_to_vol(subcort[,c("i", "j", "k","label")], fill=0)
  subcort_mask <- crop_array(subcort2 != 0)
  subcort$label <- subcort2[subcort_mask]

  # Re-level the subcortical labels.
  subcort$label <- factor(
    subcort$label, 
    levels=1:length(labels_ciftiTools),
    labels=labels_ciftiTools
  )

  # Use "unflat" index for the cortical data.
  # Make the cortical labels factors too.
  cortexL_mask <- 1:max(cortexL$idx_unflat) %in% cortexL$idx_unflat
  cortexR_mask <- 1:max(cortexR$idx_unflat) %in% cortexR$idx_unflat

  if (verbose) {
    print(Sys.time() - exec_time)
    exec_time <- Sys.time()
  }

  list(
    cortex = list(
      medial_wall_mask = list(
        left = cortexL_mask,
        right = cortexR_mask
      )
    ),
    subcort = list(
      labels = subcort$label, 
      mask = subcort_mask
    )
  )
}

#' @rdname map_cifti
#' @export
mapCIfTI <- mapcii <- function(cifti_fname, wb_path=NULL){
  map_cifti(cifti_fname, wb_path=NULL)
}
