#' Get the mapping for brainordinates in a flattened CIFTI.
#'
#' @description Wrapper for the Connectome Workbench command 
#'  \code{-cifti-export-dense-mapping}. Gives the brainstructure of each
#'  brainordinate in the flattened CIFTI (left cortex, right cortex, or one of
#'  the subcortical structures) as well as its index. For the cortex, this is
#'  useful because it indicates where the medial wall was masked out. For the
#'  subcortical data, the spatial position of the brainordinate is also
#'  obtained.
#'
#' @inheritParams cifti_fname_Param
#' @inheritParams wb_path_Param
#'
#' @importFrom utils read.csv
#'
#' @return A data.frame with brainordinates along the rows, and columns 
#'  indicating the brainstructure label ("label"), its index in the
#'  flattened CIFTI ("idx_flat"), and spatial coordinates for the subcortical 
#'  data ("i", "j", "k").
#' @export
map_flat_cifti <- function(cifti_fname, wb_path=NULL){
  wb_cmd <- get_wb_cmd_path(wb_path)

  cifti_fname <- format_path(cifti_fname)
  if (!file.exists(cifti_fname)) {
    stop(paste("cifti_fname", cifti_fname, "does not exist."))
  }

  the_time <- Sys.time()

  write_dir <- file.path(tempdir(), paste0(basename(cifti_fname), "-dense-mapping"))
  if (!dir.exists(write_dir)) { dir.create(write_dir) }
  cortexL_fname <- file.path(write_dir, "CORTEX_LEFT.txt")
  cortexR_fname <- file.path(write_dir, "CORTEX_RIGHT.txt")
  subcort_fname <- file.path(write_dir, "SUBCORT.txt")

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

  cortexL <- read.csv(cortexL_fname, sep=" ", header=FALSE)
  cortexR <- read.csv(cortexR_fname, sep=" ", header=FALSE)
  subcort <- read.csv(subcort_fname, sep=" ", header=FALSE)

  colnames(cortexL) <- c("idx_flat", "idx_unflat")
  colnames(cortexR) <- c("idx_flat", "idx_unflat")
  colnames(subcort) <- c("idx_flat", "label", "i", "j", "k")

  # Make indices start at one.
  cortexL$idx_flat <- cortexL$idx_flat + 1
  cortexL$idx_unflat <- cortexL$idx_unflat + 1
  cortexR$idx_flat <- cortexR$idx_flat + 1
  cortexR$idx_unflat <- cortexR$idx_unflat + 1
  subcort$idx_flat <- subcort$idx_flat + 1
  subcort$i <- subcort$i + 1
  subcort$j <- subcort$j + 1
  subcort$k <- subcort$k + 1

  subcort$label <- factor(
    subcort$label, 
    levels=c("CORTEX_LEFT", "CORTEX_RIGHT", levels(subcort$label))
  )

  cortexL_n <- max(cortexL$idx_unflat) # or just use last row?
  cortexL_idx <- rep(NA, cortexL_n)
  cortexL_idx[cortexL$idx_unflat] <- cortexL$idx_flat
  cortexL <- data.frame(
    idx_flat <- cortexL_idx,
    label <- factor("CORTEX_LEFT", levels=levels(subcort$label))
  )

  cortexR_n <- max(cortexR$idx_unflat) # or just use last row?
  cortexR_idx <- rep(NA, cortexR_n)
  cortexR_idx[cortexR$idx_unflat] <- cortexR$idx_flat
  cortexR <- data.frame(
    idx_flat <- cortexR_idx,
    label <- factor("CORTEX_RIGHT", levels=levels(subcort$label))
  )

  map <- data.frame(
    label = unlist(list(cortexL$label, cortexR$label, subcort$label)),
    idx_flat = c(cortexL$idx_flat, cortexR$idx_flat, subcort$idx_flat),
    i = c(rep(NA, nrow(cortexL)+nrow(cortexR)), subcort$i),
    j = c(rep(NA, nrow(cortexL)+nrow(cortexR)), subcort$j),
    k = c(rep(NA, nrow(cortexL)+nrow(cortexR)), subcort$k)
  )

  map
}

#' Unflatten a flat CIFTI using a map
#'
#' Unflatten
#'
#' @param cifti_flat The flat CIFTI.
#' @param map The map for the flat CIFTI: \code{\link{map_flat_cifti}}
#' @param fill Out-of-mask value (i.e. medial wall). Default: \code{NA}.
#'
#' @return A "cifti" object.
#' @export
#'
unflatten_cifti2 <- function(cifti_flat, map, fill=NA){
  map_inflat <- map[!is.na(map$idx_flat),]
  stopifnot(map_inflat$idx_flat == 1:nrow(map_inflat))

  map_inflatsubcort <- as.numeric(map_inflat$label) > 2

  cifti <- list(
    CORTEX_LEFT = matrix(fill, nrow=sum(map$label=="CORTEX_LEFT"), ncol=ncol(cifti_flat)),
    CORTEX_RIGHT = matrix(fill, nrow=sum(map$label=="CORTEX_RIGHT"), ncol=ncol(cifti_flat)),
    SUBCORT = list(
      DAT = cifti_flat[map_inflatsubcort,],
      LABELS = array(0, dim=sapply(map[,c("i","j","k")], max, na.rm=TRUE)),
      MASK = NA
    )
  )
  cifti$CORTEX_LEFT[(!is.na(map$idx_flat))[map$label=="CORTEX_LEFT"],] <- cifti_flat[map_inflat$label=="CORTEX_LEFT",]
  cifti$CORTEX_RIGHT[(!is.na(map$idx_flat))[map$label=="CORTEX_RIGHT"],] <- cifti_flat[map_inflat$label=="CORTEX_RIGHT",]

  # Fill mask with the label values. (Need spatial order for label).
  cifti$SUBCORT$LABELS[as.matrix(map_inflat[map_inflatsubcort,c("i","j","k")])] = map_inflat[map_inflatsubcort,"label"]

  # Then separate mask and labels.
  cifti$SUBCORT$MASK <- cifti$SUBCORT$LABELS > 0
  cifti$SUBCORT$LABELS <- as.vector(cifti$SUBCORT$LABELS[cifti$SUBCORT$MASK])
  cifti$SUBCORT$LABELS <- factor(
    cifti$SUBCORT$LABELS, 
    levels=3:length(levels(map$label)), 
    labels=levels(map$label)[3:length(levels(map$label))]
  )

  cifti
}