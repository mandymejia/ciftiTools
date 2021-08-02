# setwd("data-raw")

# File locations
parc_dir <- "../../Data/parc" # put all dlabel parcellations here
wb_path <- "../../workbench"

# Load package
library(ciftiTools)
ciftiTools.setOption("wb_path", wb_path)

# Medial wall template ---------------------------------------------------------
x <- read_xifti(demo_files()$cifti["dtseries"])
HCP_32k_mwall_template <- do.call(cbind, x$meta$cortex$medial_wall_mask)

# Template GIFTIs --------------------------------------------------------------
# devtools::install_github("damondpham/gifti")
library(gifti)
gifti_surf_template <- readgii("template.surf.gii")
gifti_surf_template["transformations"] <- list(NULL)
gifti_metric_template <- readgii("template.func.gii")
gifti_metric_template["transformations"] <- list(NULL)

# # Demo
# g <- gifti_surf_template
# g$data_meta[[1]][1,1] <- "CortexLeft"
# g$data <- list(pointset=new_pointset, triangle=new_triangle)
# g$data_info$Dim0 <- c(nrow(new_pointset), nrow(new_triangle))
# g <- gifti_metric_template
# g$data_meta[1] <- "CortexLeft"
# g$data <- split(t(data), seq(ncol(data)))
# names(g$data) <- rep("unknown", length(g$data))
# g$data_info$Dim0 <- gifti_metric_template$data_info[rep(1, 5),]

# Parcellations ----------------------------------------------------------------

# Compress the parcellations
parc_name <- list.files(parc_dir, "dlabel")
parc <- lapply(file.path(parc_dir, parc_name), read_xifti)
names(parc) <- gsub(".dlabel.nii", "", parc_name, fixed=TRUE)
parc <- lapply(parc, function(y){list(
  map = as.matrix(y),
  col = y$meta$cifti$labels$parcels[c("Red", "Green", "Blue")]
)})

# # Check work
# p_all <- rbind(
#   Schaefer2018_100Parcels_7Networks_order = c("Schaefer_100", "Yeo_7"),
#   Schaefer2018_100Parcels_17Networks_order = c("Schaefer_100", "Yeo_17"),
#   Schaefer2018_100Parcels_Kong2022_17Networks_order = c("Schaefer_100", "Kong_17"),
#   Schaefer2018_400Parcels_7Networks_order = c("Schaefer_400", "Yeo_7"),
#   Schaefer2018_400Parcels_17Networks_order = c("Schaefer_400", "Yeo_17"),
#   Schaefer2018_400Parcels_Kong2022_17Networks_order = c("Schaefer_400", "Kong_17"),
#   Schaefer2018_1000Parcels_7Networks_order = c("Schaefer_1000", "Yeo_7"),
#   Schaefer2018_1000Parcels_17Networks_order = c("Schaefer_1000", "Yeo_17"),
#   Schaefer2018_1000Parcels_Kong2022_17Networks_order = c("Schaefer_1000", "Kong_17"),
#   `Yeo2011_7Networks.split_components` = c("Yeo_7", "Yeo_7"),
#   `Yeo2011_17Networks.split_components` = c("Yeo_17", "Yeo_7")
# )
# for (ii in seq(length(parc))) {
#   cat(parc_name[ii], "\n")
#   q <- read_xifti(file.path(parc_dir, parc_name[ii]))
#   q$meta$cifti$misc <- NULL
#   idx <- rownames(p_all)==gsub(".dlabel.nii", "", parc_name[ii], fixed=TRUE)
#   testthat::expect_equal(
#     load_parc(p_all[idx,1], p_all[idx,2]), q
#   )
# }

# Save to sysdata --------------------------------------------------------------

save(HCP_32k_mwall_template, gifti_surf_template, gifti_metric_template, parc, file="../R/sysdata.rda")
