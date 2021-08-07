# setwd("data-raw")

# File locations
parc_dir <- "../../Data/parc" # put all dlabel parcellations here
wb_path <- "../../workbench"

# Load package
library(ciftiTools)
ciftiTools.setOption("wb_path", wb_path)

# Files included in ciftiTools -------------------------------------------------

## The CIFTI files are from NITRC: cifti-2_test_data-1.2.zip at
##  https://www.nitrc.org/frs/?group_id=454
##
## The surfaces are from the HCP and are included according to these data use
##  terms: Data were provided \[in part\] by the Human Connectome Project,
##  WU-Minn Consortium (Principal Investigators: David Van Essen and Kamil
##  Ugurbil; 1U54MH091657) funded by the 16 NIH Institutes and Centers
##  that support the NIH Blueprint for Neuroscience Research; and by the
##  McDonnell Center for Systems Neuroscience at Washington University.
##
## Only the inflated surfaces are available as GIFTI files. To access the other
##  surfaces included in the package (very inflated and midthickness), see
##  \code{\link{load_data}}.
##
ciftiTools.files <- list(
  cifti = unlist(list(
    dtseries = system.file(
      "extdata",
      "Conte69.MyelinAndCorrThickness.32k_fs_LR.dtseries.nii",
      package="ciftiTools"
    ),
    dscalar = system.file(
      "extdata",
      "Conte69.MyelinAndCorrThickness.6k_fs_LR.dscalar.nii",
      package="ciftiTools"
    ),
    dlabel = system.file(
      "extdata",
      "Conte69.parcellations_VGD11b.6k_fs_LR.dlabel.nii",
      package="ciftiTools"
    ),
    dscalar_ones = system.file(
      "extdata",
      "ones_1k.dscalar.nii",
      package="ciftiTools"
    )
  )),
  surf = unlist(list(
    left = system.file(
      "extdata",
      paste0("S1200.L.inflated_MSMAll.32k_fs_LR.surf.gii"),
      package="ciftiTools"
    ),
    right = system.file(
      "extdata",
      paste0("S1200.R.inflated_MSMAll.32k_fs_LR.surf.gii"),
      package="ciftiTools"
    )
  ))
)
save(ciftiTools.files, file=file.path("../data", "ciftiTools.files.RData"))

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

# Medial wall template ---------------------------------------------------------
x <- read_xifti(ciftiTools.files$cifti["dtseries"])
HCP_32k_mwall_template <- do.call(cbind, x$meta$cortex$medial_wall_mask)

# Data included in ciftiTools --------------------------------------------------

surfp <- lapply(list(
  very_inflated.L = file.path(
    "../inst/extdata",
    "S1200.L.very_inflated_MSMAll.32k_fs_LR.surf.gii"
  ),
  very_inflated.R = file.path(
    "../inst/extdata",
    "S1200.R.very_inflated_MSMAll.32k_fs_LR.surf.gii"
  ),
  midthickness.L = file.path(
    "../inst/extdata",
    "S1200.L.midthickness_MSMAll.32k_fs_LR.surf.gii"
  ),
  midthickness.R = file.path(
    "../inst/extdata",
    "S1200.R.midthickness_MSMAll.32k_fs_LR.surf.gii"
  )
), gifti::read_gifti)
surfp <- lapply(surfp, function(x){x$data$pointset})

ciftiTools.data <- list(
  surfp = surfp, parc=parc,
  HCP_32k_mwall_template = HCP_32k_mwall_template
)

# Template GIFTIs --------------------------------------------------------------
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

# Code not included: resampling 32k NIRC files to 6k ---------------------------

# Save to sysdata --------------------------------------------------------------

save(
  ciftiTools.data, ciftiTools.files,
  gifti_surf_template, gifti_metric_template,
  file="../R/sysdata.rda", compress='xz'
)
