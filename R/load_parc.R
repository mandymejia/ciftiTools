#' Load a parcellation included in \code{ciftiTools}
#'
#' Load a parcellation included in \code{ciftiTools}.
#'
#' When using these parcellations, please cite the corresponding paper(s):
#'
#'  \enumerate{
#'    \item{Yeo, B. T. T. et al. The organization of the human cerebral cortex estimated by intrinsic functional connectivity. J Neurophysiol 106, 1125-1165 (2011).}
#'    \item{Schaefer, A. et al. Local-Global Parcellation of the Human Cerebral Cortex from Intrinsic Functional Connectivity MRI. Cereb Cortex 28, 3095-3114 (2018).}
#'    \item{Kong, R. et al. Individual-Specific Areal-Level Parcellations Improve Functional Connectivity Prediction of Behavior. Cerebral Cortex (2021+) doi:10.1093/cercor/bhab101.}
#'  }
#'
#'  Note that the Schaefer parcels have been matched to networks from Kong (2021+).
#' 
#' @param name The name of the parcellation to load:
#'  \itemize{
#'    \item{\code{"Schaefer_100"}:}{   (2018) 100 parcels based on the "local-global" approach.}
#'    \item{\code{"Schaefer_400"}:}{   (2018) 400 parcels based on the "local-global" approach.}
#'    \item{\code{"Schaefer_1000"}:}{   (2018) 1000 parcels based on the "local-global" approach.}
#'    \item{\code{"Yeo_7"}:}{   (2011) 7 networks based on fcMRI clustering. Networks are further divided into 51 components.}
#'    \item{\code{"Yeo_17"}:}{   (2011) 17 networks based on fcMRI clustering. Networks are further divided into 114 components.}
#'  }
#' 
# @param map Schaefer parcels have been matched to networks from Yeo (2011) and
#  Kong (2022). Available choices depend on the parcellation:
#  \itemize{
#    \item{\code{"Schaefer_*"}:}{   \code{"Yeo_7"}, \code{"Yeo_17",} or \code{"Kong_17"}}
#    \item{\code{"Yeo_*"}:}{   None available (this argument must be \code{NULL})}
#  }
#'  \code{NULL} (default) will load the first choice, where applicable. This
#'  argument will affect the indices, colors, and names of each parcel, but not
#'  the parcel boundaries.
#'
#' @return The parcellation as a dlabel \code{"xifti"} with one column
#' 
#' @family reading
#' @export
load_parc <- function(
  name=c("Schaefer_100", "Schaefer_400", "Schaefer_1000", "Yeo_7", "Yeo_17")
  ) {

  # Get the full parcellation name based on `name` and `map`
  name <- match.arg(name,
    c("Schaefer_100", "Schaefer_400", "Schaefer_1000", "Yeo_7", "Yeo_17")
  )
  map <- ifelse(grepl("Schaefer", name), "Kong_17", "Yeo_7")
  # if (!is.null(map)) {
  #   map <- match.arg(map, c("Yeo_7", "Yeo_17", "Kong_17"))
  # } else {
  #   map <- "Yeo_7"
  # }
  name2 <- paste(name, map, sep="__")
  p_all <- c(
    #Schaefer_100__Yeo_7 = "Schaefer2018_100Parcels_7Networks_order",
    #Schaefer_100__Yeo_17 = "Schaefer2018_100Parcels_17Networks_order",
    Schaefer_100__Kong_17 = "Schaefer2018_100Parcels_Kong2022_17Networks_order",
    #Schaefer_400__Yeo_7 = "Schaefer2018_400Parcels_7Networks_order",
    #Schaefer_400__Yeo_17 = "Schaefer2018_400Parcels_17Networks_order",
    Schaefer_400__Kong_17 = "Schaefer2018_400Parcels_Kong2022_17Networks_order",
    #Schaefer_1000__Yeo_7 = "Schaefer2018_1000Parcels_7Networks_order",
    #Schaefer_1000__Yeo_17 = "Schaefer2018_1000Parcels_17Networks_order",
    Schaefer_1000__Kong_17 = "Schaefer2018_1000Parcels_Kong2022_17Networks_order",
    Yeo_7__Yeo_7 = "Yeo2011_7Networks.split_components",
    Yeo_17__Yeo_7 = "Yeo2011_17Networks.split_components"
  )
  if (!(name2 %in% names(p_all))) {
    stop("Invalid `name` and `map` pair. Refer to the documentation for `load_parc` for valid inputs.")
  }

  # Load the parcellation
  p <- ciftiTools.data$parc[[p_all[name2]]]

  # Format the parcellation as a \code{"xifti"}. Return it
  nv <- nrow(p$map) # 32492*2=64984
  np <- nrow(p$col) - 1
  z <- template_xifti()
  z$data$cortex_left <- p$map[seq(nv/2),,drop=FALSE]
  z$data$cortex_right <- p$map[seq(nv/2+1, nv),,drop=FALSE]
  z$meta$cifti <- list(
    intent=3007,
    brainstructures=c("left", "right"),
    names="parcels",
    labels=list()
    #misc = ...
  )
  z$meta$cifti$labels <- list(
    parcels = data.frame(
      Key = seq(0, np),
      Red = p$col$Red,
      Green = p$col$Green,
      Blue = p$col$Blue,
      Alpha = c(0, rep(1, np))
    )
  )
  rownames(z$meta$cifti$labels$parcels) <- rownames(p$col)
  z
}
