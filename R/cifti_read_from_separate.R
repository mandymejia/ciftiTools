#' Reads in CIFTI data
#'
#' @description Reads in CIFTI data from the separated left and right cortical GIfTI files, the subcortical NIfTI file, and optionally any surface geometry GIfTI files.
#'
#' @param cortexL_fname (Optional) File path of GIfTI data for left cortex
#' @param cortexR_fname (Optional) File path of GIfTI data for right cortex
#' @param subcortVol_fname (Optional) File path of NIfTI volume data for subcortical structures
#' @param subcortLab_fname (Optional) File path of the NIfTI labels for subcortical structures
#' @param fname_surfaceL (Optional) File path, or vector of multiple file paths, of GIFTI surface geometry file 
#'  representing left cortex
#' @param fname_surfaceR (Optional) File path, or vector of multiple file paths, of GIFTI surface geometry file 
#'  representing right cortex
#' @param surf_names (Optional) Character vector containing descriptive names of each GIFTI surface geometry provided 
#'  (e.g. midthickness, inflated, etc.). Should match the length of fname_surfaceL and/or fname_surfaceL if they are 
#'  provided. Otherwise, ignored.
#' @param wb_dir (Optional) Path to Connectome Workbench folder. If not provided, should be set by option ...
#'
#' @return An object of type 'cifti', a list containing at least 4 elements: CORTEX_LEFT, CORTX_RIGHT, VOL and LABELS.
#'  LABELS contains the brain structure labels (usually 3-21) of the subcortical elements. If surface geometry files
#'  were provided in the arguments, the list will also contain SURF_LEFT and SURF_RIGHT.
#' @export
#' @importFrom gifti readGIfTI
#' @importFrom RNifti readNifti
#'
#' @details This function uses a system wrapper for the 'wb_command' executable. The user must first download and install the Connectome Workbench,
#' available from https://www.humanconnectome.org/software/get-connectome-workbench. The 'wb_cmd' argument is the full file path to the 'wb_command' executable file.
#'
#' The subcortical brain structure labels (LABELS element of returned list) take values 3-21 and represent:
#' 3 Accumbens-L
#' 4 Accumbens-R
#' 5 Amygdala-L
#' 6 Amygdala-R
#' 7 Brain Stem
#' 8 Caudate-L
#' 9 Caudate-R
#' 10 Cerebellum-L
#' 11 Cerebellum-R
#' 12 Diencephalon-L
#' 13 Diencephalon-R
#' 14 Hippocampus-L
#' 15 Hippocampus-R
#' 16 Pallidum-L
#' 17 Pallidum-R
#' 18 Putamen-L
#' 19 Putamen-R
#' 20 Thalamus-L
#' 21 Thalamus-R
#'
cifti_read_from_separate <- function(cortexL_fname, cortexR_fname, subcortVol_fname, subcortLab_fname, fname_surfaceL,
  fname_surfaceR, dir=NULL, surf_names="surface", wb_dir=NULL){


  # Make full file paths.
  if(!is.null(fname_surface_L)) fname_surfaceL <- normalizePath(fname_surfaceL) 
  if(!is.null(fname_surface_R)) fname_surfaceR <- normalizePath(fname_surfaceR) 
  if(!is.null(fname_sphereOrigL)) fname_sphereOrigL <- normalizePath(fname_sphereOrigL)
  if(!is.null(fname_sphereOrigR)) fname_sphereOrigR <- normalizePath(fname_sphereOrigR)

  wb_dir <- check_wb_dir(wb_dir)
  if(is.null(dir)){ dir <- "."}

  result <- vector("list", length=6)
  names(result) <- c("CORTEX_LEFT", "CORTEX_RIGHT", "VOL", "LABELS", "SURF_LEFT", "SURF_RIGHT")

  # Read in GIfTI files for left and right cortex.
  if(!is.null(cortexL_fname)){
    result$CORTEX_LEFT <- do.call(cbind, readGIfTI(file.path(dir, cortexL_fname))$data)
  }
  if(!is.null(cortexR_fname)){
    result$CORTEX_RIGHT <- do.call(cbind, readGIfTI(file.path(dir, cortexR_fname))$data)
  }

  # Read in NIfTI files for subcortical data.
  if(!is.null(subcortVol_fname)){
    result$VOL <- readNifti(file.path(dir, subcortVol_fname))
  }
  if(!is.null(subcortLab_fname)){
    result$LABELS <- readNifti(file.path(dir, subcortLab_fname))
    result$LABELS[result$LABELS > 0] <- result$LABELS[result$LABELS > 0] + 2 
  }

  # Read in GIfTI surface geometry files.
  read_surf <- function(surf_fname){
    surf <- readGIfTI(surf_fname)$data
    verts <- surf$pointset
    faces <- surf$triangle
    if(min(faces)==0) faces <- faces + 1 #start vertex indexing at 1 instead of 0
    surf <- list(vertices = verts, faces = faces)
    class(surf) <- "surface"
    result$SURF_LEFT[[ii]] <- surf
    return(result)
  }
  num_surf <- length(surf_names) #number of surface types provided
  if(!is.null(fname_surfaceL)){
    result$SURF_LEFT <- vector('list', num_surf)
    names(result$SURF_LEFT) <- surf_names
    for(ii in 1:num_surf){
      result$SURF_LEFT[[ii]] <- read_surf(fname_surfaceL[ii])
    }
  }
  if(!is.null(fname_surfaceR)){
    result$SURF_RIGHT <- vector('list', num_surf)
    names(result$SURF_RIGHT) <- surf_names
    for(ii in 1:num_surf){
      result$SURF_RIGHT[[ii]] <- read_surf(fname_surfaceR[ii])
    }
  }

  class(result) <- 'cifti'
  return(result)
}
