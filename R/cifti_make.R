#' Make a cifti object from its components
#'
#' @param cortexL Data matrix for left cortex, with vertices in rows
#' @param cortexR Data matrix for right cortex, with vertices in rows
#' @param surfL_fname File path to gifti surface object for left cortex, or a vector of file names. Provide surfL_fname OR surfL.
#' @param surfL Object of class 'surface' for left cortex (a list with two elements, vertices and faces), or a list thereof. Provide surfL_fname OR surfL.
#' @param surfR_fname File path to gifti surface object for right cortex, or a vector of file names. Provide surfR_fname OR surfR.
#' @param surfR Object of class 'surface' for right cortex (a list with two elements, vertices and faces), or a list thereof
#' @param surf_label Character vector containing descriptive names of each surface geometry provided (e.g. midthickness, inflated, etc.). Must be provided if surfL_fname and/or surfL_fname provided, and length must match. Otherwise, ignored.
#' @param subcortical Data matrix for subcortical locations, with voxels in rows
#' @param mask Volumetric brain mask for subcortical locations
#' @param labels Volumetric labels for subcortical ROIs
#'
#' @return Object of class 'cifti'
#' @export
#'
cifti_make <- function(cortexL=NULL, cortexR=NULL, surfL_fname=NULL, surfL=NULL, surfR_fname=NULL, surfR=NULL, surf_label=NULL, subcortical=NULL, mask=NULL, labels=NULL) {

  #check argument compatibility
  if (!is.null(subcortical)) { if (is.null(mask)) stop('If subcortical is provided, mask must be provided also.')} else mask <- NULL
  if (!is.null(subcortical)) { if (is.null(labels)) stop('If subcortical is provided, labels must be provided also.')} else labels <- NULL
  if (!is.null(cortexL)) { if (class(cortexL) != 'matrix') stop('cortexL must be a matrix (or NULL), but it is not.') }
  if (!is.null(cortexR)) { if (class(cortexR) != 'matrix') stop('cortexR must be a matrix (or NULL), but it is not.') }
  if (!is.null(surfL_fname) & !is.null(surfL)) stop('Provide surfL_fname or surfL, but not both.')
  if (!is.null(surfR_fname) & !is.null(surfR)) stop('Provide surfR_fname or surfR, but not both.')

  #get number of left cortex gifti files or surfaces provided
  if (!is.null(surfL_fname)) {
    numsurfL <- length(surfL_fname)
  } else if (!is.null(surfL)) {
    if (is.cifti_surface(surfL)) surfL <- list(surf1 = surfL) #for single surface case
    numsurfL <- length(surfL)
  } else {
    numsurfL <- NULL
  }

  #get number of right cortex gifti files or surfaces provided
  if (!is.null(surfR_fname)) {
    numsurfR <- length(surfR_fname)
  } else if (!is.null(surfR)) {
    if (is.cifti_surface(surfR)) surfR <- list(surf1 = surfR) #for single surface case
    numsurfR <- length(surfR)
  } else {
    numsurfR <- NULL
  }

  #check that number of left and right surfaces is the same
  if (!is.null(numsurfL) & !is.null(numsurfR)) {
    if (numsurfL != numsurfR) stop('Must provide the same number of left and right surfaces, if both are provided.')
  }

  #check that the length of surf_label (if provided) matches the number of surfaces provided
  if (!is.null(numsurfL) | !is.null(numsurfR)) {
    numsurf <- max(numsurfL, numsurfR)
    if (!is.null(surf_label)) { if (numsurf != length(surf_label)) stop('Length of surf_label must match number of left or right surfaces provided.')}
    if (is.null(surf_label)) {
      if (numsurf==1) surf_label <- 'surface'
      if (numsurf > 1) surf_label <- paste0('surface',1:numsurf)
    }
  }


  # #check formatting of surfR, if provided
  # if (!is.null(surfR)) {
  #   if (numsurf==1) {
  #     if (!is.cifti_surface(surfR)) stop('If only one surface provided, surfR must be a valid surface object or NULL.  See is.cifti_surface().')
  #     surfR <- list(surface=surfR)
  #   }
  # }
  #

  #if gifti file provided, create surfaces for left cortex
  if (!is.null(surfL_fname)) {
    surfL <- vector('list', length=numsurf)
    for(ii in 1:numsurf) {
      surfL_ii <- readGIfTI(surfL_fname[ii])$data
      verts_left_ii <- surfL_ii$pointset
      faces_left_ii <- surfL_ii$triangle
      if (min(faces_left_ii)==0) faces_left_ii <- faces_left_ii + 1 #start vertex indexing at 1 instead of 0
      surfL_ii <- list(vertices = verts_left_ii, faces = faces_left_ii)
      class(surfL_ii) <- 'surface'
      surfL[[ii]] <- surfL_ii
    }
  }

  #check formatting of surfL elements
  if (!is.null(surfL)) {
    for(ii in 1:numsurf) {
      if (!is.cifti_surface(surfL[[ii]])) stop('An element of surfL is not a valid surface object.  See is.cifti_surface().')
      if (!is.null(cortexL)) { if (nrow(cortexL) != nrow(surfL[[ii]]$vertices)) stop('cortexL and left surface model(s) must have same number of vertices.')}
      class(surfL[[ii]]) <- 'surface'
    }
    names(surfL) <- surf_label
  }

  #check formatting of surfR elements
  if (!is.null(surfR)) {
    for(ii in 1:numsurf) {
      if (!is.cifti_surface(surfR[[ii]])) stop('An element of surfR is not a valid surface object.  See is.cifti_surface().')
      if (!is.null(cortexR)) { if (nrow(cortexR) != nrow(surfR[[ii]]$vertices)) stop('cortexR and right surface model(s) must have same number of vertices.')}
      class(surfR[[ii]]) <- 'surface'
    }
    names(surfR) <- surf_label
  }

  #check formatting of subcortical data
  if (!is.null(subcortical)) {
    if (class(subcortical) != 'matrix') stop('subcortical must be a matrix (or NULL), but it is not.')
    if (length(dim(mask)) != 3) stop('mask must be a 3-dimensional array, but it is not.')
    vals_mask <- sort(unique(as.vector(mask)))*1
    if (!all.equal(vals_mask, c(0,1))) stop('mask must be logical or 0/1, but it is not.')
    if (sum(mask) != nrow(subcortical)) stop(paste0('The number of voxels in the mask (',sum(mask),') must equal the number of rows in subcortical (',nrow(subcortical),'), but they do not match.'))
    if (!all.equal(dim(mask),dim(labels))) stop('mask and labels must have the same dimensions, but they do not.')
  }
  check_cols <- c(ncol(cortexL), ncol(cortexR), ncol(subcortical))
  if (length(unique(check_cols)) > 1) stop('If provided, cortexL, cortexR and subcortical must all have the same number of columns (measurements), but they do not.')

  cifti_out <- vector('list', 6)
  class(cifti_out) <- 'cifti'
  names(cifti_out) <- c("cortexL","cortexR","surfL", "surfR", "VOL","LABELS")

  if (!is.null(cortexL)) cifti_out$cortexL <- cortexL
  if (!is.null(cortexR)) cifti_out$cortexR <- cortexR
  if (!is.null(surfL)) cifti_out$surfL <- surfL
  if (!is.null(surfR)) cifti_out$surfR <- surfR
  if (!is.null(subcortical)) {
    cifti_out$VOL <- array(0, dim=c(dim(mask),ncol(subcortical)))
    for(ii in 1:ncol(subcortical)) { cifti_out$VOL[,,,ii][mask==1] <- subcortical[,ii] }
    cifti_out$LABELS <- labels
  }

  cifti_out
}
