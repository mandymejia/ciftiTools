#' Make cifti object
#'
#' @param cortex_left Data matrix for left cortex, with vertices in rows
#' @param cortex_right Data matrix for right cortex, with vertices in rows
#' @param fname_gifti_left File path to gifti surface object for left cortex, or a vector of file names. Provide fname_gifti_left OR surf_left.
#' @param surf_left Object of class 'surface' for left cortex (a list with two elements, vertices and faces), or a list thereof. Provide fname_gifti_left OR surf_left.
#' @param fname_gifti_right File path to gifti surface object for right cortex, or a vector of file names. Provide fname_gifti_right OR surf_right.
#' @param surf_right Object of class 'surface' for right cortex (a list with two elements, vertices and faces), or a list thereof
#' @param surf_names Character vector containing descriptive names of each surface geometry provided (e.g. midthickness, inflated, etc.). Must be provided if fname_gifti_left and/or fname_gifti_left provided, and length must match. Otherwise, ignored.
#' @param subcortical Data matrix for subcortical locations, with voxels in rows
#' @param mask Volumetric brain mask for subcortical locations
#' @param labels Volumetric labels for subcortical ROIs
#'
#' @return Object of class 'cifti'
#' @export
#'
cifti_make <- function(cortex_left=NULL, cortex_right=NULL, fname_gifti_left=NULL, surf_left=NULL, fname_gifti_right=NULL, surf_right=NULL, surf_names=NULL, subcortical=NULL, mask=NULL, labels=NULL){

  #check argument compatibility
  if(!is.null(subcortical)) { if(is.null(mask)) stop('If subcortical is provided, mask must be provided also.')} else mask <- NULL
  if(!is.null(subcortical)) { if(is.null(labels)) stop('If subcortical is provided, labels must be provided also.')} else labels <- NULL
  if(!is.null(cortex_left)) { if(class(cortex_left) != 'matrix') stop('cortex_left must be a matrix (or NULL), but it is not.') }
  if(!is.null(cortex_right)) { if(class(cortex_right) != 'matrix') stop('cortex_right must be a matrix (or NULL), but it is not.') }
  if(!is.null(fname_gifti_left) & !is.null(surf_left)) stop('Provide fname_gifti_left or surf_left, but not both.')
  if(!is.null(fname_gifti_right) & !is.null(surf_right)) stop('Provide fname_gifti_right or surf_right, but not both.')

  #get number of left cortex gifti files or surfaces provided
  if(!is.null(fname_gifti_left)){
    numsurf_left <- length(fname_gifti_left)
  } else if(!is.null(surf_left)){
    if(is.surface(surf_left)) surf_left <- list(surf1 = surf_left) #for single surface case
    numsurf_left <- length(surf_left)
  } else {
    numsurf_left <- NULL
  }

  #get number of right cortex gifti files or surfaces provided
  if(!is.null(fname_gifti_right)){
    numsurf_right <- length(fname_gifti_right)
  } else if(!is.null(surf_right)){
    if(is.surface(surf_right)) surf_right <- list(surf1 = surf_right) #for single surface case
    numsurf_right <- length(surf_right)
  } else {
    numsurf_right <- NULL
  }

  #check that number of left and right surfaces is the same
  if(!is.null(numsurf_left) & !is.null(numsurf_right)){
    if(numsurf_left != numsurf_right) stop('Must provide the same number of left and right surfaces, if both are provided.')
  }

  #check that the length of surf_names (if provided) matches the number of surfaces provided
  if(!is.null(numsurf_left) | !is.null(numsurf_right)) {
    numsurf <- max(numsurf_left, numsurf_right)
    if(!is.null(surf_names)){ if(numsurf != length(surf_names)) stop('Length of surf_names must match number of left or right surfaces provided.')}
    if(is.null(surf_names)){
      if(numsurf==1) surf_names <- 'surface'
      if(numsurf > 1) surf_names <- paste0('surface',1:numsurf)
    }
  }


  # #check formatting of surf_right, if provided
  # if(!is.null(surf_right)){
  #   if(numsurf==1) {
  #     if(!is.surface(surf_right)) stop('If only one surface provided, surf_right must be a valid surface object or NULL.  See is.surface().')
  #     surf_right <- list(surface=surf_right)
  #   }
  # }
  #

  #if gifti file provided, create surfaces for left cortex
  if(!is.null(fname_gifti_left)){
    surf_left <- vector('list', length=numsurf)
    for(ii in 1:numsurf){
      surf_left_ii <- readGIfTI(fname_gifti_left[ii])$data
      verts_left_ii <- surf_left_ii$pointset
      faces_left_ii <- surf_left_ii$triangle
      if(min(faces_left_ii)==0) faces_left_ii <- faces_left_ii + 1 #start vertex indexing at 1 instead of 0
      surf_left_ii <- list(vertices = verts_left_ii, faces = faces_left_ii)
      class(surf_left_ii) <- 'surface'
      surf_left[[ii]] <- surf_left_ii
    }
  }

  #check formatting of surf_left elements
  if(!is.null(surf_left)){
    for(ii in 1:numsurf){
      if(!is.surface(surf_left[[ii]])) stop('An element of surf_left is not a valid surface object.  See is.surface().')
      if(!is.null(cortex_left)) { if(nrow(cortex_left) != nrow(surf_left[[ii]]$vertices)) stop('cortex_left and left surface model(s) must have same number of vertices.')}
      class(surf_left[[ii]]) <- 'surface'
    }
    names(surf_left) <- surf_names
  }

  #check formatting of surf_right elements
  if(!is.null(surf_right)){
    for(ii in 1:numsurf){
      if(!is.surface(surf_right[[ii]])) stop('An element of surf_right is not a valid surface object.  See is.surface().')
      if(!is.null(cortex_right)) { if(nrow(cortex_right) != nrow(surf_right[[ii]]$vertices)) stop('cortex_right and right surface model(s) must have same number of vertices.')}
      class(surf_right[[ii]]) <- 'surface'
    }
    names(surf_right) <- surf_names
  }

  #check formatting of subcortical data
  if(!is.null(subcortical)) {
    if(class(subcortical) != 'matrix') stop('subcortical must be a matrix (or NULL), but it is not.')
    if(length(dim(mask)) != 3) stop('mask must be a 3-dimensional array, but it is not.')
    vals_mask <- sort(unique(as.vector(mask)))*1
    if(!all.equal(vals_mask, c(0,1))) stop('mask must be logical or 0/1, but it is not.')
    if(sum(mask) != nrow(subcortical)) stop(paste0('The number of voxels in the mask (',sum(mask),') must equal the number of rows in subcortical (',nrow(subcortical),'), but they do not match.'))
    if(!all.equal(dim(mask),dim(labels))) stop('mask and labels must have the same dimensions, but they do not.')
  }
  check_cols <- c(ncol(cortex_left), ncol(cortex_right), ncol(subcortical))
  if(length(unique(check_cols)) > 1) stop('If provided, cortex_left, cortex_right and subcortical must all have the same number of columns (measurements), but they do not.')

  cifti_out <- vector('list', 6)
  class(cifti_out) <- 'cifti'
  names(cifti_out) <- c("CORTEX_LEFT","CORTEX_RIGHT","SURF_LEFT", "SURF_RIGHT", "VOL","LABELS")

  if(!is.null(cortex_left)) cifti_out$CORTEX_LEFT <- cortex_left
  if(!is.null(cortex_right)) cifti_out$CORTEX_RIGHT <- cortex_right
  if(!is.null(surf_left)) cifti_out$SURF_LEFT <- surf_left
  if(!is.null(surf_right)) cifti_out$SURF_RIGHT <- surf_right
  if(!is.null(subcortical)){
    cifti_out$VOL <- array(0, dim=c(dim(mask),ncol(subcortical)))
    for(ii in 1:ncol(subcortical)){ cifti_out$VOL[,,,ii][mask==1] <- subcortical[,ii] }
    cifti_out$LABELS <- labels
  }

  return(cifti_out)
}
