#' Make cifti object
#'
#' @param cortex_left Data matrix for left cortex, with vertices in rows
#' @param cortex_right Data matrix for right cortex, with vertices in rows
#' @param surf_left Surface model for left cortex (a list with two elements, vertices and faces)
#' @param surf_right Surface model for right cortex (a list with two elements, vertices and faces)
#' @param subcortical Data matrix for subcortical locations, with voxels in rows
#' @param mask Volumetric brain mask for subcortical locations
#' @param labels Volumetric labels for subcortical ROIs
#'
#' @return Object of class 'cifti'
#' @export
#'
cifti_make <- function(cortex_left=NULL, cortex_right=NULL, surf_left=NULL, surf_right=NULL, subcortical=NULL, mask=NULL, labels=NULL){

  #check argument compatibility
  if(!is.null(subcortical)) { if(is.null(mask)) stop('If subcortical is provided, mask must be provided also.')} else mask <- NULL
  if(!is.null(subcortical)) { if(is.null(labels)) stop('If subcortical is provided, labels must be provided also.')} else labels <- NULL
  if(!is.null(cortex_left)) { if(class(cortex_left) != 'matrix') stop('cortex_left must be a matrix (or NULL), but it is not.') }
  if(!is.null(cortex_right)) { if(class(cortex_right) != 'matrix') stop('cortex_right must be a matrix (or NULL), but it is not.') }

  #check formatting of surf_left
  if(!is.null(surf_left)){
    if(class(surf_left) != 'list' | !all.equal(names(surf_left), c('vertices','faces')) ) stop('surf_left must be NULL or a list with two elements: vertices and faces')
    if(!is.null(cortex_left)) { if(nrow(cortex_left) != nrow(surf_left$vertices)) stop('cortex_left and surf_left must have same number of vertices.')}
    if(ncol(surf_left$vertices) != 3 | class(surf_left$vertices) != 'matrix') stop('surf_left$vertices must be a matrix with 3 columns')
    if(ncol(surf_left$faces) != 3 | class(surf_left$faces) != 'matrix') stop('surf_left$faces must be a matrix with 3 columns')
    if(min(surf_left$faces) == 0) stop('Vertex indexing in surf_left$faces should start at 1, not 0.')
  }

  #check formatting of surf_right
  if(!is.null(surf_right)){
    if(class(surf_right) != 'list' | !all.equal(names(surf_right), c('vertices','faces')) ) stop('surf_right must be NULL or a list with two elements: vertices and faces')
    if(!is.null(cortex_right)) { if(nrow(cortex_right) != nrow(surf_right$vertices)) stop('cortex_right and surf_right must have same number of vertices.')}
    if(ncol(surf_right$vertices) != 3 | class(surf_right$vertices) != 'matrix') stop('surf_right$vertices must be a matrix with 3 columns')
    if(ncol(surf_right$faces) != 3 | class(surf_right$faces) != 'matrix') stop('surf_right$faces must be a matrix with 3 columns')
    if(min(surf_right$faces) == 0) stop('Vertex indexing in surf_right$faces should start at 1, not 0.')
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
