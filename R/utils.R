#' Summarise cifti objects
#'
#' Summary method for class "cifti"
#'
#' @param object an object of class "cifti"
#' @param ... further arguments passed to or from other methods.
#' @export
#' @method summary cifti
summary.cifti <- function(object, ...){
  out <- list()
  class(out) <- "summary.cifti"
  out$includes <- names(object)[!sapply(object, is.null)]
  if('VOL' %in% out$includes) out$includes <- c(out$includes[!(out$includes %in% c('VOL','LABELS'))],'SUBCORTICAL')
  if('CORTEX_LEFT' %in% out$includes) out$CORTEX_LEFT <- dim(object$CORTEX_LEFT)
  if('CORTEX_RIGHT' %in% out$includes) out$CORTEX_RIGHT <- dim(object$CORTEX_RIGHT)
  if('SURF_LEFT' %in% out$includes) out$SURF_LEFT <- names(out$SURF_LEFT)
  if('SURF_RIGHT' %in% out$includes) out$SURF_RIGHT <- names(out$SURF_RIGHT)
  if('SUBCORTICAL' %in% out$includes) out$VOL <- list(dim_vol=dim(object$VOL), nvox_vol=sum(object$LABELS>0))
  if('SUBCORTICAL' %in% out$includes) out$LABELS <- table(object$LABELS[object$LABELS>0])
  return(out)
}


#' @param x an object of class "summary.cifti"
#' @export
#' @method print summary.cifti
#' @rdname summary.cifti
print.summary.cifti <- function(x, ...){
  cat("Brain Structures: ", paste(x$includes, collapse=', '), " \n")
  if('CORTEX_LEFT' %in% x$includes) cat("Left Cortex: ", x$CORTEX_LEFT[1], "surface vertices, ", x$CORTEX_LEFT[2], "measurements \n")
  if('CORTEX_RIGHT' %in% x$includes) cat("Right Cortex: ", x$CORTEX_RIGHT[1], "surface vertices, ", x$CORTEX_RIGHT[2], "measurements \n")
  if('SURF_LEFT' %in% x$includes) cat("Left Surface Models: ", paste(x$SURF_LEFT, collapse=', '))
  if('SURF_RIGHT' %in% x$includes) cat("Right Surface Models: ", paste(x$SURF_RIGHT, collapse=', '))
  if('SUBCORTICAL' %in% x$includes){
    cat("Subcortical: ", x$VOL[[2]], "voxels, ", x$VOL[[1]][4], "measurements \n")
    cat("Subcortical Labels:")
    print(x$LABELS)
  }
}

#' @export
#' @method print cifti
#' @rdname summary.cifti
print.cifti <- function(x, ...) {
  print.summary.cifti(summary(x))
}


is.cifti <- function(x){
  if(!is.list(x)) {message('not a list'); return(FALSE)}
  if(!all.equal(names(x), c('CORTEX_LEFT','CORTEX_RIGHT','SURF_LEFT','SURF_RIGHT','VOL','LABELS'))) { message('names are not correct'); return(FALSE) }
  if(!is.null(x$CORTEX_LEFT)){ if(!is.matrix(x$CORTEX_LEFT)) { message('x$CORTEX_LEFT not a matrix.'); return(FALSE) } }
  if(!is.null(x$CORTEX_RIGHT)){ if(!is.matrix(x$CORTEX_RIGHT)) { message('x$CORTEX_RIGHT not a matrix.'); return(FALSE) } }
  if(!is.null(x$SURF_LEFT)){
    if(!is.list(x$SURF_LEFT)) { message('x$SURF_LEFT not a list'); return(FALSE) }
    if(min(sapply(x$SURF_LEFT, is.surface)) == 0) { message('At least one element of x$SURF_LEFT not a valid surface object.'); return(FALSE) }
  }
  if(!is.null(x$VOL)){ if(!is.array(x$VOL) | !is.numeric(x$VOL)) { message('x$VOL not a numeric array'); return(FALSE) } }
  if(!is.null(x$LABELS)){ if(!is.array(x$LABELS)  | !is.numeric(x$LABELS)) { message('x$LABELS not a numeric array'); return(FALSE) } }

  return(TRUE)
}

is.surface <- function(x){
  if(!is.list(x)) { message('Not a list'); return(FALSE) }
  if(length(x) != 2) { message('Must be a list with 2 elements'); return(FALSE) }
  if(!all.equal(names(x), c('vertices','faces'))) { message('Elements of x must be named "vertices" and "faces"'); return(FALSE) }
  if(ncol(x$vertices) != 3) { message('x$vertices must have 3 columns'); return(FALSE) }
  if(ncol(x$faces) != 3) { message('x$faces must have 3 columns'); return(FALSE) }
  if(!is.numeric(x$faces)) { message('x$faces must be numeric'); return(FALSE) }
  if(!is.numeric(x$vertices)) { message('x$vertices must be numeric'); return(FALSE) }
  if(!all.equal(x$faces, round(x$faces), check.attributes=FALSE)) { message('x$faces must be only integers'); return(FALSE) }

  V <- nrow(x$vertices)
  if(max(x$faces) > V) { message('Max vertex index in x$faces is too high'); return(FALSE) }
  if(min(x$faces) < 1) { message('Min vertex index in x$faces is too low'); return(FALSE) }

  return(TRUE)
}

