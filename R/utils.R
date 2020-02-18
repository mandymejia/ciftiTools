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
