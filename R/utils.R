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


#' Checks whether object is a valid cifti object
#'
#' @param x A list in the format of a cifti object.
#'
#' @return Logical indicating whether x is a valid cifti object
#' @export
#'
is.cifti <- function(x){
  if(!is.list(x)) {message('x is not a list'); return(FALSE)}
  names_x <- names(x)
  if(length(names_x) != 6) { message('x must contain 6 elements (CORTEX_LEFT, CORTEX_RIGHT, SURF_LEFT, SURF_RIGHT, VOL, LABELS)'); return(FALSE) }
  names_ok <- sapply(names_x, function(name) return(name %in% c('CORTEX_LEFT','CORTEX_RIGHT','SURF_LEFT','SURF_RIGHT','VOL','LABELS')))
  if(min(names_ok) == 0) {
    message('Elements of x must be named CORTEX_LEFT, CORTEX_RIGHT, SURF_LEFT, SURF_RIGHT, VOL, LABELS'); return(FALSE)
  }
  if(!is.null(x$CORTEX_LEFT)){ if(!is.matrix(x$CORTEX_LEFT)) { message('x$CORTEX_LEFT not a matrix.'); return(FALSE) } }
  if(!is.null(x$CORTEX_RIGHT)){ if(!is.matrix(x$CORTEX_RIGHT)) { message('x$CORTEX_RIGHT not a matrix.'); return(FALSE) } }

  if(!is.null(x$SURF_LEFT)){
    nsurf_left <- length(x$SURF_LEFT)
    if(!is.list(x$SURF_LEFT)) { message('x$SURF_LEFT not a list'); return(FALSE) }
    if(min(sapply(x$SURF_LEFT, is.cifti_surface)) == 0) { message('At least one element of x$SURF_LEFT not a valid surface object.'); return(FALSE) }
    nvert_left <- sapply(x$SURF_LEFT, function(x) nrow(x$vertices))
    if((min(nvert_left) != max(nvert_left))) { message('All surfaces in x$SURF_LEFT must have the same number of vertices.'); return(FALSE) }
    if(!is.null(x$CORTEX_LEFT)) { if(nvert_left[1] != nrow(x$CORTEX_LEFT)) { message('Number of vertices in x$CORTEX_LEFT and surfaces in x$SURF_LEFT must match.'); return(FALSE) } }
  }

  if(!is.null(x$SURF_RIGHT)){
    nsurf_right <- length(x$SURF_RIGHT)
    if(!is.list(x$SURF_RIGHT)) { message('x$SURF_RIGHT not a list'); return(FALSE) }
    if(min(sapply(x$SURF_RIGHT, is.cifti_surface)) == 0) { message('At least one element of x$SURF_RIGHT not a valid surface object.'); return(FALSE) }
    nvert_right <- sapply(x$SURF_RIGHT, function(x) nrow(x$vertices))
    if((min(nvert_right) != max(nvert_right))) { message('All surfaces in x$SURF_RIGHT must have the same number of vertices.'); return(FALSE) }
    if(!is.null(x$CORTEX_RIGHT)) { if(nvert_right[1] != nrow(x$CORTEX_RIGHT)) { message('Number of vertices in x$CORTEX_RIGHT and surfaces in x$SURF_RIGHT must match.'); return(FALSE) } }
  }

  if(!is.null(x$SURF_RIGHT) & !is.null(x$SURF_LEFT)){
    if(nsurf_left != nsurf_right) warning('The number of surface models in x$SURF_RIGHT and x$SURF_LEFT are not the same.')
  }

  if(!is.null(x$VOL)){ if(!is.array(x$VOL) | !is.numeric(x$VOL)) { message('x$VOL not a numeric array'); return(FALSE) } }
  if(!is.null(x$LABELS)){ if(!is.array(x$LABELS)  | !is.numeric(x$LABELS)) { message('x$LABELS not a numeric array'); return(FALSE) } }

  return(TRUE)
}

#' Checks whether object is a valid cifti_surface object
#'
#' @param x A list in the format of a cifti_surface object.
#'
#' @return Logical indicating whether x is a valid cifti_surface object
#' @export
is.cifti_surface <- function(x){
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

#' Converts a file path, GIFTI object, or cifti_surface object to a cifti_surface object.
#'
#' @param surf What to make a cifti_surface from
#'
#' @return The cifti_surface object.
#' @export
#'
#' @importFrom gifti readGIfTI is.gifti
make_cifti_surface <- function(surf){
  gifti_to_surf <- function(gifti){
    surf <- gifti$data
    verts <- surf$pointset
    faces <- surf$triangle
    if(min(faces)==0) faces <- faces + 1 #start vertex indexing at 1 instead of 0
    surf <- list(vertices = verts, faces = faces)
    class(surf) <- "cifti_surface"
    return(surf)
  }
  # file path
  if(is.character(surf) & length(surf)==1 & file.exists(surf) & !dir.exists(surf)){
    surf <- gifti::readGIfTI(surf)
  }
  # GIFTI
  if(is.gifti(surf)){ surf <- gifti_to_surf(surf) }
  # Return cifti_surface or error.
  if(!is.cifti_surface(surf)){ 
    stop("The object could not be converted into a cifti_surface object.")
  } 
  return(surf)
}

#' Gets CIFTI file extension
#'
#' @param fname_cifti Path to CIFTI file, including full file name and extension
#'
#' @return Character file extension of CIFTI file, e.g. 'dtseries.nii', 'dscalar.nii'.
#' @export
#'
get_cifti_extn <- function(fname_cifti){
  fname_cifti <- basename(fname_cifti)
  fname_parts <- unlist(strsplit(fname_cifti, split='.', fixed = TRUE)) #split by "."
  extn <- paste(rev(fname_parts)[c(2,1)], collapse='.') #"dtseries.nii", "dscalar.nii", etc.
  return(extn)
}

#' Normalizes a path, placing it in a dir if it is relative. If the path is already absolute,
#'  dir is ignored.
#'
#' @param path The path to normalize.
#' @param dir The directory to look in, if the path is relative.
#' 
#' @importFrom R.utils isAbsolutePath
#' 
#' @return The normalized, absolute path.
#' 
make_abs_path <- function(path, dir=NULL){
  if(!is.null(path)){
    if(!is.null(dir)){
      path <- ifelse(isAbsolutePath(path), path, file.path(dir, path))
    }
    path <- normalizePath(path, mustWork=FALSE)
  }
  return(path)
}

#' Helper function that checks if a directory exists. If it is NULL, use a default (default is the current working directory).
#' Raises an error if the directory does not exist.
#' 
#' @param dir The directory.
#' @param default Use this if dir is NULL (default is the current working directory).
#' @param make If the directory does not exist, should it be made? Default is FALSE.
#'
#' @return The directory.
#' 
check_dir <- function(dir, default=NULL, make=FALSE){
  # TO DO: get absolute path to dir?
  if(identical(dir, NULL)){ 
    if(identical(default, NULL)){ default <- getwd() }
    dir <- default
  } 
  if(!dir.exists(dir)){ 
    if(make){
      dir.create(dir)
    } else {
      stop(paste("The directory", dir, "does not exist, check and try again or use make==TRUE.")) 
    }
  }
  # TO DO: Check that the user has write permissions in outdir
  return(dir)
}

#' Get the default file name suffix for a certain type of separated file.
#'
#' @param label the file type: one of "cortexL", "cortexR", "subcortVol" or "subcortLab"
cifti_separate_default_suffix <- function(label){
  label <- match.arg(label, c("cortexL", "cortexR", "subcortVol", "subcortLab",
    "cortexL_ROI", "cortexR_ROI", "subcort_ROI"))
  switch(label,
    cortexL = "L.func.gii",
    cortexR = "R.func.gii",
    subcortVol = "nii",
    subcortLab = "labels.nii",
    cortexL_ROI = "L_ROI.func.gii",
    cortexR_ROI = "R_ROI.func.gii",
    subcort_ROI = "ROI.nii",
  )
}