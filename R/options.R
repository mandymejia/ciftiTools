#' Checks if a ciftiTool option (and value) are valid.
#'
#' @param opt The option. 
#' @param val The value to set the option as.
#' 
#' @return \code{NULL}.
#'
ciftiTools.checkOption <- function(opt, val=NULL){
  stopifnot(opt %in% c("wb_path", "make_dirs", "EPS"))
  if(is.null(val)){ return(invisible(NULL)) }
  if (opt == "wb_path") {
    if (!file.exists(val)) { 
      warning(paste0("The wb_path value '" , normalizePath(val, mustWork=FALSE), "' does not exist.")) 
    }
  } else if (opt == "make_dirs") {
    if (!is.logical(val)) {
      stop("The make_dirs option must be TRUE or FALSE.")
    }
  } else if (opt == "EPS") {
    stopifnot(is.numeric(val))
    stopifnot(val > 0)
    stopifnot(val < 1)
  } else { stop() }
  invisible(NULL)
}

#' Sets an R option (with prefix "ciftiTools_"). Right now, the ciftiTool
#'  options are "wb_path" (path to Workbench Command), "make_dirs"
#'  (Should a directory to write in be made if it does not exist, or should an 
#'  error be raised?), and "EPS" (Threshold for detecting constant voxels).
#'
#' @param opt The option. 
#' @param val The value to set the option as.
#'
#' @return The new value, \code{val}.
#' @export
#'
ciftiTools.setOption <- function(opt, val) {
  ciftiTools.checkOption(opt, val)
  val <- list(val)
  names(val) <- paste0("ciftiTools_", opt)
  options(val)
  invisible(val)
}

#' Gets an R option (with prefix "ciftiTools_") value. Right now, the ciftiTool
#'  options are "wb_path" (path to Workbench Command), "make_dirs"
#'  (Should a directory to write in be made if it does not exist, or should an 
#'  error be raised?), and "EPS" (Threshold for detecting constant voxels).
#'
#' @param opt The option.
#'
#' @return The value, \code{val}.
#' @export
#'
ciftiTools.getOption <- function(opt) {
  ciftiTools.checkOption(opt)
  getOption(paste0("ciftiTools_", opt))
}

.onAttach <- function(...) {
  if (interactive()) {
    if (is.null(getOption("ciftiTools_wb_path"))) {
      packageStartupMessage(wb_path_request())
    }
  }
  ciftiTools.setOption("make_dirs", FALSE)
  ciftiTools.setOption("EPS", 1e-8)
}
