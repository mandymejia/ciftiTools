#' Message on attach
#' 
#' Direct user to indicate Connectome Workbench path and check gifti package version.
#'
#' @param ... Not used
#' @importFrom utils packageVersion
#'
.onAttach <- function(...) {
  if (interactive()) {
    if (is.null(getOption("ciftiTools_wb_path"))) {
      packageStartupMessage(wb_path_request())
    }
  }
  ciftiTools.setOption("EPS", 1e-8)
  ciftiTools.setOption("suppress_msgs", TRUE)
}