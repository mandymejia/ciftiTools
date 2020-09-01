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

  gifti_version <- try(packageVersion("gifti"), silent=TRUE)
  if ("package_version" %in% class(gifti_version)) {
    if (gifti_version < "0.7.5.9001") {
      warning(paste(
        "'gifti' package must be installed from github, not CRAN, for writecii.",
        "Run devtools::install_github('muschellij2/gifti')"
      ))
    }
  }
}