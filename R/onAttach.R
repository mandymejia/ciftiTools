#' Message on attach
#' 
#' Direct user to indicate Connectome Workbench path and check gifti package version.
#'
#' @param ... Not used
#' 
#' @return \code{NULL}, invisibly
#'
#' @keywords internal
.onAttach <- function(...) {
  if (interactive()) {
    if (is.null(getOption("ciftiTools_wb_path"))) {
      packageStartupMessage(welcome_msg())
    }
  }
  ciftiTools.setOption("EPS", 1e-8)
  ciftiTools.setOption("suppress_msgs", TRUE)

  invisible(NULL)
}

#' Welcome message
#' 
#' Print the welcome message.
#' 
#' @keywords internal
#' @return The message (length 1 character vector)
welcome_msg <- function() {
  paste0(
    "\n*****************************************************************\n",
    "*   Welcome to ciftiTools! Please cite our paper in your work:  *\n", 
    "*                   > citation('ciftiTools')                    *",
    wb_path_request()
  )
}