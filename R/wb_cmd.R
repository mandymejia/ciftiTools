#' Request \code{"wb_path"}
#' 
#' Print a message requesting that the \code{ciftiTools} option \code{"wb_path"}
#'  be set.
#' 
#' @keywords internal
#' @return The message (length 1 character vector)
wb_path_request <- function() {
  paste(
    "\n******************************************************************************************",
    "ciftiTools requires the path to the Connectome Workbench folder, or directly to the", 
    "`wb_command(.exe)`. Please execute `ciftiTools.setOption('wb_path', 'path/to/workbench')`.", 
    "Or, provide the path to each function with the `wb_path` argument.",
    "******************************************************************************************\n",
    sep='\n'
  )
}

#' Get the Connectome Workbench command path
#' 
#' Retrieves the path to the Connectome Workbench executable from a file path
#'  that may point to the executable itself, or to the Workbench folder which
#'  contains it (i.e., "path/to/workbench/bin_linux64/wb_command" or 
#'  "path/to/workbench".)
#' 
#' @param wb_path (Optional) Path to the Connectome Workbench folder or 
#'  executable. 
#' @return The path to the Connectome Workbench executable
#'
#' @export
#'
get_wb_cmd_path <- function(wb_path) {

  # If `wb_path` doesn't exist, raise a warning but still use it.
  # (Sometimes the file does exist, even if `file.exists` fails?)
  if (!file.exists(wb_path)) {
    warning(paste0(
      "The `wb_path` option value '",
      wb_path, "' does not reference an existing directory or file. ",
      "Will try to use it anyway. If this problem persists, try ",
      "providing the full path to the Connectome Workbench executable ",
      "rather than the path to the containing folder, or a relative path.\n"
    ))
    wb_cmd_path <- wb_path
    return(wb_cmd_path)
  }

  # First, check if `wb_path` points to the executable itself.
  if (grepl("wb_command$|wb_command\\.exe$", wb_path)) { 
    wb_cmd_path <- wb_path
  # If not, check if it points to a containing folder.
  } else {
    possible_paths <- c(
      # Containing folders (bin_*) within the Workbench.
      file.path(wb_path, "wb_command"),
      file.path(wb_path, "wb_command.exe"),
      # The Workbench folder itself.
      file.path(wb_path, "bin_linux64", "wb_command"),
      file.path(wb_path, "bin_rh_linux64", "wb_command"),
      file.path(wb_path, "bin_macosx64", "wb_command"),
      file.path(wb_path, "bin_windows64", "wb_command.exe")
    )
    possible_paths <- possible_paths[vapply(possible_paths, file.exists, FALSE)]
    if (length(possible_paths) == 0) {
      warning(paste0(
        "The `wb_path` option value '",
        wb_path, "' exists, but does not seem to be the Workbench executable, ",
        "nor a folder containing the executable. Will try to use it anyway. ",
        "If this problem persists, try ",
        "providing the full path to the Connectome Workbench executable ",
        "rather than the path to the containing folder, or a relative path.\n"
      ))
      wb_cmd_path <- wb_path
    } else if (length(possible_paths) > 1) {
      warning(paste0(
        "Found these possible Workbench executables: '",
        paste0(possible_paths, collapse="', '"), "'. ",
        "Using the first. If another should be used, provide the full path ",
        "to the executable rather than the path to the containing folder, or",
        "a relative path.\n"
      ))
      wb_cmd_path <- possible_paths[1]
    } else {
      # cat(paste0("Using this executable: '", possible_paths, "'.\n"))
      wb_cmd_path <- possible_paths
    }
  }

  wb_cmd_path
}

#' Wrapper for Connectome Workbench Commands
#'
#' Runs a Connectome Workbench command that has already been formatted.
#'
#' @param cmd The full command, beginning after the workbench path.
#' @param intern Return printed output? If \code{FALSE} (default), return
#'  logical indicating success instead.
#'
#' @return If \code{intern==FALSE}, a logical indicating if the command finished successfully.
#'  If \code{intern==TRUE}, the printed output of the command.
#'
run_wb_cmd <- function(cmd, intern=FALSE){
  wb_cmd <- ciftiTools.getOption("wb_path")
  if (is.null(wb_cmd)) { stop(wb_path_request()) }

  cmd <- paste(sys_path(wb_cmd), cmd)
  
  out <- system(cmd, intern=intern)
  #out_print <- invisible(capture.output( out <- system(cmd, intern=intern) ))

  ciftiTools_msg("Using the Connectome Workbench.")

  if (!intern) {
    out <- out == 0
    if (!out) {
      message(paste0(
        "The Connectome Workbench command failed with code ", out, 
        ". The command was:\n", cmd
      ))
    }
  }

  invisible(out)
}