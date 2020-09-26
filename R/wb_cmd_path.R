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
#' Retrieves the Connectome Workbench path from the \code{ciftiTools_wb_path} 
#'  option or the provided argument. This path can be the Connectome Workbench
#'  folder or the `wb_command` executable.
#' 
#' @inheritParams wb_path_Param
#' @param verbose Default: \code{FALSE}.
#'
#' @export
#'
get_wb_cmd_path <- function(wb_path=NULL, verbose=FALSE) {
  # Retrieve the wb_cmd path. Prioritize the function argument over the option.
  if (is.null(wb_path)) {
    wb_path <- getOption("ciftiTools_wb_path")
    if (is.null(wb_path)) {
      stop(wb_path_request())
    } 
    wb_path_source <- "option"
  } else {
    if (!is.null(getOption("ciftiTools_wb_path"))) {
      if (verbose) {
        cat("Overriding existing 'ciftiTools_wb_path' option with 'wb_path' argument provided in function call.")
      }
    }
    wb_path_source <- "arg"
  }

  # Check if wb_path is valid. 
  wb_path_source_name <- ifelse(wb_path_source=="option", 
    "The 'ciftiTools_wb_path' option", 
    "The 'wb_path' argument in the function call")
  if (!file.exists(wb_path)) {
    stop(paste(wb_path_source_name, "is set as", wb_path, "which does not reference an existing directory or file."))
  }

  infer_wb_cmd_path <- function(wb_path) {
    files_in_path <- list.files(wb_path)

    # Check known OS first.
    if ("bin_linux64" %in% files_in_path) {
      wb_cmd_path <- file.path(wb_path, "bin_linux64", "wb_command")
    } else if ("bin_rh_linux64" %in% files_in_path) {
      wb_cmd_path <- file.path(wb_path, "bin_rh_linux64", "wb_command")
    } else if ("bin_macosx64" %in% files_in_path) {
      wb_cmd_path <- file.path(wb_path, "bin_macosx64", "wb_command")
    } else if ("bin_windows64" %in% files_in_path) {
      wb_cmd_path <- file.path(wb_path, "bin_windows64", "wb_command.exe")
    } else {
      
      # Fail value: NULL
      wb_cmd_path <- NULL

      # If no recognized OS found, try regex.
      is_bin <- grepl("bin", files_in_path, fixed=TRUE)
      if (sum(is_bin) == 1) {
        bin <- files_in_path[is_bin]
        bin_files <- list.files(file.path(wb_path, bin))
        is_cmd <- grepl("wb_command", bin_files)

        if (sum(is_cmd) == 1) {
          wb_cmd_path <- file.path(wb_path, bin, bin_files[is_cmd])
        }
      }
    }
  }

  if (!dir.exists(wb_path)) {
    wb_cmd_path <- wb_path
  } else {
    wb_cmd_path <- infer_wb_cmd_path(wb_path)
    if (is.null(wb_cmd_path)) {
      # Maybe the zip superfolder was used.
      wb_cmd_path <- infer_wb_cmd_path(file.path(wb_path, "workbench")) 
      if (is.null(wb_cmd_path)) {
        stop(paste0("The Workbench folder ", wb_path, " exists but does not contain the wb_cmd in a `bin_[OS]64` folder. Tried to look for a file with `wb_command` in its name located in a folder with `bin` in its name, but this also failed (no such unique file)."))
      }
    }
    if (!file.exists(wb_cmd_path)) {
      stop(paste("The inferred wb_cmd path, ", wb_cmd_path, " does not reference an existing file."))
    }
  }

  wb_cmd_path
}