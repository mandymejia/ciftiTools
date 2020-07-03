wb_path_request <- function(){
  return(paste(
    "\n******************************************************************************************",
    "ciftiTools requires the path to the Connectome Workbench folder, or directly to the", 
    "`wb_command(.exe)`. Please execute `ciftiTools.setOption('wb_path', 'path/to/workbench')`.", 
    "Or, provide the path to each function with the `wb_path` argument.",
    "******************************************************************************************\n",
    sep='\n'
  ))
}

ciftiTools.setOption <- function(opt, val){
  stopifnot(opt %in% "wb_path")
  val <- list(val)
  names(val) <- paste0("ciftiTools_", opt)
  options(val)
  invisible(val)
}

get_wb_cmd_path <- function(wb_path, verbose=FALSE){
  # Retrieve the wb_cmd path. Prioritize the function argument over the option.
  if(is.null(wb_path)){
    wb_path <- getOption("ciftiTools_wb_path")
    if(is.null(wb_path)){
      stop(wb_path_request())
    } 
    wb_path_source <- "option"
  } else {
    if(!is.null(getOption("ciftiTools_wb_path"))){
      if(verbose){
        print("Overriding existing 'ciftiTools_wb_path' option with 'wb_path' argument provided in function call.")
      }
    }
    wb_path_source <- "arg"
  }

  # Check if wb_path is valid. 
  wb_path_source_name <- ifelse(wb_path_source=="option", 
    "The 'ciftiTools_wb_path' option", 
    "The 'wb_path' argument in the function call")
  if(!file.exists(wb_path)){
    stop(paste(wb_path_source_name, "is set as", wb_path, "which does not reference an existing directory or file."))
  }

  infer_wb_cmd_path <- function(wb_path){
    files_in_path <- list.files(wb_path)
    if("bin_linux64" %in% files_in_path){
      wb_cmd_path <- file.path(wb_path, "bin_linux64", "wb_command")
    } else if("bin_macosx64" %in% files_in_path){
      wb_cmd_path <- file.path(wb_path, "bin_macosx64", "wb_command")
    } else if("bin_windows64" %in% files_in_path){
      wb_cmd_path <- file.path(wb_path, "bin_windows64", "wb_command.exe")
    } else {
      wb_cmd_path <- NULL
    }
    return(wb_cmd_path)
  }

  if(!dir.exists(wb_path)){
    wb_cmd_path <- wb_path
  } else {
    wb_cmd_path <- infer_wb_cmd_path(wb_path)
    if(is.null(wb_cmd_path)){
      # Maybe the zip superfolder was used.
      wb_cmd_path <- infer_wb_cmd_path(file.path(wb_path, "workbench")) 
      if(is.null(wb_cmd_path)){
        stop(paste0("The Workbench folder ", wb_path, " exists but does not contain the wb_cmd in a bin_[OS]64 folder."))
      }
    }
    if(!file.exists(wb_cmd_path)){
      stop(paste("The inferred wb_cmd path, ", wb_cmd_path, " does not reference an existing file."))
    }
  }

  return(wb_cmd_path)
}

.onAttach <- function(...){
  if(interactive()){
    if(is.null(getOption("ciftiTools_wb_path"))){
      packageStartupMessage(wb_path_request())
    }
  }
}
