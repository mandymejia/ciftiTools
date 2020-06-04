cwb_option_request <- function(){
  return(paste(
    "\n********************************************************************************",
    "ciftiTools requires the path to the Connectome Workbench 'wb_command.exe' file.",
    "Please execute `options(ciftiTools_cwb_path = 'path/to/wb_command.exe')`",
    "(recommended), or provide the path to each function with the `wb_dir` argument.",
    "********************************************************************************\n",
    sep='\n'
  ))
}

check_wb_dir <- function(wb_dir){
  # Retrieve the wb_dir path. Prioritize the function argument over the option.
  if(is.null(wb_dir)){
    wb_dir <- getOption("ciftiTools_cwb_path")
    if(is.null(wb_dir)){
      stop(cwb_option_request())
    } 
    wb_dir_source <- "option"
  } else {
    if(!is.null(getOption("ciftiTools_cwb_path"))){
      print("Overriding existing 'ciftiTools_cwb_path' option with 'wb_dir' argument provided in function call.")
    }
    wb_dir_source <- "arg"
  }

  # Check if wb_dir is valid. 
  wb_dir_source_name <- ifelse(wb_dir_source=="option", 
    "The 'ciftiTools_cwb_path' option", 
    "The 'wb_dir' argument in the function call")
  if(!file.exists(wb_dir)){
    stop(paste(wb_dir_source_name, "is set as", wb_dir, "which does not reference an existing file."))
  }

  return(wb_dir)
}

.onAttach <- function(...){
  if(interactive()){
    if(is.null(getOption("ciftiTools_cwb_path"))){
      packageStartupMessage(cwb_option_request())
    }
  }
}
