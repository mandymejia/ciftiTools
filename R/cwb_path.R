cwb_option_request <- function(){
  return(paste(
    "\n********************************************************************************",
    "ciftiTools requires the path to the Connectome Workbench 'wb_command.exe' file.",
    "Please execute `options(ciftiTools_cwb_path = 'path/to/wb_command.exe')`",
    "(recommended), or provide the path to each function with the `wb_cmd` argument.",
    "********************************************************************************\n",
    sep='\n'
  ))
}

check_wb_cmd <- function(wb_cmd){
  # Retrieve the wb_cmd path. Prioritize the function argument over the option.
  if(is.null(wb_cmd)){
    wb_cmd <- getOption("ciftiTools_cwb_path")
    if(is.null(wb_cmd)){
      stop(cwb_option_request())
    } 
    wb_cmd_source <- "option"
  } else {
    if(!is.null(getOption("ciftiTools_cwb_path"))){
      print("Overriding existing 'ciftiTools_cwb_path' option with 
        'wb_cmd' argument provided in function call.")
    }
    wb_cmd_source <- "arg"
  }

  # Check if wb_cmd is valid. 
  wb_cmd_source_name <- ifelse(wb_cmd_source=="option", 
    "The 'ciftiTools_cwb_path' option", 
    "The 'wb_cmd' argument in the function call")
  if(!file.exists(wb_cmd)){
    stop(paste(wb_cmd_source_name, "is set as", 
      wb_cmd, "which does not reference an existing file."))
  }

  return(wb_cmd)
}

.onAttach <- function(...){
  if(interactive()){
    if(is.null(getOption("ciftiTools_cwb_path"))){
      packageStartupMessage(cwb_option_request())
    }
  }
}
