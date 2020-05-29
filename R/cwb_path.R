cwb_option_request <- function(){
  return(
    "The path to the Connectome Workbench 'wb_command.exe' file must be provided to ciftiTools.
      Please use `options(ciftiTools_cwb_path = 'path/to/wb_command.exe')` (recommended), 
      or provide the path to each function with a `wb_cmd` argument."
  )
}

check_wb_cmd <- function(wb_cmd){
  if(is.null(wb_cmd)){
    wb_cmd <- getOption("ciftiTools_cwb_path")
    if(is.null(wb_cmd)){
      stop(cwb_option_request())
    } else {
      if(!file.exists(wb_cmd)){
        stop("The 'ciftiTools_cwb_path' option is set but does not reference an existing file.")
      }
    }
  } else {
    if(!is.null(getOption("ciftiTools_cwb_path"))){
      print("Overriding existing 'ciftiTools_cwb_path' option with 
        'wb_cmd' argument provided in function call.")
    }
    if(!file.exists(wb_cmd)){
      stop("The 'ciftiTools_cwb_path' argument was provided, but does not reference an existing file.")
    }
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