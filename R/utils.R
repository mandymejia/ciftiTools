#' Format a path
#'
#' Normalize and validate a path. Optionally, place it in a directory. 
#'
#' @param path The path to normalize.
#' @param dir (Optional) the directory to append to the beginning of the path.
#'  \code{NULL} (default) to not append any directory, leaving \code{path}
#'  unchanged.
#' @param mode The mode for \code{\link{file.access}} to verify existence, 
#'  writing permission, or reading permission. Use NA (default) to not perform 
#'  any check.
#' 
#' @return The normalized path, or \code{NULL} if the path was \code{NULL}.
#' 
format_path <- function(path, dir=NULL, mode=NA) {

  # Do nothing if the path is NULL.
  if (is.null(path)) { return(path) }

  # Append dir if provided.
  if (!is.null(dir)) { path <-file.path(dir, path) }
  path <- normalizePath(path, mustWork=FALSE)

  # Create the directory if it does not exist and the ciftiTools option 
  #   "make_dirs" is TRUE.
  if (!file.exists(dirname(path))) { 
    if (getOption("ciftiTools_make_dirs")) {
      # Suppress warnings if the directory goes back and forth, like this:
      #   my/dir/../dir/file.txt
      dir.create(dirname(path), recursive=TRUE, showWarnings=FALSE)
    } else {
      stop(paste0(
        "The directory \"", dirname(path), 
        "\" does not exist. Check and try again, or use ",
        "`ciftiTools.setOption(\"make_dirs\", TRUE)}`."
      )) 
    }
  }

  # Get the full file path (for Linux: previous normalizePath() does not get 
  #   full file path if dir did not exist.)
  path <- file.path(normalizePath(dirname(path)), basename(path))

  # Check existence/writing permission/reading permission of the path.
  #   [TO DO]: Resolve-- "Please note that it is not a good idea to use this 
  #   function to test before trying to open a file. On a multi-tasking system, 
  #   it is possible that the accessibility of a file will change between the  
  #   time you call file.access() and the time you try to open the file. It is 
  #   better to wrap file open attempts in try.
  stopifnot(all(mode %in% c(NA, 0, 2, 4)))
  for(m in mode) {
    if (is.na(mode)) { next }
    if (file.access(dirname(path), m) != 0) { 
      warning(paste0(
        "The directory \"", dirname(path), "\"",
        c("doesn't exist", "", "is not writeable", "", "is not readable")[m+1],
        ".\n"
      ))
    }
  }

  path
}

#' File name check
#'
#' Simple check if something is an existing file.
#'
#' @param x The potential file name
#'
#' @return Whether \code{x} is an existing file.
#'
is.fname <- function(x){
  if(!(length(x)==1 & is.character(x))){ return(FALSE) }
  file.exists(x) & !dir.exists(x)
}

#' Format a path for \code{\link{system}}. Right now, it just escapes spaces and
#'  parentheses with \code{"\\\\"}.
#'
#' @param R_path The name of the file. It should be properly formatted: if it 
#'  exists, \code{file.exists(R_path)} should be \code{TRUE}.
#'
#' @return The name of the file.
#'
sys_path <- function(R_path) {
  R_path <- gsub("(", "\\(", R_path, fixed=TRUE)
  R_path <- gsub(")", "\\)", R_path, fixed=TRUE)
  gsub(" ", "\\ ", R_path, fixed=TRUE)
}

#' Get the names of the arguments of a function as a character vector.
#'
#' @param fun The function to get the argument names for.
#'
#' @return The names of the arguments of \code{fun} as a character vector.
#'
get_kwargs <- function(fun) {
  kwargs <- names(as.list(args(fun)))
  kwargs <- kwargs[1:(length(kwargs)-1)] # last is empty
}

#' Merges two kwarg lsits. If a kwarg is present in both lists but with different values,
#' an error is raised.
#' @param kwargsA The first list of kwargs.
#' @param kwargsB The second list of kwargs. If duplicates are present, the default
#'  message recommends the user to remove the kwarg here in favor of placing the
#'  correct one in \code{kwargsA}.
#' @param labelA (Optional) Descriptor of \code{kwargsA} for error statement. Default "first kwarg(s)".
#' @param labelB (Optional) Descriptor of \code{kwargsB} for error statement. Default "second kwarg(s)".
#' @param extraMsg (Optional) Extra text for error statement. "[DEFAULT]" (default) will use this message: 
#'  "Note that a kwarg only has to be provided to one of these. Place the correct value in the first 
#'  location and remove the kwarg from the second location".
#'
#' @return A list with the union of \code{kwargsA} and \code{kwargsB}.
#'
merge_kwargs <- function(kwargsA, kwargsB, 
  labelA="first kwarg(s)", labelB="second kwarg(s)", 
  extraMsg="[DEFAULT]") {

  # Identify repeated kwargs.
  repeatedB_bool <- names(kwargsB) %in% names(kwargsA)
  repeated <- names(kwargsB)[repeatedB_bool]
  # Stop if any repeated kwargs differ.
  kwargs_mismatch <- !mapply(identical, kwargsA[repeated], kwargsB[repeated])
  if (sum(kwargs_mismatch) > 0) {
    if(identical(extraMsg, "[DEFAULT]")){
      extraMsg <- "Note that a kwarg only has to be provided to one of these. \
        Place the correct value in the first location and remove the kwarg \
        from the second location"
    }
    stop(paste0(
      "A keyword argument(s) was provided twice with different values. Here is the kwarg(s) in disagreement:\n",
      "The ", labelA, " was:\n",
      "\"", paste0(kwargsA[kwargs_mismatch], collapse="\", \""), "\".\n",
      "The ", labelB, " was:\n",
      "\"", paste0(kwargsB[kwargs_mismatch], collapse="\", \""), "\".\n",
      extraMsg
    ))
  }
  kwargs <- c(kwargsA, kwargsB[!repeatedB_bool])
}

#' Match user inputs to expected values
#'
#' Match each user input to an expected/allowed value. Raise a warning if either 
#'  several user inputs match the same expected value, or at least one could not
#'  be matched to any expected value. \code{ciftiTools} uses this function to 
#'  match keyword arguments for a function call. Another use is to match 
#'  brainstructure labels ("left", "right", or "subcortical").
#'
#' @param user Character vector of user input. These will be matched to 
#'  \code{expected} using \code{match.arg()}.
#' @param expected Character vector of expected/allowed values.
#' @param unrecognized_kwarg_action If any value in \code{user} could not be 
#'  matched, or repeated matches occured, what should happen? Possible values 
#'  are \code{"warning"} (default), \code{"stop"} (raise error), and 
#'  \code{"nothing"}. 
#'
#' @return The matched user inputs.
#'
match_input <- function(
  user, expected, 
  unrecognized_kwarg_action=c("warning", "stop", "nothing")) {

  matched <- match.arg(user, expected, several.ok=TRUE)
  unrecognized_kwarg_action <- match.arg(
    unrecognized_kwarg_action, 
    c("warning", "stop", "nothing")
  )
  if (length(matched) != length(user)) {
    msg <- paste0(
      "User-input kwargs did not match true kwargs. ",
      "Either several matched the same true kwarg, ",
      "or at least one did not match any.\n",
      "The user-input kwargs were:\n",
      "\"", paste0(user, collapse="\", \""), "\".\n",
      "The true kwargs were:\n",
      "\"", paste0(expected, collapse="\", \""), "\".",
    )
    if (unrecognized_kwarg_action=="stop") {
      stop(msg)
    } else if (unrecognized_kwarg_action=="warning") {
      warning(msg)
    }
  }

  matched
}