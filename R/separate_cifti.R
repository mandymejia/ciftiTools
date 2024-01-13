#' \code{separate_cifti} wrapper
#'
#' Calls \code{separate_cifti} using the file names listed in the
#'  argument \code{sep_fnames}.
#'
#' Currently used by \code{read_cifti} and \code{resample_cifti}.
#'
#' @inheritParams cifti_fname_Param
#' @inheritParams brainstructures_Param_LR
#' @inheritParams ROI_brainstructures_Param_LR
#' @param sep_fnames  Where to write the separated files (override
#'  their default file names). This is a named list
#'  where each entry's name is a file type label, and each entry's value
#'  is a file name indicating where to write the corresponding separated file.
#'  The recognized file type labels are: "cortexL", "cortexR",
#'  "ROIcortexL", "ROIcortexR", "subcortVol", and "subcortLabs".
#'
#'  Entry values can be \code{NULL}, in which case a default file name will be
#'  used: see \code{\link{cifti_component_suffix}}. Default file names
#'  will also be used for files that need to be separated/written but without a
#'  corresponding entry in \code{sep_fnames}.
#'
#'  Entries in \code{sep_fnames} will be ignored if they are not needed
#'  based on \code{[ROI_]brainstructures}. For example, if
#'  \code{brainstructures="left"}, then \code{sep_fnames$cortexR} will be
#'  ignored if specified.
#'
#'  The \code{write_dir} argument can be used to place each separated file in
#'  the same directory.
#' @param write_dir Where should the separate files be placed? \code{NULL}
#'  (default) will write them to the current working directory.
#'
#'  \code{write_dir} must already exist, or an error will occur.
#'
#' @return The return value of the \code{separate_cifti} call
#'
#' @keywords internal
#'
separate_cifti_wrapper <- function(
  cifti_fname, brainstructures=NULL, ROI_brainstructures=NULL,
  sep_fnames=NULL, write_dir=NULL) {

  # Get kwargs.
  sep_kwargs <- list(
    cifti_fname=cifti_fname,
    brainstructures=brainstructures, ROI_brainstructures=ROI_brainstructures,
    write_dir=write_dir
  )

  # Get expected file names.
  expected_labs <- get_kwargs(ciftiTools::separate_cifti)
  expected_labs <- expected_labs[grepl("fname", expected_labs, fixed=TRUE)]
  expected_labs <- expected_labs[expected_labs != "cifti_fname"]
  # Check file names.
  if (!is.null(sep_fnames)) {
    match_input(names(sep_fnames), gsub("_.*", "", expected_labs),
      user_value_label="sep_fnames")
    sep_kwargs[names(sep_fnames)] <- sep_fnames
  }
  # Do separate_cifti.
  sep_kwargs[vapply(sep_kwargs, is.null, FALSE)] <- NULL
  do.call(separate_cifti, sep_kwargs)
}

#' Separate CIFTI: file names
#'
#' File paths for writing GIFTI and NIFTI files from a CIFTI or \code{"xifti"}
#' 
#' If \code{write_dir} is \code{NULL}, only components with provided file names
#'  will be written. If \code{write_dir} is not \code{NULL}, files for all
#'  existing brian components will be written to that directory. 
#'
#' @param intent 3002 (default), 3006, or 3007
#' @param brainstructures (Optional) character vector indicating a subset of 
#'  brain structure(s) to write: \code{"left"} cortex, \code{"right"} cortex, 
#'  and/or \code{"subcortical"} structures. Can also be \code{"all"} to write 
#'  out all existing brain structures. Default: \code{c("left","right")}.
#' @param ROI_brainstructures Which ROIs should be obtained? \code{"all"}
#'  (default) to obtain ROIs for each of the \code{brainstructures}. \code{NULL}
#'  to not obtain any ROIs. This should be a subset of \code{brainstructures}.
#' @param cortexL_fname,cortexR_fname (Optional) GIFTI file names
#'  (*.\[func/label\].gii) to save the \[left/right\] cortex data to. 
#'  dtseries and dscalar files should use "func", whereas dlabel files should
#'  use "label".
#'
#'  If \code{NULL} and \code{write_dir} is provided, defaults to
#'  \code{"*[L/R].\[func/label\].gii"}, where * is the file name component of
#'  \code{cifti_fname}.
#' @param ROIcortexL_fname,ROIcortexR_fname (Optional) GIFTI file names
#'  (*.\[func/label\].gii) to save the \[left/right\] cortex ROI to.
#'  dtseries and dscalar files should use "func", whereas dlabel files should
#'  use "label".
#'
#'  If \code{NULL} and \code{write_dir} is provided, defaults to
#'  \code{"*ROI_[L/R].\[func/label\].gii"}, where * is the file name component of
#'  \code{cifti_fname}.
#'
#'  The cortical ROIs typically represent the medial wall
#'  mask, with values of 1 for in-ROI (non-medial wall) vertices and 0 for
#'  out-of-ROI (medial wall) vertices. Will be written in \code{write_dir}.
#' @param subcortVol_fname,subcortLabs_fname (Optional) NIFTI file names to save
#'  the subcortical \[volume/labels\] to. Provide both or neither.
#'
#'  If \code{NULL} and \code{write_dir} is provided, defaults to
#'  \code{"*[/.labels].nii"}, where * is the file name component of
#'  \code{cifti_fname}.
#' @param ROIsubcortVol_fname (Optional) NIFTI file names to save
#'  the subcortical ROI to.
#'
#'  If \code{NULL} and \code{write_dir} is provided, defaults to
#'  \code{"*ROI.nii"}, where * is the file name component of
#'  \code{cifti_fname}.
#'
#'  The subcortical ROI typically represents the volumetric
#'  mask for the entire subcortical structure, with values of 1 for in-ROI
#'  (in subcortex) voxels and 0 for out-of-ROI (not in subcortex) voxels. Will
#'  be written in \code{write_dir}.
#' @param write_dir (Optional) A path to an existing directory. If provided,
#'  every component in the \code{"xifti"} will be written to this directory,
#'  using automatically-generated names if their \code{*_fname} argument was
#'  not provided. Otherwise if \code{write_dir} is \code{NULL}, only the
#'  components for which their \code{*_fname} was provided will be written.
#'
#' @return List of three: \code{do}, \code{ROI_do}, and \code{sep_fnames}
#'
#' @keywords internal
separate_cifti_files <- function(
  intent=3006,
  brainstructures=c("left","right"),
  cortexL_fname=NULL, cortexR_fname=NULL,
  subcortVol_fname=NULL, subcortLabs_fname=NULL,
  ROI_brainstructures="all",
  ROIcortexL_fname=NULL, ROIcortexR_fname=NULL,
  ROIsubcortVol_fname=NULL,
  write_dir=NULL){

  if (is.null(write_dir) && all(c(
    is.null(cortexL_fname), is.null(cortexR_fname),
    is.null(subcortVol_fname), is.null(subcortLabs_fname),
    is.null(ROIcortexL_fname), is.null(ROIcortexR_fname),
    is.null(ROIsubcortVol_fname)
  ))) {
    stop("Provide either `write_dir` or at least one of the individual separate file names to write.")
  }

  # Get the brainstructures.
  brainstructures <- match_input(
    brainstructures, c("left","right","subcortical","all"),
    user_value_label="brainstructures"
  )
  if ("all" %in% brainstructures) {
    brainstructures <- c("left","right","subcortical")
  }

  # Subset the brainstructures if `write_dir` was not provided.
  do <- c("left","right","subcortical","subcortical") %in% brainstructures
  if (is.null(write_dir)) {
    do <- do & (!c(
      is.null(cortexL_fname),
      is.null(cortexR_fname),
      is.null(subcortVol_fname),
      is.null(subcortLabs_fname)
    ))
  }
  names(do) <- c("left", "right", "subVol", "subLab")

  # Get the ROI brainstructures.
  if (!is.null(ROI_brainstructures)) {
    ROI_brainstructures <- match_input(ROI_brainstructures, c(brainstructures, "all"), user_value_label="ROI_brainstructures")
    if ("all" %in% ROI_brainstructures) {
      ROI_brainstructures <- brainstructures
    }
    stopifnot(length(unique(ROI_brainstructures)) == length(ROI_brainstructures))
  }

  # Subset the ROI brainstructures if `write_dir` was not provided.
  ROI_do <- c("left","right","subcortical") %in% ROI_brainstructures
  if (is.null(write_dir)) {
    ROI_do <- ROI_do & (!c(
      is.null(ROIcortexL_fname),
      is.null(ROIcortexR_fname),
      is.null(ROIsubcortVol_fname)
    ))
  }
  names(ROI_do) <- c("left", "right", "sub")

  default_fname <- function(x){ paste0("sep.", cifti_component_suffix(x)) }

  if (do['left']) {
    # Left cortex
    if (is.null(cortexL_fname)) { cortexL_fname <- default_fname("cortexL") }
    cortexL_fname <- format_path(cortexL_fname, write_dir, mode=2)
    if (intent == 3007) { cortexL_fname <- gsub(".func.gii", ".label.gii", cortexL_fname, fixed=TRUE) }
    if (ROI_do['left']) {
      if (is.null(ROIcortexL_fname)) { ROIcortexL_fname <- default_fname("ROIcortexL") }
      ROIcortexL_fname <- format_path(ROIcortexL_fname, write_dir, mode=2)
    } else { ROIcortexL_fname <- NULL }
  } else { cortexL_fname <- ROIcortexL_fname <- NULL }

  if (do['right']) {
    # Right cortex
    if (is.null(cortexR_fname)) { cortexR_fname <- default_fname("cortexR") }
    cortexR_fname <- format_path(cortexR_fname, write_dir, mode=2)
    if (intent == 3007) { cortexR_fname <- gsub(".func.gii", ".label.gii", cortexR_fname, fixed=TRUE) }
    if (ROI_do['right']) {
      if (is.null(ROIcortexR_fname)) { ROIcortexR_fname <- default_fname("ROIcortexR") }
      ROIcortexR_fname <- format_path(ROIcortexR_fname, write_dir, mode=2)
    } else { ROIcortexR_fname <- NULL }
  } else { cortexR_fname <- ROIcortexR_fname <- NULL }

  if (do['subVol']) {
    # Subcortex
    if (is.null(subcortVol_fname)) { subcortVol_fname <- default_fname("subcortVol") }
    subcortVol_fname <- format_path(subcortVol_fname, write_dir, mode=2)
    if (ROI_do['sub']) {
      if (is.null(ROIsubcortVol_fname)) { ROIsubcortVol_fname <- default_fname("ROIsubcort") }
      ROIsubcortVol_fname <- format_path(ROIsubcortVol_fname, write_dir, mode=2)
    } else { ROIsubcortVol_fname <- NULL }
  } else { subcortVol_fname <- ROIsubcortVol_fname <- NULL }

  if (do["subLab"]) {
    if (is.null(subcortLabs_fname)) { subcortLabs_fname <- default_fname("subcortLabs") }
    subcortLabs_fname <- format_path(subcortLabs_fname, write_dir, mode=2)
  } else { subcortLabs_fname <- NULL }

  sep_fnames <- unlist(list(
    cortexL = cortexL_fname,
    ROIcortexL = ROIcortexL_fname,
    cortexR = cortexR_fname,
    ROIcortexR = ROIcortexR_fname,
    subcortVol = subcortVol_fname,
    subcortLabs = subcortLabs_fname,
    ROIsubcortVol = ROIsubcortVol_fname
  ))

  # Stop if any file paths are identical.
  if (length(unique(sep_fnames)) != length(sep_fnames)) {
    print(sep_fnames)
    stop(paste0(
      "The file paths for the separated xifti/CIFTI components are above. ",
      "Some file paths were identical, but ",
      "the same path cannot be used to write out different components. ",
      "Check if identical file names were specified, or if any provided ",
      "file name overlapped with a default file name.\n\n"
    ))
  }

  list(do=do, ROI_do=ROI_do, sep_fnames=sep_fnames)
}

#' Separate a CIFTI file
#'
#' Separate a CIFTI file into GIFTI files for the cortical data and NIFTI files
#'  for the subcortical data and labels. ROIs can also be written to indicate
#'  the medial wall mask (cortex) and volume mask (subcortex). This uses the
#'  Connectome Workbench command \code{-cifti-separate}.
#'
#' Time unit, start, and step (dtseries files) will not be written to the GIFTI/NIFTIs.
#'  Column names (dscalar files) will not be written to the GIFTIs, as well as label
#'  names and colors (dlabel files). (Haven't checked the NIFTIs yet.)
#'
#'  ROI/medial wall behavior: If there are 32k vertices in the left cortex with
#'  3k representing the medial wall, then both \code{cortexL_fname} and
#'  \code{ROIcortexL_fname} will have 32k entries, 3k of which having a value of
#'  0 indicating the medial wall. The non-medial wall entries will have the
#'  data values in \code{cortexL_fname} and a value of 1 in
#'  \code{ROIcortexL_fname}. Thus, exporting \code{ROIcortexL_fname} is vital if
#'  the data values include 0, because 0-valued non-medial wall vertices and
#'  medial wall vertices cannot be distinguished from one another within
#'  \code{cortexL_fname} alone.
#'
#'
#' @inheritParams cifti_fname_Param
#' @inheritParams separate_cifti_files
#'
#' @return A named character vector with the file paths to the written
#'  NIFTI and GIFTI files
#'
#' @family writing
#' @export
#'
#' @section Connectome Workbench:
#' This function interfaces with the \code{"-cifti-separate"} Workbench command.
#'
separate_cifti <- function(cifti_fname,
  brainstructures=NULL,
  cortexL_fname=NULL, cortexR_fname=NULL,
  subcortVol_fname=NULL, subcortLabs_fname=NULL,
  ROI_brainstructures="all",
  ROIcortexL_fname=NULL, ROIcortexR_fname=NULL,
  ROIsubcortVol_fname=NULL,
  write_dir=NULL) {

  # [DEV NOTE] This function is written in a similar way to `write_xifti2`.

  # Check if CIFTI exists.
  cifti_fname <- format_path(cifti_fname)
  if (!file.exists(cifti_fname)) {
    stop(paste("The `cifti_fname` \"", cifti_fname, "\" does not exist."))
  }

  # Read metadata.
  # Check which brainstructures are present.
  cifti_info <- info_cifti(cifti_fname)
  bs_present <- cifti_info$cifti$brainstructures
  if (is.null(brainstructures)) { brainstructures <- cifti_info$cifti$brainstructures }

  # Get the components of the CIFTI file path.
  bname_cifti <- basename(cifti_fname)
  extn_cifti <- get_cifti_extn(bname_cifti)

  # Convert extension to numerical intent.
  check_cifti_type(cifti_info$cifti$intent, extn_cifti)
  if (extn_cifti %in% c("dtseries.nii", "dscalar.nii", "dlabel.nii")) {
    intent_cifti <- supported_intents()$value[
      match(extn_cifti, supported_intents()$extension)
    ]
  } else {
    intent_cifti <- 3006
  }

  # Get output files: which ones to write, and their names.
  x <- separate_cifti_files(
    intent = intent_cifti,
    brainstructures=brainstructures,
    cortexL_fname=cortexL_fname, cortexR_fname=cortexR_fname,
    subcortVol_fname=subcortVol_fname, subcortLabs_fname=subcortLabs_fname,
    ROI_brainstructures=ROI_brainstructures,
    ROIcortexL_fname=ROIcortexL_fname, ROIcortexR_fname=ROIcortexR_fname,
    ROIsubcortVol_fname=ROIsubcortVol_fname, write_dir=write_dir
  )
  do <- x$do; ROI_do <- x$ROI_do; sep_fnames <- x$sep_fnames; rm(x)

  if (all(!do)) { message("Nothing to separate."); return(invisible(NULL)) }

  # Modify output file names.
  fix_sep_fname <- function(x){ file.path(
    dirname(x),
    gsub(extn_cifti, basename(x), bname_cifti, fixed=TRUE)
  ) }
  sep_fnames <- vapply(sep_fnames, fix_sep_fname, "")

  # Check that requested brainstructures are present.
  # Modify `do` and `sep_fnames` if necessary.
  bs2 <- c(do["left"], do["right"], do["subVol"] | do["subLab"])
  names(bs2)[names(bs2) == "subVol"] <- "subcortical"
  if (!all(names(bs2)[bs2] %in% bs_present)) {
    warning(paste0(
      "Only the following brainstructures are present in the CIFTI file: ",
      paste(bs_present, collapse=", "), "\n"
    ))
    if (!("left" %in% bs_present)) { 
      do["left"] <- FALSE
      sep_fnames[c("cortexL", "ROIcortexL")] <- list(NULL)
    }
    if (!("right" %in% bs_present)) {
      do["right"] <- FALSE
      sep_fnames[c("cortexR", "ROIcortexR")] <- list(NULL)
    }
    if (!("subcortical" %in% bs_present)) {
      do[c("subVol", "subLab")] <- FALSE
      sep_fnames[c("subcortVol", "subcortLabs", "ROIsubcortVol")] <- list(NULL)
    }
    sep_fnames <- unlist(list(sep_fnames))
  }
  if (all(!do)) { message("Nothing to separate."); return(invisible(NULL)) }

  # Build the Connectome Workbench command.
  what <- switch(as.character(cifti_info$cifti$intent),
    `3002` = "metric", `3006` = "metric", `3007` = "label", "metric"
  )
  cmd <- paste("-cifti-separate", sys_path(cifti_fname), "COLUMN")
  if (do['left']) {
    cmd <- paste(cmd, paste0('-',what,' CORTEX_LEFT'), sys_path(sep_fnames["cortexL"]))
    if (ROI_do['left']) { cmd <- paste(cmd, '-roi', sys_path(sep_fnames["ROIcortexL"])) }
  }
  if (do['right']) {
    cmd <- paste(cmd, paste0('-',what,' CORTEX_RIGHT'), sys_path(sep_fnames["cortexR"]))
    if (ROI_do['right']) { cmd <- paste(cmd, '-roi', sys_path(sep_fnames["ROIcortexR"])) }
  }
  if (do['subVol']) {
    cmd <- paste(cmd, '-volume-all', sys_path(sep_fnames["subcortVol"]))
    if (ROI_do['sub']) { cmd <- paste(cmd, '-roi', sys_path(sep_fnames["ROIsubcortVol"])) }
  }
  if (do['subLab']) {
    # `-cifti-separate` expects to write out the volume data if the labels are written.
    # So, here we write the volume data to a dummy tempfile, to get the labels only.
    if (!do['subVol']) {
      subVol_tfname <- sys_path(format_path("subVol.nii", tempdir(), mode=2))
      cmd <- paste(cmd, '-volume-all', subVol_tfname)
    }
    cmd <- paste(cmd, '-label', sys_path(sep_fnames["subcortLabs"]))
  }

  # Run it.
  run_wb_cmd(cmd)

  invisible(sep_fnames)
}

#' @rdname separate_cifti
#' @export
separateCIfTI <- function(cifti_fname,
  brainstructures=c("left","right"),
  cortexL_fname=NULL, cortexR_fname=NULL,
  subcortVol_fname=NULL, subcortLabs_fname=NULL,
  ROI_brainstructures="all",
  ROIcortexL_fname=NULL, ROIcortexR_fname=NULL,
  ROIsubcortVol_fname=NULL,
  write_dir=NULL){

  separate_cifti(
    cifti_fname=cifti_fname, brainstructures=brainstructures,
    cortexL_fname=cortexL_fname, cortexR_fname=cortexR_fname,
    subcortVol_fname=subcortVol_fname, subcortLabs_fname=subcortLabs_fname,
    ROI_brainstructures=ROI_brainstructures,
    ROIcortexL_fname=ROIcortexL_fname, ROIcortexR_fname=ROIcortexR_fname,
    ROIsubcortVol_fname=ROIsubcortVol_fname,
    write_dir=write_dir
  )
}

#' @rdname separate_cifti
#' @export
separatecii <- function(cifti_fname,
  brainstructures=c("left","right"),
  cortexL_fname=NULL, cortexR_fname=NULL,
  subcortVol_fname=NULL, subcortLabs_fname=NULL,
  ROI_brainstructures="all",
  ROIcortexL_fname=NULL, ROIcortexR_fname=NULL,
  ROIsubcortVol_fname=NULL,
  write_dir=NULL){

  separate_cifti(
    cifti_fname=cifti_fname, brainstructures=brainstructures,
    cortexL_fname=cortexL_fname, cortexR_fname=cortexR_fname,
    subcortVol_fname=subcortVol_fname, subcortLabs_fname=subcortLabs_fname,
    ROI_brainstructures=ROI_brainstructures,
    ROIcortexL_fname=ROIcortexL_fname, ROIcortexR_fname=ROIcortexR_fname,
    ROIsubcortVol_fname=ROIsubcortVol_fname,
    write_dir=write_dir
  )
}
