#' Platform detection and per-platform overrides
#' @keywords internal
#' @name platform
NULL

# ----------------------------------------------------------------------------
# Predicates
# ----------------------------------------------------------------------------

#' Is the host running macOS Tahoe (26.0+)?
#' @keywords internal
is_tahoe <- function() {
  if (unname(Sys.info()["sysname"]) != "Darwin") return(FALSE)
  v <- tryCatch(
    as.numeric_version(system("sw_vers -productVersion", intern = TRUE)),
    error = function(e) NA, warning = function(w) NA
  )
  isTRUE(!is.na(v) && v >= as.numeric_version("26.0"))
}

# ----------------------------------------------------------------------------
# Registry of per-platform overrides
# ----------------------------------------------------------------------------

# Entry fields:
#   name   : unique label
#   when   : nullary predicate; entry fires iff TRUE
#   apply  : nullary thunk to run when `when()` is TRUE
#   reason : free-form rationale
#
# Entries whose `apply` enables rgl's web backend should also set
# `ciftiTools.web_render = TRUE` so view functions know the web path is
# available (and don't fall back to the HTML-widget path).
.platform_overrides <- list(
  list(
    name   = "macOS Tahoe -> rgl web backend",
    when   = is_tahoe,
    apply  = function() options(rgl.useNULL = TRUE, ciftiTools.web_render = TRUE),
    reason = "Apple's OpenGL/XQuartz path is broken on macOS Tahoe (26.0+)."
  )
)

#' Run every override whose `when` matches the host.
#' Errors inside `apply` become warnings so a bad override can't block attach.
#' @keywords internal
apply_platform_overrides <- function() {
  fired <- character()
  for (ov in .platform_overrides) {
    matches <- isTRUE(tryCatch(ov$when(), error = function(e) FALSE))
    if (!matches) next
    tryCatch(
      ov$apply(),
      error = function(e) warning(
        "Platform override '", ov$name, "' failed: ", conditionMessage(e),
        call. = FALSE
      )
    )
    fired <- c(fired, ov$name)
  }
  invisible(fired)
}

# ----------------------------------------------------------------------------
# View-function dependency check
# ----------------------------------------------------------------------------

#' Verify the rgl rendering backend has what it needs.
#' Required deps depend on the path that will run:
#'   native               -> rgl
#'   HTML widget fallback -> rgl + htmlwidgets
#'   web PNG (Tahoe etc)  -> rgl + webshot2 + htmlwidgets + Chrome
#' @param fname As passed to the view function.
#' @keywords internal
check_render_backend <- function(fname = FALSE) {
  on_useNULL <- isTRUE(getOption("rgl.useNULL"))
  on_web     <- isTRUE(getOption("ciftiTools.web_render"))
  wants_png  <- isTRUE(fname) ||
    (is.character(fname) && all(endsWith(fname, ".png")))

  will_use_web_png   <- on_useNULL && on_web && wants_png
  will_render_widget <- on_useNULL && !will_use_web_png

  needed <- "rgl"
  if (will_render_widget) needed <- c(needed, "htmlwidgets")
  if (will_use_web_png)   needed <- c(needed, "webshot2", "htmlwidgets")
  missing_pkgs <- unique(needed[
    !vapply(needed, requireNamespace, FALSE, quietly = TRUE)
  ])

  chrome_missing <- FALSE
  if (will_use_web_png && !"webshot2" %in% missing_pkgs) {
    chrome <- tryCatch(
      suppressMessages(chromote::find_chrome()),
      error = function(e) NULL
    )
    chrome_missing <- is.null(chrome) || !nzchar(chrome)
  }

  problems <- character()
  if (length(missing_pkgs)) {
    problems <- c(problems, sprintf(
      "Missing R package(s): %s. Install with `install.packages(c(%s))`.",
      paste(missing_pkgs, collapse = ", "),
      paste(sprintf("'%s'", missing_pkgs), collapse = ", ")
    ))
  }
  if (chrome_missing) {
    problems <- c(problems, paste0(
      "Missing Chrome/Chromium for rgl's web backend. Either install ",
      "Chrome/Chromium, or in R run: ",
      "chromote::local_chrome_version(\"latest-stable\", ",
      "binary = \"chrome-headless-shell\")"
    ))
  }
  if (length(problems)) {
    stop("`view_xifti_surface` cannot render:\n  - ",
         paste(problems, collapse = "\n  - "),
         call. = FALSE)
  }
  invisible(NULL)
}
