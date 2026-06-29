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

#' Is the rgl web backend in use?
#' Single source of truth for branching on web vs native render path.
#' @noRd
web_render_active <- function() {
  isTRUE(getOption("ciftiTools.web_render"))
}

# ----------------------------------------------------------------------------
# Actions
# ----------------------------------------------------------------------------

#' Switch ciftiTools onto the rgl web render backend.
#' Sets both options atomically — `rgl.useNULL` (no native display) is
#' required for the web path, so it's enforced here rather than left to
#' callers to remember.
#' @noRd
enable_web_render <- function() {
  options(rgl.useNULL = TRUE, ciftiTools.web_render = TRUE)
}

#' Startup notice when the web render backend is active.
#' Returns "" when web render isn't active, so callers can always paste it.
#' Body lines nest inside the welcome banner's bottom section in the same
#' style as `wb_path_request()` — `*****` borders are owned by `welcome_msg()`.
#' @noRd
web_render_notice <- function() {
  if (!web_render_active()) return("")
  paste(
    "\n                                                                 ",
    "       Tahoe users: Browser-based fix has been implemented       ",
    "     for interactive and PNG visualization. Please disregard     ",
    "       rgl warnings and install webshot2 and htmlwidgets.        ",
    sep = '\n'
  )
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
# Web-backend entries use `enable_web_render` so the useNULL + web_render
# pair stays in lockstep.
.platform_overrides <- list(
  list(
    name   = "macOS Tahoe -> rgl web backend",
    when   = is_tahoe,
    apply  = enable_web_render,
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
  # Power user opting in via options() should still get the useNULL pair.
  if (web_render_active() && !isTRUE(getOption("rgl.useNULL"))) {
    enable_web_render()
  }
  on_useNULL <- isTRUE(getOption("rgl.useNULL"))
  on_web     <- web_render_active()
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

  problems <- character()
  if (length(missing_pkgs)) {
    problems <- c(problems, sprintf(
      "Missing R package(s): %s. Install with `install.packages(c(%s))`.",
      paste(missing_pkgs, collapse = ", "),
      paste(sprintf("'%s'", missing_pkgs), collapse = ", ")
    ))
  }

  # Chrome state for web PNG: silent (chrome-headless-shell), warn (regular
  # Chrome — flaky), or error (no Chrome at all).
  if (will_use_web_png && !"webshot2" %in% missing_pkgs) {
    chrome <- tryCatch(
      suppressMessages(chromote::find_chrome()),
      error = function(e) NULL
    )
    install_cmd <- "chromote::local_chrome_version(\"latest-stable\", binary = \"chrome-headless-shell\")"
    if (is.null(chrome) || !nzchar(chrome)) {
      problems <- c(problems, paste0(
        "Missing Chrome/Chromium for rgl's web backend. In R run: ", install_cmd
      ))
    } else if (!grepl("chrome-headless-shell", chrome, fixed = TRUE)) {
      warning(
        "Using regular Chrome for PNG generation; this is known to flake ",
        "under repeated calls (e.g. knitting). For reliable output, ",
        "install chrome-headless-shell: ", install_cmd,
        call. = FALSE
      )
    }
  }
  if (length(problems)) {
    stop("`view_xifti_surface` cannot render:\n  - ",
         paste(problems, collapse = "\n  - "),
         call. = FALSE)
  }
  invisible(NULL)
}
