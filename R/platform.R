#' Platform detection and per-platform overrides
#'
#' Two layers:
#'
#' 1. **Predicates** (`is_*`) report facts about the host OS.
#' 2. **Overrides registry** (`.platform_overrides`) encodes
#'    "if predicate X holds, run apply()". Adding a new platform-specific
#'    behavior means adding one entry to the registry — no edits to
#'    `.onAttach` or view functions.
#'
#' @keywords internal
#' @name platform
NULL

# ----------------------------------------------------------------------------
# Predicates
# ----------------------------------------------------------------------------

#' Is the host running macOS Tahoe (26.0+)?
#'
#' Mirrors the helper of the same name defined locally inside
#' `rgl::.onLoad` (`rgl/R/zzz.R`). Reproduced because rgl doesn't export it.
#'
#' @keywords internal
is_tahoe <- function() {
  if (unname(Sys.info()["sysname"]) != "Darwin") return(FALSE)
  v <- tryCatch(
    as.numeric_version(system("sw_vers -productVersion", intern = TRUE)),
    error = function(e) NA, warning = function(w) NA
  )
  isTRUE(!is.na(v) && v >= as.numeric_version("26.0"))
}

# When a new OS-specific bug needs handling, add a sibling predicate above.

# ----------------------------------------------------------------------------
# Registry of per-platform overrides
# ----------------------------------------------------------------------------

# Each entry is a list with four fields:
#   name   : human-readable, unique label
#   when   : nullary predicate; entry fires iff TRUE
#   apply  : nullary side-effecting thunk to run when `when()` is TRUE
#   reason : free-form rationale (for users + introspection)
#
# To handle a new OS-specific quirk, append a new entry.
.platform_overrides <- list(
  list(
    name   = "macOS Tahoe -> rgl web backend",
    when   = is_tahoe,
    apply  = function() options(rgl.useNULL = TRUE),
    reason = paste(
      "Apple's OpenGL/XQuartz path is broken on macOS Tahoe (26.0+).",
      "Force rgl into null-device mode so `view_xifti_surface` routes",
      "PNG output through `rgl::snapshot3d()` -> `webshot2` -> Chrome."
    )
  )
)

#' Apply every override whose `when` predicate matches the host
#'
#' Called once from `.onAttach`. Errors inside `apply` are demoted to
#' warnings so a misbehaving override can't block package attach.
#'
#' @keywords internal
#' @return Character vector of override names that fired, invisibly.
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

#' Verify the rgl rendering backend has what it needs
#'
#' Called at the entry of view functions so users get an actionable error
#' instead of a stack trace from deep inside `rgl::snapshot3d` or
#' `webshot2::webshot`. Collects all missing dependencies so the user gets
#' a single error listing everything (rather than one-at-a-time).
#'
#' @keywords internal
#' @return Invisibly `NULL`; stops with an error if anything is missing.
check_render_backend <- function() {
  # The web backend is in play iff `rgl.useNULL` option is TRUE. The option
  # was already set (or not) by `apply_platform_overrides()` at attach time,
  # so we don't need to ask rgl directly — the option is the source of truth
  # regardless of whether rgl is installed.
  on_web <- isTRUE(getOption("rgl.useNULL"))

  needed <- "rgl"
  if (on_web) needed <- c(needed, "webshot2", "htmlwidgets")
  missing_pkgs <- needed[
    !vapply(needed, requireNamespace, FALSE, quietly = TRUE)
  ]

  # Chrome check only meaningful once chromote is reachable (webshot2 dep).
  chrome_missing <- FALSE
  if (on_web && !"webshot2" %in% missing_pkgs) {
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
