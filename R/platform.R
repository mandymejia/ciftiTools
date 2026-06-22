#' Platform detection
#'
#' Tiny module for OS-version branching. Predicates are evaluated lazily and
#' cached, so callers can use them freely without re-shelling out.
#'
#' @keywords internal
#' @name platform
NULL

# Private cache: filled by the predicates on first call.
.platform <- new.env(parent = emptyenv())

#' Is the host running macOS Tahoe (26.0+)?
#'
#' Mirrors the helper of the same name defined locally inside
#' `rgl::.onLoad` (`rgl/R/zzz.R`, ~line 39). Reproduced here because
#' rgl doesn't export it. Result is cached after the first call.
#'
#' @keywords internal
#' @return `TRUE` on macOS 26.0 or newer, `FALSE` otherwise.
is_tahoe <- function() {
  if (is.null(.platform$is_tahoe)) {
    .platform$is_tahoe <- if (unname(Sys.info()["sysname"]) != "Darwin") {
      FALSE
    } else {
      v <- tryCatch(
        as.numeric_version(system("sw_vers -productVersion", intern = TRUE)),
        error = function(e) NA, warning = function(w) NA
      )
      isTRUE(!is.na(v) && v >= as.numeric_version("26.0"))
    }
  }
  .platform$is_tahoe
}

#' Verify that the rgl rendering backend has what it needs
#'
#' Called at the entry of view functions so users get an actionable install
#' message instead of a stack trace from deep inside `rgl::snapshot3d` or
#' `webshot2::webshot`.
#'
#' - `rgl` is always required (Suggests).
#' - When rgl is in null-device mode (Tahoe, or any user-set
#'   `options(rgl.useNULL = TRUE)`), the web backend is in play and we also
#'   need `webshot2`, `htmlwidgets`, and a Chrome/Chromium binary.
#'
#' @keywords internal
#' @return Invisibly `NULL`; stops with an error if anything is missing.
check_render_backend <- function() {
  # `rgl::rgl.useNULL()` needs rgl installed; if missing, fall back to
  # `is_tahoe()` so a fresh Tahoe user gets ALL their missing deps in one go.
  rgl_ok <- requireNamespace("rgl", quietly = TRUE)
  on_web <- if (rgl_ok) rgl::rgl.useNULL() else is_tahoe()

  needed <- "rgl"
  if (on_web) needed <- c(needed, "webshot2", "htmlwidgets")
  missing_pkgs <- needed[
    !vapply(needed, requireNamespace, FALSE, quietly = TRUE)
  ]

  # Only check for Chrome once chromote is reachable (it's a webshot2 dep).
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
