# Internal infix helper for defaults.
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    return(y)
  }
  if (length(x) == 1 && (is.na(x) || identical(x, ""))) {
    return(y)
  }
  x
}

is_absolute_path <- function(path) {
  grepl("^(/|[A-Za-z]:[/\\\\])", path)
}

# Internal: locate the host project root.
# Wraps `here::here()` (preferred) and falls back to an upward DESCRIPTION
# / pyproject.toml walk so an installed package still has a sensible
# heuristic. Always wrap call sites in `tryCatch()` because callers run
# inside an installed package have no project root at all.
.morie_project_root <- function(start = getwd(), max_up = 10L) {
  out <- tryCatch(here::here(), error = function(e) NULL)
  if (!is.null(out) && nzchar(out) && dir.exists(out)) {
    return(normalizePath(out, winslash = "/", mustWork = FALSE))
  }
  current <- normalizePath(start, winslash = "/", mustWork = FALSE)
  for (i in seq_len(max_up)) {
    if (file.exists(file.path(current, "DESCRIPTION")) ||
        file.exists(file.path(current, "pyproject.toml")) ||
        file.exists(file.path(current, ".here"))) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) break
    current <- parent
  }
  stop(
    "Unable to detect project root. Provide `project_root` explicitly.",
    call. = FALSE
  )
}

#' Resolve standard project paths
#'
#' @param project_root Project root directory. If `NULL`, inferred from the
#'   current working directory.
#' @return Named list of key paths.
#' @examples
#' tryCatch(morie_paths(),
#'   error = function(e) message("not inside a morie project tree")
#' )
#' @export
morie_paths <- function(project_root = NULL) {
  root <- project_root %||% .morie_project_root()
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)

  list(
    project_root = root,
    data_dir = file.path(root, "data"),
    cache_dir = file.path(root, "data", "cache"),
    datasets_dir = file.path(root, "data", "datasets"),
    outputs_dir = file.path(root, "data", "manifest", "outputs"),
    outputs_manifest = file.path(root, "data", "manifest", "outputs_manifest.csv"),
    rtests_dir = file.path(root, "libexec", "config", "tests", "rtests"),
    pytests_dir = file.path(root, "libexec", "config", "tests", "pytests"),
    tools_dir = file.path(root, "libexec", "config", "tools"),
    docs_dir = file.path(root, "docs")
  )
}
