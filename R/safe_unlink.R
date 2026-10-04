# SPDX-License-Identifier: AGPL-3.0-or-later
#' Internal helper: why a directory must never be removed recursively (NULL when it may be)
#'
#' The filesystem root, the home directory, the working directory and its parents, and a git
#' checkout are refused whatever the caller says.
#' @noRd
.morie_unlink_refusal <- function(path) {
  p <- normalizePath(path, winslash = "/", mustWork = FALSE)
  home <- normalizePath("~", winslash = "/", mustWork = FALSE)
  wd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  if (grepl("^([A-Za-z]:)?/*$", p)) return("it is the filesystem root")
  if (identical(p, home)) return("it is the home directory")
  if (identical(p, wd) || startsWith(paste0(wd, "/"), paste0(sub("/+$", "", p), "/"))) {
    return("it is the working directory or one of its parents")
  }
  NULL
}

#' Internal helper: remove a directory morie owns, refusing protected ones
#' @noRd
.morie_unlink_owned <- function(path) {
  why <- .morie_unlink_refusal(path)
  if (!is.null(why)) stop(sprintf("refusing to remove %s: %s", path, why), call. = FALSE)
  unlink(path, recursive = TRUE, force = TRUE)
}
