# SPDX-License-Identifier: AGPL-3.0-or-later
#
# extdata resolution across the family.
#
# rmoriedata is a hard Imports, so every file it ships is present
# wherever rmorie is installed. Fixtures that both packages once
# carried are shipped by rmoriedata alone; rmorie keeps only what is
# its own. Look in rmorie first so a local override still wins, then
# fall through to rmoriedata.

#' Resolve a bundled extdata path in rmorie, then rmoriedata
#'
#' @param ... Path components below `extdata`, as for
#'   \code{system.file}.
#' @param mustWork Whether to error when the file is in neither
#'   package.
#' @return The path, or `""` when absent and `mustWork` is `FALSE`.
#' @noRd
.rmorie_extdata <- function(..., mustWork = FALSE) {
  p <- system.file("extdata", ..., package = "rmorie")
  if (!nzchar(p) || !file.exists(p)) {
    p <- system.file("extdata", ..., package = "rmoriedata")
  }
  if (mustWork && (!nzchar(p) || !file.exists(p))) {
    stop(
      sprintf(
        "%s is in neither rmorie nor rmoriedata.",
        file.path(...)
      ),
      call. = FALSE
    )
  }
  p
}
