# One exported cheatsheet(), dispatching to the per-module ones.
#
# Every module used to define a top-level `cheatsheet`, and R sources all
# of R/ into one environment, so the definitions collapsed to whichever
# file sorted last and the exported cheatsheet() returned that module's
# text no matter what was asked about. Each module now owns
# .<module>_cheatsheet and this dispatches to it, restoring the Python
# behaviour where every module carries its own.

#' Method cheat sheet
#'
#' @param module Module name, e.g. "hntfst". Omit to list what is
#'   available.
#' @return The module's cheat sheet as a string, or the available module
#'   names when \code{module} is missing.
#' @export
cheatsheet <- function(module = NULL) {
  # topenv resolves to this package namespace once installed and to the
  # global environment when R/ is merely sourced, so the same code works
  # in both packages and in a bare source() session
  ns <- topenv(environment())
  nms <- grep("^[.].*_cheatsheet$", ls(ns, all.names = TRUE), value = TRUE)
  avail <- sort(sub("_cheatsheet$", "", sub("^[.]", "", nms)))
  if (is.null(module)) return(avail)
  nm <- paste0(".", module, "_cheatsheet")
  if (!exists(nm, envir = ns, inherits = FALSE)) {
    stop(sprintf("cheatsheet: no cheat sheet for %s", module))
  }
  get(nm, envir = ns)()
}
