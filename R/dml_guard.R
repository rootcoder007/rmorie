# SPDX-License-Identifier: AGPL-3.0-or-later

#' Internal guard for DoubleML cross-fitting via {future}
#'
#' DoubleML cross-fits through \pkg{future}. Three failure modes are
#' guarded here, uniformly, for every `$fit()` call site:
#'
#' 1. Worker launch can fail outright on constrained machines
#'    (`FutureLaunchError`). The fits are small, so parallel workers buy
#'    nothing: evaluate sequentially in-process.
#' 2. future's connection-misuse diagnostic can segfault R uncatchably
#'    on some builds (`diff_connections()` inside `FutureResult`).
#' 3. future raises an "UNRELIABLE VALUE ... future.seed=TRUE"
#'    RNG-misuse condition; call sites seed explicitly via
#'    `set.seed(random_state)`, so it is a false alarm that would
#'    otherwise trip R CMD check under `stop_on_warning`.
#'
#' Usage at a call site (restores everything on exit):
#' \preformatted{
#'   .gst <- .morie_dml_guard_begin()
#'   on.exit(.morie_dml_guard_end(.gst), add = TRUE)
#' }
#'
#' @return An opaque state list for `.morie_dml_guard_end`.
#' @keywords internal
#' @noRd
.morie_dml_guard_begin <- function() {
  st <- list(
    plan = NULL,
    opts = options(
      future.rng.onMisuse = "ignore",
      future.connections.onMisuse = "ignore"
    )
  )
  if (requireNamespace("future", quietly = TRUE)) {
    st$plan <- future::plan("sequential")
  }
  st
}

#' @rdname dot-morie_dml_guard_begin
#' @param st State list returned by `.morie_dml_guard_begin`.
#' @keywords internal
#' @noRd
.morie_dml_guard_end <- function(st) {
  options(st$opts)
  if (!is.null(st$plan)) {
    future::plan(st$plan)
  }
  invisible(NULL)
}
