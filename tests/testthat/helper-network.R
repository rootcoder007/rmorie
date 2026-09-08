# SPDX-License-Identifier: AGPL-3.0-or-later
# curl-free network gate: testthat::skip_if_offline() requires the curl
# package, which is not an rmorie runtime dependency. This helper skips
# on a plain TCP probe instead, so network-gated tests degrade the same
# way on machines without curl.
skip_if_no_network <- function(host = "8.8.8.8", port = 53, timeout = 2) {
  # hostnames are fine too: socketConnection resolves them.
  ok <- FALSE
  con <- tryCatch(
    suppressWarnings(
      socketConnection(host, port = port, timeout = timeout,
                       blocking = TRUE, open = "r+")
    ),
    error = function(e) NULL
  )
  if (!is.null(con)) {
    close(con)
    ok <- TRUE
  }
  if (!ok) testthat::skip(paste0("no network route to ", host))
}

# Skip a test when a remote endpoint is reachable at the network layer
# (so skip_if_no_network passes) but the upstream service itself is
# failing: a proxy error page, or an empty body from a portal that is up
# but not serving. Lives here rather than in a test file so that tests
# EARLIER in a file can use it -- testthat evaluates a file top to
# bottom, so a helper defined at line 95 does not exist at line 47.
.skip_on_upstream_error <- function(expr) {
  tryCatch(
    expr,
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("non-JSON|HTTP fetch|empty body|upstream|503|502|504",
                msg, ignore.case = TRUE)) {
        testthat::skip(paste("Upstream service unhealthy:", msg))
      }
      stop(e)
    }
  )
}

# A live open-data portal can transiently resolve to an EMPTY resource (a
# valid data.frame with 0 rows) WITHOUT throwing -- upstream data
# variability, not a dispatch bug.
.skip_if_empty <- function(df) {
  if (is.data.frame(df) && nrow(df) == 0L) {
    testthat::skip("Upstream resource returned 0 rows (empty)")
  }
  invisible(df)
}
