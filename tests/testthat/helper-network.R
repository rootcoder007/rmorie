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
