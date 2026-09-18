# SPDX-License-Identifier: AGPL-3.0-or-later

#' Default plot for morie rich-result objects
#'
#' Every morie analysis returns an object of class
#' \code{morie_rich_result} (e.g. \code{morie_otis_result},
#' \code{morie_tps_result}, \code{morie_fairness_result}). This default
#' \code{plot} method gives every such result a quick visual: it
#' bar-charts the numeric summary metrics (\code{$summary_lines}), or, if
#' there are none, plots the first numeric column of the first result
#' table. For full publication figures use the dedicated figure-export
#' functions \code{\link{morie_otis_figures}} and
#' \code{\link{morie_tps_figures}}.
#'
#' @param x A \code{morie_rich_result} object.
#' @param ... Passed to the underlying base-graphics call.
#' @return Invisibly \code{NULL}; called for its plotting side effect.
#' @examples
#' r <- structure(
#'   list(title = "demo", summary_lines = list(a = 1, b = 2, c = 3),
#'        tables = list()),
#'   class = c("morie_demo_result", "morie_rich_result", "list"))
#' plot(r)
#' @exportS3Method graphics::plot morie_rich_result
plot.morie_rich_result <- function(x, ...) {
  ttl <- if (!is.null(x$title)) as.character(x$title)[1L] else "morie result"
  # 1) numeric single-value summary metrics -> bar chart
  sl <- x$summary_lines
  num <- numeric(0)
  if (length(sl)) {
    vals <- suppressWarnings(vapply(sl, function(v) {
      v <- v[[1L]]
      if (is.numeric(v) && length(v) == 1L) as.numeric(v) else NA_real_
    }, numeric(1)))
    num <- vals[is.finite(vals)]
  }
  if (length(num)) {
    graphics::barplot(num, names.arg = substr(names(num), 1L, 14L),
                      las = 2, main = ttl, ...)
    return(invisible(NULL))
  }
  # 2) fall back to the first numeric column of the first table
  if (length(x$tables) && is.data.frame(x$tables[[1L]])) {
    tb <- x$tables[[1L]]
    nc <- which(vapply(tb, is.numeric, logical(1)))
    if (length(nc)) {
      plot(tb[[nc[1L]]], type = "h", ylab = names(tb)[nc[1L]],
           xlab = "row", main = ttl, ...)
      return(invisible(NULL))
    }
  }
  # 3) nothing numeric -> titled empty canvas
  graphics::plot.new()
  graphics::title(main = ttl)
  invisible(NULL)
}
