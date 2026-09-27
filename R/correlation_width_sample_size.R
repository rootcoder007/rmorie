# SPDX-License-Identifier: AGPL-3.0-or-later
#' Sample size for a correlation confidence interval of given width
#'
#' Bonett & Wright (2000): the interval tanh(atanh(theta) -/+ z c / sqrt(n -
#' b)) with b = 3, c = 1 (Pearson); b = 3, c^2 = 1 + theta^2 / 2 (Spearman); b =
#' 4, c^2 = 0.437 (Kendall tau-a). approach = "exact" returns the smallest n
#' whose width is at most width; "two-stage" their approximation n0 = 4 c^2 (1 -
#' theta^2)^2 (z / w)^2 + b, n = ceiling((n0 - b) (w0 / w)^2 + b). The exact
#' search reproduces 67 of the 72 entries of Hedderich, Sachs & Reynarowych
#' (2023, Table 7.85).
#'
#' @param theta Planning value of the correlation.
#' @param width Required width.
#' @param method "pearson", "spearman" or "kendall".
#' @param conf_level Confidence level.
#' @param approach "exact" or "two-stage".
#' @return Named list: n, width_at_n.
#' @references Bonett, D. G. & Wright, T. A. (2000). Psychometrika 65, 23-28.
#' @examples
#' corwsn(0.5, 0.2, "spearman")$n
#' @export
corwsn <- function(theta, width, method = c("pearson", "spearman", "kendall"), conf_level = 0.95,
                   approach = c("exact", "two-stage")) {
  method <- match.arg(method)
  approach <- match.arg(approach)
  if (!(theta > -1 && theta < 1 && width > 0 && width < 2)) {
    stop("need -1 < theta < 1 and 0 < width < 2", call. = FALSE)
  }
  b <- if (method == "kendall") 4 else 3
  c2 <- switch(method, pearson = 1, spearman = 1 + theta^2 / 2, kendall = 0.437)
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  zt <- atanh(theta)
  w_at <- function(n) {
    h <- z * sqrt(c2) / sqrt(n - b)
    tanh(zt + h) - tanh(zt - h)
  }
  n0 <- 4 * c2 * (1 - theta^2)^2 * (z / width)^2 + b
  if (approach == "exact") {
    n <- max(b + 1, floor(n0 / 2))
    while (w_at(n) > width) n <- n + 1
    while (n > b + 1 && w_at(n - 1) <= width) n <- n - 1
  } else {
    n <- ceiling((n0 - b) * (w_at(n0) / width)^2 + b)
  }
  list(n = n, width_at_n = w_at(n))
}
