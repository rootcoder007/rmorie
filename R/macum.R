# SPDX-License-Identifier: AGPL-3.0-or-later
#' Cumulative random-effects meta-analysis
#'
#' Sorts by `order` and recomputes the DerSimonian-Laird summary after each
#' study, so row j is the meta-analysis available once study j had appeared.
#' Source consulted: Lau, Schmid and Chalmers (1995), Journal of Clinical
#' Epidemiology 48(1), 45-57.
#'
#' @param yi,vi study effects and their within-study variances.
#' @param order optional sort key (e.g. year).
#' @param level confidence level for each cumulative interval.
#' @return list: estimate, cumulative, se, ci_lower, ci_upper, tau2,
#'   order_index, n, method.
#' @keywords internal
#' @examples
#' macum(c(0.1, 0.3, -0.2, 0.45), c(0.02, 0.05, 0.03, 0.08))$cumulative
#' @export
macum <- function(yi, vi, order = NULL, level = 0.95) {
  y <- as.numeric(yi); v <- as.numeric(vi); k <- length(y)
  idx <- if (is.null(order)) seq_len(k) else base::order(as.numeric(order))
  crit <- k02z(0.5 + 0.5 * level)
  est <- numeric(k); ses <- numeric(k); lo <- numeric(k); hi <- numeric(k); t2s <- numeric(k)
  for (j in seq_len(k)) {
    sub <- idx[seq_len(j)]
    if (j == 1L) { t2 <- 0; mu <- y[sub]; vr <- v[sub] }
    else { d <- k02dl(y[sub], v[sub]); t2 <- d$tau2; mu <- d$mu; vr <- d$var }
    se <- sqrt(vr)
    est[j] <- mu; ses[j] <- se; lo[j] <- mu - crit * se; hi[j] <- mu + crit * se; t2s[j] <- t2
  }
  list(estimate = est[k], cumulative = est, se = ses, ci_lower = lo,
       ci_upper = hi, tau2 = t2s, order_index = as.integer(idx - 1L), n = k,
       method = "Cumulative random-effects meta-analysis (Lau, Schmid & Chalmers 1995)")
}

# CANONICAL TEST
# r <- macum(c(0.10,0.30,-0.20,0.45,0.05,0.22), c(0.02,0.05,0.03,0.08,0.01,0.04))
# stopifnot(abs(r$cumulative[1] - 0.10) < 1e-15)

#' @rdname macum
#' @keywords internal
#' @export
morie_macum <- macum
