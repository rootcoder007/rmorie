# SPDX-License-Identifier: AGPL-3.0-or-later
#' Mantel log-rank test comparing two survival curves
#'
#' At each distinct death time with \eqn{d} deaths among \eqn{r} at
#' risk, \eqn{r_1} of them in group 1, \eqn{E_1 \mathrel{+}= d r_1 / r}
#' and \eqn{V \mathrel{+}= d (r_1/r)(1 - r_1/r)(r-d)/(r-1)}; the
#' statistic is \eqn{(O_1 - E_1)^2 / V} on one degree of freedom.  The
#' \eqn{(r-d)/(r-1)} factor is the finite-population correction of the
#' hypergeometric variance.  Ties in time are pooled into one risk set.
#'
#' @param time Follow-up times.
#' @param event 1 for an observed death, 0 for right censoring.
#' @param group Two distinct labels; the lower sorts to group 1.
#' @return List with \code{statistic}, \code{p_value}, \code{observed},
#'   \code{expected}, \code{var}, \code{n}, \code{method}.
#' @references Mantel (1966), Cancer Chemotherapy Reports 50:163-170; Peto and Peto (1972), JRSS A 135:185-207.  The coded form was read from Therneau's survival package, src/survdiff2.c, rho = 0 branch of the G-rho family.
#' @export
#' @examples
#' set.seed(1)
#' r <- Logrank(time = sort(runif(10)), event = rbinom(10, 1, 0.5), group = rbinom(10, 1, 0.5)); TRUE
Logrank <- function(time, event, group) {
  time <- .t4_vec(time)
  event <- .t4_vec(event)
  g <- .t4_vec(group)
  n <- length(time)
  if (length(event) != n || length(g) != n) stop("time, event and group must be the same length")
  labels <- sort(unique(g))
  if (length(labels) != 2L) stop("Logrank compares exactly two groups")
  ord <- order(time, seq_len(n))
  tt <- time[ord]
  ee <- event[ord]
  gg <- ifelse(g[ord] == labels[1], 1L, 2L)
  o1 <- sum(ee[gg == 1L])
  e1 <- 0
  v <- 0
  i <- 1L
  while (i <= n) {
    j <- i
    while (j + 1L <= n && tt[j + 1L] == tt[i]) j <- j + 1L
    r <- n - i + 1L
    r1 <- sum(gg[i:n] == 1L)
    d <- sum(ee[i:j])
    if (d > 0) {
      f <- r1 / r
      e1 <- e1 + d * f
      if (r > 1L) v <- v + d * f * (1 - f) * (r - d) / (r - 1)
    }
    i <- j + 1L
  }
  chi2 <- if (v > 0) (o1 - e1)^2 / v else NaN
  p <- if (v > 0) stats::pchisq(chi2, 1, lower.tail = FALSE) else NaN
  .t4_result(statistic = chi2, p_value = p, observed = o1, expected = e1,
             var = v, n = as.integer(n),
             method = "Log-rank (Mantel) two-sample test")
}
