# SPDX-License-Identifier: AGPL-3.0-or-later
#' TMLE for a non-smooth functional: the counterfactual median difference
#'
#' A median has no influence curve until the density at the median
#' exists, and a plug-in through an untargeted CDF inherits first-order
#' bias.  The fix is to target the CDF and then invert: \code{F_a(t)} is
#' targeted at every distinct observed outcome value, the median is read
#' off by linear interpolation of the targeted CDF at 1/2, and the
#' influence curve is \code{IC_{m_a} = -IC_{F_a(m_a)}/f_a(m_a)} with
#' \code{f_a} a Gaussian kernel density built from the targeted
#' increments at bandwidth \code{bw}.  A bandwidth much below the grid
#' spacing collapses \code{f_a} and inflates the SE.
#'
#' @param y Outcome.
#' @param D Binary treatment.
#' @param X Covariates.
#' @param bw Positive kernel bandwidth for the density at the median.
#' @return List with \code{estimate}, \code{se}, \code{m1}, \code{m0},
#'   \code{f1}, \code{f0}, \code{n}.
#' @references Diaz, I. (2017). Journal of Statistical Planning and
#'   Inference 190:39-51; van der Laan, M. J. & Rubin, D. (2006). IJB
#'   2(1):11.
#' @export
Tmlnsm <- function(y, D, X, bw) {
  yv <- as.numeric(y); Dv <- as.numeric(D); n <- length(yv); bw <- as.numeric(bw)
  if (n == 0L || length(Dv) != n)
    stop("Tmlnsm: y and D must share one length")
  if (!(bw > 0)) stop("Tmlnsm: bw must be positive")
  Xm <- as.matrix(X)
  if (nrow(Xm) != n) stop("Tmlnsm: X must have one row per subject")
  W <- cbind(1, Xm)
  gb <- .s4_glmbin(W, Dv)
  g <- .s4_clip(.s4_expit(as.numeric(W %*% gb)), 0.025, 0.975)
  grid <- sort(unique(yv)); K <- length(grid)
  bk <- .tmlmpi_cdf_bank(yv, Dv, W, g, grid)
  F <- bk$F; IC <- bk$IC
  out <- list()
  for (a in 1:2) {
    j <- K
    for (k in seq_len(K)) if (F[[a]][k] >= 0.5) { j <- k; break }
    if (j == 1L) {
      m <- grid[1L]; w <- 0
      icf <- IC[[a]][1L, ]
    } else {
      f0 <- F[[a]][j - 1L]; f1 <- F[[a]][j]
      w <- if (f1 <= f0) 0 else (0.5 - f0) / (f1 - f0)
      m <- grid[j - 1L] + w * (grid[j] - grid[j - 1L])
      icf <- (1 - w) * IC[[a]][j - 1L, ] + w * IC[[a]][j, ]
    }
    inc <- F[[a]] - c(0, F[[a]][-K])
    u <- (m - grid) / bw
    dens <- sum(inc * exp(-0.5 * u * u) / (bw * sqrt(2 * pi)))
    if (dens < 1e-12)
      stop("Tmlnsm: kernel density at the median is zero; widen bw")
    out[[a]] <- list(m = m, dens = dens, ic = -icf / dens)
  }
  est <- out[[2]]$m - out[[1]]$m
  ic <- out[[2]]$ic - out[[1]]$ic
  se <- if (n > 1L) sqrt(sum((ic - mean(ic))^2) / (n - 1) / n) else NaN
  .t1_result(estimate = est, se = se, m1 = out[[2]]$m, m0 = out[[1]]$m,
             f1 = out[[2]]$dens, f0 = out[[1]]$dens, n = n,
             method = "TMLE for the counterfactual median difference")
}
