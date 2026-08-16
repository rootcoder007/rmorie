# Imbens-Kalyanaraman plug-in bandwidth for regression discontinuity.
# Source: Imbens, G. and Kalyanaraman, K. (2009), Optimal bandwidth
# choice for the regression discontinuity estimator, NBER working
# paper w14726 (published 2012, Review of Economic Studies 79(3),
# 933-959): the three-step algorithm of their Sec. 4.4 with the edge
# (triangular) kernel constant C_K = 3.4375 printed in their Sec. 6.2.
#
# Native implementation mirroring Python morie.fn.causrddh exactly,
# including the two departures from the paper's typeset formulas that
# the Python arm documents and that reproduce the paper's own Sec. 6.2
# worked example:
#   - eq. (4.8) density estimate carries the factor 2 in the
#     denominator, f_hat = (N_l + N_r) / (2 N h_1);
#   - the regularisation counts N_2 are taken within the
#     median-trimmed sample while the pilot quadratics are fitted on
#     the full-sample window.

#' .mor_ik_median
#'
#' Part of the causrddh_native implementation; see the file header for
#' the source it follows.
#'
#' @param v See Usage.
#' @return One of two values, depending on the branch taken.
#' @export
.mor_ik_median <- function(v) {
  s <- sort(as.numeric(v))
  n <- length(s)
  if (n == 0L) stop("empty side")
  m <- n %/% 2L
  if (n %% 2L == 1L) s[m + 1L] else 0.5 * (s[m] + s[m + 1L])
}

# minimum-norm least squares, matching numpy lstsq(rcond=None)
#' Minimum-norm least squares, matching numpy lstsq(rcond=None)
#'
#' Part of the causrddh_native implementation; see the file header for
#' the source it follows.
#'
#' @param X See Usage.
#' @param y See Usage.
#' @return A vector, from \code{as.numeric}.
#' @export
.mor_ik_ols <- function(X, y) as.numeric(.ghc_pinv(X) %*% as.numeric(y))

#' Imbens-Kalyanaraman optimal RDD bandwidth
#'
#' Plug-in bandwidth minimising the asymptotic mean squared error of
#' the local linear regression-discontinuity estimator (Imbens and
#' Kalyanaraman 2009, Sec. 4.4).  Step 1 forms a pilot Silverman
#' window and estimates the running-variable density and the
#' conditional variance at the cutoff; step 2 fits a global cubic to
#' get the third derivative and from it side-specific pilot windows,
#' then a quadratic on each side for the second derivatives; step 3
#' combines them with the regularisation terms of their eq. (4.10).
#'
#' @param x Running variable.
#' @param y Outcome.
#' @param cutoff Threshold, default 0.
#' @return A list with \code{estimate} (the bandwidth) and the full
#'   set of intermediate quantities: \code{h1}, \code{f_hat},
#'   \code{sigma2}, \code{n_left_h1}, \code{n_right_h1},
#'   \code{mean_left_h1}, \code{mean_right_h1}, \code{m3},
#'   \code{h2_left}, \code{h2_right}, \code{m2_left}, \code{m2_right},
#'   \code{n2_left}, \code{n2_right}, \code{n2_left_full},
#'   \code{n2_right_full}, \code{r_left}, \code{r_right},
#'   \code{h_unregularized}, \code{kernel_constant}, \code{n},
#'   \code{method}.
#' @references Imbens, G. and Kalyanaraman, K. (2012). Optimal
#'   bandwidth choice for the regression discontinuity estimator.
#'   Review of Economic Studies, 79(3), 933-959.
#' @export
morie_causrddh <- function(x, y, cutoff = 0) {
  xa <- as.numeric(x); ya <- as.numeric(y)
  n <- length(xa)
  if (n < 10L) stop("need at least 10 observations")
  cc <- as.numeric(cutoff)
  d <- xa - cc
  CK <- 3.4375                       # edge kernel, their Sec. 6.2

  ## Step 1
  sx <- sd(xa)
  h1 <- 1.84 * sx * n^(-0.2)
  il <- (d >= -h1) & (d < 0)
  ir <- (d >= 0) & (d <= h1)
  nl <- sum(il); nr <- sum(ir)
  if (nl < 3L || nr < 3L)
    stop("fewer than 3 observations within the pilot window on one side of the cutoff")
  yl <- ya[il]; yr <- ya[ir]
  s2l <- var(yl); s2r <- var(yr)
  f_hat <- (nl + nr) / (2 * n * h1)
  sigma2 <- ((nl - 1) * s2l + (nr - 1) * s2r) / (nl + nr)

  ## Step 2
  left <- d < 0
  right <- d >= 0
  n_neg <- sum(left); n_pos <- sum(right)
  med_l <- .mor_ik_median(d[left])
  med_r <- .mor_ik_median(d[right])
  keep <- (d >= med_l) & (d <= med_r)
  dk <- d[keep]; yk <- ya[keep]
  Xc <- cbind(1, as.numeric(dk >= 0), dk, dk^2, dk^3)
  g <- .mor_ik_ols(Xc, yk)
  m3 <- 6 * g[5]
  base <- (sigma2 / (f_hat * max(m3 * m3, 0.01)))^(1 / 7)
  h2r <- 3.56 * base * n_pos^(-1 / 7)
  h2l <- 3.56 * base * n_neg^(-1 / 7)

  quad <- function(mask, mask_trim) {
    dm <- d[mask]; ym <- ya[mask]
    n2 <- length(dm)
    if (n2 < 4L) stop("fewer than 4 observations in a pilot quadratic window")
    b <- .mor_ik_ols(cbind(1, dm, dm^2), ym)
    n2_trim <- sum(mask & mask_trim)
    list(m2 = 2 * b[3], n_full = n2, n_trim = max(n2_trim, 1L))
  }
  qr_ <- quad(right & (d <= h2r), d <= med_r)
  ql_ <- quad(left & (d >= -h2l), d >= med_l)
  m2r <- qr_$m2; n2r <- qr_$n_trim; n2r_full <- qr_$n_full
  m2l <- ql_$m2; n2l <- ql_$n_trim; n2l_full <- ql_$n_full

  ## Step 3
  rr <- 720 * sigma2 / (n2r * h2r^4)
  rl <- 720 * sigma2 / (n2l * h2l^4)
  curv <- (m2r - m2l)^2
  h_opt <- CK * (2 * sigma2 / (f_hat * (curv + rr + rl)))^0.2 * n^(-0.2)
  h_unreg <- if (curv > 0)
    CK * (2 * sigma2 / (f_hat * curv))^0.2 * n^(-0.2) else Inf

  list(estimate = h_opt, h1 = h1, f_hat = f_hat, sigma2 = sigma2,
       n_left_h1 = nl, n_right_h1 = nr,
       mean_left_h1 = mean(yl), mean_right_h1 = mean(yr),
       m3 = m3, h2_left = h2l, h2_right = h2r,
       m2_left = m2l, m2_right = m2r,
       n2_left = n2l, n2_right = n2r,
       n2_left_full = n2l_full, n2_right_full = n2r_full,
       r_left = rl, r_right = rr,
       h_unregularized = h_unreg, kernel_constant = CK, n = n,
       method = paste("Imbens-Kalyanaraman (2009/2012) plug-in bandwidth,",
                      "edge kernel, NBER w14726 algorithm"))
}
