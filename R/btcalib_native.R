# Bootstrap-calibrated confidence interval.
# Source: Loh (1991), Statistica Sinica 1(2), 477-491, Sec. 2.1
# Eqs. 1-2 and Sec. 2.2 equivalence
# (fetched-wave3/Bootstrap_calibration_for_confidence_interval_
# construction_and_selection..pdf; read from the rendered scan).
# Mirrors Python morie.fn.btcalib exactly.

#' Loh exact bootstrap calibration of the t interval
#'
#' beta_hat_i = 1 - Phi(|t*_i|) per bootstrap sample (Loh Eq. 2);
#' alpha' is the 2a-quantile of the beta_hat_i and the interval is
#' the normal-theory interval at level alpha'.  Sec. 2.2:
#' z_{1-alpha'} equals the (1-2a)-quantile of |t*|, so the result IS
#' the bootstrap-t interval; the identity gap is reported.
#'
#' @param x Numeric sample.
#' @param alpha Two-sided non-coverage (0.05 = 95%).
#' @param B Bootstrap samples.
#' @param seed SplitMix64 seed (mirrors the Python arm).
#' @return A list with elements \code{estimate}, \code{lower},
#'   \code{upper}, \code{alpha_prime}, \code{z_calibrated},
#'   \code{identity_gap}, \code{alpha}, \code{B}, \code{seed},
#'   \code{method}.
#' @references Loh, W.-Y. (1991). Bootstrap calibration for
#'   confidence interval construction and selection. Statistica
#'   Sinica, 1(2), 477-491.
#' @export
morie_btcalib <- function(x, alpha = 0.05, B = 1000, seed = 0) {
  xv <- as.numeric(x)
  n <- length(xv)
  if (n < 5) stop("need at least five observations")
  if (alpha <= 0 || alpha >= 1) stop("alpha must be in (0, 1)")
  a <- alpha / 2
  e <- .ghc_rng(seed)
  that <- mean(xv)
  sig <- sqrt(sum((xv - that)^2) / (n - 1))
  sqn <- sqrt(n)
  tstars <- numeric(B)
  betas <- numeric(B)
  for (b in seq_len(B)) {
    idx <- pmin(floor(.ghc_unif(e, n) * n), n - 1) + 1
    xb <- xv[idx]
    mb <- mean(xb)
    sb <- sqrt(sum((xb - mb)^2) / (n - 1))
    if (sb <= 0) sb <- 1e-300
    t_ <- sqn * (mb - that) / sb
    tstars[b] <- abs(t_)
    betas[b] <- 1 - pnorm(abs(t_))
  }
  sb_ <- sort(betas)
  idx <- max(min(ceiling(2 * a * B), B), 1)
  alpha_prime <- sb_[idx]
  z_cal <- qnorm(1 - alpha_prime)
  half_cal <- z_cal * sig / sqn
  st <- sort(tstars)
  half_bt <- st[B - idx + 1] * sig / sqn
  list(estimate = that, lower = that - half_cal,
       upper = that + half_cal, alpha_prime = alpha_prime,
       z_calibrated = z_cal, identity_gap = abs(half_cal - half_bt),
       alpha = alpha, B = as.integer(B), seed = seed,
       method = "Loh (1991) exact bootstrap calibration (Eqs. 1-2)")
}
