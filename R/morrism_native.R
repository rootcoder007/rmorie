# Morris elementary effects screening.
# Source: Morris (1991), Technometrics 33, 161-174; Campolongo et al.
# (2007), EMS 22, 1509-1518; Saltelli et al. (2008), Global
# Sensitivity Analysis: The Primer, Sec. 3.2-3.3, Eq. 3.1
# (fetched-wave3/saltelli-2008-gsa-primer.pdf).  Mirrors Python
# morie.fn.morrisM exactly: all randomness comes from the shared
# SplitMix64 stream (.ghc_rng / .ghc_unif), consumed draw for draw in
# the same order (k base uniforms, k order keys, one sign uniform per
# factor step).

#' Morris elementary effects screening
#'
#' Saltelli et al. (2008) Eq. 3.1: EE_i = (Y(X + Delta e_i) - Y(X)) /
#' Delta on a p-level grid with Delta = p/(2(p-1)); r random
#' trajectories perturb factors one at a time in random order and
#' direction.  Reports mu (mean EE), mu_star (mean |EE|, Campolongo),
#' sigma (sd of EE) per factor.
#'
#' @param fun Model function taking a length-k numeric vector.
#' @param k Number of input factors.
#' @param r Number of trajectories.
#' @param p Even number of grid levels.
#' @param seed SplitMix64 seed (mirrors the Python arm).
#' @param bounds Optional list (or 2-column matrix) of per-factor
#'   (low, high) ranges; default unit cube.
#' @return A list with elements \code{mu}, \code{mu_star},
#'   \code{sigma}, \code{ee}, \code{n_runs}, \code{delta}, \code{r},
#'   \code{p}, \code{seed}, \code{method}.
#' @references Morris, M. D. (1991). Technometrics, 33, 161-174.
#'   Campolongo, F., Cariboni, J. and Saltelli, A. (2007).
#'   Environmental Modelling and Software, 22, 1509-1518.  Saltelli,
#'   A. et al. (2008). Global Sensitivity Analysis: The Primer,
#'   Wiley.
#' @export
morie_morrism <- function(fun, k, r = 10, p = 4, seed = 0,
                          bounds = NULL) {
  k <- as.integer(k); r <- as.integer(r); p <- as.integer(p)
  if (k < 1 || r < 1) stop("k and r must be positive")
  if (p < 2 || p %% 2 != 0) stop("p must be an even integer >= 2")
  if (is.null(bounds)) {
    bounds <- matrix(c(rep(0, k), rep(1, k)), ncol = 2)
  } else if (is.list(bounds)) {
    bounds <- do.call(rbind, lapply(bounds, as.numeric))
  }
  if (nrow(bounds) != k || any(bounds[, 2] <= bounds[, 1])) {
    stop("bounds must be k pairs with low < high")
  }
  delta <- p / (2 * (p - 1))
  lv <- (seq_len(p) - 1) / (p - 1)
  levels_ <- lv[lv <= 1 - delta + 1e-12]
  nl <- length(levels_)
  e <- .ghc_rng(seed)
  ee <- vector("list", k)
  n_runs <- 0L
  scale_ <- function(u) bounds[, 1] + u * (bounds[, 2] - bounds[, 1])
  for (tr in seq_len(r)) {
    ub <- .ghc_unif(e, k)
    x <- levels_[pmin(floor(ub * nl), nl - 1) + 1]
    keys <- .ghc_unif(e, k)
    ord <- order(keys)
    y <- as.numeric(fun(scale_(x)))
    n_runs <- n_runs + 1L
    for (i in ord) {
      step <- if (.ghc_unif(e, 1) < 0.5) delta else -delta
      if (x[i] + step > 1 + 1e-12 || x[i] + step < -1e-12) step <- -step
      x2 <- x
      x2[i] <- x[i] + step
      y2 <- as.numeric(fun(scale_(x2)))
      n_runs <- n_runs + 1L
      ee[[i]] <- c(ee[[i]], (y2 - y) / step)
      x <- x2
      y <- y2
    }
  }
  mu <- vapply(ee, mean, numeric(1))
  mu_star <- vapply(ee, function(v) mean(abs(v)), numeric(1))
  sigma <- vapply(ee, function(v) if (length(v) > 1) sd(v) else NaN,
                  numeric(1))
  list(mu = mu, mu_star = mu_star, sigma = sigma, ee = ee,
       n_runs = n_runs, delta = delta, r = r, p = p, seed = seed,
       method = "Morris elementary effects (Saltelli 2008 Eq. 3.1)")
}
