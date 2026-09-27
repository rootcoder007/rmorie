.csr_lfun <- function(P, win, r) {
  n <- nrow(P)
  area <- (win[2] - win[1]) * (win[4] - win[3])
  D <- as.matrix(stats::dist(P))
  dd <- numeric(0)
  wt <- numeric(0)
  for (i in 1:n) for (j in 1:n) {
    if (i != j && D[i, j] <= r[length(r)]) {
      dd <- c(dd, D[i, j])
      wt <- c(wt, 1 / .ppm_iso_weight(P[i, 1], P[i, 2], D[i, j], win))
    }
  }
  o <- order(dd)
  cs <- cumsum(wt[o])
  K <- vapply(r, function(h) {
    k <- sum(dd[o] <= h)
    if (k == 0) 0 else cs[k]
  }, 0) * area / (n * (n - 1))
  sqrt(K / pi)
}

#' DCLF and MAD Monte Carlo tests of complete spatial randomness
#'
#' Compares Besag's \eqn{L(r) = \sqrt{K(r)/\pi}} (isotropic K,
#' \eqn{\lambda^2 = n(n-1)/|W|^2}) with its CSR value r over
#' \eqn{0 \le r \le} rmax: the integrated squared deviation
#' (Diggle-Cressie-Loosmore-Ford, \eqn{u = } rmax times the mean of
#' \eqn{(L(r) - r)^2} over the grid) or the maximum absolute deviation,
#' exactly the statistics of \code{spatstat.explore::dclf.test} and
#' \code{mad.test} with \code{Lest} and \code{use.theo = TRUE}. The null
#' distribution comes from \code{nsim} binomial patterns drawn from Philox
#' stream s of \code{seed} (identical to the Python arm); p-value
#' (1 + #{T_sim >= T_obs}) / (nsim + 1).
#'
#' @param points Two-column matrix of coordinates.
#' @param window c(xmin, xmax, ymin, ymax).
#' @param nsim Number of simulated patterns.
#' @param seed Philox seed.
#' @param statistic \code{"dclf"} or \code{"mad"}.
#' @param rmax Upper end of the r interval (default the Kest rule).
#' @param nr Number of r values.
#' @return List with \code{statistic}, \code{p_value}, \code{simulated},
#'   \code{nsim}, \code{r}, \code{L}, \code{method}.
#' @references Diggle, P. J. (1986). Displaced amacrine cells in the retina
#'   of a rabbit: analysis of a bivariate spatial point pattern. Journal of
#'   Neuroscience Methods 18, 115-125.
#'
#'   Loosmore, N. B. and Ford, E. D. (2006). Statistical inference using the
#'   G or K point pattern spatial statistics. Ecology 87, 1925-1931.
#' @examples
#' P <- cbind(c(.1, .4, .35, .8, .7, .55, .2, .9, .15, .6), c(.2, .8, .3, .6, .15, .5, .65, .9, .95, .85))
#' CSRGlobalTest(P, c(0, 1, 0, 1), nsim = 19, statistic = "mad")$p_value
#' @export
CSRGlobalTest <- function(points, window, nsim = 99L, seed = 1L, statistic = "dclf", rmax = NULL, nr = 513L) {
  if (!statistic %in% c("dclf", "mad")) stop("statistic must be 'dclf' or 'mad'")
  P <- as.matrix(points)
  n <- nrow(P)
  if (n < 2) stop("need at least two points")
  if (is.null(rmax)) rmax <- .ppm_rmax(n, window)
  r <- rmax * (0:(nr - 1)) / (nr - 1)
  disc <- function(L) if (statistic == "mad") max(abs(L - r)) else rmax * mean((L - r)^2)
  Lobs <- .csr_lfun(P, window, r)
  obs <- disc(Lobs)
  sims <- numeric(nsim)
  for (s in seq_len(nsim)) {
    u <- .morie_random_uniform(2 * n, seed = seed, stream = s - 1L)
    Q <- cbind(window[1] + (window[2] - window[1]) * u[seq(1, 2 * n, 2)],
               window[3] + (window[4] - window[3]) * u[seq(2, 2 * n, 2)])
    sims[s] <- disc(.csr_lfun(Q, window, r))
  }
  list(statistic = obs, p_value = (1 + sum(sims >= obs)) / (nsim + 1), simulated = sims, nsim = nsim, r = r,
       L = Lobs, method = statistic)
}
