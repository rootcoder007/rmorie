# Interval mapping of quantitative trait loci by LOD score.
# Sources: Lander, E. S. & Botstein, D. (1989) "Mapping Mendelian
# Factors Underlying Quantitative Traits Using RFLP Linkage Maps",
# Genetics 121(1), 185-199 (equations (4), (5a)-(5c), (6) and (7),
# the EM algorithm for the mixture likelihood, and the LOD
# threshold of 0.83 at a 5% single-marker error rate); Dempster,
# A. P., Laird, N. M. & Rubin, D. B. (1977) "Maximum Likelihood from
# Incomplete Data via the EM Algorithm", Journal of the Royal
# Statistical Society. Series B 39(1), 1-38; Haldane, J. B. S.
# (1919) "The combination of linkage values, and the calculation
# of distances between the loci of linked factors", Journal of
# Genetics 8(4), 299-309.
#
# Native implementation mirroring Python morie.fn.rqtmpl exactly:
# the same Haldane map function, the same backcross genotype
# probabilities, the same closed-form Gaussian log-likelihood, the
# same single-marker regression LOD, the same EM iteration for the
# mixture likelihood, and the same LOD/ELOD/threshold formulas.
# No random draws occur in this arm, so the shared generator is
# not used and the arms produce identical numeric results
# deterministically.

LOG10E <- log10(exp(1))

#' Haldane's map function
#'
#' Maps a distance in Morgans to a recombination fraction via the
#' Haldane (1919) formula \code{0.5 * (1 - exp(-2d))}.
#'
#' @param distance Map distance in Morgans, non-negative.
#' @return Recombination fraction in \code{[0, 0.5)}.
#' @references Haldane, J. B. S. (1919). The combination of linkage
#'   values, and the calculation of distances between the loci of
#'   linked factors. Journal of Genetics, 8(4), 299-309.
#' @export

# Base R has no erf/erfc; both are pnorm in disguise. Defined here so
# the arm stays base-R only, as the package requires.
.erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
.erfc <- function(x) 2 * pnorm(-x * sqrt(2))

morie_haldane <- function(distance) {
  d <- as.numeric(distance)
  if (d < 0) stop("rqtmpl: map distance cannot be negative")
  0.5 * (1 - exp(-2 * d))
}

#' Inverse Haldane map function
#'
#' Recombination fraction back to Morgans.
#'
#' @param r Recombination fraction in \code{[0, 0.5)}.
#' @return Map distance in Morgans.
#' @references Haldane, J. B. S. (1919).
#' @export
morie_inverse_haldane <- function(r) {
  r <- as.numeric(r)
  if (r < 0 || r >= 0.5)
    stop(sprintf("rqtmpl: a recombination fraction must lie in [0, 0.5), got %r", r))
  -0.5 * log(1 - 2 * r)
}

#' QTL genotype probabilities from flanking markers
#'
#' Backcross coding, 0 or 1 at every locus, no interference, so
#' the two intervals contribute independently and the result is
#' the normalised product of the two recombination probabilities.
#'
#' @param left Numeric vector of left-marker genotypes (0 or 1).
#' @param right Numeric vector of right-marker genotypes (0 or 1).
#' @param r_left Recombination fraction on the left interval,
#'   in \code{[0, 0.5]}.
#' @param r_right Recombination fraction on the right interval,
#'   in \code{[0, 0.5]}.
#' @return Numeric vector of length 2, \code{c(G(0), G(1))}.
#' @export
morie_genotype_probabilities <- function(left, right, r_left, r_right) {
  rl <- as.numeric(r_left); rr <- as.numeric(r_right)
  if (rl < 0 || rl > 0.5)
    stop(sprintf("rqtmpl: recombination fractions lie in [0, 0.5], got %r", r_left))
  if (rr < 0 || rr > 0.5)
    stop(sprintf("rqtmpl: recombination fractions lie in [0, 0.5], got %r", r_right))
  q0 <- (if (0 != as.integer(left)) rl else 1 - rl) *
        (if (as.integer(right) != 0) rr else 1 - rr)
  q1 <- (if (1 != as.integer(left)) rl else 1 - rl) *
        (if (as.integer(right) != 1) rr else 1 - rr)
  tot <- q0 + q1
  if (tot <= 0) stop("rqtmpl: the flanking marker configuration has probability zero")
  c(q0 / tot, q1 / tot)
}

# Gaussian log-likelihood of a residual vector with variance sigma2.
# Internal helper mirroring _normal_ll.
.normal_ll <- function(resid, sigma2) {
  n <- length(resid)
  -0.5 * n * log(2 * pi * sigma2) - sum(resid * resid) / (2 * sigma2)
}

#' Single-marker regression LOD
#'
#' Regression of the phenotype on one marker genotype; returns the
#' LOD both from the likelihood ratio and from the closed form
#' \code{(n/2) log10(RSS0 / RSS1)}.
#'
#' @param y Numeric phenotype vector.
#' @param g Numeric marker-genotype vector (same length as \code{y}).
#' @return A named list with keys \code{estimate}, \code{lod},
#'   \code{lod_likelihood}, \code{a}, \code{b}, \code{sigma2},
#'   \code{rss}, \code{rss_null}, \code{n}, \code{method}.
#' @references Lander, E. S. & Botstein, D. (1989), eq (4).
#' @export
morie_single_marker <- function(y, g) {
  y <- as.numeric(y); g <- as.numeric(g)
  n <- length(y)
  if (n != length(g))
    stop("rqtmpl: y and g must have the same length")
  if (n < 3L)
    stop("rqtmpl: need at least three individuals")
  gs <- as.numeric(g)
  if (max(gs) == min(gs))
    stop("rqtmpl: the marker is monomorphic, so no effect is identified")
  my <- sum(y) / n
  mg <- sum(gs) / n
  num <- sum((gs - mg) * (y - my))
  den <- sum((gs - mg)^2)
  b <- num / den
  a <- my - b * mg
  r1 <- y - (a + b * gs)
  r0 <- y - my
  rss1 <- sum(r1 * r1)
  rss0 <- sum(r0 * r0)
  s1 <- rss1 / n
  s0 <- rss0 / n
  lod_lr <- (.normal_ll(r1, s1) - .normal_ll(r0, s0)) * LOG10E
  lod_cf <- if (rss1 > 0) 0.5 * n * log10(rss0 / rss1) else Inf
  list(estimate = lod_cf, lod = lod_cf, lod_likelihood = lod_lr,
       a = a, b = b, sigma2 = s1, rss = rss1, rss_null = rss0,
       n = n,
       method = "single-marker regression LOD; Lander & Botstein (1989) eq (4)")
}

#' Interval mapping by EM on the mixture likelihood
#'
#' Implements the EM algorithm of Lander & Botstein (1989) for the
#' two-component mixture likelihood (7) at one QTL position; the
#' E step is the posterior QTL genotype probability given the
#' current parameters, the M step is a weighted regression, and the
#' log-likelihood is recorded at every iteration so the monotone
#' increase is visible.
#'
#' @param y Numeric phenotype vector.
#' @param left Numeric vector of left-marker genotypes.
#' @param right Numeric vector of right-marker genotypes.
#' @param r_left Recombination fraction on the left interval.
#' @param r_right Recombination fraction on the right interval.
#' @param max_iter Maximum number of EM iterations (default 200).
#' @param tol Convergence tolerance on successive log-likelihoods
#'   (default 1e-10).
#' @return A named list with keys \code{estimate}, \code{lod},
#'   \code{a}, \code{b}, \code{sigma2}, \code{loglik},
#'   \code{loglik_null}, \code{iterations}, \code{loglik_history},
#'   \code{posterior}, \code{n}, \code{method}.
#' @references Lander, E. S. & Botstein, D. (1989), eq (7);
#'   Dempster, A. P., Laird, N. M. & Rubin, D. B. (1977).
#' @export
morie_interval_map <- function(y, left, right, r_left, r_right,
                               max_iter = 200L, tol = 1e-10) {
  y <- as.numeric(y); left <- as.numeric(left); right <- as.numeric(right)
  n <- length(y)
  if (!(n == length(left) && n == length(right)))
    stop("rqtmpl: y and the two marker vectors must have the same length")
  G <- vector("list", n)
  for (i in seq_len(n))
    G[[i]] <- morie_genotype_probabilities(left[i], right[i],
                                           r_left, r_right)
  my <- sum(y) / n
  a <- my
  b <- 0.1 * (max(y) - min(y) + 1e-12)
  s2 <- sum((y - my)^2) / n
  history <- numeric(0)
  post <- numeric(n)
  for (iter in seq_len(as.integer(max_iter))) {
    ll <- 0
    for (i in seq_len(n)) {
      d0 <- exp(-((y[i] - a)^2) / (2 * s2))
      d1 <- exp(-((y[i] - (a + b))^2) / (2 * s2))
      m0 <- G[[i]][1] * d0
      m1 <- G[[i]][2] * d1
      tot <- m0 + m1
      if (tot <= 0)
        stop(sprintf("rqtmpl: the mixture vanished at individual %d", i - 1L))
      post[i] <- m1 / tot
      ll <- ll + log(tot / sqrt(2 * pi * s2))
    }
    history <- c(history, ll)
    if (length(history) > 1L && abs(history[length(history)] -
                                     history[length(history) - 1L]) < tol)
      break
    sw <- sum(post)
    if (sw <= 0 || sw >= n) {
      b_new <- 0
      a_new <- my
    } else {
      a_new <- sum(y * (1 - post)) / (n - sw)
      a_plus_b <- sum(y * post) / sw
      b_new <- a_plus_b - a_new
    }
    s2 <- sum((1 - post) * (y - a_new)^2 +
              post * (y - (a_new + b_new))^2) / n
    a <- a_new; b <- b_new
  }
  s0 <- sum((y - my)^2) / n
  ll0 <- -0.5 * n * (log(2 * pi * s0) + 1)
  lod <- (history[length(history)] - ll0) * LOG10E
  list(estimate = lod, lod = lod, a = a, b = b, sigma2 = s2,
       loglik = history[length(history)], loglik_null = ll0,
       iterations = length(history), loglik_history = history,
       posterior = post, n = n,
       method = "interval mapping by EM on the mixture likelihood; Lander & Botstein (1989) eq (7)")
}

#' LOD scan along an interval
#'
#' Walks a putative QTL along the interval between the two flanking
#' markers and reports the LOD at every position, the position with
#' the maximum LOD, and the fit at that position.
#'
#' @param y Numeric phenotype vector.
#' @param left Numeric vector of left-marker genotypes.
#' @param right Numeric vector of right-marker genotypes.
#' @param length Length of the interval in Morgans, positive.
#' @param step Step size in Morgans (default 0.01).
#' @param ... Forwarded to \code{morie_interval_map}
#'   (\code{max_iter}, \code{tol}).
#' @return A named list with keys \code{estimate}, \code{peak_lod},
#'   \code{peak_position}, \code{position}, \code{lod}, \code{fit},
#'   \code{method}.
#' @references Lander, E. S. & Botstein, D. (1989).
#' @export
morie_scan_interval <- function(y, left, right, length, step = 0.01, ...) {
  length <- as.numeric(length)
  if (length <= 0)
    stop("rqtmpl: the interval length must be positive")
  positions <- numeric(0); lods <- numeric(0); fits <- list()
  d <- 0
  while (d <= length + 1e-12) {
    r1 <- morie_haldane(min(d, length))
    r2 <- morie_haldane(max(length - d, 0))
    f <- morie_interval_map(y, left, right, r1, r2, ...)
    positions <- c(positions, d)
    lods <- c(lods, f$lod)
    fits[[length(fits) + 1L]] <- f
    d <- d + as.numeric(step)
  }
  k <- which.max(lods)
  list(estimate = lods[k], peak_lod = lods[k],
       peak_position = positions[k], position = positions,
       lod = lods, fit = fits[[k]],
       method = "interval scan; Lander & Botstein (1989)")
}

#' Expected LOD per progeny
#'
#' Equations (5a)-(5c) of Lander & Botstein (1989): the exact
#' expected LOD and the small-effect Taylor approximation, with
#' the gap between them rather than one silently substituted for
#' the other.
#'
#' @param var_qtl QTL variance, non-negative.
#' @param var_residual Residual variance, strictly positive.
#' @return A named list with keys \code{elod}, \code{approximation},
#'   \code{gap}, \code{ratio}, \code{note}.
#' @references Lander, E. S. & Botstein, D. (1989), eqs (5a)-(5c).
#' @export
morie_elod <- function(var_qtl, var_residual) {
  vq <- as.numeric(var_qtl); vr <- as.numeric(var_residual)
  if (vr <= 0)
    stop("rqtmpl: the residual variance must be positive")
  if (vq < 0)
    stop("rqtmpl: a variance cannot be negative")
  exact <- 0.5 * log10(1 + vq / vr)
  approx <- 0.22 * (vq / vr)
  list(elod = exact, approximation = approx,
       gap = approx - exact, ratio = vq / vr,
       note = "0.22 = (1/2) log10(e); the approximation is a Taylor expansion and drifts upward as the effect grows")
}

#' Single-marker LOD significance threshold
#'
#' Solves for the upper-tail quantile \code{z} of the standard
#' normal at level \code{alpha} by bisection on the complementary
#' error function, then reports \code{T = (1/2) log10(e) z^2}.
#' The Python arm notes that this threshold applies to a single
#' marker only -- a genome scan needs a permutation threshold.
#'
#' @param alpha Significance level in \code{(0, 1)}, default 0.05.
#' @return A named list with keys \code{threshold}, \code{z},
#'   \code{alpha}, \code{note}.
#' @references Lander, E. S. & Botstein, D. (1989).
#' @export
morie_threshold <- function(alpha = 0.05) {
  a <- as.numeric(alpha)
  if (a <= 0 || a >= 1)
    stop("rqtmpl: alpha must lie in (0, 1)")
  lo <- 0; hi <- 40
  for (k in seq_len(200L)) {
    mid <- (lo + hi) / 2
    # .erfc(mid / sqrt(2)) > a  <=>  P(Z > mid) > a
    if (.erfc(mid / sqrt(2)) > a) lo <- mid else hi <- mid
  }
  z <- (lo + hi) / 2
  list(threshold = 0.5 * LOG10E * z * z, z = z, alpha = a,
       note = "single-marker only; a genome scan needs a permutation threshold")
}

#' Number of progeny for even odds of detection
#'
#' Equation (6) of Lander & Botstein (1989): the ratio of the
#' single-marker threshold to the ELOD. Refuses the degenerate
#' case of zero QTL variance.
#'
#' @param var_qtl QTL variance, strictly positive for detection.
#' @param var_residual Residual variance, strictly positive.
#' @param alpha Significance level in \code{(0, 1)}, default 0.05.
#' @return A named list with keys \code{n}, \code{threshold},
#'   \code{elod}.
#' @references Lander, E. S. & Botstein (1989), eq (6).
#' @export
morie_progeny_required <- function(var_qtl, var_residual, alpha = 0.05) {
  t <- morie_threshold(alpha)$threshold
  e <- morie_elod(var_qtl, var_residual)$elod
  if (e <= 0)
    stop("rqtmpl: a QTL with no variance is never detected")
  list(n = t / e, threshold = t, elod = e)
}

#' R cheatsheet for the rqtmpl arm
#'
#' One-line description of the method, in the same spirit as the
#' Python \code{cheatsheet} helper.
#'
#' @return Character string summarising interval mapping, LOD, the
#'   single-marker threshold, ELOD and the 0.22 approximation.
#' @export
morie_cheatsheet <- function() {
  paste("rqtmpl: interval mapping walks a QTL along an interval",
        "and maximises the MIXTURE likelihood (7) by EM, because",
        "the QTL genotype is unknown -- G_i(x) comes from the",
        "flanking markers. LOD = log10 of the likelihood ratio,",
        "and at a marker it collapses to the single-marker",
        "regression LOD (n/2) log10(RSS0/RSS1). T = 0.83 at 5%",
        "for ONE marker; a genome scan needs permutations.",
        "ELOD = (1/2) log10(1 + var_qtl/var_res), with the",
        "paper's 0.22 approximation kept alongside it.")
}

# Compact alias mirroring interval_mapping = scan_interval.
# Exported under the same name as the Python arm's ledger alias.
#' @export
morie_interval_mapping <- morie_scan_interval

# Main entry point per TASK.md: morie_rqtmpl. The Python arm
# exposes the eight callable functions individually rather than a
# single dispatch entry point, so this entry point returns the
# collection -- element names match the Python attribute names --
# without inventing dispatch behaviour that is not in the Python
# file.
#' @export
morie_rqtmpl <- function() {
  list(haldane = morie_haldane,
       inverse_haldane = morie_inverse_haldane,
       genotype_probabilities = morie_genotype_probabilities,
       single_marker = morie_single_marker,
       interval_map = morie_interval_map,
       scan_interval = morie_scan_interval,
       interval_mapping = morie_scan_interval,
       elod = morie_elod,
       threshold = morie_threshold,
       progeny_required = morie_progeny_required,
       cheatsheet = morie_cheatsheet,
       LOG10E = LOG10E)
}
