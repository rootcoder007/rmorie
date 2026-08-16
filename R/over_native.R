# Propensity-score overlap (common support) diagnostics.
# Sources: Rosenbaum, P. R. and Rubin, D. B. (1983), The central role
# of the propensity score in observational studies for causal effects,
# Biometrika 70(1), 41-55 (overlap/common support is the positivity
# half of strong ignorability); Crump, R. K., Hotz, V. J., Imbens,
# G. W. and Mitnik, O. A. (2009), Dealing with limited overlap in
# estimation of average treatment effects, Biometrika 96(1), 187-199
# (trimming to the overlap region).  The distributional comparison is
# the two-sample Kolmogorov-Smirnov (Smirnov) test.
#
# Native implementation mirroring Python morie.fn.over exactly,
# including the same two-sample KS p-value policy: exact by the
# path-counting recursion when n1 n2 < 10000 and there are no ties,
# asymptotic otherwise.

# Exact P(D < d) for the two-sample statistic, no ties: the lattice
# path recursion of R's psmirnov2x.
#' Exact P(D < d) for the two-sample statistic, no ties: the lattice
#'
#' path recursion of R\'s psmirnov2x.
#'
#' @param d Numeric; combined arithmetically in the body.
#' @param n1 A count; the body uses it as \code{seq_len(...)}.
#' @param n2 A count; the body uses it as \code{seq_len(...)}.
#' @param two_sided A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return The value of \code{[}.
#' @export
.mor_ks_psmirnov <- function(d, n1, n2, two_sided = TRUE) {
  md <- n1; nd <- n2
  q <- (0.5 + floor(d * md * nd - 1e-7)) / (md * nd)
  u <- numeric(n2 + 1L)
  for (j in 0:n2)
    u[j + 1L] <- if (two_sided && (j / nd) > q) 0 else 1
  for (i in seq_len(n1)) {
    w <- i / (i + nd)
    u[1L] <- if ((i / md) > q) 0 else w * u[1L]
    for (j in seq_len(n2)) {
      gap <- i / md - j / nd
      cmp <- if (two_sided) abs(gap) else gap
      u[j + 1L] <- if (cmp > q) 0 else w * u[j + 1L] + u[j]
    }
  }
  u[n2 + 1L]
}

# Two-sided asymptotic Kolmogorov series with Stephens' correction.
#' Two-sided asymptotic Kolmogorov series with Stephens\' correction
#'
#' A step of the over_native implementation. Called by \code{.mor_ks_2samp}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param d Numeric; combined arithmetically in the body.
#' @param n Numeric; passed to \code{sqrt}.
#' @return A numeric value.
#' @export
.mor_ks_sf <- function(d, n) {
  lam <- d * (sqrt(n) + 0.12 + 0.11 / sqrt(n))
  if (lam < 0.04) {
    # Q(lam) -> 1 as lam -> 0, but the alternating series below does not
    # converge there: its terms stay near 2 and the truncated sum lands
    # on an arbitrary value (0 at lam = 0).  Return the limit.  This is
    # the branch taken when two samples are identical, i.e. D = 0 with
    # ties, where the correct p-value is 1.
    return(1)
  }
  s <- 0
  for (j in 1:100) {
    term <- 2 * (-1)^(j - 1) * exp(-2 * j * j * lam * lam)
    s <- s + term
    if (abs(term) < 1e-12) break
  }
  max(0, min(1, s))
}

# Two-sample Kolmogorov-Smirnov statistic and p-value.
#' Two-sample Kolmogorov-Smirnov statistic and p-value
#'
#' A step of the over_native implementation. Called by \code{morie_over}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a See Usage.
#' @param b See Usage.
#' @return A list with \code{stat}, \code{p}, \code{n1}, \code{n2}, \code{d_plus}, \code{d_minus}, \code{n_ties}.
#' @export
.mor_ks_2samp <- function(a, b) {
  x <- sort(as.numeric(a)); y <- sort(as.numeric(b))
  n1 <- length(x); n2 <- length(y)
  if (n1 < 1L || n2 < 1L) stop("both samples need at least one observation")
  allv <- sort(unique(c(x, y)))
  ties <- (n1 + n2) - length(allv)
  ecdf_at <- function(v, u) vapply(u, function(z) sum(v <= z), numeric(1)) / length(v)
  diffs <- ecdf_at(x, allv) - ecdf_at(y, allv)
  dplus <- max(0, max(diffs))
  dminus <- max(0, -min(diffs))
  en <- n1 * n2 / (n1 + n2)
  d <- max(dplus, dminus)
  pv <- if (n1 * n2 < 10000 && ties == 0L)
    1 - .mor_ks_psmirnov(d, n1, n2) else .mor_ks_sf(d, en)
  list(stat = d, p = max(0, min(1, pv)), n1 = n1, n2 = n2,
       d_plus = dplus, d_minus = dminus, n_ties = ties)
}

#' Propensity-score overlap diagnostics
#'
#' Reports the common-support interval shared by the treated and
#' control propensity scores, the percentage of observations that
#' trimming to that interval would discard (Crump et al. 2009), and a
#' two-sample Kolmogorov-Smirnov comparison of the two score
#' distributions.  Overlap is the positivity half of the strong
#' ignorability condition of Rosenbaum and Rubin (1983); without it no
#' matching or weighting estimator is identified in the affected
#' region.
#'
#' @param ps_treated Propensity scores of treated units.
#' @param ps_control Propensity scores of control units.
#' @return A list with \code{overlap_range} (\code{c(0, 0)} when the
#'   supports do not intersect), \code{pct_trimmed}, \code{ks_stat},
#'   \code{ks_p}, \code{n_treated} and \code{n_control}.
#' @references Crump, R. K., Hotz, V. J., Imbens, G. W. and Mitnik,
#'   O. A. (2009). Dealing with limited overlap in estimation of
#'   average treatment effects. Biometrika, 96(1), 187-199.
#' @export
morie_over <- function(ps_treated, ps_control) {
  pt <- as.numeric(ps_treated); pc <- as.numeric(ps_control)
  if (length(pt) == 0L || length(pc) == 0L)
    stop("Both arrays must be non-empty.")
  lo <- max(min(pt), min(pc))
  hi <- min(max(pt), max(pc))
  overlap_range <- if (lo < hi) c(lo, hi) else c(0, 0)
  n_total <- length(pt) + length(pc)
  in_t <- sum(pt >= lo & pt <= hi)
  in_c <- sum(pc >= lo & pc <= hi)
  n_trimmed <- n_total - in_t - in_c
  ks <- .mor_ks_2samp(pt, pc)
  list(overlap_range = overlap_range,
       pct_trimmed = if (n_total > 0) 100 * n_trimmed / n_total else 0,
       ks_stat = ks$stat, ks_p = ks$p,
       n_treated = length(pt), n_control = length(pc))
}
