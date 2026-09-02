# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Native statistical primitives (feat/native-specializations,
# module 26). Closed-form replacements for small single-purpose
# CRAN packages: ppcor (partial / semi-partial correlation and their
# tests) and randtests (runs test, turning-point test, and companions).
# Each is a few lines of exact algebra; tests/cross validates them
# against the reference packages to machine precision.

# ---------------------------------------------------------------------------
# Partial + semi-partial correlation (replaces ppcor)
# ---------------------------------------------------------------------------

#' Partial correlation matrix (native)
#'
#' The correlation between every pair of variables with all the others
#' partialled out, computed from the precision matrix
#' \eqn{P = R^{-1}}: \eqn{r_{ij\cdot\mathrm{rest}} =
#' -P_{ij} / \sqrt{P_{ii} P_{jj}}}. Reproduces \code{ppcor::pcor}.
#'
#' @param data Numeric matrix or data frame (columns = variables).
#' @param method Correlation method: \code{"pearson"} (default),
#'   \code{"spearman"}, or \code{"kendall"}.
#' @return A list with \code{estimate} (partial-correlation matrix),
#'   \code{p.value}, \code{statistic} (t), \code{n}, \code{gp} (number
#'   of variables conditioned on), \code{method}.
#' @references Kim, S. (2015). ppcor: An R package for a fast
#'   calculation to semi-partial correlation coefficients.
#'   \emph{Communications for Statistical Applications and Methods},
#'   22(6), 665-674.
#' @examples
#' morie_partial_cor(mtcars[, c("mpg", "wt", "disp")])$estimate
#' @export
morie_partial_cor <- function(data, method = "pearson") {
  X <- as.matrix(data)
  storage.mode(X) <- "double"
  n <- nrow(X)
  gp <- ncol(X) - 2L
  R <- stats::cor(X, method = method)
  P <- tryCatch(solve(R), error = function(e) .morie_ginv(R))
  d <- sqrt(diag(P))
  est <- -P / outer(d, d)
  diag(est) <- 1
  # t statistic on n - 2 - gp df
  df <- n - 2L - gp
  stat <- est * sqrt(df / (1 - est^2))
  pv <- 2 * stats::pt(-abs(stat), df)
  diag(pv) <- 0
  dimnames(est) <- dimnames(pv) <- dimnames(stat) <- dimnames(R)
  list(estimate = est, p.value = pv, statistic = stat,
       n = n, gp = gp, method = method)
}

#' Partial correlation test for one pair given controls (native)
#'
#' \eqn{r_{xy\cdot z}} with its t-test on \eqn{n - 2 - |z|} degrees of
#' freedom. Reproduces \code{ppcor::pcor.test}.
#'
#' @param x,y Numeric vectors.
#' @param z Numeric vector, matrix, or data frame of controls.
#' @param method Correlation method.
#' @return A one-row data frame: \code{estimate}, \code{p.value},
#'   \code{statistic}, \code{n}, \code{gp}, \code{Method}.
#' @examples
#' morie_partial_cor_test(mtcars$mpg, mtcars$wt, mtcars$disp)
#' @export
morie_partial_cor_test <- function(x, y, z, method = "pearson") {
  z <- as.matrix(z)
  dat <- cbind(x = as.numeric(x), y = as.numeric(y), z)
  pc <- morie_partial_cor(dat, method = method)
  data.frame(estimate = pc$estimate["x", "y"],
             p.value = pc$p.value["x", "y"],
             statistic = pc$statistic["x", "y"],
             n = pc$n, gp = ncol(z),
             Method = method, stringsAsFactors = FALSE)
}

#' Semi-partial (part) correlation matrix (native)
#'
#' The correlation between \eqn{x_i} and \eqn{x_j} with the other
#' variables partialled out of \eqn{x_j} only:
#' \eqn{-P_{ij} / (\sqrt{P_{ii}}\, \sqrt{P_{jj} - P_{ij}^2 / P_{ii}})}.
#' Reproduces \code{ppcor::spcor}.
#'
#' @inheritParams morie_partial_cor
#' @return A list mirroring \code{\link{morie_partial_cor}} with the
#'   semi-partial coefficients in \code{estimate}.
#' @examples
#' morie_semipartial_cor(mtcars[, c("mpg", "wt", "disp")])$estimate
#' @export
morie_semipartial_cor <- function(data, method = "pearson") {
  X <- as.matrix(data)
  storage.mode(X) <- "double"
  n <- nrow(X)
  gp <- ncol(X) - 2L
  # Ding-VanderWeele/Kim semi-partial: from the COVARIANCE precision.
  cvx <- stats::cov(X, method = method)
  icvx <- tryCatch(solve(cvx), error = function(e) .morie_ginv(cvx))
  est <- -stats::cov2cor(icvx) / sqrt(diag(cvx)) /
    sqrt(abs(diag(icvx) - t(t(icvx^2) / diag(icvx))))
  diag(est) <- 1
  df <- n - 2L - gp
  stat <- est * sqrt(df / (1 - est^2))
  pv <- 2 * stats::pt(-abs(stat), df)
  diag(pv) <- 0
  dimnames(est) <- dimnames(pv) <- dimnames(stat) <- dimnames(cvx)
  list(estimate = est, p.value = pv, statistic = stat,
       n = n, gp = gp, method = method)
}

# ---------------------------------------------------------------------------
# Randomness tests (replaces randtests)
# ---------------------------------------------------------------------------

#' Wald-Wolfowitz runs test (native)
#'
#' Tests a numeric series for randomness by dichotomizing around a
#' threshold and counting runs, with the exact normal approximation.
#' Reproduces \code{randtests::runs.test}.
#'
#' @param x Numeric vector.
#' @param threshold Cut point (default the median); values equal to
#'   the threshold are dropped, as in \pkg{randtests}.
#' @param alternative \code{"two.sided"} (default),
#'   \code{"left.sided"} (too few runs), or \code{"right.sided"}.
#' @return A list with \code{statistic} (z), \code{p.value},
#'   \code{runs}, \code{n1}, \code{n2}, \code{method}.
#' @references Wald, A., & Wolfowitz, J. (1940). On a test whether two
#'   samples are from the same population. \emph{Annals of
#'   Mathematical Statistics}, 11(2), 147-162.
#' @examples
#' set.seed(1); morie_runs_test(rnorm(50))$p.value
#' @export
morie_runs_test <- function(x, threshold = stats::median(x),
                            alternative = "two.sided") {
  x <- x[x != threshold]
  s <- sign(x - threshold)
  n1 <- sum(s > 0)
  n2 <- sum(s < 0)
  n <- n1 + n2
  runs <- 1L + sum(s[-1] != s[-length(s)])
  mu <- 2 * n1 * n2 / n + 1
  v <- 2 * n1 * n2 * (2 * n1 * n2 - n) / (n^2 * (n - 1))
  z <- (runs - mu) / sqrt(v)
  p <- switch(alternative,
    two.sided = 2 * stats::pnorm(-abs(z)),
    left.sided = stats::pnorm(z),
    right.sided = stats::pnorm(z, lower.tail = FALSE),
    stop("bad alternative"))
  list(statistic = z, p.value = p, runs = runs, n1 = n1, n2 = n2,
       method = "Runs Test (rmorie native)")
}

#' Turning-point test (native)
#'
#' Counts local maxima and minima and compares to the expectation
#' \eqn{2(n-2)/3} under randomness. Reproduces
#' \code{randtests::turning.point.test}.
#'
#' @param x Numeric vector.
#' @return A list with \code{statistic} (z), \code{p.value},
#'   \code{tp} (turning-point count), \code{method}.
#' @examples
#' set.seed(1); morie_turning_point_test(rnorm(60))$p.value
#' @export
morie_turning_point_test <- function(x) {
  n <- length(x)
  tp <- sum(vapply(2:(n - 1), function(i)
    (x[i] > x[i - 1] && x[i] > x[i + 1]) ||
      (x[i] < x[i - 1] && x[i] < x[i + 1]), logical(1)))
  mu <- 2 * (n - 2) / 3
  v <- (16 * n - 29) / 90
  z <- (tp - mu) / sqrt(v)
  list(statistic = z, p.value = 2 * stats::pnorm(-abs(z)),
       tp = tp, method = "Turning Point Test (rmorie native)")
}

#' Difference-sign test (native)
#'
#' Counts positive first differences and compares to \eqn{(n-1)/2}.
#' Reproduces \code{randtests::difference.sign.test}.
#'
#' @param x Numeric vector.
#' @return A list with \code{statistic}, \code{p.value}, \code{ds}
#'   (count of positive differences), \code{method}.
#' @examples
#' set.seed(4)
#' x <- rnorm(80)
#' res <- morie_difference_sign_test(x)
#' res$p.value
#' @export
morie_difference_sign_test <- function(x) {
  d <- diff(x)
  ds <- sum(d > 0)
  n <- length(x)
  mu <- (n - 1) / 2
  v <- (n + 1) / 12
  z <- (ds - mu) / sqrt(v)
  list(statistic = z, p.value = 2 * stats::pnorm(-abs(z)),
       ds = ds, method = "Difference Sign Test (rmorie native)")
}

#' Bartels rank test of randomness (native)
#'
#' The rank-based von Neumann ratio: the sum of squared successive
#' rank differences over the rank variance. Reproduces
#' \code{randtests::bartels.rank.test}.
#'
#' @param x Numeric vector.
#' @param alternative \code{"two.sided"} (default),
#'   \code{"left.sided"}, or \code{"right.sided"}.
#' @return A list with \code{statistic} (standardized), \code{rvn}
#'   (the ratio), \code{p.value}, \code{method}.
#' @references Bartels, R. (1982). The rank version of von Neumann's
#'   ratio test for randomness. \emph{JASA}, 77(377), 40-46.
#' @examples
#' set.seed(4)
#' res <- morie_bartels_rank_test(rnorm(80))
#' res$p.value
#' @export
morie_bartels_rank_test <- function(x, alternative = "two.sided") {
  n <- length(x)
  r <- rank(x)
  rvn <- sum(diff(r)^2) / sum((r - (n + 1) / 2)^2)
  # standardized: (rvn - 2) / sd, sd^2 = 4(n-2)(5n^2-2n-9)/(5n(n+1)(n-1)^2)
  mu <- 2
  v <- 4 * (n - 2) * (5 * n^2 - 2 * n - 9) /
    (5 * n * (n + 1) * (n - 1)^2)
  z <- (rvn - mu) / sqrt(v)
  p <- switch(alternative,
    two.sided = 2 * stats::pnorm(-abs(z)),
    left.sided = stats::pnorm(z),
    right.sided = stats::pnorm(z, lower.tail = FALSE),
    stop("bad alternative"))
  list(statistic = z, rvn = rvn, p.value = p,
       method = "Bartels Rank Test (rmorie native)")
}

# ---------------------------------------------------------------------------
# E-value family (replaces EValue) -- Ding-VanderWeele closed forms
# ---------------------------------------------------------------------------

#' Internal helper: E-value from a risk-ratio-scale point
#' @noRd
.morie_e_from_rr <- function(rr) {
  if (!is.finite(rr) || rr <= 0) return(NA_real_)
  r <- if (rr < 1) 1 / rr else rr
  if (r <= 1) 1 else r + sqrt(r * (r - 1))
}

#' Internal helper: approximate risk ratio from another effect scale
#'
#' The published VanderWeele-Ding approximate conversions used by the
#' EValue package: OR/HR (rare vs common outcome) and standardized
#' mean difference / OLS.
#' @noRd
.morie_rr_approx <- function(est, type = c("RR", "OR", "HR", "OLS",
                                           "MD"),
                             rare = TRUE, sd = NULL) {
  type <- match.arg(type)
  switch(type,
    RR = est,
    OR = if (rare) est else sqrt(est),
    HR = if (rare) est else
      (1 - 0.5^sqrt(est)) / (1 - 0.5^sqrt(1 / est)),
    OLS = ,
    MD = {
      d <- if (identical(type, "OLS")) est / sd else est
      exp(0.91 * d)
    })
}

#' E-value for a bias-scale effect estimate (native)
#'
#' The minimum strength of unmeasured confounding (on the risk-ratio
#' scale) that could explain away an observed association, plus the
#' E-value for the CI bound closest to the null. Handles RR, OR, HR
#' (rare / common outcome), and OLS / standardized-mean-difference
#' estimands via the published VanderWeele-Ding conversions.
#' Reproduces \code{EValue::evalue} / \code{EValue::evalues.*}.
#'
#' @param est Point estimate on the \code{type} scale.
#' @param type One of \code{"RR"}, \code{"OR"}, \code{"HR"},
#'   \code{"OLS"}, \code{"MD"}.
#' @param lo,hi Optional CI bounds (same scale as \code{est}).
#' @param rare For OR/HR, whether the outcome is rare (default TRUE).
#' @param sd For OLS, the outcome standard deviation.
#' @param true Null value (default 1 for ratio scales, 0 for MD/OLS).
#' @return A list with \code{point} (E-value for the estimate) and
#'   \code{ci} (E-value for the near-null CI bound, or NA).
#' @references Ding, P., & VanderWeele, T. J. (2016). Sensitivity
#'   analysis without assumptions. \emph{Epidemiology}, 27(3),
#'   368-377.
#' @examples
#' morie_evalue(3.9, "RR", lo = 2.4)
#' @export
morie_evalue <- function(est, type = "RR", lo = NULL, hi = NULL,
                         rare = TRUE, sd = NULL, true = NULL) {
  type <- match.arg(type, c("RR", "OR", "HR", "OLS", "MD"))
  if (is.null(true)) true <- if (type %in% c("MD", "OLS")) 0 else 1
  to_rr <- function(v) {
    rr <- .morie_rr_approx(v, type, rare = rare, sd = sd)
    # centre on the null: for MD/OLS the conversion already maps 0->1
    rr
  }
  point <- .morie_e_from_rr(to_rr(est))
  ci <- NA_real_
  bounds <- c(lo, hi)
  if (length(bounds)) {
    rr_b <- vapply(bounds, to_rr, numeric(1))
    # the E-value for a CI is driven by the bound nearest the null (1)
    if (any(rr_b < 1) && any(rr_b > 1)) {
      ci <- 1                     # interval crosses the null
    } else {
      near <- bounds[which.min(abs(rr_b - 1))]
      ci <- .morie_e_from_rr(to_rr(near))
    }
  }
  list(point = point, ci = ci)
}
