# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Backdoor-adjusted ATE by stratification (Causbckd). Bit-identical
# mirror of src/morie/fn/causbckd.py. Strata are processed in C-locale
# string order to match the Python arm exactly.

#' Backdoor-adjusted average treatment effect by stratification
#'
#' Plug-in estimator of the Pearl adjustment formula
#' \eqn{P(y | do(x)) = \sum_s P(y | x, s) P(s)} (Pearl 2009,
#' Statistics Surveys 3, Eq. 25, Section 3.3.1) for a binary treatment
#' and a discrete admissible set: the ATE is the stratum-share weighted
#' sum of within-stratum differences of arm means. The standard error
#' treats stratum shares as fixed:
#' \eqn{Var = \sum_z (n_z/n)^2 (s_{1z}^2/n_{1z} + s_{0z}^2/n_{0z})}
#' with within-arm sample variances.
#'
#' @param y Outcome, length n.
#' @param x Binary treatment, coded 0/1.
#' @param z Discrete stratum labels of the admissible set.
#' @return List with \code{estimate}, \code{se}, \code{strata},
#'   \code{n}, \code{method}. A stratum with an empty arm is a
#'   positivity violation and raises an error.
#' @references Pearl, J. (2009), Causal inference in statistics: An
#'   overview, Statistics Surveys 3, 96-146, doi:10.1214/09-SS057,
#'   Eq. 25, Section 3.3.1; local copy
#'   fetched-wave3/pearl-2009-causal-inference-statistics-overview-StatSurveys3.pdf.
#' @export
#' @examples
#' set.seed(1)
#' Causbckd(y = rnorm(50), x = rbinom(50, 1, 0.5),
#'          z = sample(c("a", "b"), 50, replace = TRUE))
Causbckd <- function(y, x, z) {
  y <- as.numeric(y)
  xv <- as.numeric(x)
  zl <- as.character(z)
  n <- length(y)
  if (length(xv) != n || length(zl) != n) {
    stop("y, x, z must have equal length", call. = FALSE)
  }
  if (!all(xv %in% c(0, 1))) stop("x must be binary 0/1", call. = FALSE)
  ks <- sort(unique(zl), method = "radix")
  ate <- 0
  v <- 0
  strata <- list()
  for (k in ks) {
    i1 <- which(zl == k & xv == 1)
    i0 <- which(zl == k & xv == 0)
    nz <- length(i1) + length(i0)
    if (length(i1) == 0L || length(i0) == 0L) {
      stop(sprintf(
        "stratum %s has an empty treatment or control arm (positivity violation)",
        k), call. = FALSE)
    }
    d <- mean(y[i1]) - mean(y[i0])
    w <- nz / n
    ate <- ate + w * d
    v1 <- if (length(i1) > 1L) stats::var(y[i1]) / length(i1) else 0
    v0 <- if (length(i0) > 1L) stats::var(y[i0]) / length(i0) else 0
    v <- v + w * w * (v1 + v0)
    strata[[k]] <- list(share = w, effect = d,
                        n1 = length(i1), n0 = length(i0))
  }
  list(estimate = ate, se = sqrt(v), strata = strata, n = n,
       method = "Pearl (2009) Eq. 25 backdoor adjustment, stratified plug-in")
}
