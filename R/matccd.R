# SPDX-License-Identifier: AGPL-3.0-or-later
#' Estimate the odds ratio without ever estimating the matching sets
#'
#' Matching removes confounding by design, but it introduces one nuisance
#' parameter per matched set, and those parameters do not go away as the
#' sample grows -- there is always one more per new set. Ordinary logistic
#' regression therefore returns an odds ratio biased away from one, badly
#' so for 1:1 pairs. Conditioning on the number of cases in each set
#' cancels the nuisance parameters exactly, leaving a likelihood in the
#' exposure effect alone.
#'
#' Formula: for a set with one case, \code{L_s(beta) = exp(beta x_case) /
#' sum_j exp(beta x_j)}; the maximum is found by Newton-Raphson --
#' Breslow and Day (1980), Chapter 7.
#'
#' @param cases Per-observation case indicator, 1 for a case, 0 otherwise.
#' @param controls Optional control indicator; if supplied it must be
#'   \code{1 - cases}, and it is checked.
#' @param matching_id Matched-set label per observation, one case per set.
#' @param exposure Exposure value per observation; binary or continuous.
#' @param level Confidence level.
#' @param max_iter Newton steps.
#' @param tol Convergence tolerance on the score.
#' @return List with \code{estimate}, \code{log_or}, \code{se}, \code{ci},
#'   \code{information}, \code{loglik}, \code{n_sets}, \code{n_obs},
#'   \code{iters}, \code{converged}.
#' @references Breslow, N. E. and Day, N. E. (1980). Statistical Methods
#'   in Cancer Research, Volume I. IARC Scientific Publications No. 32,
#'   Lyon, Chapter 7.
#' @export
Matccd <- function(cases, controls, matching_id, exposure, level = 0.95,
                   max_iter = 100, tol = 1e-12) {
  y <- as.numeric(cases); sid <- as.integer(matching_id)
  x <- as.numeric(exposure); n <- length(y)
  if (n == 0L) stop("no observations")
  if (length(sid) != n || length(x) != n)
    stop("all inputs must have the same length")
  if (any(!(y %in% c(0, 1)))) stop("cases must be coded 0 or 1")
  if (!is.null(controls)) {
    cc <- as.numeric(controls)
    if (length(cc) != n || any(abs(cc - (1 - y)) > 1e-12))
      stop("controls must be the complement of cases")
  }
  keys <- sort(unique(sid))
  sets <- lapply(keys, function(k) which(sid == k))
  for (idx in sets) {
    if (sum(y[idx]) != 1) stop("every matched set needs exactly one case")
    if (length(idx) < 2L)
      stop("every matched set needs at least one control")
  }
  beta <- 0; it <- 0L; conv <- FALSE; info <- 0
  for (it in seq_len(as.integer(max_iter))) {
    score <- 0; info <- 0
    for (idx in sets) {
      mx <- max(beta * x[idx])
      ex <- exp(beta * x[idx] - mx)
      s0 <- sum(ex); s1 <- sum(ex * x[idx]); s2 <- sum(ex * x[idx]^2)
      xc <- x[idx][y[idx] == 1][1]
      score <- score + xc - s1 / s0
      info <- info + s2 / s0 - (s1 / s0)^2
    }
    if (info <= 0)
      stop(paste("the conditional information is zero; the exposure does",
                 "not vary within any matched set"))
    beta <- beta + score / info
    if (abs(score) < as.numeric(tol)) { conv <- TRUE; break }
  }
  ll <- 0
  for (idx in sets) {
    mx <- max(beta * x[idx])
    s0 <- sum(exp(beta * x[idx] - mx))
    xc <- x[idx][y[idx] == 1][1]
    ll <- ll + beta * xc - (mx + log(s0))
  }
  se <- 1 / sqrt(info)
  z <- .s03qnorm(1 - (1 - as.numeric(level)) / 2)
  .t1_result(estimate = exp(beta), log_or = beta, se = se,
             ci = c(exp(beta - z * se), exp(beta + z * se)),
             information = info, loglik = ll, n_sets = length(keys),
             n_obs = n, iters = it, converged = as.numeric(conv),
             method = "Conditional MLE for matched case-control data")
}
