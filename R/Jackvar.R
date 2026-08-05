# SPDX-License-Identifier: AGPL-3.0-or-later
#' Jackknife variance for survey estimates
#'
#' For the delete-one jackknife of an unweighted mean the estimator
#' reduces algebraically to s^2 / n, the textbook variance of the sample
#' mean, which is the check the tests apply.  Replicate weights may be
#' supplied instead, in which case each replicate is whatever the
#' supplied weights make it.
#'
#' Formula: v_J = ((R - 1)/R) sum_r (theta_r - theta)^2.
#'
#' @param y Observations.
#' @param weights Optional full-sample weights; equal by default.
#' @param replicates Optional R x n matrix of replicate weight sets;
#'   the delete-one jackknife by default.
#' @return List with \code{estimate}, \code{variance}, \code{theta},
#'   \code{theta_replicates}, \code{se}, \code{n}, \code{method}.
#' @references Wolter (2007), Introduction to Variance Estimation, 2nd
#'   ed., Springer, ch. 4, eq. (4.2.5).
#' @export
Jackvar <- function(y, weights = NULL, replicates = NULL) {
  v <- .s03vec(y)
  n <- length(v)
  if (n < 2L) stop("jackknife_variance_survey: need at least two observations")
  w <- if (is.null(weights)) rep(1, n) else .s03vec(weights)
  if (length(w) != n) stop("jackknife_variance_survey: weights and y have different lengths")
  if (any(w < 0)) stop("jackknife_variance_survey: weights must be non-negative")
  wmean <- function(v, w) {
    sw <- sum(w)
    if (sw == 0) stop("jackknife_variance_survey: replicate weights sum to zero")
    sum(w * v) / sw
  }
  rep <- if (is.null(replicates)) {
    m <- matrix(rep(w, each = n), n, n)
    diag(m) <- 0
    m
  } else .s03mat(replicates)
  if (ncol(rep) != n) stop("jackknife_variance_survey: replicates must have n columns")
  R <- nrow(rep)
  if (R < 2L) stop("jackknife_variance_survey: need at least two replicates")
  theta <- wmean(v, w)
  th <- vapply(seq_len(R), function(r) wmean(v, rep[r, ]), 0)
  var <- (R - 1) / R * sum((th - theta)^2)
  .t1_result(estimate = var, variance = var, theta = theta,
             theta_replicates = th, se = sqrt(var), n = n,
             method = "v_J = ((R-1)/R) sum_r (theta_r - theta)^2, Wolter (2007) eq. (4.2.5)")
}
