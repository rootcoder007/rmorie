.msml_logit <- function(X, y, max_iter = 100, tol = 1e-9) {
  D <- cbind(1, X)
  beta <- rep(0, ncol(D))
  probs <- function(b) 1 / (1 + exp(-pmin(pmax(as.vector(D %*% b), -35), 35)))
  for (it in seq_len(max_iter)) {
    p <- probs(beta)
    w <- pmax(p * (1 - p), 1e-10)
    step <- solve(crossprod(D, w * D), crossprod(D, y - p))
    beta <- beta + as.vector(step)
    if (max(abs(step)) < tol) break
  }
  probs(beta)
}

#' Marginal structural model by IPTW, and DBSCAN on point locations
#'
#' `marginal_structural_model` fits `E(Y(abar)) = b0 + b1 sum_t a_t` by
#' weighted least squares with stabilised inverse-probability-of-treatment
#' weights `prod_t f(A_t | past A) / f(A_t | past A, L_1..L_t)`, both
#' densities by Newton-Raphson logistic regression (Robins, Hernan and
#' Brumback 2000).  `spatial_dbscan` returns DBSCAN clusters of point
#' locations (`DbscanClusters`; Ester et al. 1996).
#'
#' @param y Outcome.
#' @param treatment_history Binary n x T treatment matrix.
#' @param covariate_history n x T time-varying confounders.
#' @param coords Point coordinates.
#' @param eps Neighbourhood radius.
#' @param min_pts Minimum points of a core point (itself included).
#' @param border_points Assign border points to clusters.
#' @return `marginal_structural_model` a list with `estimate`, `intercept`,
#'   `weights`, `ess`, `n`, `n_periods`; `spatial_dbscan` a list with
#'   `statistic` (number of clusters), `cluster`, `n_noise`, `sizes`.
#' @references Robins, J. M., Hernan, M. A. and Brumback, B. (2000).
#'   Marginal structural models and causal inference in epidemiology.
#'   Epidemiology 11, 550-560. Ester, M., Kriegel, H.-P., Sander, J. and Xu,
#'   X. (1996). A density-based algorithm for discovering clusters in large
#'   spatial databases with noise. KDD-96, 226-231.
#' @examples
#' spatial_dbscan(rbind(c(0, 0), c(0, 1), c(1, 0), c(9, 9), c(9, 8), c(8, 9), c(5, 5)), eps = 1.5, min_pts = 3)$cluster
#' @export
marginal_structural_model <- function(y, treatment_history, covariate_history) {
  y <- as.numeric(y)
  A <- as.matrix(treatment_history) * 1
  L <- as.matrix(covariate_history) * 1
  n <- nrow(A)
  T <- ncol(A)
  if (length(y) != n || !all(dim(L) == dim(A))) stop("shapes disagree")
  if (!all(A %in% c(0, 1))) stop("treatment_history must be binary 0/1.")
  sw <- rep(1, n)
  for (t in seq_len(T)) {
    a <- A[, t]
    if (min(a) == max(a)) next
    pn <- if (t > 1) .msml_logit(A[, seq_len(t - 1), drop = FALSE], a) else rep(mean(a), n)
    pd <- pmin(pmax(.msml_logit(cbind(A[, seq_len(t - 1), drop = FALSE], L[, seq_len(t), drop = FALSE]), a), 1e-6),
               1 - 1e-6)
    sw <- sw * ifelse(a == 1, pn, 1 - pn) / ifelse(a == 1, pd, 1 - pd)
  }
  cum <- rowSums(A)
  b <- as.vector(solve(crossprod(cbind(1, cum), sw * cbind(1, cum)), crossprod(cbind(1, cum), sw * y)))
  list(estimate = b[2], intercept = b[1], weights = sw, ess = sum(sw)^2 / sum(sw^2), n = n, n_periods = T,
       method = "Marginal structural model fit by IPTW (stabilised weights)")
}

#' @rdname marginal_structural_model
#' @export
spatial_dbscan <- function(coords, eps = 1, min_pts = 5, border_points = TRUE) {
  lab <- DbscanClusters(coords, eps, min_pts, border_points = border_points)$cluster
  k <- if (length(lab)) max(lab) else 0
  list(statistic = k, cluster = lab, n_noise = sum(lab == 0), sizes = vapply(seq_len(k), function(c) sum(lab == c), 0L))
}
