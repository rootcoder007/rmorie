# Borusyak-Jaravel-Spiess imputation estimator for event studies.
# Source: Borusyak, K., Jaravel, X. and Spiess, J. (2024), Revisiting
# event study designs: robust and efficient estimation, Review of
# Economic Studies 91(6), 3253-3285 (arXiv 2108.12419).  Their
# Assumption 1 (parallel trends as a two-way fixed-effect model for
# the untreated potential outcome) and Theorem 2, which gives the
# efficient linear unbiased estimator in three imputation steps:
#
#   1. Using the UNTREATED observations only, fit
#      Y_it = A_it' lambda_i + X_it' delta + eps_it by OLS;
#   2. For each treated observation set Yhat_it(0) from that fit and
#      tau_it = Y_it - Yhat_it(0);
#   3. Estimate the target by the weighted sum sum_it w_it tau_it.
#
# Because no treated observation enters step 1, an already-treated
# unit can never act as a control, which is the robustness property
# the paper is about; and among unbiased estimators this one is
# efficient (their Theorem 1).
#
# Native implementation mirroring Python morie.fn.boryis exactly:
# same alternating two-way solve, same tolerance, same exact linear
# estimator weights and the same unit-clustered score sum.

#' .mor_bjs_check
#'
#' A step of the boryis_native implementation. Called by \code{morie_impute_untreated}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param obs Numeric; passed to \code{sum}.
#' @param n Numeric; combined arithmetically in the body.
#' @param T Numeric; combined arithmetically in the body.
#' @return One of two values, depending on the branch taken.
#' @export
.mor_bjs_check <- function(obs, n, T) {
  if (sum(obs) < n + T - 1L)
    stop(sprintf(paste("only %d untreated cells for %d unit and period",
                       "effects; the model is not identified. Every unit",
                       "needs an untreated period and every period an",
                       "untreated unit."), sum(obs), n + T - 1L))
  ru <- apply(obs, 1, any)
  if (!all(ru))
    stop(sprintf(paste("%d unit(s) are treated in every period, so their",
                       "untreated level cannot be imputed."), sum(!ru)))
  cu <- apply(obs, 2, any)
  if (!all(cu))
    stop(sprintf(paste("%d period(s) have no untreated unit, so that",
                       "period's effect cannot be identified."), sum(!cu)))
}

# Alternating solve of the two-way (unit, period) normal equations
# restricted to `obs`, plus optional covariate coefficients.
#' Alternating solve of the two-way (unit, period) normal equations
#'
#' restricted to `obs`, plus optional covariate coefficients.
#'
#' @param obs A matrix; passed to \code{nrow}.
#' @param u_a Numeric; combined arithmetically in the body.
#' @param u_l Numeric; combined arithmetically in the body.
#' @param u_b Numeric; combined arithmetically in the body.
#' @param Xc Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.
#' @param max_iter Coerced to integer by the body, with \code{as.integer}. Defaults to \code{2000L}.
#' @param tol Passed to \code{<}. Defaults to \code{1e-13}.
#' @return A list with \code{a}, \code{lam}, \code{b}.
#' @export
.mor_bjs_solve <- function(obs, u_a, u_l, u_b, Xc, max_iter = 2000L,
                           tol = 1e-13) {
  n <- nrow(obs)
  T <- ncol(obs)
  n_i <- rowSums(obs)
  m_t <- colSums(obs)
  a <- numeric(n)
  lam <- numeric(T)
  k <- if (is.null(Xc)) 0L else length(Xc)
  b <- if (k == 0L) NULL else numeric(k)
  XtX <- NULL
  if (k > 0L) {
    XtX <- matrix(0, k, k)
    for (p in seq_len(k)) for (q in seq_len(k))
      XtX[p, q] <- sum(Xc[[p]][obs] * Xc[[q]][obs])
    XtX <- XtX + 1e-12 * diag(k)
  }
  xb <- function(bb) {
    m <- matrix(0, n, T)
    for (p in seq_len(k)) m <- m + bb[p] * Xc[[p]]
    m
  }
  for (it in seq_len(as.integer(max_iter))) {
    a0 <- a
    l0 <- lam
    other <- ifelse(obs, matrix(lam, n, T, byrow = TRUE), 0)
    if (!is.null(b)) other <- other + ifelse(obs, xb(b), 0)
    a <- (u_a - rowSums(other)) / n_i
    other <- ifelse(obs, matrix(a, n, T), 0)
    if (!is.null(b)) other <- other + ifelse(obs, xb(b), 0)
    lam <- (u_l - colSums(other)) / m_t
    if (!is.null(b)) {
      rest <- ifelse(obs, matrix(a, n, T) + matrix(lam, n, T, byrow = TRUE), 0)
      rhs <- u_b - vapply(seq_len(k), function(p) sum(Xc[[p]][obs] * rest[obs]),
                          numeric(1))
      b <- as.numeric(solve(XtX, rhs))
    }
    if (max(max(abs(a - a0)), max(abs(lam - l0))) < tol) break
  }
  list(a = a, lam = lam, b = b)
}

#' Impute untreated potential outcomes
#'
#' Step 1-2 of Borusyak, Jaravel and Spiess (2024), Theorem 2: fit the
#' two-way fixed-effect model on untreated cells only and use it to
#' impute \eqn{Y_{it}(0)} everywhere.
#'
#' @param Y Balanced outcome matrix, units by periods.
#' @param treated Logical matrix of treated cells.
#' @param X Optional list of covariate matrices, each units by
#'   periods.
#' @param max_iter,tol Solver controls.
#' @return A list with \code{Y0}, \code{alpha} (unit effects),
#'   \code{lambda} (period effects) and \code{beta} (covariate
#'   coefficients, \code{NULL} when \code{X} is absent).
#' @references Borusyak, K., Jaravel, X. and Spiess, J. (2024).
#'   Revisiting event study designs. Review of Economic Studies,
#'   91(6), 3253-3285.
#' @export
#' @examples
#' set.seed(1)
#' Y <- matrix(rnorm(20), 10, 2)
#' treated <- matrix(FALSE, 10, 2)
#' treated[8:10, 2] <- TRUE
#' morie_impute_untreated(Y, treated)
morie_impute_untreated <- function(Y, treated, X = NULL, max_iter = 2000L,
                                   tol = 1e-13) {
  Y <- as.matrix(Y)
  n <- nrow(Y)
  T <- ncol(Y)
  obs <- !treated
  .mor_bjs_check(obs, n, T)
  Xc <- NULL
  if (!is.null(X)) Xc <- if (is.list(X)) X else list(as.matrix(X))
  Yo <- ifelse(obs, Y, 0)
  u_b <- if (is.null(Xc)) NULL else
    vapply(Xc, function(M) sum(M[obs] * Y[obs]), numeric(1))
  s <- .mor_bjs_solve(obs, rowSums(Yo), colSums(Yo), u_b, Xc, max_iter, tol)
  Y0 <- matrix(s$a, n, T) + matrix(s$lam, n, T, byrow = TRUE)
  if (!is.null(s$b)) for (p in seq_along(Xc)) Y0 <- Y0 + s$b[p] * Xc[[p]]
  list(Y0 = Y0, alpha = s$a, lambda = s$lam, beta = s$b)
}

# Exact linear weights v with sum(v * Y) == the estimate, used for the
# unit-clustered variance.
#' Exact linear weights v with sum(v * Y) == the estimate, used for the
#'
#' unit-clustered variance.
#'
#' @param W A matrix; passed to \code{nrow}.
#' @param treated A flag; the body branches on it.
#' @param Xc Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.
#' @param max_iter Passed to \code{.mor_bjs_solve}. Defaults to \code{2000L}.
#' @param tol Passed to \code{.mor_bjs_solve}. Defaults to \code{1e-13}.
#' @return The value of \code{ifelse}.
#' @export
.mor_bjs_weights <- function(W, treated, Xc, max_iter = 2000L, tol = 1e-13) {
  obs <- !treated
  n <- nrow(W)
  T <- ncol(W)
  u_b <- if (is.null(Xc)) NULL else
    vapply(Xc, function(M) sum(M * W), numeric(1))
  s <- .mor_bjs_solve(obs, rowSums(W), colSums(W), u_b, Xc, max_iter, tol)
  proj <- matrix(s$a, n, T) + matrix(s$lam, n, T, byrow = TRUE)
  if (!is.null(s$b)) for (p in seq_along(Xc)) proj <- proj + s$b[p] * Xc[[p]]
  ifelse(treated, W, -proj * obs)
}

#' Borusyak-Jaravel-Spiess event-study imputation estimator
#'
#' The efficient robust estimator of Borusyak, Jaravel and Spiess
#' (2024), Theorem 2.  Unit and period effects are fitted on untreated
#' observations only, untreated potential outcomes are imputed for the
#' treated cells, and the treated residuals are averaged with the
#' requested weights.  Unlike a two-way fixed-effects regression, no
#' already-treated observation contributes to the counterfactual.
#'
#' @param y Outcome, long format.
#' @param D Binary 0/1 absorbing treatment, long format.
#' @param unit,time Panel identifiers.
#' @param X Optional time-varying covariates (vector or matrix, one
#'   column per covariate), long format.
#' @param weights Target weights: \code{NULL} or \code{"equal"}
#'   (default) averages treated cells equally; \code{"cohort"} weights
#'   by cohort size; or supply your own per-observation weights.  All
#'   three routes are available, since the paper's Theorem 2 imputes
#'   identically whatever the estimand.
#' @return A list with \code{estimate}, \code{se}, \code{ci},
#'   \code{tau_it}, \code{imputed_y0}, \code{weights},
#'   \code{linearity_residual}, \code{se_note}, \code{event},
#'   \code{cohort_att}, \code{pretrend_by_rel},
#'   \code{pretrend_max_abs}, \code{pretrend_note},
#'   \code{unit_effects}, \code{period_effects},
#'   \code{covariate_coef}, \code{n_treated_cells},
#'   \code{n_untreated_cells}, \code{n_units}, \code{n_periods},
#'   \code{no_forbidden_comparisons}, \code{method}.
#' @references Borusyak, K., Jaravel, X. and Spiess, J. (2024).
#'   Revisiting event study designs: robust and efficient estimation.
#'   Review of Economic Studies, 91(6), 3253-3285.
#' @export
#' @examples
#' set.seed(1)
#' nu <- 6; T <- 5
#' unit <- rep(1:nu, each = T)
#' time <- rep(1:T, nu)
#' ft <- rep(c(Inf, Inf, Inf, 4, 4, 3), each = T)
#' D <- as.integer(time >= ft)
#' y <- rnorm(nu * T) + D * 1.5
#' morie_boryis(y, D, unit, time)
morie_boryis <- function(y, D, unit, time, X = NULL, weights = NULL) {
  p <- .mor_did_panel(y, unit, time)
  Y <- p$Y
  units <- p$units
  periods <- p$periods
  ft <- .mor_did_first(D, unit, time, units, periods)
  g <- ft$g
  Dm <- ft$Dm
  treated <- Dm > 0
  if (!any(treated)) stop("no observation is treated.")
  Xp <- NULL
  if (!is.null(X)) {
    Xa <- as.matrix(X)
    Xp <- lapply(seq_len(ncol(Xa)),
                 function(j) .mor_did_panel(Xa[, j], unit, time)$Y)
  }
  im <- morie_impute_untreated(Y, treated, Xp)
  Y0 <- im$Y0
  tau <- Y - Y0
  n_u <- nrow(Y)
  T <- ncol(Y)

  if (is.null(weights) || identical(weights, "equal")) {
    W <- treated * 1
  } else if (identical(weights, "cohort")) {
    W <- matrix(0, n_u, T)
    for (gg in sort(unique(g[is.finite(g)]))) {
      rows <- g == gg
      W[rows, ] <- treated[rows, , drop = FALSE] * (sum(rows) / n_u)
    }
    W <- W * treated
  } else {
    Wa <- as.numeric(weights)
    W <- if (length(Wa) == length(as.numeric(y)))
      .mor_did_panel(Wa, unit, time)$Y else matrix(Wa, n_u, T)
    W <- ifelse(treated, W, 0)
    if (sum(W) <= 0)
      stop("the supplied weights put no mass on treated cells.")
  }
  W <- W / sum(W)
  est <- sum(W * tau)

  v <- .mor_bjs_weights(W, treated, Xp)
  e <- ifelse(treated, tau - est, Y - Y0)
  scores <- rowSums(v * e)
  se <- sqrt(n_u / max(n_u - 1, 1) * sum(scores^2))
  linearity_residual <- sum(v * Y) - est

  rel <- matrix(NA_real_, n_u, T)
  fin <- is.finite(g)
  if (any(fin))
    rel[fin, ] <- matrix(seq_len(T) - 1L, sum(fin), T, byrow = TRUE) - g[fin]
  event <- list()
  pre <- list()
  for (r in sort(unique(rel[!is.na(rel)]))) {
    m <- !is.na(rel) & rel == r
    k <- sprintf("%.17g", r)
    if (r >= 0) event[[k]] <- mean(tau[m]) else pre[[k]] <- mean(tau[m])
  }
  cohort_att <- list()
  for (gg in sort(unique(g[fin]))) {
    m <- (g == gg) & treated
    if (any(m)) cohort_att[[sprintf("%.17g", gg)]] <- mean(tau[m])
  }
  z <- 1.959963984540054
  list(estimate = est, se = se, ci = c(est - z * se, est + z * se),
       tau_it = tau, imputed_y0 = Y0, weights = v,
       linearity_residual = linearity_residual,
       se_note = paste("clustered on unit using the estimator's exact linear",
                       "weights; treated cells contribute their deviation",
                       "from the average effect, which is the conservative",
                       "choice"),
       event = event, cohort_att = cohort_att, pretrend_by_rel = pre,
       pretrend_max_abs = if (length(pre)) max(abs(unlist(pre))) else 0,
       pretrend_note = paste("the imputation uses EVERY untreated period, so",
                             "parallel trends is assumed throughout the",
                             "pre-period, not only just before adoption;",
                             "these residuals are zero in expectation under",
                             "that assumption"),
       unit_effects = im$alpha, period_effects = im$lambda,
       covariate_coef = im$beta,
       n_treated_cells = sum(treated), n_untreated_cells = sum(!treated),
       n_units = n_u, n_periods = T,
       no_forbidden_comparisons = paste("no treated observation enters the",
                                        "fit, so an already-treated unit",
                                        "cannot act as a control"),
       method = "Borusyak-Jaravel-Spiess (2024) imputation estimator")
}
