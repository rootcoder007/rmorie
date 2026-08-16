# SPDX-License-Identifier: AGPL-3.0-or-later
#
# weight_native.R -- module 15: the propensity-score-weighting family
# (Phase 29.1, the WeightIt replacement).
#
# Routers where the math already exists (.fit_propensity for logistic
# scores, .morie_entropy_balance for Hainmueller weights,
# morie_matching_balance for diagnostics); new native math only for
# CBPS (Imai-Ratkovic GMM) and the NNLS SuperLearner ensemble.
# Cross-validated against WeightIt / CBPS in tests.

#' .morie_weight_result
#'
#' A step of the weight_native implementation. Called by \code{morie_weight_cbps}, \code{morie_weight_entropy}, \code{morie_weight_ow} and 2 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param weights A vector; its length is taken.
#' @param propensity See Usage.
#' @param method See Usage.
#' @param estimand See Usage.
#' @param call See Usage.
#' @param stabilize Defaults to \code{FALSE}.
#' @param trim Defaults to \code{NULL}.
#' @return The value of \code{out}, as built in the body.
#' @export
.morie_weight_result <- function(weights, propensity, method, estimand,
                                 call, stabilize = FALSE, trim = NULL) {
  ess <- sum(weights)^2 / sum(weights^2)
  out <- list(weights = as.numeric(weights),
              propensity = as.numeric(propensity),
              method = method, estimand = estimand,
              stabilize = stabilize, trim = trim,
              ess = ess, n = length(weights), call = call)
  class(out) <- "morie_weight"
  out
}

#' Print method for \code{morie_weight} objects
#'
#' @param x A \code{morie_weight} object.
#' @param ... Ignored; accepted for S3 consistency.
#' @examples
#' \donttest{
#' d <- data.frame(t = rbinom(100, 1, 0.4), x = rnorm(100))
#' obj <- morie_weight_trimming(morie_weight_ps(d, "t", "x"))
#' print(obj)
#' }
#' @export
print.morie_weight <- function(x, ...) {
  cat(sprintf("morie_weight: %s (estimand %s)\n", x$method, x$estimand))
  cat(sprintf("  n = %d  ESS = %.1f  weight range [%.3f, %.3f]\n",
              x$n, x$ess, min(x$weights), max(x$weights)))
  invisible(x)
}

#' .morie_weight_from_ps
#'
#' A step of the weight_native implementation. Called by \code{morie_weight_cbps}, \code{morie_weight_ps}, \code{morie_weight_super}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param ps Numeric; combined arithmetically in the body.
#' @param t01 See Usage.
#' @param estimand See Usage.
#' @return The value of \code{switch}.
#' @export
.morie_weight_from_ps <- function(ps, t01, estimand) {
  switch(estimand,
    ATE = ifelse(t01 == 1, 1 / ps, 1 / (1 - ps)),
    ATT = ifelse(t01 == 1, 1, ps / (1 - ps)),
    ATC = ifelse(t01 == 1, (1 - ps) / ps, 1),
    stop("estimand must be ATE, ATT, or ATC.", call. = FALSE)
  )
}

#' Propensity-score weights (logistic base learner)
#'
#' Logistic propensity estimation via the native fitter with the
#' standard estimand-specific weight transforms; optional
#' stabilization (Austin 2009) and symmetric trimming.
#'
#' @srrstats {G2.1} Inputs validated below.
#' @param data Data frame.
#' @param treatment Binary treatment column (0/1).
#' @param covariates Covariate column names.
#' @param estimand "ATE" (default), "ATT", or "ATC".
#' @param stabilize Multiply by the marginal treatment probability
#'   (stabilized IPW). Default FALSE.
#' @param trim Optional symmetric propensity trim, e.g. 0.01 clips
#'   scores to the interval 0.01 to 0.99.
#' @return A \code{morie_weight} object.
#' @references Austin (2009); Robins, Hernan & Brumback (2000).
#' @examples
#' d <- data.frame(t = rbinom(100, 1, 0.4), x = rnorm(100))
#' morie_weight_ps(d, "t", "x")
#' @export
morie_weight_ps <- function(data, treatment, covariates,
                            estimand = "ATE", stabilize = FALSE,
                            trim = NULL) {
  stopifnot(is.data.frame(data))
  need <- c(treatment, covariates)
  missing_cols <- setdiff(need, names(data))
  if (length(missing_cols)) {
    stop("Columns missing from data: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  t01 <- as.numeric(data[[treatment]])
  ps <- .fit_propensity(data, treatment, covariates)
  if (!is.null(trim)) ps <- pmin(pmax(ps, trim), 1 - trim)
  w <- .morie_weight_from_ps(ps, t01, estimand)
  if (isTRUE(stabilize)) {
    p_t <- mean(t01)
    w <- w * ifelse(t01 == 1, p_t, 1 - p_t)
  }
  .morie_weight_result(w, ps, "logistic propensity", estimand,
                       match.call(), stabilize = stabilize, trim = trim)
}

#' Entropy-balancing weights (Hainmueller 2012)
#'
#' Routes to the shared native entropy-balancing solver
#' (\code{smallstats_native.R}): control units are reweighted so their
#' covariate means match the treated means exactly (ATT).
#'
#' @inheritParams morie_weight_ps
#' @return A \code{morie_weight} object (ATT estimand).
#' @references Hainmueller (2012) Political Analysis 20(1).
#' @examples
#' d <- data.frame(t = rbinom(100, 1, 0.4), x = rnorm(100))
#' morie_weight_entropy(d, "t", "x")
#' @export
morie_weight_entropy <- function(data, treatment, covariates) {
  stopifnot(is.data.frame(data))
  t_mask <- as.numeric(data[[treatment]]) == 1
  X <- as.matrix(data[, covariates, drop = FALSE])
  fit <- .morie_entropy_balance(t_mask, X)
  w <- rep(1, nrow(data))
  w[!t_mask] <- fit$w
  .morie_weight_result(w, rep(NA_real_, nrow(data)),
                       "entropy balancing (Hainmueller 2012)", "ATT",
                       match.call())
}

#' Covariate-balancing propensity score (Imai & Ratkovic 2014)
#'
#' Native just-identified CBPS: solves the covariate-balance moment
#' conditions \eqn{E\[(T - p(X)) X / (p(X)(1-p(X)))\] = 0} directly
#' (the exactly-identified estimator), via Newton iterations on the
#' logistic index. Balance is thus built into the score rather than
#' checked after the fact.
#'
#' @inheritParams morie_weight_ps
#' @param max_iter Maximum damped-Newton iterations. Default 100.
#' @param tol Convergence tolerance on the balance-moment norm.
#'   Default 1e-10.
#' @return A \code{morie_weight} object.
#' @references Imai & Ratkovic (2014) JRSS-B 76(1).
#' @examples
#' d <- data.frame(t = rbinom(120, 1, 0.5), x = rnorm(120))
#' morie_weight_cbps(d, "t", "x")
#' @export
morie_weight_cbps <- function(data, treatment, covariates,
                              estimand = "ATE", max_iter = 100L,
                              tol = 1e-10) {
  stopifnot(is.data.frame(data))
  t01 <- as.numeric(data[[treatment]])
  X <- cbind(1, as.matrix(data[, covariates, drop = FALSE]))
  storage.mode(X) <- "double"
  # Newton on g(beta) = X' [(T - p) / (p(1-p))] * p(1-p) weighting:
  # the just-identified CBPS moment is X'(T - p)/(p(1-p)) = 0; use
  # damped Newton with the analytic Jacobian.
  beta <- stats::glm.fit(X, t01,
                         family = stats::binomial())$coefficients
  for (it in seq_len(max_iter)) {
    eta <- as.numeric(X %*% beta)
    p <- 1 / (1 + exp(-eta))
    p <- pmin(pmax(p, 1e-8), 1 - 1e-8)
    r <- (t01 - p) / (p * (1 - p))
    g <- as.numeric(crossprod(X, r)) / nrow(X)
    if (max(abs(g)) < tol) break
    # d r / d beta = -[1/(p(1-p)) + (T-p)(2p-1)/(p(1-p))^2] p(1-p) x
    dr <- -(1 + r * (2 * p - 1))
    J <- crossprod(X, X * dr) / nrow(X)
    step <- tryCatch(solve(J, g), error = function(e) .morie_ginv(J) %*% g)
    beta_new <- beta - 0.5 * as.numeric(step) # damped
    if (!all(is.finite(beta_new))) break
    beta <- beta_new
  }
  ps <- 1 / (1 + exp(-as.numeric(X %*% beta)))
  ps <- pmin(pmax(ps, 1e-6), 1 - 1e-6)
  w <- .morie_weight_from_ps(ps, t01, estimand)
  .morie_weight_result(w, ps, "CBPS (Imai-Ratkovic 2014, exact)",
                       estimand, match.call())
}

#' Overlap weights (Li, Morgan & Zaslavsky 2018)
#'
#' Weight = 1 - p(X) for treated units and p(X) for controls: targets
#' the subpopulation with the best covariate overlap and is bounded by
#' construction (no extreme weights).
#'
#' @inheritParams morie_weight_ps
#' @return A \code{morie_weight} object (estimand "ATO").
#' @references Li, Morgan & Zaslavsky (2018) JASA 113(521).
#' @examples
#' d <- data.frame(t = rbinom(100, 1, 0.4), x = rnorm(100))
#' morie_weight_ow(d, "t", "x")
#' @export
morie_weight_ow <- function(data, treatment, covariates) {
  stopifnot(is.data.frame(data))
  t01 <- as.numeric(data[[treatment]])
  ps <- .fit_propensity(data, treatment, covariates)
  w <- ifelse(t01 == 1, 1 - ps, ps)
  .morie_weight_result(w, ps, "overlap weights (Li et al. 2018)",
                       "ATO", match.call())
}

#' Stabilized IPW (Austin 2009 / Robins et al. 2000)
#'
#' @inheritParams morie_weight_ps
#' @return A \code{morie_weight} object with stabilized ATE weights.
#' @examples
#' d <- data.frame(t = rbinom(100, 1, 0.4), x = rnorm(100))
#' morie_weight_stabilized(d, "t", "x")
#' @export
morie_weight_stabilized <- function(data, treatment, covariates,
                                    trim = NULL) {
  morie_weight_ps(data, treatment, covariates, estimand = "ATE",
                  stabilize = TRUE, trim = trim)
}

#' Trim extreme weights from a morie_weight object
#'
#' Symmetric percentile trimming (Crump et al. 2009 spirit): weights
#' above the (1-q) quantile are capped there.
#'
#' @param w A \code{morie_weight} object.
#' @param q Upper-quantile cap. Default 0.99.
#' @return The trimmed \code{morie_weight} object.
#' @examples
#' d <- data.frame(t = rbinom(100, 1, 0.4), x = rnorm(100))
#' morie_weight_trimming(morie_weight_ps(d, "t", "x"))
#' @export
morie_weight_trimming <- function(w, q = 0.99) {
  stopifnot(inherits(w, "morie_weight"))
  cap <- stats::quantile(w$weights, q, names = FALSE)
  w$weights <- pmin(w$weights, cap)
  w$trim <- q
  w$ess <- sum(w$weights)^2 / sum(w$weights^2)
  w$method <- paste0(w$method, sprintf(" + trim(q=%.2f)", q))
  w
}

#' SuperLearner-style ensemble propensity weights
#'
#' Native stacking: fits a small library of propensity learners
#' (logistic, logistic-with-interactions, and optionally GBM/ranger
#' when installed), combines their cross-validated predictions by
#' non-negative least squares on the CV log-loss surface (native NNLS
#' via Lawson-Hanson active set), and converts the ensemble scores to
#' estimand weights.
#'
#' @inheritParams morie_weight_ps
#' @param n_folds Cross-validation folds for the stacking weights.
#' @return A \code{morie_weight} object; \code{$learner_weights} gives
#'   the ensemble coefficients.
#' @references van der Laan, Polley & Hubbard (2007).
#' @examples
#' d <- data.frame(t = rbinom(150, 1, 0.5), x1 = rnorm(150), x2 = rnorm(150))
#' morie_weight_super(d, "t", c("x1", "x2"))
#' @export
morie_weight_super <- function(data, treatment, covariates,
                               estimand = "ATE", n_folds = 5L) {
  stopifnot(is.data.frame(data))
  t01 <- as.numeric(data[[treatment]])
  n <- nrow(data)
  X <- as.matrix(data[, covariates, drop = FALSE])

  learners <- list(
    logistic = function(tr, te) {
      f <- stats::glm.fit(cbind(1, X[tr, , drop = FALSE]), t01[tr],
                          family = stats::binomial())
      1 / (1 + exp(-as.numeric(cbind(1, X[te, , drop = FALSE]) %*%
                                 f$coefficients)))
    },
    logistic_sq = function(tr, te) {
      XX <- cbind(X, X^2)
      f <- stats::glm.fit(cbind(1, XX[tr, , drop = FALSE]), t01[tr],
                          family = stats::binomial())
      1 / (1 + exp(-as.numeric(cbind(1, XX[te, , drop = FALSE]) %*%
                                 f$coefficients)))
    }
  )
  if (requireNamespace("ranger", quietly = TRUE)) {
    learners$ranger <- function(tr, te) {
      df_tr <- data.frame(t = factor(t01[tr]), X[tr, , drop = FALSE])
      fit <- ranger::ranger(t ~ ., data = df_tr, probability = TRUE,
                            num.trees = 200L)
      stats::predict(fit,
                     data = data.frame(X[te, , drop = FALSE]))$predictions[, "1"]
    }
  }

  set.seed(1L)
  folds <- sample(rep(seq_len(n_folds), length.out = n))
  Zcv <- matrix(NA_real_, n, length(learners))
  for (f in seq_len(n_folds)) {
    tr <- which(folds != f); te <- which(folds == f)
    for (li in seq_along(learners)) {
      Zcv[te, li] <- tryCatch(learners[[li]](tr, te),
                              error = function(e) rep(mean(t01[tr]),
                                                      length(te)))
    }
  }
  Zcv[!is.finite(Zcv)] <- mean(t01) # a failed learner predicts the base rate
  Zcv <- pmin(pmax(Zcv, 1e-6), 1 - 1e-6)

  # NNLS stacking: minimize ||t - Z a||^2 s.t. a >= 0 (Lawson-Hanson
  # active set), then normalize to sum 1.
  a <- .morie_nnls(Zcv, t01)
  if (sum(a) <= 0) a <- rep(1 / length(a), length(a))
  a <- a / sum(a)

  full_preds <- vapply(seq_along(learners), function(li) {
    tryCatch(learners[[li]](seq_len(n), seq_len(n)),
             error = function(e) rep(mean(t01), n))
  }, numeric(n))
  full_preds[!is.finite(full_preds)] <- mean(t01)
  ps <- pmin(pmax(as.numeric(full_preds %*% a), 1e-6), 1 - 1e-6)
  w <- .morie_weight_from_ps(ps, t01, estimand)
  out <- .morie_weight_result(w, ps, "SuperLearner (native NNLS stack)",
                              estimand, match.call())
  out$learner_weights <- stats::setNames(a, names(learners))
  out
}

# Non-negative least squares via projected coordinate descent --
# convergent for the convex NNLS objective and immune to the singular
# normal-equation solves that break active-set methods when learner
# predictions are collinear.
#' Non-negative least squares via projected coordinate descent --
#'
#' convergent for the convex NNLS objective and immune to the singular
#' normal-equation solves that break active-set methods when learner
#' predictions are collinear.
#'
#' @param A A matrix; indexed by row and column.
#' @param b Numeric; combined arithmetically in the body.
#' @param tol Defaults to \code{1e-10}.
#' @param max_iter A count; the body uses it as \code{seq_len(...)}. Defaults to \code{2000L}.
#' @return The value of \code{x}, as built in the body.
#' @export
.morie_nnls <- function(A, b, tol = 1e-10, max_iter = 2000L) {
  p <- ncol(A)
  x <- rep(0, p)
  g2 <- colSums(A^2)
  g2[g2 == 0] <- 1
  r <- b - A %*% x
  for (it in seq_len(max_iter)) {
    max_change <- 0
    for (j in seq_len(p)) {
      xj_new <- max(0, x[j] + sum(A[, j] * r) / g2[j])
      ch <- xj_new - x[j]
      if (ch != 0) {
        r <- r - A[, j] * ch
        x[j] <- xj_new
        if (abs(ch) > max_change) max_change <- abs(ch)
      }
    }
    if (max_change < tol) break
  }
  x
}

#' Balance diagnostics for a morie_weight object
#'
#' Routes to the native balance engine
#' (\code{\link{morie_matching_balance}}): standardized mean
#' differences and variance ratios before/after weighting, plus the
#' effective sample size.
#'
#' @param w A \code{morie_weight} object.
#' @param data,treatment,covariates The inputs the weights were
#'   built from.
#' @return A data.frame of balance statistics with attribute
#'   \code{ess}.
#' @examples
#' d <- data.frame(t = rbinom(100, 1, 0.4), x = rnorm(100))
#' morie_weight_diagnostic(morie_weight_ps(d, "t", "x"), d, "t", "x")
#' @export
morie_weight_diagnostic <- function(w, data, treatment, covariates) {
  stopifnot(inherits(w, "morie_weight"),
            length(w$weights) == nrow(data))
  # morie_matching_balance takes the weights as a COLUMN NAME.
  data[[".morie_w_"]] <- w$weights
  res <- morie_matching_balance(data, treatment, covariates,
                                weights = ".morie_w_")
  bal <- res$balance_table
  attr(bal, "ess") <- w$ess
  attr(bal, "max_smd") <- res$max_smd
  bal
}
