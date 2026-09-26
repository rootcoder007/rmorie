# SPDX-License-Identifier: AGPL-3.0-or-later

#' Threshold-specific ordinal-logit primitive (MRM)
#'
#' R parity of \code{morie.mrm_primitives.threshold_specific_ordinal()}.
#' Adapted from O'Connell & Laniyonu (2025) \emph{Race & Justice}
#' 15(3):428--453, where a Bayesian cumulative-logit model is fit with
#' race / gender coefficients allowed to VARY by cumulative threshold.
#' The empirically critical finding -- bias concentrated at the
#' low->medium cutoff but not the medium->high cutoff -- is invisible
#' to standard proportional-odds specifications.
#'
#' This R port is the \strong{frequentist} analogue: for each cutpoint
#' \eqn{k = 1, \ldots, K-1}{k = 1, ..., K-1} a separate binary logit is fit to the
#' indicator \eqn{1\{Y \le k\}}{1\{Y <= k\}}, so the coefficient vector
#' \eqn{\beta_k}{beta_k} is unconstrained across thresholds.  When
#' proportional odds is tested by Brant's (1990) Wald test on those
#' fits.  The threshold-specific fits run via
#' \code{\link[stats]{glm}} with \code{family = binomial("logit")}.
#'
#' Standard threshold (proportional-odds, K levels, p covariates):
#' \deqn{P(Y \le k \mid X) = \mathrm{logit}^{-1}(\alpha_k - X \beta)}{P(Y <= k mid X) =
#' logit^-1(alpha_k - X beta)}
#'
#' Threshold-specific extension (one coefficient vector per cutpoint):
#' \deqn{P(Y \le k \mid X) = \mathrm{logit}^{-1}(\alpha_k - X \beta_k)}{P(Y <= k mid X) =
#' logit^-1(alpha_k - X beta_k)}
#'
#' @references
#' O'Connell, M. & Laniyonu, A. (2025). Threshold-specific
#'   cumulative-logit models for actuarial-risk audit.
#'   \emph{Race & Justice}, 15(3), 428--453.
#' @name mrm_primitives_ordinal
#' @seealso \code{mrm_score_net_residual} (internal helper)
NULL


# NOTE: .mrm_result() and print.morie_mrm_result() are defined in
# mrm_primitives_gentrification.R and are reused (not redefined) here.


#' Internal helper: Tso Logit Ll
#' @noRd
.tso_logit_ll <- function(eta, y) {
  # log-likelihood of a single binary logit with linear predictor `eta`
  # (intercept already folded in).  Uses log1p(exp(-|eta|)) for stability.
  sum(y * eta - ifelse(eta >= 0, eta + log1p(exp(-eta)), log1p(exp(eta))))
}


#' Internal helper: Brant (1990) test of proportional odds
#'
#' Wald test that the slopes of the K - 1 cumulative binary logits
#' 1{Y <= k} are equal, with the cross-threshold covariance
#' Cov(b_k, b_l) = (X'W_k X)^-1 X'W_kl X (X'W_l X)^-1, W_kl = pi_k (1 - pi_l)
#' for k < l (pi = P(Y <= k)).  brant::brant copies each off-diagonal block
#' to its mirror without transposing; here Cov(b_l, b_k) = Cov(b_k, b_l)'.
#' @noRd
.mrm_brant_test <- function(X, y, K) {
  X <- as.matrix(X)
  p <- ncol(X)
  Xi <- cbind(1, X)
  J <- K - 1L
  fits <- lapply(seq_len(J) - 1L, function(k) {
    f <- suppressWarnings(stats::glm.fit(Xi, as.numeric(y <= k),
                                         family = stats::binomial()))
    list(beta = unname(f$coefficients), pi = f$fitted.values)
  })
  inv <- lapply(fits, function(f) solve(crossprod(Xi * (f$pi * (1 - f$pi)), Xi)))
  V <- matrix(0, J * (p + 1L), J * (p + 1L))
  for (k in seq_len(J)) {
    for (l in k:J) {
      blk <- inv[[k]] %*% crossprod(Xi * (fits[[k]]$pi * (1 - fits[[l]]$pi)), Xi) %*%
        inv[[l]]
      ik <- (k - 1L) * (p + 1L) + seq_len(p + 1L)
      il <- (l - 1L) * (p + 1L) + seq_len(p + 1L)
      V[ik, il] <- blk
      V[il, ik] <- t(blk)
    }
  }
  keep <- unlist(lapply(seq_len(J), function(k) (k - 1L) * (p + 1L) + 1L + seq_len(p)))
  bs <- unlist(lapply(fits, function(f) f$beta[-1L]))
  D <- do.call(rbind, lapply(seq_len(J)[-1L], function(k) {
    m <- matrix(0, p, J * p)
    m[, seq_len(p)] <- diag(p)
    m[, (k - 1L) * p + seq_len(p)] <- -diag(p)
    m
  }))
  Db <- D %*% bs
  stat <- as.numeric(crossprod(Db, solve(D %*% V[keep, keep] %*% t(D), Db)))
  df <- (K - 2L) * p
  list(stat = stat, df = as.integer(df),
       p = stats::pchisq(stat, df, lower.tail = FALSE))
}


#' Fit a threshold-specific cumulative-logit ordinal regression
#'
#' For each cumulative cutpoint \eqn{k = 1, \ldots, K-1}{k = 1, ..., K-1}, fits an
#' independent logistic regression of \eqn{1\{Y \le k\}}{1\{Y <= k\}} on the
#' covariates.  Optionally tests proportional odds by Brant's (1990)
#' Wald test that the slopes agree across the K - 1 fits, with their
#' joint covariance.
#'
#' @param data data.frame, one row per unit.
#' @param outcome_col Character; name of the ordinal outcome column.
#'   Either an ordered factor / integer code or a character column
#'   (in which case \code{ordinal_levels} should be passed explicitly).
#' @param covariate_cols Character vector of predictor columns.
#'   Categorical predictors should be one-hot dummied before passing.
#' @param ordinal_levels Optional character vector giving the explicit
#'   ordering of the outcome categories (low-to-high).  If \code{NULL}
#'   and the outcome is a factor, \code{levels()} is used; otherwise
#'   \code{sort(unique())} (rarely what you want -- pass this).
#' @param fit_proportional_odds_first Logical; if \code{TRUE} (default)
#'   run Brant's test of proportional odds.
#' @param max_iter,tol IRLS / GLM control passed to \code{\link[stats]{glm.fit}}.
#' @return An object of class \eqn{c("mrm_threshold_specific_ordinal",
#'   "morie_mrm_result", "list")} with elements
#'   \code{threshold_labels}, \code{covariate_names},
#'   \code{coefficients} (a (K-1) x p matrix), \code{cutpoints},
#'   \code{log_likelihood}, \code{n_obs}, and (if requested)
#'   \code{proportional_odds_stat} (Brant's Wald chi-square),
#'   \code{proportional_odds_df}, \code{proportional_odds_p}.
#' @export
#' @examples
#' set.seed(1)
#' if (FALSE) {
#'   df <- data.frame(
#'     y = sample(c("low", "med", "high"), 200, replace = TRUE),
#'     race = rbinom(200, 1, 0.4),
#'     age  = rnorm(200)
#'   )
#'   mrm_threshold_specific_ordinal(df,
#'     outcome_col = "y",
#'     covariate_cols = c("race", "age"),
#'     ordinal_levels = c("low", "med", "high")
#'   )
#' }
mrm_threshold_specific_ordinal <- function(
  data,
  outcome_col,
  covariate_cols,
  ordinal_levels = NULL,
  fit_proportional_odds_first = TRUE,
  max_iter = 200L,
  tol = 1e-6
) {
  stopifnot(is.data.frame(data),
            is.character(outcome_col), length(outcome_col) == 1L,
            outcome_col %in% names(data),
            is.character(covariate_cols),
            all(covariate_cols %in% names(data)))

  y_raw <- data[[outcome_col]]
  if (is.null(ordinal_levels)) {
    ordinal_levels <- if (is.factor(y_raw)) {
      # Taking the ordinal scale from a factor means trusting its level
      # ORDER. `factor(x, levels = c("low", "med", "high"))` states that
      # order deliberately and is fine unordered. The dangerous case is
      # the DEFAULT: `factor(c("low", "med", "high"))` sorts levels
      # alphabetically to c("high", "low", "med"), and every threshold
      # would silently be computed against the wrong scale. Those two
      # are indistinguishable after the fact, so warn only when the
      # levels are in sorted order -- exactly when the default could
      # have produced them (G2.5).
      .morie_check_factor(y_raw, ordered = NA, arg = outcome_col)
      lv <- levels(y_raw)
      if (!is.ordered(y_raw) && identical(lv, sort(lv))) {
        warning(sprintf(paste0("`%s` is an unordered factor whose levels are ",
                               "in alphabetical order (%s). If that is not ",
                               "the ordinal scale, pass `ordinal_levels` ",
                               "explicitly or use an ordered factor."),
                        outcome_col, paste(lv, collapse = " < ")),
                call. = FALSE)
      }
      lv
    } else {
      sort(unique(stats::na.omit(y_raw)))
    }
  }
  K <- length(ordinal_levels)
  if (K < 3L) {
    stop("threshold-specific ordinal needs >= 3 levels; got ", K)
  }

  y <- match(as.character(y_raw), as.character(ordinal_levels)) - 1L
  if (anyNA(y)) {
    stop("outcome contains values not in ordinal_levels=",
         paste(ordinal_levels, collapse = ", "))
  }

  # Covariates may carry a non-standard class with numeric storage (a
  # `units` column, a haven labelled vector). as.matrix() on those can
  # yield a character matrix and silently turn the fit into nonsense, so
  # coerce each column to plain numeric first (G2.11).
  X <- vapply(covariate_cols,
              function(cc) .morie_coerce_units(data[[cc]], arg = cc),
              numeric(nrow(data)))
  X <- matrix(X, nrow = nrow(data),
              dimnames = list(NULL, covariate_cols))
  storage.mode(X) <- "double"
  n <- nrow(X)
  p <- ncol(X)

  threshold_labels <- vapply(
    seq_len(K - 1L) - 1L,
    function(k) paste0(ordinal_levels[k + 1L], "_vs_",
                       ordinal_levels[k + 2L], "+"),
    character(1)
  )

  coefs <- matrix(0.0, K - 1L, p,
                  dimnames = list(threshold_labels, covariate_cols))
  cutpoints <- numeric(K - 1L)
  total_ll <- 0.0

  for (k in seq_len(K - 1L) - 1L) {
    y_k <- as.integer(y <= k)
    fit <- stats::glm.fit(
      cbind(1.0, X), y_k, family = stats::binomial("logit"),
      control = list(maxit = max_iter, epsilon = tol),
      intercept = FALSE
    )
    cf <- unname(fit$coefficients)
    cutpoints[k + 1L]  <- cf[1L]
    coefs[k + 1L, ]    <- cf[-1L]
    eta_k <- cf[1L] + as.vector(X %*% cf[-1L])
    total_ll <- total_ll + .tso_logit_ll(eta_k, y_k)
  }

  out <- list(
    threshold_labels = threshold_labels,
    covariate_names  = covariate_cols,
    coefficients     = coefs,
    cutpoints        = cutpoints,
    log_likelihood   = total_ll,
    n_obs            = n,
    proportional_odds_stat = NA_real_,
    proportional_odds_df   = NA_integer_,
    proportional_odds_p    = NA_real_
  )

  if (isTRUE(fit_proportional_odds_first)) {
    bt <- .mrm_brant_test(X, y, K)
    out$proportional_odds_stat <- bt$stat
    out$proportional_odds_df   <- bt$df
    out$proportional_odds_p    <- bt$p
  }

  decision <-
    if (is.finite(out$proportional_odds_p) &&
        out$proportional_odds_p < 0.05) "REJECTED" else "not rejected"
  interp <- paste0(
    sprintf("Threshold-specific ordinal logit, K=%d levels, p=%d covariates, n=%d.",
            K, p, n),
    if (is.finite(out$proportional_odds_p))
      sprintf("\
  Brant proportional-odds test: chi2=%.3f on df=%d, p=%.4f (%s at alpha=0.05).",
              out$proportional_odds_stat,
              out$proportional_odds_df,
              out$proportional_odds_p, decision)
    else ""
  )
  out$interpretation <- interp

  class(out) <- c("mrm_threshold_specific_ordinal",
                  "morie_mrm_result", "morie_rich_result", "list")
  out
}


#' Extract coefficient(s) for one covariate across all thresholds
#'
#' Convenience accessor mirroring
#' \code{ThresholdSpecificOrdinalResult.coefficient_by_threshold()}.
#'
#' @param x A result from \code{\link{mrm_threshold_specific_ordinal}}.
#' @param covariate Character, name of one covariate.
#' @return A named numeric vector keyed by threshold label.
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   y = sample(c("low", "med", "high"), 200, replace = TRUE),
#'   race = rbinom(200, 1, 0.4),
#'   age  = rnorm(200)
#' )
#' fit <- mrm_threshold_specific_ordinal(df,
#'   outcome_col = "y",
#'   covariate_cols = c("race", "age"),
#'   ordinal_levels = c("low", "med", "high")
#' )
#' str(mrm_threshold_coefficient(fit, "race"), max.level = 1)
#' @export
mrm_threshold_coefficient <- function(x, covariate) {
  stopifnot(inherits(x, "mrm_threshold_specific_ordinal"),
            is.character(covariate), length(covariate) == 1L,
            covariate %in% x$covariate_names)
  v <- x$coefficients[, covariate]
  names(v) <- x$threshold_labels
  v
}
