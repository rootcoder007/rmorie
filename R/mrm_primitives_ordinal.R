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
#' \code{MASS} is available we delegate to \code{\link[MASS]{polr}}
#' for the proportional-odds (PO) baseline; otherwise the PO baseline
#' is fit by a stacked-IRLS approximation matching the Python
#' implementation.  The threshold-specific fits always run via
#' \code{\link[stats]{glm}} with \code{family = binomial("logit")}.
#'
#' Standard threshold (proportional-odds, K levels, p covariates):
#' \deqn{P(Y \le k \mid X) = \mathrm{logit}^{-1}(\alpha_k - X \beta)}{P(Y <= k mid X) = logit^-1(alpha_k - X beta)}
#'
#' Threshold-specific extension (one coefficient vector per cutpoint):
#' \deqn{P(Y \le k \mid X) = \mathrm{logit}^{-1}(\alpha_k - X \beta_k)}{P(Y <= k mid X) = logit^-1(alpha_k - X beta_k)}
#'
#' @references
#' O'Connell, M. & Laniyonu, A. (2025). Threshold-specific
#'   cumulative-logit models for actuarial-risk audit.
#'   \emph{Race & Justice}, 15(3), 428--453.
#' @name mrm_primitives_ordinal
#' @seealso \code{\link{mrm_score_net_residual}}
NULL


# NOTE: .mrm_result() and print.morie_mrm_result() are defined in
# mrm_primitives_gentrification.R and are reused (not redefined) here.


.tso_logit_ll <- function(eta, y) {
  # log-likelihood of a single binary logit with linear predictor `eta`
  # (intercept already folded in).  Uses log1p(exp(-|eta|)) for stability.
  sum(y * eta - ifelse(eta >= 0, eta + log1p(exp(-eta)), log1p(exp(eta))))
}


.tso_fit_po_stacked <- function(X, y, K, max_iter, tol) {
  # Fallback proportional-odds fit (no MASS): stack the K-1 cutpoint
  # binary problems and constrain beta to be shared while letting
  # cutpoint intercepts differ.  Mirrors _logit_fit_no_intercept().
  n <- nrow(X)
  p <- ncol(X)
  X_stack <- do.call(rbind, replicate(K - 1L, X, simplify = FALSE))
  y_stack <- unlist(lapply(seq_len(K - 1L) - 1L,
                           function(k) as.integer(y <= k)))
  D <- matrix(0, n * (K - 1L), K - 1L)
  for (k in seq_len(K - 1L)) {
    D[((k - 1L) * n + 1L):(k * n), k] <- 1.0
  }
  Xpo <- cbind(D, X_stack)
  fit <- stats::glm.fit(
    Xpo, y_stack, family = stats::binomial("logit"),
    control = list(maxit = max_iter, epsilon = tol),
    intercept = FALSE
  )
  list(
    intercepts = unname(fit$coefficients[seq_len(K - 1L)]),
    beta = unname(fit$coefficients[(K - 1L + 1L):(K - 1L + p)])
  )
}


#' Fit a threshold-specific cumulative-logit ordinal regression
#'
#' For each cumulative cutpoint \eqn{k = 1, \ldots, K-1}{k = 1, ..., K-1}, fits an
#' independent logistic regression of \eqn{1\{Y \le k\}}{1\{Y <= k\}} on the
#' covariates.  Optionally fits the proportional-odds baseline and
#' returns the likelihood-ratio test of PO vs. threshold-specific.
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
#'   the proportional-odds baseline is fit and an LR test against the
#'   threshold-specific model is reported.
#' @param max_iter,tol IRLS / GLM control passed to \code{\link[stats]{glm.fit}}.
#' @return An object of class \code{c("mrm_threshold_specific_ordinal",
#'   "morie_mrm_result", "list")} with elements
#'   \code{threshold_labels}, \code{covariate_names},
#'   \code{coefficients} (a (K-1) x p matrix), \code{cutpoints},
#'   \code{log_likelihood}, \code{n_obs}, and (if requested)
#'   \code{proportional_odds_lr_stat}, \code{proportional_odds_lr_df},
#'   \code{proportional_odds_p}.
#' @export
#' @examples
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
      levels(y_raw)
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

  X <- as.matrix(data[, covariate_cols, drop = FALSE])
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
    proportional_odds_lr_stat = NA_real_,
    proportional_odds_lr_df   = NA_integer_,
    proportional_odds_p       = NA_real_
  )

  if (isTRUE(fit_proportional_odds_first)) {
    ll_po <- NA_real_
    used_polr <- FALSE
    if (requireNamespace("MASS", quietly = TRUE)) {
      ord_y <- factor(ordinal_levels[y + 1L],
                      levels = ordinal_levels, ordered = TRUE)
      dd <- cbind(data.frame(.y = ord_y),
                  as.data.frame(X, stringsAsFactors = FALSE))
      f  <- stats::as.formula(
        paste(".y ~", paste(covariate_cols, collapse = " + "))
      )
      po_fit <- try(MASS::polr(f, data = dd, method = "logistic",
                               Hess = FALSE), silent = TRUE)
      if (!inherits(po_fit, "try-error")) {
        ll_po       <- as.numeric(stats::logLik(po_fit))
        used_polr   <- TRUE
      }
    }
    if (!used_polr) {
      po <- .tso_fit_po_stacked(X, y, K, max_iter, tol)
      ll_po <- 0.0
      for (k in seq_len(K - 1L) - 1L) {
        eta_k <- po$intercepts[k + 1L] + as.vector(X %*% po$beta)
        ll_po <- ll_po + .tso_logit_ll(eta_k, as.integer(y <= k))
      }
    }
    lr   <- 2.0 * (total_ll - ll_po)
    df_l <- (K - 2L) * p
    out$proportional_odds_lr_stat <- as.numeric(lr)
    out$proportional_odds_lr_df   <- as.integer(df_l)
    out$proportional_odds_p       <-
      if (df_l > 0L && is.finite(lr) && lr > 0)
        stats::pchisq(lr, df = df_l, lower.tail = FALSE)
      else
        1.0
  }

  decision <-
    if (is.finite(out$proportional_odds_p) &&
        out$proportional_odds_p < 0.05) "REJECTED" else "not rejected"
  interp <- paste0(
    sprintf("Threshold-specific ordinal logit, K=%d levels, p=%d covariates, n=%d.",
            K, p, n),
    if (is.finite(out$proportional_odds_p))
      sprintf("\
  Proportional-odds LR test: chi2=%.3f on df=%d, p=%.4f (%s at alpha=0.05).",
              out$proportional_odds_lr_stat,
              out$proportional_odds_lr_df,
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
#' @export
mrm_threshold_coefficient <- function(x, covariate) {
  stopifnot(inherits(x, "mrm_threshold_specific_ordinal"),
            is.character(covariate), length(covariate) == 1L,
            covariate %in% x$covariate_names)
  v <- x$coefficients[, covariate]
  names(v) <- x$threshold_labels
  v
}
