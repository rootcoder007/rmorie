# SPDX-License-Identifier: AGPL-3.0-or-later

#' Ridge / LASSO / ElasticNet regularization path (R parity)
#'
#' Wraps \code{glmnet::glmnet}.  Returns the coefficient path across
#' the supplied \code{alphas} (lambda grid in glmnet terminology).
#'
#' @param x Numeric matrix of predictors.
#' @param y Numeric response.
#' @param penalty One of "ridge", "lasso", "elasticnet".
#' @param alphas Lambda grid.  Defaults to a logspace.
#' @param l1_ratio glmnet alpha; only used when penalty = "elasticnet".
#' @return Named list: estimate, coef_path, alphas, penalty, l1_ratio,
#'   n, method.
#' @examples
#' # See the package vignettes for usage examples:
#' #   vignette(package = "rmorie")
#' @export
morie_regularization_path <- function(x, y, penalty = c("ridge", "lasso", "elasticnet"),
                                alphas = NULL, l1_ratio = 0.5) {
  x <- .morie_ensure_design_matrix(x)
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    stop("Function 'morie_regularization_path' requires package 'glmnet'. Install with install.packages('glmnet').")
  }
  penalty <- match.arg(penalty)
  if (is.null(dim(x))) x <- matrix(x, ncol = 1)
  x <- as.matrix(x)
  y <- as.numeric(y)
  n <- nrow(x)
  p <- ncol(x)
  if (is.null(alphas)) alphas <- 10^seq(-3, 2, length.out = 50)
  gn_alpha <- switch(penalty,
    ridge = 0,
    lasso = 1,
    elasticnet = l1_ratio
  )
  # Native path: warm-start coordinate descent from the largest lambda
  # down (glmnet's own pathwise strategy), then report in increasing-
  # lambda order to match `alphas`.
  lam_desc <- sort(alphas, decreasing = TRUE)
  betas <- matrix(0, ncol(x), length(lam_desc))
  a0s <- numeric(length(lam_desc))
  warm <- NULL
  for (li in seq_along(lam_desc)) {
    fit <- .morie_coord_descent(x, y, alpha = gn_alpha,
                                lambda = lam_desc[li], warm = warm)
    betas[, li] <- fit$beta
    a0s[li] <- fit$intercept
    warm <- fit$beta_std
  }
  ord <- order(lam_desc)
  lam <- lam_desc[ord]
  coef_path <- t(rbind(a0s[ord], betas[, ord, drop = FALSE]))
  colnames(coef_path) <- c("(intercept)", colnames(x) %||% paste0("x", seq_len(p) - 1L))
  list(
    estimate   = as.numeric(coef_path[nrow(coef_path), ]),
    coef_path  = coef_path,
    alphas     = lam,
    penalty    = penalty,
    l1_ratio   = if (penalty == "elasticnet") l1_ratio else NA_real_,
    n          = n,
    method     = sprintf("Regularization path (%s)", penalty)
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a
