# SPDX-License-Identifier: AGPL-3.0-or-later
.schab_ols_small <- function(X, y) {
  xtx_inv <- solve(crossprod(X))
  beta <- drop(xtx_inv %*% crossprod(X, y))
  res <- drop(y - X %*% beta)
  df <- nrow(X) - ncol(X)
  if (df < 1L) stop("no residual degrees of freedom", call. = FALSE)
  s2 <- sum(res^2) / df
  list(beta = beta, se = sqrt(s2 * diag(xtx_inv)), sigma2 = s2, df = df)
}

#' Papadakis nearest-neighbour adjustment for field trials
#'
#' Schabenberger & Gotway (2005) Sec. 6.1.3.2: fit the treatment-only model,
#' average the residuals of the East-West and North-South neighbours of each
#' plot (Stroup, Baenziger & Mulitze 1994), and fit Z = beta0 + tau_l + beta1
#' x1 + beta2 x2 by OLS in place of the block effects of the design model
#' (6.10). Edge plots average the neighbours they have; `combined = TRUE`
#' uses one covariate averaging all available neighbours. Non-iterative: the
#' iterated version has no guaranteed fixed point.
#'
#' @param z Numeric responses.
#' @param row,col Integer lattice position of each plot.
#' @param treatment Treatment label of each plot.
#' @param combined Use one combined neighbour covariate.
#' @return Named list: levels, treatment_effects (contrasts with the first
#'   level), se_treatment, beta_neighbour, se_neighbour, intercept, sigma2,
#'   df, adjusted_means, covariates.
#' @references Papadakis, J. S. (1937). Stroup, W. W., Baenziger, P. S. &
#'   Mulitze, D. K. (1994). Crop Science 34, 62-66. Schabenberger & Gotway
#'   (2005), Sec. 6.1.3.2, pp. 318-319.
#' @examples
#' r <- rep(1:4, each = 3)
#' cc <- rep(1:3, 4)
#' papadk(r + 0.1 * cc, r, cc, rep(c("a", "b", "c"), 4))$treatment_effects
#' @export
papadk <- function(z, row, col, treatment, combined = FALSE) {
  z <- as.numeric(z)
  n <- length(z)
  if (length(row) != n || length(col) != n || length(treatment) != n) {
    stop("`z`, `row`, `col` and `treatment` must be the same length", call. = FALSE)
  }
  key <- paste(row, col)
  if (anyDuplicated(key)) stop("each (row, col) position may hold only one plot", call. = FALSE)
  lv <- sort(unique(as.character(treatment)))
  if (length(lv) < 2L) stop("need at least two treatments", call. = FALSE)
  trt <- as.character(treatment)
  resid <- z - ave(z, trt)
  nbr <- function(offs) {
    vapply(seq_len(n), function(i) {
      v <- resid[match(paste(row[i] + offs[, 1L], col[i] + offs[, 2L]), key)]
      v <- v[!is.na(v)]
      if (length(v)) mean(v) else 0
    }, numeric(1))
  }
  groups <- if (combined) list(rbind(c(0, -1), c(0, 1), c(-1, 0), c(1, 0))) else
    list(rbind(c(0, -1), c(0, 1)), rbind(c(-1, 0), c(1, 0)))
  cv <- lapply(groups, nbr)
  X <- cbind(1, vapply(lv[-1L], function(l) as.numeric(trt == l), numeric(n)), do.call(cbind, cv))
  fit <- .schab_ols_small(X, z)
  t <- length(lv) - 1L
  nb <- fit$beta[-seq_len(1L + t)]
  cbar <- vapply(cv, mean, 1)
  list(levels = lv, treatment_effects = unname(fit$beta[1L + seq_len(t)]),
       se_treatment = unname(fit$se[1L + seq_len(t)]), beta_neighbour = unname(nb),
       se_neighbour = unname(fit$se[-seq_len(1L + t)]), intercept = unname(fit$beta[1L]),
       sigma2 = fit$sigma2, df = fit$df,
       adjusted_means = unname(fit$beta[1L] + c(0, fit$beta[1L + seq_len(t)]) + sum(nb * cbar)),
       covariates = cv)
}

#' First-difference model for elongated field layouts
#'
#' Besag & Kempton (1986); Schabenberger & Gotway (2005) eqs (6.12)-(6.13):
#' with first differences within a column homoscedastic and uncorrelated,
#' Delta Z = Delta X tau + Delta e is an OLS model. Differencing removes the
#' intercept, so treatment effects are contrasts with the first level, and
#' sigma2 is the residual sum of squares over n - rank(X).
#'
#' @param z Numeric responses.
#' @param treatment Treatment label of each plot.
#' @param column Optional column of each plot; differences within a column.
#' @param row Optional row of each plot, the order within a column.
#' @return Named list: levels, tau, se, sigma2, df, n_differences.
#' @references Besag, J. & Kempton, R. (1986). Biometrics 42, 231-251.
#'   Schabenberger & Gotway (2005), eqs (6.12)-(6.13), p. 320.
#' @examples
#' fdiffm(c(1, 2.2, 2.9, 4.1, 5.3, 5.8), rep(c("a", "b"), 3))$tau
#' @export
fdiffm <- function(z, treatment, column = NULL, row = NULL) {
  z <- as.numeric(z)
  n <- length(z)
  if (is.null(column)) column <- rep(0, n)
  if (is.null(row)) row <- seq_len(n)
  if (length(treatment) != n || length(column) != n || length(row) != n) {
    stop("`z`, `treatment`, `column` and `row` must be the same length", call. = FALSE)
  }
  lv <- sort(unique(as.character(treatment)))
  if (length(lv) < 2L) stop("need at least two treatments", call. = FALSE)
  d <- vapply(lv[-1L], function(l) as.numeric(as.character(treatment) == l), numeric(n))
  d <- matrix(d, n)
  zs <- numeric(0)
  xs <- NULL
  for (cl in sort(unique(column))) {
    idx <- which(column == cl)
    idx <- idx[order(row[idx])]
    if (length(idx) < 2L) next
    a <- idx[-length(idx)]
    b <- idx[-1L]
    zs <- c(zs, z[a] - z[b])
    xs <- rbind(xs, d[a, , drop = FALSE] - d[b, , drop = FALSE])
  }
  fit <- .schab_ols_small(xs, zs)
  list(levels = lv, tau = unname(fit$beta), se = unname(fit$se), sigma2 = fit$sigma2,
       df = fit$df, n_differences = length(zs))
}
