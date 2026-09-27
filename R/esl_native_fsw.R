# SPDX-License-Identifier: AGPL-3.0-or-later
#' Incremental forward stagewise regression
#'
#' Moves the coefficient of the predictor most correlated with the residual
#' by eps sign(<x_j, r>) at each step (ESL Alg. 3.4; with basis functions as
#' columns, Alg. 16.1). As eps goes to 0 the path tends to
#' lars(type = "forward.stagewise").
#'
#' @param X Predictors or basis functions, n by p.
#' @param y Response.
#' @param eps Step size.
#' @param max_steps Step limit.
#' @param standardize Scale centred columns to unit norm.
#' @param tol Stop when the largest correlation is below this.
#' @return Named list: coef (original scale), intercept, path, chosen
#'   (1-based), l1_norm, steps.
#' @references Hastie, Tibshirani & Friedman (2009), secs. 3.8.1 and 16.2.
#' @examples
#' i <- 1:40
#' X <- cbind(sin(i), cos(2 * i), log(i))
#' morie_esl_forward_stagewise(X, 1 + 2 * X[, 1] - X[, 2], eps = 0.01, max_steps = 200)$coef
#' @export
morie_esl_forward_stagewise <- function(X, y, eps = 0.01, max_steps = 5000, standardize = TRUE, tol = 1e-12) {
  X <- as.matrix(X)
  y <- as.numeric(y)
  if (length(y) != nrow(X) || eps <= 0) stop("need X and y with matching rows and eps > 0", call. = FALSE)
  xbar <- colMeans(X)
  Xc <- sweep(X, 2, xbar)
  sc <- if (standardize) sqrt(colSums(Xc^2)) else rep(1, ncol(X))
  sc[sc <= 0] <- 1
  Xs <- sweep(Xc, 2, sc, "/")
  r <- y - mean(y)
  beta <- numeric(ncol(X))
  path <- list(beta)
  chosen <- integer(0)
  for (s in seq_len(max_steps)) {
    cc <- drop(crossprod(Xs, r))
    j <- which.max(abs(cc))
    if (abs(cc[j]) < tol) break
    d <- if (cc[j] > 0) eps else -eps
    beta[j] <- beta[j] + d
    r <- r - d * Xs[, j]
    path[[length(path) + 1]] <- beta
    chosen <- c(chosen, j)
  }
  coef <- beta / sc
  list(coef = coef, intercept = mean(y) - sum(coef * xbar), path = do.call(rbind, path), chosen = chosen,
       l1_norm = sum(abs(beta)), steps = length(chosen))
}

#' L1-normalised margin of an additive classifier
#'
#' min_i y_i f(x_i) / sum_k |alpha_k| (ESL eq 16.7).
#'
#' @param y Labels in -1, +1.
#' @param fitted f(x_i).
#' @param coefficients alpha_k.
#' @return Named list: margin, l1_norm, argmin (1-based).
#' @references Hastie, Tibshirani & Friedman (2009), sec. 16.2.2.
#' @examples
#' morie_esl_l1_margin(c(1, -1, 1), c(0.8, -0.5, 0.2), c(0.5, 0.3))$margin
#' @export
morie_esl_l1_margin <- function(y, fitted, coefficients) {
  if (length(y) != length(fitted) || any(!y %in% c(-1, 1))) stop("need labels in {-1, +1} matching the fitted values", call. = FALSE)
  l1 <- sum(abs(coefficients))
  if (l1 == 0) stop("the coefficients are all zero", call. = FALSE)
  m <- y * fitted
  k <- which.min(m)
  list(margin = m[k] / l1, l1_norm = l1, argmin = k)
}
