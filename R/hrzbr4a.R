# SPDX-License-Identifier: AGPL-3.0-or-later

#' Maximum-score estimator for a random-coefficients binary response
#'
#' Horowitz (2009), Semiparametric and Nonparametric Methods in
#' Econometrics, Section 4.1, equations (4.2a) to (4.2d) (page 96) and
#' Section 4.3.2, equations (4.20)-(4.21) (page 106).  A
#' random-coefficients model Y* = X'(beta + nu) + V is the general
#' binary-response model with a HETEROSKEDASTIC error U = X'nu + V.
#' Mean independence does not identify beta (Example 4.1, page 98);
#' median independence does (Theorem 4.1).  Under it,
#' median(Y|X=x) = I(x'beta >= 0), so beta maximises
#'
#'   S_ms(b) = (1/n) sum_i (2 Y_i - 1) I(X_i'b >= 0)
#'
#' over abs(b_1) = 1.  The rate is n^(-1/3) and the limit is NOT
#' normal, so standard errors do not give confidence intervals
#' (page 108).
#'
#' The objective is a step function of b, so it is maximised by exact
#' enumeration of the sign patterns the data can produce when there is
#' one free coefficient, and on a fixed grid otherwise.  Nothing is
#' random and nothing exits on a tolerance.
#'
#' @param x Numeric matrix of covariates, n by d; the first column
#'   carries the scale normalisation and should be continuous.
#' @param y Numeric binary 0/1 outcome vector.
#' @param ngrid Integer grid points per free coefficient when d > 2.
#' @param blim Numeric half-width of that grid.
#' @return Named list with estimate, score, ncand, correct, rate,
#'   limit, seusable, n, method.
#' @keywords internal
#' @examples
#' n <- 200
#' x <- cbind(seq(-2, 2, length.out = n), cos(seq_len(n) * 0.8))
#' Binresp(x, as.numeric(as.numeric(x %*% c(1, 0.6)) >= 0))$correct
#' @export
Binresp <- function(x, y, ngrid = 41L, blim = 5) {
  X <- if (is.null(dim(x))) matrix(x, ncol = 1L) else as.matrix(x)
  yv <- as.numeric(y)
  if (nrow(X) != length(yv)) X <- t(X)
  n <- nrow(X)
  d <- ncol(X)
  if (any(!(yv %in% c(0, 1)))) {
    stop("y must be binary 0/1 for a binary-response model.", call. = FALSE)
  }
  if (d < 2L) {
    stop("need at least two covariates for a scale normalisation.",
         call. = FALSE)
  }
  sc <- function(b) sum((2 * yv - 1) * (as.numeric(X %*% b) >= 0)) / n

  if (d == 2L) {
    nz <- abs(X[, 2L]) > 1e-12
    cuts <- sort(-X[nz, 1L] / X[nz, 2L])
    cand <- if (length(cuts)) list(cuts[1L] - 1) else list(0)
    if (length(cuts) > 1L) {
      for (k in seq_len(length(cuts) - 1L)) {
        cand[[length(cand) + 1L]] <- 0.5 * (cuts[k] + cuts[k + 1L])
      }
    }
    if (length(cuts)) cand[[length(cand) + 1L]] <- cuts[length(cuts)] + 1
  } else {
    axis <- seq(-blim, blim, length.out = as.integer(ngrid))
    cand <- list(rep(0, d - 1L))
    for (j in seq_len(d - 1L)) {
      new <- list()
      for (base in cand) {
        for (v in axis) {
          nb <- base
          nb[j] <- v
          new[[length(new) + 1L]] <- nb
        }
      }
      cand <- new
    }
  }

  best <- NULL
  bestval <- -1e300
  for (cc in cand) {
    b <- c(1, as.numeric(cc))
    v <- sc(b)
    if (v > bestval) {
      bestval <- v
      best <- b
    }
  }
  pred <- as.numeric(as.numeric(X %*% best) >= 0)
  list(estimate = best, score = bestval, ncand = length(cand),
       correct = mean(pred == yv), rate = -1 / 3, limit = "nonnormal",
       seusable = FALSE, n = n,
       method = "Horowitz (2009) eq. (4.2), (4.21) maximum score")
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Binresp
#' @keywords internal
#' @export
morie_horowitz_binary_response_model <- Binresp
