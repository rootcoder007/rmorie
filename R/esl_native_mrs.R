# SPDX-License-Identifier: AGPL-3.0-or-later
#' Multivariate adaptive regression splines
#'
#' Forward pass adding the reflected hinge pair B_m(x) (x_j - t)_+, B_m(x) (t -
#' x_j)_+ that most reduces the residual sum of squares (knots at observed
#' values, at most degree factors, collinear hinges skipped), then backward
#' deletion keeping the model with the smallest GCV = RSS / (N (1 - M / N)^2), M
#' = r + c (r - 1) / 2 (ESL sec 9.4, eqs 9.19-9.20).
#'
#' @param X Predictors, N by p.
#' @param y Response.
#' @param max_terms Maximum forward terms including the constant.
#' @param degree Maximum interaction order.
#' @param penalty GCV knot cost (default 2 for degree 1, else 3).
#' @param thresh Relative RSS decrease below which the forward pass stops.
#' @param query Points to predict (default X).
#' @return Named list: terms, coefficients, gcv, rss, fitted, prediction,
#'   forward_terms.
#' @references Friedman, J. H. (1991). Annals of Statistics 19, 1-67.
#' @examples
#' i <- 1:60
#' X <- cbind(((7 * i) %% 59) / 59 * 3, ((11 * i) %% 61) / 61 * 2)
#' morie_esl_mars(X, pmax(X[, 1] - 1, 0) + 0.05 * cos(9 * i), max_terms = 5)$terms
#' @export
morie_esl_mars <- function(X, y, max_terms = 11, degree = 1, penalty = NULL, thresh = 1e-8, query = NULL) {
  X <- as.matrix(X)
  y <- as.numeric(y)
  N <- nrow(X)
  p <- ncol(X)
  if (length(y) != N || max_terms < 1 || degree < 1) stop("need matching X and y, max_terms >= 1 and degree >= 1", call. = FALSE)
  cc <- if (is.null(penalty)) (if (degree == 1) 2 else 3) else penalty
  hinge <- function(x, t, s) if (s > 0) pmax(x - t, 0) else pmax(t - x, 0)
  terms <- list(list())
  cols <- list(rep(1, N))
  Q <- matrix(1 / sqrt(N), N, 1)
  e <- y - mean(y)
  rss0 <- sum(e^2)
  tolv <- 1e-9 * sqrt(N)
  ortho <- function(v) v - Q %*% crossprod(Q, v)
  while (length(terms) < max_terms) {
    best <- NULL
    for (m in seq_along(terms)) {
      term <- terms[[m]]
      if (length(term) >= degree) next
      used <- vapply(term, function(z) z$j, numeric(1))
      Bm <- cols[[m]]
      for (j in setdiff(seq_len(p), used)) {
        for (t in sort(unique(X[Bm != 0, j]))) {
          u <- drop(ortho(Bm * pmax(X[, j] - t, 0)))
          v <- drop(ortho(Bm * pmax(t - X[, j], 0)))
          red <- 0
          keep <- numeric(0)
          nu <- sqrt(sum(u^2))
          if (nu > tolv) {
            u <- u / nu
            red <- red + sum(u * e)^2
            keep <- c(keep, 1)
            v <- v - sum(u * v) * u
          }
          nv <- sqrt(sum(v^2))
          if (nv > tolv) {
            v <- v / nv
            red <- red + sum(v * e)^2
            keep <- c(keep, -1)
          }
          if (length(keep) && (is.null(best) || red > best$red + 1e-12)) best <- list(red = red, m = m, j = j, t = t, keep = keep)
        }
      }
    }
    if (is.null(best) || best$red <= thresh * rss0) break
    for (s in best$keep) {
      if (length(terms) >= max_terms) break
      col <- cols[[best$m]] * hinge(X[, best$j], best$t, s)
      q <- drop(ortho(col))
      nq <- sqrt(sum(q^2))
      if (nq <= tolv) next
      q <- q / nq
      e <- e - sum(q * e) * q
      Q <- cbind(Q, q)
      terms[[length(terms) + 1]] <- c(terms[[best$m]], list(list(j = best$j, t = best$t, s = s)))
      cols[[length(cols) + 1]] <- col
    }
  }
  Bmat <- do.call(cbind, cols)
  fitrss <- function(keep) sum(qr.resid(qr(Bmat[, keep, drop = FALSE]), y)^2)
  gcv <- function(r, nt) {
    M <- nt + cc * (nt - 1) / 2
    if (M < N) r / N / (1 - M / N)^2 else Inf
  }
  active <- seq_along(terms)
  best_set <- active
  best_g <- gcv(fitrss(active), length(active))
  while (length(active) > 1) {
    rr <- vapply(active[-1], function(k) fitrss(setdiff(active, k)), numeric(1))
    k <- active[-1][which.min(rr)]
    active <- setdiff(active, k)
    g <- gcv(min(rr), length(active))
    if (g < best_g - 1e-15) {
      best_g <- g
      best_set <- active
    }
  }
  cf <- unname(qr.coef(qr(Bmat[, best_set, drop = FALSE]), y))
  evalt <- function(term, M) {
    v <- rep(1, nrow(M))
    for (z in term) v <- v * hinge(M[, z$j], z$t, z$s)
    v
  }
  Qm <- if (is.null(query)) X else rbind(query)
  pred <- drop(vapply(best_set, function(k) evalt(terms[[k]], Qm), numeric(nrow(Qm))) %*% cf)
  list(terms = terms[best_set], coefficients = cf, gcv = best_g, rss = fitrss(best_set),
       fitted = drop(Bmat[, best_set, drop = FALSE] %*% cf), prediction = pred, forward_terms = terms)
}
