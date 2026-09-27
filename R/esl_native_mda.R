# SPDX-License-Identifier: AGPL-3.0-or-later
#' Mixture discriminant analysis by EM
#'
#' Each class is a Gaussian mixture sum_r pi_kr phi(x; mu_kr, Sigma) with a
#' covariance common to all subclasses; EM alternates within-class subclass
#' responsibilities and a weighted LDA M-step, and Pr(G = k | x) is
#' proportional to Pi_k times the class mixture density (ESL eqs 12.59-12.61).
#'
#' @param X Predictors, N by p.
#' @param g Class labels.
#' @param subclasses Subclasses per class (one value or one per class).
#' @param weights Optional starting responsibilities: a list, one n_k by R_k
#'   matrix per class (as mda.start); by default each class is split into
#'   contiguous blocks after sorting on its first column.
#' @param query Points to classify (default X).
#' @param max_iter,tol EM controls.
#' @return Named list: posterior, prediction, means, mixing, covariance,
#'   loglik, iterations, converged, classes.
#' @references Hastie, T. & Tibshirani, R. (1996). JRSS B 58, 155-176.
#' @examples
#' i <- 1:90
#' X <- cbind(sin(i) + ((i %% 3) == 0) * 1.5, cos(2 * i) + ((i %% 3) == 1) * 1.2)
#' morie_esl_mda(X, i %% 3, 2)$loglik
#' @export
morie_esl_mda <- function(X, g, subclasses = 2, weights = NULL, query = NULL, max_iter = 1000, tol = 1e-12) {
  X <- as.matrix(X)
  cl <- sort(unique(g))
  N <- nrow(X)
  p <- ncol(X)
  K <- length(cl)
  R <- if (length(subclasses) == 1) rep(subclasses, K) else subclasses
  idx <- lapply(cl, function(k) which(g == k))
  if (is.null(weights)) {
    W <- lapply(seq_len(K), function(k) {
      ix <- idx[[k]]
      o <- order(X[ix, 1], ix)
      nk <- length(ix)
      r <- pmin(((seq_len(nk) - 1) * R[k]) %/% nk, R[k] - 1)
      w <- matrix(0, nk, R[k])
      w[cbind(o, r + 1)] <- 1
      w
    })
  } else {
    W <- lapply(weights, as.matrix)
  }
  prior <- lengths(idx) / N
  mstep <- function(W) {
    S <- matrix(0, p, p)
    mu <- vector("list", K)
    pi <- vector("list", K)
    for (k in seq_len(K)) {
      Xk <- X[idx[[k]], , drop = FALSE]
      mu[[k]] <- sweep(crossprod(W[[k]], Xk), 1, colSums(W[[k]]), "/")
      pi[[k]] <- colSums(W[[k]]) / nrow(Xk)
      for (r in seq_len(R[k])) {
        D <- sweep(Xk, 2, mu[[k]][r, ])
        S <- S + crossprod(D * W[[k]][, r], D) / N
      }
    }
    list(mu = mu, pi = pi, S = S)
  }
  logd <- function(x, m, Si, ld) -0.5 * (p * log(2 * pi) + ld + rowSums((sweep(rbind(x), 2, m) %*% Si) * sweep(rbind(x), 2, m)))
  prev <- -Inf
  conv <- FALSE
  it <- 0L
  for (it in seq_len(max_iter)) {
    ms <- mstep(W)
    Si <- solve(ms$S)
    ld <- as.numeric(determinant(ms$S)$modulus)
    ll <- 0
    for (k in seq_len(K)) {
      Xk <- X[idx[[k]], , drop = FALSE]
      lp <- vapply(seq_len(R[k]), function(r) log(ms$pi[[k]][r]) + logd(Xk, ms$mu[[k]][r, ], Si, ld), numeric(nrow(Xk)))
      lp <- matrix(lp, nrow(Xk))
      mx <- apply(lp, 1, max)
      e <- exp(lp - mx)
      W[[k]] <- e / rowSums(e)
      ll <- ll + sum(mx + log(rowSums(e)))
    }
    if (abs(ll - prev) < tol * (1 + abs(ll))) {
      conv <- TRUE
      break
    }
    prev <- ll
  }
  ms <- mstep(W)
  Si <- solve(ms$S)
  ld <- as.numeric(determinant(ms$S)$modulus)
  Q <- if (is.null(query)) X else rbind(query)
  lc <- vapply(seq_len(K), function(k) {
    lp <- vapply(seq_len(R[k]), function(r) log(ms$pi[[k]][r]) + logd(Q, ms$mu[[k]][r, ], Si, ld), numeric(nrow(Q)))
    lp <- matrix(lp, nrow(Q))
    mx <- apply(lp, 1, max)
    log(prior[k]) + mx + log(rowSums(exp(lp - mx)))
  }, numeric(nrow(Q)))
  lc <- matrix(lc, nrow(Q))
  post <- exp(lc - apply(lc, 1, max))
  post <- post / rowSums(post)
  list(posterior = post, prediction = cl[max.col(post, ties.method = "first")], means = ms$mu, mixing = ms$pi,
       covariance = ms$S, loglik = ll, iterations = it, converged = conv, classes = cl)
}
