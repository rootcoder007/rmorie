# SPDX-License-Identifier: AGPL-3.0-or-later
.esl17_theta <- function(W, B, diagv) {
  p <- nrow(W)
  Th <- matrix(0, p, p)
  for (j in seq_len(p)) {
    t22 <- 1 / (diagv[j] - sum(W[j, -j] * B[j, -j]))
    Th[j, j] <- t22
    Th[-j, j] <- -B[j, -j] * t22
  }
  (Th + t(Th)) / 2
}

#' Gaussian graphical model with known structure
#'
#' Maximum-likelihood Sigma and Theta = Sigma^-1 with zeros for the missing
#' edges by the modified regression algorithm (ESL Alg. 17.1, eqs 17.11-17.19):
#' each node is regressed on its neighbours using the current W.
#'
#' @param S Empirical covariance matrix.
#' @param adjacency Logical or 0/1 adjacency matrix (diagonal ignored).
#' @param max_iter,tol Convergence controls.
#' @return Named list: Sigma, Theta, iterations, converged.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 17.3.1.
#' @examples
#' S <- matrix(c(10, 1, 5, 4, 1, 10, 2, 6, 5, 2, 10, 3, 4, 6, 3, 10), 4)
#' A <- matrix(c(0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0), 4)
#' round(morie_esl_ggm_fit(S, A)$Sigma, 2)
#' @export
morie_esl_ggm_fit <- function(S, adjacency, max_iter = 1000, tol = 1e-12) {
  S <- as.matrix(S)
  A <- as.matrix(adjacency) != 0
  diag(A) <- FALSE
  p <- nrow(S)
  if (ncol(S) != p || any(A != t(A))) stop("S must be square and adjacency symmetric", call. = FALSE)
  W <- S
  B <- matrix(0, p, p)
  it <- 0L
  converged <- FALSE
  for (it in seq_len(max_iter)) {
    delta <- 0
    for (j in seq_len(p)) {
      nb <- which(A[j, ])
      b <- numeric(p)
      if (length(nb)) b[nb] <- solve(W[nb, nb, drop = FALSE], S[nb, j])
      B[j, ] <- b
      new <- drop(W[-j, -j, drop = FALSE] %*% b[-j])
      delta <- max(delta, abs(new - W[-j, j]))
      W[-j, j] <- new
      W[j, -j] <- new
    }
    if (delta < tol) {
      converged <- TRUE
      break
    }
  }
  list(Sigma = W, Theta = .esl17_theta(W, B, diag(S)), iterations = it, converged = converged)
}

#' Graphical lasso
#'
#' Maximises log det Theta - trace(S Theta) - lambda |Theta|_1 by ESL Alg. 17.2:
#' W starts at S + lambda I and each column solves a modified lasso by the
#' coordinate descent of eq 17.26; the diagonal is penalised as in glasso.
#'
#' @param S Empirical covariance matrix.
#' @param lambda Penalty.
#' @param max_iter,tol,max_inner Convergence controls.
#' @return Named list: Sigma, Theta, iterations, converged.
#' @references Friedman, J., Hastie, T. & Tibshirani, R. (2008). Biostatistics
#'   9, 432-441.
#' @examples
#' i <- 1:60
#' Z <- cbind(sin(i), cos(2 * i) + 0.5 * sin(i), sin(3 * i))
#' round(morie_esl_graphical_lasso(cov(Z), 0.1)$Theta, 3)
#' @export
morie_esl_graphical_lasso <- function(S, lambda, max_iter = 1000, tol = 1e-12, max_inner = 10000) {
  S <- as.matrix(S)
  p <- nrow(S)
  if (ncol(S) != p || lambda < 0) stop("S must be square and lambda >= 0", call. = FALSE)
  W <- S + lambda * diag(p)
  B <- matrix(0, p, p)
  soft <- function(x, t) sign(x) * max(abs(x) - t, 0)
  it <- 0L
  converged <- FALSE
  for (it in seq_len(max_iter)) {
    delta <- 0
    for (j in seq_len(p)) {
      idx <- seq_len(p)[-j]
      b <- B[j, idx]
      for (k in seq_len(max_inner)) {
        dmax <- 0
        for (a in seq_along(idx)) {
          r <- S[idx[a], j] - sum(W[idx[a], idx[-a]] * b[-a])
          new <- soft(r, lambda) / W[idx[a], idx[a]]
          dmax <- max(dmax, abs(new - b[a]))
          b[a] <- new
        }
        if (dmax < tol) break
      }
      B[j, idx] <- b
      new <- drop(W[idx, idx, drop = FALSE] %*% b)
      delta <- max(delta, abs(new - W[idx, j]))
      W[idx, j] <- new
      W[j, idx] <- new
    }
    if (delta < tol) {
      converged <- TRUE
      break
    }
  }
  list(Sigma = W, Theta = .esl17_theta(W, B, diag(W)), iterations = it, converged = converged)
}

#' Regressions and partial correlations implied by a precision matrix
#'
#' beta_jk = -theta_jk / theta_jj, residual variance 1 / theta_jj and partial
#' correlation -theta_jk / sqrt(theta_jj theta_kk) (ESL eqs 17.6-17.9).
#'
#' @param Theta Precision matrix.
#' @return Named list: coefficients, residual_variance, partial_correlation.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 17.3.
#' @examples
#' morie_esl_precision_regression(solve(matrix(c(2, 0.5, 0.5, 1), 2)))$partial_correlation
#' @export
morie_esl_precision_regression <- function(Theta) {
  Th <- as.matrix(Theta)
  if (nrow(Th) != ncol(Th) || any(diag(Th) <= 0)) stop("Theta must be square with a positive diagonal", call. = FALSE)
  B <- -Th / diag(Th)
  diag(B) <- 0
  P <- -Th / sqrt(outer(diag(Th), diag(Th)))
  diag(P) <- 1
  list(coefficients = B, residual_variance = 1 / diag(Th), partial_correlation = P)
}

#' Ising model: exact maximum likelihood
#'
#' p(x) = exp(sum theta_j0 x_j + sum over edges theta_jk x_j x_k - Phi) fitted by
#' Newton with exact moments over the 2^p states (ESL eqs 17.28-17.35); the
#' same fit as the Poisson log-linear model of the 2^p table (eq 17.45).
#'
#' @param X Binary data, N by p (small p).
#' @param edges Two-column matrix of 1-based node pairs.
#' @param max_iter,tol Newton controls.
#' @return Named list: main, edges (pairs with theta), loglik, Phi, iterations,
#'   converged.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 17.4.1.
#' @examples
#' t <- 1:200
#' Xb <- cbind(as.integer(sin(t) > 0), as.integer(sin(t) + cos(3 * t) > 0.2),
#'   as.integer(cos(2 * t) > 0))
#' morie_esl_ising_fit(Xb, rbind(c(1, 2), c(2, 3)))$edges
#' @export
morie_esl_ising_fit <- function(X, edges, max_iter = 100, tol = 1e-12) {
  X <- as.matrix(X)
  E <- t(apply(rbind(edges), 1, sort))
  p <- ncol(X)
  if (any(!X %in% c(0, 1)) || any(E[, 1] == E[, 2]) || any(E < 1 | E > p)) {
    stop("X must be 0/1 and edges must join distinct nodes", call. = FALSE)
  }
  if (p > 16) stop("exact enumeration is limited to p <= 16", call. = FALSE)
  st <- as.matrix(expand.grid(rep(list(0:1), p)))
  tf <- function(M) cbind(M, M[, E[, 1], drop = FALSE] * M[, E[, 2], drop = FALSE])
  Tm <- tf(st)
  tbar <- colMeans(tf(X))
  colnames(Tm) <- NULL
  th <- numeric(ncol(Tm))
  mom <- function(th) {
    e <- drop(Tm %*% th)
    m <- max(e)
    pr <- exp(e - m) / sum(exp(e - m))
    mu <- drop(crossprod(Tm, pr))
    list(mu = mu, cov = crossprod(Tm, Tm * pr) - tcrossprod(mu), phi = m + log(sum(exp(e - m))))
  }
  it <- 0L
  converged <- FALSE
  for (it in seq_len(max_iter)) {
    mm <- mom(th)
    step <- solve(mm$cov, tbar - mm$mu)
    th <- th + step
    if (max(abs(step)) < tol) {
      converged <- TRUE
      break
    }
  }
  mm <- mom(th)
  list(main = th[seq_len(p)], edges = cbind(E, theta = th[-seq_len(p)]), loglik = nrow(X) * (sum(th * tbar) - mm$phi),
       Phi = mm$phi, iterations = it, converged = converged)
}

#' EM for a multivariate normal with missing values
#'
#' Conditional means for the missing coordinates and the covariance
#' correction c_i of eq 17.44 (ESL Ex. 17.9; Little & Rubin 2002), giving the
#' maximum-likelihood mean and covariance; NA marks missing entries.
#'
#' @param X Data matrix with NA for missing values.
#' @param max_iter,tol EM controls.
#' @return Named list: mean, cov, loglik (observed data), iterations,
#'   converged.
#' @references Little, R. J. A. & Rubin, D. B. (2002). Statistical Analysis with
#'   Missing Data, sec. 11.2.
#' @examples
#' i <- 1:60
#' Z <- cbind(sin(i), cos(2 * i) + 0.5 * sin(i))
#' Z[i %% 5 == 0, 2] <- NA
#' morie_esl_mvn_em_missing(Z)$mean
#' @export
morie_esl_mvn_em_missing <- function(X, max_iter = 1000, tol = 1e-12) {
  X <- as.matrix(X)
  n <- nrow(X)
  p <- ncol(X)
  if (any(colSums(!is.na(X)) == 0)) stop("a column is entirely missing", call. = FALSE)
  mu <- colMeans(X, na.rm = TRUE)
  S <- diag(pmax(vapply(seq_len(p), function(j) {
    v <- X[!is.na(X[, j]), j]
    mean((v - mean(v))^2)
  }, numeric(1)), 1e-8), p)
  it <- 0L
  converged <- FALSE
  for (it in seq_len(max_iter)) {
    Xh <- X
    C <- matrix(0, p, p)
    for (i in seq_len(n)) {
      m <- is.na(X[i, ])
      if (!any(m)) next
      o <- !m
      if (any(o)) {
        G <- S[m, o, drop = FALSE] %*% solve(S[o, o, drop = FALSE])
        Xh[i, m] <- mu[m] + drop(G %*% (X[i, o] - mu[o]))
        C[m, m] <- C[m, m] + S[m, m, drop = FALSE] - G %*% S[o, m, drop = FALSE]
      } else {
        Xh[i, m] <- mu[m]
        C[m, m] <- C[m, m] + S[m, m, drop = FALSE]
      }
    }
    nmu <- colMeans(Xh)
    D <- sweep(Xh, 2, nmu)
    nS <- (crossprod(D) + C) / n
    delta <- max(abs(nmu - mu), abs(nS - S))
    mu <- nmu
    S <- nS
    if (delta < tol) {
      converged <- TRUE
      break
    }
  }
  ll <- 0
  for (i in seq_len(n)) {
    o <- !is.na(X[i, ])
    d <- X[i, o] - mu[o]
    So <- S[o, o, drop = FALSE]
    ll <- ll - 0.5 * (sum(o) * log(2 * pi) + as.numeric(determinant(So)$modulus) + drop(d %*% solve(So, d)))
  }
  list(mean = unname(mu), cov = unname(S), loglik = ll, iterations = it, converged = converged)
}
