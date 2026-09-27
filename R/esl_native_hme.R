# SPDX-License-Identifier: AGPL-3.0-or-later
.esl9_log_gate <- function(coef, z) {
  eta <- c(drop(coef %*% z), 0)
  m <- max(eta)
  eta - (m + log(sum(exp(eta - m))))
}

.esl9_fit_gate <- function(Z, W, coef, iters = 25) {
  N <- nrow(Z)
  q <- ncol(Z)
  K <- ncol(W)
  b <- as.numeric(t(coef))
  probs <- function(bb) {
    eta <- cbind(Z %*% matrix(bb, q, K - 1), 0)
    eta <- eta - apply(eta, 1, max)
    e <- exp(eta)
    e / rowSums(e)
  }
  obj <- function(P) sum(ifelse(W > 0, W * log(pmax(P, 1e-300)), 0))
  P <- probs(b)
  cur <- obj(P)
  wt <- rowSums(W)
  for (it in seq_len(iters)) {
    grad <- as.numeric(crossprod(Z, W[, seq_len(K - 1), drop = FALSE] - wt * P[, seq_len(K - 1), drop = FALSE]))
    info <- matrix(0, (K - 1) * q, (K - 1) * q)
    for (k in seq_len(K - 1)) for (l in seq_len(K - 1)) {
      info[(k - 1) * q + seq_len(q), (l - 1) * q + seq_len(q)] <- crossprod(Z, Z * (wt * P[, k] * ((k == l) - P[, l])))
    }
    step <- solve(info + 1e-8 * diag(nrow(info)), grad)
    fac <- 1
    while (fac >= 1 / 1024) {
      cand <- b + fac * step
      Pc <- probs(cand)
      oc <- obj(Pc)
      if (oc >= cur - 1e-12) {
        b <- cand
        P <- Pc
        cur <- oc
        break
      }
      fac <- fac / 2
    }
    if (max(abs(fac * step)) < 1e-10) break
  }
  t(matrix(b, q, K - 1))
}

#' Hierarchical mixture of experts fitted by EM
#'
#' Two-level HME (ESL eqs 9.25-9.30): softmax gates g_j(x), g_l|j(x) and
#' Gaussian linear-regression (or logistic) experts; EM alternates branch
#' posteriors with weighted least squares (weighted IRLS) for the experts and
#' ridge-stabilised Newton fits of the gates on the posterior weights (Jordan
#' & Jacobs 1994). An intercept is added; the start assigns softened blocks of
#' the first input to the K^2 experts.
#'
#' @param X Predictors, N by p.
#' @param y Response (0/1 for classification).
#' @param K Branching factor.
#' @param task "regression" or "classification".
#' @param max_iter,tol EM controls.
#' @return Named list: loglik, loglik_path, experts, sigma2, top_gate, sub_gates,
#'   fitted, iterations, converged.
#' @references Jordan, M. I. & Jacobs, R. A. (1994). Neural Computation 6,
#'   181-214.
#' @examples
#' i <- 1:60
#' x <- ((7 * i) %% 59) / 59 * 4 - 2
#' morie_esl_hme(cbind(x), ifelse(x < 0, 2 * x, 0.5 - x), max_iter = 20)$loglik
#' @export
morie_esl_hme <- function(X, y, K = 2, task = c("regression", "classification"), max_iter = 500, tol = 1e-10) {
  task <- match.arg(task)
  X <- as.matrix(X)
  y <- as.numeric(y)
  N <- nrow(X)
  if (length(y) != N || K < 2) stop("need matching X and y and K >= 2", call. = FALSE)
  Z <- cbind(1, X)
  q <- ncol(Z)
  E <- K * K
  o <- order(X[, 1], seq_len(N))
  blk <- pmin(((seq_len(N) - 1) * E) %/% N, E - 1)
  H <- matrix(0.1 / (E - 1), N, E)
  H[cbind(o, blk + 1)] <- 0.9
  top <- matrix(0, K - 1, q)
  sub <- rep(list(matrix(0, K - 1, q)), K)
  beta <- matrix(0, E, q)
  s2 <- rep(1, E)
  wls <- function(w, yy) {
    sw <- sqrt(pmax(w, 0))
    qr.coef(qr(Z * sw), yy * sw)
  }
  wlogit <- function(w, b) {
    for (it in seq_len(50)) {
      eta <- drop(Z %*% b)
      p <- 1 / (1 + exp(-pmin(pmax(eta, -30), 30)))
      v <- pmax(p * (1 - p), 1e-10)
      nb <- wls(w * v, eta + (y - p) / v)
      if (max(abs(nb - b)) < 1e-10) return(nb)
      b <- nb
    }
    b
  }
  logdens <- function(e) {
    m <- drop(Z %*% beta[e, ])
    if (task == "regression") return(-0.5 * (y - m)^2 / s2[e] - 0.5 * log(2 * pi * s2[e]))
    x <- ifelse(y == 1, -m, m)
    -(pmax(x, 0) + log1p(exp(-abs(x))))
  }
  path <- numeric(0)
  prev <- -Inf
  conv <- FALSE
  it <- 0L
  for (it in seq_len(max_iter)) {
    for (e in seq_len(E)) {
      if (task == "regression") {
        beta[e, ] <- wls(H[, e], y)
        s2[e] <- max(sum(H[, e] * (y - drop(Z %*% beta[e, ]))^2) / max(sum(H[, e]), 1e-300), 1e-12)
      } else {
        beta[e, ] <- wlogit(H[, e], beta[e, ])
      }
    }
    hj <- vapply(seq_len(K), function(j) rowSums(H[, (j - 1) * K + seq_len(K), drop = FALSE]), numeric(N))
    top <- .esl9_fit_gate(Z, hj, top)
    for (j in seq_len(K)) sub[[j]] <- .esl9_fit_gate(Z, H[, (j - 1) * K + seq_len(K), drop = FALSE], sub[[j]])
    LD <- vapply(seq_len(E), logdens, numeric(N))
    lj <- matrix(0, N, E)
    for (i in seq_len(N)) {
      gt <- .esl9_log_gate(top, Z[i, ])
      for (j in seq_len(K)) lj[i, (j - 1) * K + seq_len(K)] <- gt[j] + .esl9_log_gate(sub[[j]], Z[i, ]) + LD[i, (j - 1) * K + seq_len(K)]
    }
    m <- apply(lj, 1, max)
    ex <- exp(lj - m)
    H <- ex / rowSums(ex)
    ll <- sum(m + log(rowSums(ex)))
    path <- c(path, ll)
    if (abs(ll - prev) < tol * (1 + abs(ll))) {
      conv <- TRUE
      break
    }
    prev <- ll
  }
  fitted <- vapply(seq_len(N), function(i) {
    gt <- .esl9_log_gate(top, Z[i, ])
    f <- 0
    for (j in seq_len(K)) {
      gs <- .esl9_log_gate(sub[[j]], Z[i, ])
      for (l in seq_len(K)) {
        mm <- sum(beta[(j - 1) * K + l, ] * Z[i, ])
        f <- f + exp(gt[j] + gs[l]) * (if (task == "regression") mm else 1 / (1 + exp(-pmin(pmax(mm, -30), 30))))
      }
    }
    f
  }, numeric(1))
  list(loglik = ll, loglik_path = path, experts = beta, sigma2 = if (task == "regression") s2 else NULL, top_gate = top,
       sub_gates = sub, fitted = fitted, iterations = it, converged = conv)
}
