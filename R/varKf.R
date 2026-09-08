# SPDX-License-Identifier: AGPL-3.0-or-later
#' Sparse variational Gaussian process and its ELBO
#'
#' Titsias (2009), Variational learning of inducing variables in sparse
#' Gaussian processes, AISTATS 5, 567-574, equation (9): log p(y) >= log
#' N(y | 0, Q_nn + sigma^2 I) - tr(K_nn - Q_nn)/(2 sigma^2) with Q_nn =
#' K_nm K_mm^-1 K_mn.  The first term is the DTC marginal likelihood; the
#' second, the Nystrom residual trace, is the penalty that makes the bound
#' a bound -- it is what stops the inducing points being chosen to
#' overfit, and is the whole difference from the earlier projected-process
#' approximations.  The AISTATS volume is free but was not retrievable
#' here; the bound is quoted in its standard published form.  Both terms
#' are returned separately so the penalty is visible.
#'
#' @param X,y training data.
#' @param Z inducing inputs.
#' @param gamma RBF width.
#' @param sigma2 noise variance.
#' @param jitter added to K_mm.
#' @param X_test test inputs.
#' @return list: estimate, elbo, fit_term, trace_penalty, trace, pred,
#'   var, method.
#' @keywords internal
#' @examples
#' Svgp(matrix(c(0, 1, 2, 3), 4, 1), c(0, 1, 0, 1),
#'      matrix(c(0, 3), 2, 1))$elbo
#' @export
Svgp <- function(X, y, Z = NULL, gamma = 1, sigma2 = 1e-2, jitter = 1e-8,
                 X_test = NULL) {
  g <- as.numeric(gamma)
  rbf <- function(x, z) {
    s <- 0
    for (a in seq_along(x)) { d <- x[a] - z[a]
    s <- s + d * d }
    exp(-g * s)
  }
  Xm <- .s03mat(X)
  yv <- .s03vec(y)
  Zm <- if (!is.null(Z)) .s03mat(Z) else Xm
  n <- nrow(Xm)
  m <- nrow(Zm)
  Kmm <- matrix(0, m, m)
  for (i in seq_len(m)) for (j in seq_len(m)) Kmm[i, j] <- rbf(Zm[i, ], Zm[j, ])
  for (i in seq_len(m)) Kmm[i, i] <- Kmm[i, i] + as.numeric(jitter)
  Knm <- matrix(0, n, m)
  for (i in seq_len(n)) for (j in seq_len(m)) Knm[i, j] <- rbf(Xm[i, ], Zm[j, ])
  Q <- matrix(0, n, n)
  trace <- 0
  for (i in seq_len(n)) {
    w <- .s03cholsolve(Kmm, Knm[i, ])
    for (j in seq_len(n)) {
      s <- 0
      for (t in seq_len(m)) s <- s + Knm[j, t] * w[t]
      Q[i, j] <- s
    }
    trace <- trace + 1 - Q[i, i]
  }
  S <- Q
  for (i in seq_len(n)) S[i, i] <- S[i, i] + as.numeric(sigma2)
  L <- .s03chol(S)
  logdet <- 0
  for (i in seq_len(n)) logdet <- logdet + (if (L[i, i] > 0) 2 * log(L[i, i]) else 0)
  sol <- .s03cholsolve(S, yv)
  quad <- 0
  for (i in seq_len(n)) quad <- quad + yv[i] * sol[i]
  fit <- -0.5 * (n * log(2 * pi) + logdet + quad)
  pen <- -0.5 * trace / as.numeric(sigma2)
  Xt <- if (!is.null(X_test)) .s03mat(X_test) else Xm
  pred <- numeric(nrow(Xt))
  var_ <- numeric(nrow(Xt))
  for (t in seq_len(nrow(Xt))) {
    kz <- numeric(m)
    for (j in seq_len(m)) kz[j] <- rbf(Xt[t, ], Zm[j, ])
    w <- .s03cholsolve(Kmm, kz)
    qs <- numeric(n)
    for (i in seq_len(n)) {
      s <- 0
      for (a in seq_len(m)) s <- s + Knm[i, a] * w[a]
      qs[i] <- s
    }
    p <- 0
    for (i in seq_len(n)) p <- p + qs[i] * sol[i]
    pred[t] <- p
    u <- .s03cholsolve(S, qs)
    q <- 0
    for (i in seq_len(n)) q <- q + qs[i] * u[i]
    var_[t] <- 1 - q
  }
  list(estimate = fit + pen, elbo = fit + pen, fit_term = fit,
       trace_penalty = pen, trace = trace, pred = pred, var = var_,
       method = "Titsias (2009) collapsed variational bound, eq. (9)")
}
