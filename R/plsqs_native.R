# R arm of plsqs -- PLS1 regression by NIPALS.
# Wold, S., Sjostrom, M. & Eriksson, L. (2001) "PLS-regression: a basic tool
# of chemometrics", Chemometrics and Intelligent Laboratory Systems 58(2),
# 109-130, doi:10.1016/S0169-7439(01)00155-1.
# Mirrors src/morie/fn/plsqs.py.

.plsqs_EPS <- 1e-12

morie_plsqs_pls_regression <- function(X, Y, n_components = 2) {
  Xm <- as.matrix(X); storage.mode(Xm) <- "double"
  y <- as.numeric(Y)
  n <- nrow(Xm); p <- ncol(Xm)
  if (n == 0L) stop("plsqs: no observations")
  if (length(y) != n)
    stop(sprintf("plsqs: %d rows but %d responses", n, length(y)))
  a <- as.integer(n_components)
  if (a < 1L) stop("plsqs: at least one component is required")
  a <- if (n > 1L) min(a, p, n - 1L) else min(a, p)

  xbar <- colSums(Xm) / n; ybar <- sum(y) / n
  E <- sweep(Xm, 2L, xbar, "-"); f <- y - ybar
  ss_x0 <- sum(E ^ 2); ss_y0 <- sum(f ^ 2)

  W <- list(); T <- list(); P <- list(); q <- numeric(0)
  ex_x <- numeric(0); ex_y <- numeric(0)
  for (it in seq_len(a)) {
    w <- as.numeric(crossprod(E, f))
    nw <- sqrt(sum(w ^ 2))
    if (nw <= .plsqs_EPS) break
    w <- w / nw
    t <- as.numeric(E %*% w)
    tt <- sum(t ^ 2)
    if (tt <= .plsqs_EPS) break
    pl <- as.numeric(crossprod(E, t)) / tt
    qj <- sum(t * f) / tt
    ss_x <- sum(outer(t, pl) ^ 2)
    ss_y <- qj * qj * tt
    E <- E - outer(t, pl)
    f <- f - qj * t
    W[[length(W) + 1L]] <- w; T[[length(T) + 1L]] <- t
    P[[length(P) + 1L]] <- pl; q <- c(q, qj)
    ex_x <- c(ex_x, if (ss_x0 > .plsqs_EPS) ss_x / ss_x0 else 0.0)
    ex_y <- c(ex_y, if (ss_y0 > .plsqs_EPS) ss_y / ss_y0 else 0.0)
  }
  a <- length(W)
  if (a == 0L) stop("plsqs: the response has no covariance with X")

  PW <- matrix(0.0, a, a)
  for (r in seq_len(a)) for (c in seq_len(a))
    PW[r, c] <- sum(P[[r]] * W[[c]])
  diag(PW) <- diag(PW) + .plsqs_EPS
  A <- crossprod(PW); b <- as.numeric(crossprod(PW, q))
  Lc <- chol(A)
  z <- as.numeric(backsolve(Lc, forwardsolve(t(Lc), b)))
  beta <- rep(0.0, p)
  for (c in seq_len(a)) beta <- beta + W[[c]] * z[c]
  intercept <- ybar - sum(beta * xbar)
  fitted <- intercept + as.numeric(Xm %*% beta)
  resid <- y - fitted
  sse <- sum(resid ^ 2)
  r2 <- if (ss_y0 > .plsqs_EPS) 1.0 - sse / ss_y0 else 0.0

  list(estimate = beta, coefficients = beta, intercept = intercept,
       fitted = fitted, residuals = resid,
       scores = lapply(seq_len(n), function(i)
         vapply(seq_len(a), function(c) T[[c]][i], numeric(1))),
       weights = W, loadings = P, y_loadings = q,
       explained_x = ex_x, explained_y = ex_y,
       n_components = as.integer(a), r_squared = r2,
       n = as.integer(n), p = as.integer(p),
       method = "PLS1 regression by NIPALS (Wold, Sjostrom & Eriksson 2001)",
       note = paste0("components maximise covariance with y, not variance ",
                     "of X -- that is what separates PLS from principal ",
                     "component regression"))
}

.plsqs_cheatsheet <- function() {
  paste0("plsqs: morie_plsqs_pls_regression(X, Y, n_components) -> NIPALS ",
         "PLS1 (Wold, Sjostrom & Eriksson 2001)")
}

morie_plsqs <- morie_plsqs_pls_regression
