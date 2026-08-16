# R arm of funmix -- functional clustering: a K-component Gaussian mixture
# on B-spline coefficients fitted by EM. James, G. M. & Sugar, C. A. (2003)
# JASA 98(462), 397-408; de Boor, C. (1978) A Practical Guide to Splines,
# Ch. IX; Ramsay, J. O. & Silverman, B. W. (2005) Functional Data Analysis,
# 2nd ed., Ch. 3.
# Mirrors src/morie/fn/funmix.py.

.funmix_EPS <- 1e-12

.funmix_rows <- function(x) {
  if (is.matrix(x)) {
    m <- x
  } else if (is.data.frame(x)) {
    m <- as.matrix(x)
  } else if (is.list(x)) {
    m <- do.call(rbind, lapply(x, as.numeric))
  } else {
    m <- matrix(as.numeric(x), nrow = 1L)
  }
  storage.mode(m) <- "double"
  m
}

# Clamped uniform knot vector for n_basis B-splines.
.funmix_knots <- function(tmin, tmax, n_basis, degree) {
  n_int <- n_basis - degree - 1L
  if (n_int < 0L) stop("funmix: n_basis must be at least degree + 1")
  inner <- if (n_int > 0L)
    tmin + (tmax - tmin) * (seq_len(n_int)) / (n_int + 1.0) else numeric(0)
  c(rep(tmin, degree + 1L), inner, rep(tmax, degree + 1L))
}

# One row of the B-spline design, by the Cox-de Boor recursion.
.funmix_bspline_row <- function(x, kn, degree, n_basis, tmax) {
  m <- length(kn) - 1L
  N <- numeric(m)
  for (i in seq_len(m)) if (kn[i] <= x && x < kn[i + 1L]) N[i] <- 1.0
  if (x >= tmax) {                      # close the right end
    for (i in seq.int(m, 1L)) if (kn[i] < kn[i + 1L]) { N[i] <- 1.0; break }
  }
  for (d in seq_len(degree)) {
    for (i in seq_len(m - d)) {
      left <- 0.0
      den <- kn[i + d] - kn[i]
      if (den > .funmix_EPS) left <- (x - kn[i]) / den * N[i]
      right <- 0.0
      den <- kn[i + d + 1L] - kn[i + 1L]
      if (den > .funmix_EPS && i + 1L <= m)
        right <- (kn[i + d + 1L] - x) / den * N[i + 1L]
      N[i] <- left + right
    }
  }
  N[seq_len(n_basis)]
}

# First principal direction by power iteration -- deterministic start.
.funmix_first_pc <- function(C, p) {
  n <- nrow(C)
  mean_ <- colMeans(C)
  Z <- sweep(C, 2L, mean_, "-")
  S <- crossprod(Z) / max(n - 1L, 1L)
  v <- rep(1.0 / sqrt(p), p)
  for (i in seq_len(200L)) {
    u <- as.numeric(S %*% v)
    nu <- sqrt(sum(u * u))
    if (nu < 1e-300) break
    u <- u / nu
    if (max(abs(u - v)) < 1e-13) { v <- u; break }
    v <- u
  }
  # sign convention: largest-magnitude entry positive, so the projection --
  # and therefore the initial labelling -- is not sign-arbitrary
  big <- which.max(abs(v))
  if (v[big] < 0.0) v <- -v
  list(score = as.numeric(Z %*% v), pc = v)
}

.funmix_cholsolve <- function(A, b) {
  Lc <- chol(A)
  as.numeric(backsolve(Lc, forwardsolve(t(Lc), b)))
}

#' morie_funmix_functional_mixture
#'
#' Part of the funmix_native implementation; see the file header for the
#' source it follows.
#'
#' @param Y See Usage.
#' @param K See Usage.
#' @param t Defaults to \code{NULL}.
#' @param n_basis Defaults to \code{5L}.
#' @param degree Defaults to \code{3L}.
#' @param max_iter Defaults to \code{300L}.
#' @param tol Defaults to \code{1e-10}.
#' @param var_floor Defaults to \code{1e-08}.
#' @return A list with \code{estimate}, \code{labels}, \code{posterior}, \code{proportions}, \code{coefficients}, \code{variances}, \code{mean_curves}, \code{basis}, \code{knots}, \code{curve_coefficients}, \code{grid}, \code{loglik}, \code{loglik_path}, \code{bic}, \code{aic}, \code{entropy}, \code{n_parameters}, \code{iterations}, \code{converged}, \code{K}, \code{n}, \code{n_basis}, \code{degree}, \code{method}, \code{note}.
#' @export
morie_funmix_functional_mixture <- function(Y, K, t = NULL, n_basis = 5L,
                                            degree = 3L, max_iter = 300L,
                                            tol = 1e-10, var_floor = 1e-8) {
  M <- .funmix_rows(Y)
  n <- nrow(M); m <- ncol(M)
  if (n == 0L) stop("funmix: no curves")
  K <- as.integer(K)
  if (K < 1L) stop("funmix: K must be at least 1")
  if (K > n) stop(sprintf("funmix: %d components for %d curves", K, n))
  tv <- if (is.null(t)) (seq_len(m) - 1.0) / (m - 1.0) else as.numeric(t)
  if (length(tv) != m)
    stop(sprintf("funmix: %d grid points but curves of length %d",
                 length(tv), m))
  p <- as.integer(n_basis); degree <- as.integer(degree)
  if (p > m)
    stop(sprintf(paste0("funmix: %d basis functions for %d time points -- ",
                        "the coefficient fit is not identified"), p, m))

  tmin <- min(tv); tmax <- max(tv)
  if (tmax - tmin <= .funmix_EPS) stop("funmix: the grid has no extent")
  kn <- .funmix_knots(tmin, tmax, p, degree)
  B <- t(vapply(tv, function(x) .funmix_bspline_row(x, kn, degree, p, tmax),
                numeric(p)))
  if (!is.matrix(B)) B <- matrix(B, nrow = m)

  # coefficients per curve: ridge-stabilised least squares on the basis.
  # The ridge is scaled to the matrix -- a fixed 1e-10 does nothing when the
  # entries are themselves small.
  BtB <- crossprod(B)
  scale_ <- sum(diag(BtB)) / p
  diag(BtB) <- diag(BtB) + 1e-8 * scale_
  C <- matrix(0.0, n, p)
  for (i in seq_len(n))
    C[i, ] <- .funmix_cholsolve(BtB, as.numeric(crossprod(B, M[i, ])))

  # deterministic initialisation: cut the first principal score into K equal
  # groups. No random restart, so two implementations start alike.
  pcres <- .funmix_first_pc(C, p)
  ord <- order(pcres$score, seq_len(n))
  lab0 <- integer(n)
  for (rank in seq_len(n))
    # R binds %/% TIGHTER than *, so (rank-1L) * K %/% n would be
    # (rank-1L) * (K %/% n) = 0 and the whole initial partition would
    # collapse into one class. The parentheses are load-bearing.
    lab0[ord[rank]] <- min(((rank - 1L) * K) %/% n, K - 1L)

  grand <- colMeans(C)
  total_var <- sum(vapply(seq_len(p), function(a)
    sum((C[, a] - grand[a]) ^ 2) / n, 0)) / p
  floor_ <- max(var_floor * max(total_var, .funmix_EPS), 1e-300)

  pi_ <- numeric(K)
  mu <- matrix(0.0, K, p)
  sg <- matrix(0.0, K, p)
  for (j in seq_len(K)) {
    idx <- which(lab0 == (j - 1L))
    if (length(idx) == 0L) idx <- ord[min(j, n)]
    pi_[j] <- length(idx) / n
    for (a in seq_len(p)) {
      mu[j, a] <- sum(C[idx, a]) / length(idx)
      v <- sum((C[idx, a] - mu[j, a]) ^ 2) / length(idx)
      sg[j, a] <- max(v, floor_)
    }
  }

  logdens <- function(i, j)
    sum(-0.5 * log(2.0 * pi * sg[j, ]) -
          0.5 * (C[i, ] - mu[j, ]) ^ 2 / sg[j, ])

  path <- numeric(0)
  post <- matrix(0.0, n, K)
  ll <- -Inf; it <- 0L; converged <- FALSE
  for (it in seq_len(as.integer(max_iter))) {
    ll_new <- 0.0
    for (i in seq_len(n)) {
      lp <- vapply(seq_len(K), function(j)
        log(max(pi_[j], 1e-300)) + logdens(i, j), 0)
      mx <- max(lp)
      ssum <- sum(exp(lp - mx))
      ll_new <- ll_new + mx + log(ssum)
      post[i, ] <- exp(lp - mx) / ssum
    }
    path <- c(path, ll_new)
    if (it > 1L && abs(ll_new - ll) <= tol * (abs(ll) + 1.0)) {
      ll <- ll_new; converged <- TRUE; break
    }
    ll <- ll_new
    for (j in seq_len(K)) {
      nk <- sum(post[, j])
      pi_[j] <- nk / n
      nk <- max(nk, 1e-300)
      for (a in seq_len(p)) {
        mu[j, a] <- sum(post[, j] * C[, a]) / nk
        sg[j, a] <- max(sum(post[, j] * (C[, a] - mu[j, a]) ^ 2) / nk, floor_)
      }
    }
  }

  mean_curves <- mu %*% t(B)
  if (!is.matrix(mean_curves)) mean_curves <- matrix(mean_curves, nrow = K)

  # canonical component order: a mixture is identified only up to
  # relabelling, so sort by the integral of the mean curve (trapezoid).
  integral <- function(cv)
    sum(0.5 * (cv[seq_len(m - 1L)] + cv[seq.int(2L, m)]) *
          (tv[seq.int(2L, m)] - tv[seq_len(m - 1L)]))
  ints <- vapply(seq_len(K), function(j) integral(mean_curves[j, ]), 0)
  ordk <- order(ints, seq_len(K))
  pi_ <- pi_[ordk]
  mu <- mu[ordk, , drop = FALSE]
  sg <- sg[ordk, , drop = FALSE]
  mean_curves <- mean_curves[ordk, , drop = FALSE]
  post <- post[, ordk, drop = FALSE]
  # REPORTED labels follow the Python spec and are 0-based
  labels <- apply(post, 1L, which.max) - 1L

  nfree <- K - 1L + K * p + K * p
  bic <- -2.0 * ll + nfree * log(n)
  aic <- -2.0 * ll + 2.0 * nfree
  # entropy of the classification: 0 means every curve is assigned with
  # certainty, and a large value means K is doing no work
  ent <- -sum(post * log(pmax(post, 1e-300)))

  list(estimate = as.numeric(labels), labels = as.numeric(labels),
       posterior = post, proportions = pi_, coefficients = mu,
       variances = sg, mean_curves = mean_curves, basis = B, knots = kn,
       curve_coefficients = C, grid = tv,
       loglik = ll, loglik_path = path, bic = bic, aic = aic,
       entropy = ent, n_parameters = as.integer(nfree),
       iterations = as.integer(it), converged = converged,
       K = as.integer(K), n = as.integer(n), n_basis = as.integer(p),
       degree = as.integer(degree),
       method = paste0("functional clustering: a K-component Gaussian ",
                       "mixture on B-spline coefficients fitted by EM, ",
                       "deterministic principal-score initialisation ",
                       "(James & Sugar 2003)"),
       note = paste0("components are returned sorted by the integral of ",
                     "their mean curve -- a mixture is identified only up ",
                     "to relabelling, and a canonical order is what makes ",
                     "two correct fits comparable"))
}

.funmix_cheatsheet <- function() {
  paste0("funmix: morie_funmix_functional_mixture(Y, K) -> EM clustering of ",
         "curves through a spline basis, canonically ordered components ",
         "(James & Sugar 2003, JASA 98:397-408)")
}

morie_funmix <- morie_funmix_functional_mixture
