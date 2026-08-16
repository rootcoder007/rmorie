# R arm of fgam -- the functional generalized additive model
#   E[Y|X] = theta0 + int F(X(t), t) dt
# with F a tensor product of cubic B-splines and separate second-difference
# penalties in the level and time directions.
#
# McLean, M. W., Hooker, G., Staicu, A.-M., Scheipl, F. & Ruppert, D. (2014)
# "Functional generalized additive models", Journal of Computational and
# Graphical Statistics 23(1), 249-269, doi:10.1080/10618600.2012.729985.
#
# Mirrors src/morie/fn/fgam.py, including the Cox-de Boor recursion, so the
# two arms agree basis function by basis function rather than only in the fit.

.fgam_EPS <- 1e-12

.fgam_grid_weights <- function(n_t) {
  if (n_t < 2L) return(1.0)
  h <- 1.0 / (n_t - 1L)
  w <- rep(h, n_t); w[1L] <- 0.5 * h; w[n_t] <- 0.5 * h
  w
}

.fgam_knots <- function(lo, hi, n_basis, degree = 3L) {
  n_int <- n_basis - degree - 1L
  if (n_int < 0L)
    stop(sprintf("fgam: a cubic basis needs at least %d functions",
                 degree + 1L))
  span <- hi - lo
  if (span <= .fgam_EPS) { span <- 1.0; hi <- lo + 1.0 }
  inner <- if (n_int > 0L)
    lo + span * (seq_len(n_int)) / (n_int + 1.0) else numeric(0)
  c(rep(lo, degree + 1L), inner, rep(hi, degree + 1L))
}

.fgam_bspline <- function(x, kn, n_basis, degree = 3L) {
  hi <- kn[length(kn)]
  if (x >= hi) x <- hi - .fgam_EPS
  if (x <= kn[1L]) x <- kn[1L] + .fgam_EPS
  m <- length(kn) - 1L
  B <- numeric(m)
  for (j in seq_len(m))
    if (kn[j] <= x && x < kn[j + 1L]) B[j] <- 1.0
  for (d in seq_len(degree)) {
    for (j in seq_len(length(kn) - d - 1L)) {
      a <- 0.0
      den1 <- kn[j + d] - kn[j]
      if (den1 > .fgam_EPS) a <- a + (x - kn[j]) / den1 * B[j]
      den2 <- kn[j + d + 1L] - kn[j + 1L]
      if (den2 > .fgam_EPS) a <- a + (kn[j + d + 1L] - x) / den2 * B[j + 1L]
      B[j] <- a
    }
  }
  B[seq_len(n_basis)]
}

.fgam_diff_penalty <- function(n, order = 2L) {
  rows <- n - order
  D <- matrix(0.0, max(rows, 0L), n)
  if (rows > 0L) for (i in seq_len(rows)) {
    if (order == 2L) {
      D[i, i] <- 1.0; D[i, i + 1L] <- -2.0; D[i, i + 2L] <- 1.0
    } else {
      D[i, i] <- -1.0; D[i, i + 1L] <- 1.0
    }
  }
  crossprod(D)
}

#' morie_fgam_functional_gam
#'
#' Part of the fgam_native implementation; see the file header for the
#' source it follows.
#'
#' @param X See Usage.
#' @param Y See Usage.
#' @param basis Defaults to \code{NULL}.
#' @param n_x Defaults to \code{6}.
#' @param n_t Defaults to \code{6}.
#' @param lam_x Defaults to \code{1}.
#' @param lam_t Defaults to \code{1}.
#' @return A list with \code{estimate}, \code{fitted}, \code{residuals}, \code{coefficients}, \code{intercept}, \code{surface}, \code{surface_x}, \code{edf}, \code{r_squared}, \code{n_x}, \code{n_t}, \code{lam_x}, \code{lam_t}, \code{linear_deviation}, \code{n}, \code{method}, \code{note}.
#' @export
morie_fgam_functional_gam <- function(X, Y, basis = NULL, n_x = 6, n_t = 6,
                                      lam_x = 1.0, lam_t = 1.0) {
  Xm <- as.matrix(X); storage.mode(Xm) <- "double"
  y <- as.numeric(Y)
  n <- nrow(Xm)
  if (n == 0L) stop("fgam: no curves")
  if (length(y) != n)
    stop(sprintf("fgam: %d curves but %d responses", n, length(y)))
  T <- ncol(Xm)
  if (!is.null(basis)) { n_x <- as.integer(basis); n_t <- as.integer(basis) }
  n_x <- as.integer(n_x); n_t <- as.integer(n_t)
  if (n_x < 4L || n_t < 4L)
    stop("fgam: each cubic marginal basis needs at least 4 functions")
  w <- .fgam_grid_weights(T)
  grid <- if (T > 1L) (seq_len(T) - 1.0) / (T - 1.0) else 0.0

  lo <- min(Xm); hi <- max(Xm)
  kx <- .fgam_knots(lo, hi, n_x)
  kt <- .fgam_knots(0.0, 1.0, n_t)
  Bt <- t(vapply(grid, function(t) .fgam_bspline(t, kt, n_t), numeric(n_t)))

  p <- n_x * n_t
  Z <- matrix(0.0, n, p)
  for (i in seq_len(n)) {
    row <- numeric(p)
    for (t in seq_len(T)) {
      bx <- .fgam_bspline(Xm[i, t], kx, n_x)
      wt <- w[t]
      for (a in seq_len(n_x)) {
        if (abs(bx[a]) <= .fgam_EPS) next
        base <- (a - 1L) * n_t
        for (b in seq_len(n_t))
          row[base + b] <- row[base + b] + wt * bx[a] * Bt[t, b]
      }
    }
    Z[i, ] <- row
  }

  ybar <- sum(y) / n
  yc <- y - ybar
  ZtZ <- crossprod(Z)
  Zty <- as.numeric(crossprod(Z, yc))

  Px <- .fgam_diff_penalty(n_x)
  Pt <- .fgam_diff_penalty(n_t)
  for (a in seq_len(n_x)) for (b in seq_len(n_x)) {
    if (abs(Px[a, b]) <= .fgam_EPS) next
    for (c in seq_len(n_t))
      ZtZ[(a - 1L) * n_t + c, (b - 1L) * n_t + c] <-
        ZtZ[(a - 1L) * n_t + c, (b - 1L) * n_t + c] + lam_x * Px[a, b]
  }
  for (c in seq_len(n_t)) for (d in seq_len(n_t)) {
    if (abs(Pt[c, d]) <= .fgam_EPS) next
    for (a in seq_len(n_x))
      ZtZ[(a - 1L) * n_t + c, (a - 1L) * n_t + d] <-
        ZtZ[(a - 1L) * n_t + c, (a - 1L) * n_t + d] + lam_t * Pt[c, d]
  }
  # Numerical floor scaled to the matrix, matching the Python arm: a fixed
  # absolute ridge is scale-blind and leaves the coefficients unidentified
  # at about 1e-5.
  scale <- sum(diag(ZtZ)) / p
  ridge <- if (scale > .fgam_EPS) 1e-8 * scale else 1e-10
  diag(ZtZ) <- diag(ZtZ) + ridge

  Lc <- chol(ZtZ)
  theta <- as.numeric(backsolve(Lc, forwardsolve(t(Lc), Zty)))
  fitted <- ybar + as.numeric(Z %*% theta)
  resid <- y - fitted

  # tr(H) with H = Z (Z'Z + P)^-1 Z': one solve per row
  edf <- 0.0
  for (i in seq_len(n)) {
    zi <- Z[i, ]
    edf <- edf + sum(zi * as.numeric(backsolve(Lc, forwardsolve(t(Lc), zi))))
  }

  sst <- sum(yc ^ 2); sse <- sum(resid ^ 2)
  r2 <- if (sst > .fgam_EPS) 1.0 - sse / sst else 0.0

  nx_out <- 11L
  xs <- lo + (hi - lo) * (seq_len(nx_out) - 1.0) / (nx_out - 1.0)
  surface <- vector("list", nx_out)
  for (j in seq_len(nx_out)) {
    bx <- .fgam_bspline(xs[j], kx, n_x)
    surface[[j]] <- vapply(seq_len(T), function(t)
      sum(outer(bx, Bt[t, ]) * matrix(theta, n_x, n_t, byrow = TRUE)),
      numeric(1))
  }
  lin <- 0.0
  for (t in seq_len(T)) {
    col <- vapply(seq_len(nx_out), function(j) surface[[j]][t], numeric(1))
    mx <- sum(xs) / nx_out; mc <- sum(col) / nx_out
    den <- sum((xs - mx) ^ 2)
    sl <- if (den > .fgam_EPS) sum((xs - mx) * (col - mc)) / den else 0.0
    lin <- max(lin, max(abs(col - (mc + sl * (xs - mx)))))
  }

  list(
    estimate = fitted, fitted = fitted, residuals = resid,
    coefficients = theta, intercept = ybar,
    surface = surface, surface_x = as.numeric(xs),
    edf = edf, r_squared = r2,
    n_x = as.integer(n_x), n_t = as.integer(n_t),
    lam_x = as.numeric(lam_x), lam_t = as.numeric(lam_t),
    linear_deviation = lin, n = as.integer(n),
    method = paste0("functional generalized additive model, tensor-product ",
                    "cubic B-splines with separate second-difference ",
                    "penalties (McLean et al. 2014)"),
    note = paste0("F(x, t) = beta(t) x recovers the functional linear ",
                  "model; linear_deviation is how far the fitted surface ",
                  "departs from that, so the extra flexibility is measured ",
                  "rather than assumed")
  )
}

.fgam_cheatsheet <- function() {
  paste0("fgam: morie_fgam_functional_gam(X, Y, n_x, n_t, lam_x, lam_t) -> ",
         "E[Y|X] = theta0 + int F(X(t), t) dt by tensor-product penalised ",
         "splines (McLean et al. 2014, JCGS 23(1), 249-269)")
}

morie_fgam <- morie_fgam_functional_gam
