# Mirror of the 25 sp* Python modules that were still generated stubs.
# Byte-identical between r-package/morie/R and r-morie-oss/R.
#
# Primary source for the spatial statistics: Schabenberger, O. & Gotway,
# C. A. (2005), Statistical Methods for Spatial Data Analysis, Chapman &
# Hall/CRC.  Equations verified in the PDF: (1.4), (1.5), (1.10), (1.14),
# (1.16), (1.17), (3.7), (3.8), (4.13)-(4.15), (4.57), (4.58),
# (6.35)-(6.38), and the rho bound in the prose of Sec. 6.2.2.1 p. 336.
# Methods NOT in that book carry their own primary source in `method`.
#
# Every helper below runs a FIXED number of iterations and fixes
# eigenvector signs, because an early exit or a sign flip on one language
# arm and not the other silently breaks Py<->R parity.

#' .morie_spx_dot
#'
#' Part of the sp_fill implementation; see the file header for the
#' source it follows.
#'
#' @param a See Usage.
#' @param b See Usage.
#' @return The value of \code{.morie_fsum}.
#' @export
.morie_spx_dot <- function(a, b) .morie_fsum(a * b)

#' .morie_spx_matvec
#'
#' Part of the sp_fill implementation; see the file header for the
#' source it follows.
#'
#' @param A See Usage.
#' @param b See Usage.
#' @return A vector, from \code{vapply}.
#' @export
.morie_spx_matvec <- function(A, b) {
  vapply(seq_len(nrow(A)), function(i) .morie_fsum(A[i, ] * b), numeric(1))
}

#' .morie_spx_matmul
#'
#' Part of the sp_fill implementation; see the file header for the
#' source it follows.
#'
#' @param A See Usage.
#' @param B See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
.morie_spx_matmul <- function(A, B) {
  out <- matrix(0, nrow(A), ncol(B))
  for (i in seq_len(nrow(A))) {
    for (j in seq_len(ncol(B))) {
      out[i, j] <- .morie_fsum(A[i, ] * B[, j])
    }
  }
  out
}

#' .morie_spx_trace
#'
#' Part of the sp_fill implementation; see the file header for the
#' source it follows.
#'
#' @param A See Usage.
#' @return The value of \code{.morie_fsum}.
#' @export
.morie_spx_trace <- function(A) .morie_fsum(diag(A))

#' Gauss-Jordan with partial pivoting; raises rather than returning
#'
#' garbage on a singular system.
#'
#' @param A See Usage.
#' @param b See Usage.
#' @return A vector, from \code{vapply}.
#' @export
.morie_spx_solve <- function(A, b) {
  # Gauss-Jordan with partial pivoting; raises rather than returning
  # garbage on a singular system.
  n <- nrow(A)
  if (n != length(b) || ncol(A) != n) {
    stop("linear system is not square or is inconsistent")
  }
  M <- cbind(A, as.numeric(b))
  for (cc in seq_len(n)) {
    p <- cc - 1L + which.max(abs(M[cc:n, cc]))
    if (abs(M[p, cc]) < 1e-300) stop("linear system is singular")
    if (p != cc) {
      tmp <- M[cc, ]
      M[cc, ] <- M[p, ]
      M[p, ] <- tmp
    }
    pv <- M[cc, cc]
    for (r in seq_len(n)) {
      if (r == cc) next
      f <- M[r, cc] / pv
      if (f == 0) next
      k <- cc:(n + 1L)
      M[r, k] <- M[r, k] - f * M[cc, k]
    }
  }
  vapply(seq_len(n), function(i) M[i, n + 1L] / M[i, i], numeric(1))
}

#' (sign, log|det|) by LU with partial pivoting
#'
#' Part of the sp_fill implementation; see the file header for the
#' source it follows.
#'
#' @param A See Usage.
#' @return A vector, from \code{c}.
#' @export
.morie_spx_logabsdet <- function(A) {
  # (sign, log|det|) by LU with partial pivoting.
  n <- nrow(A)
  M <- A
  sgn <- 1
  acc <- 0
  for (cc in seq_len(n)) {
    p <- cc - 1L + which.max(abs(M[cc:n, cc]))
    if (abs(M[p, cc]) < 1e-300) {
      return(c(0, -Inf))
    }
    if (p != cc) {
      tmp <- M[cc, ]
      M[cc, ] <- M[p, ]
      M[p, ] <- tmp
      sgn <- -sgn
    }
    pv <- M[cc, cc]
    if (pv < 0) sgn <- -sgn
    acc <- acc + log(abs(pv))
    if (cc < n) {
      for (r in (cc + 1L):n) {
        f <- M[r, cc] / pv
        if (f == 0) next
        k <- cc:n
        M[r, k] <- M[r, k] - f * M[cc, k]
      }
    }
  }
  c(sgn, acc)
}

#' .morie_spx_lstsq
#'
#' Part of the sp_fill implementation; see the file header for the
#' source it follows.
#'
#' @param A See Usage.
#' @param y See Usage.
#' @param ridge Defaults to \code{0}.
#' @return The value of \code{.morie_spx_solve}.
#' @export
.morie_spx_lstsq <- function(A, y, ridge = 0) {
  G <- .morie_spx_matmul(t(A), A)
  if (ridge) diag(G) <- diag(G) + ridge
  .morie_spx_solve(G, .morie_spx_matvec(t(A), y))
}

#' .morie_spx_fixsign
#'
#' Part of the sp_fill implementation; see the file header for the
#' source it follows.
#'
#' @param v See Usage.
#' @return One of two values, depending on the branch taken.
#' @export
.morie_spx_fixsign <- function(v) {
  j <- which.max(abs(v))
  if (v[j] < 0) -v else v
}

#' Top-k eigenpairs of a SYMMETRIC matrix by power iteration + deflation
#'
#' The start vector is fixed and slightly non-uniform: an all-ones start
#' is orthogonal to the leading eigenvector of some ordinary matrices
#' and fails silently when it is.
#'
#' @param A See Usage.
#' @param k See Usage.
#' @param iters Defaults to \code{400L}.
#' @return A list with \code{values}, \code{vectors}.
#' @export
.morie_spx_topeigs <- function(A, k, iters = 400L) {
  # Top-k eigenpairs of a SYMMETRIC matrix by power iteration + deflation.
  # The start vector is fixed and slightly non-uniform: an all-ones start
  # is orthogonal to the leading eigenvector of some ordinary matrices and
  # fails silently when it is.
  n <- nrow(A)
  if (k < 1L || k > n) stop("k must lie between 1 and the matrix order")
  M <- A
  vals <- numeric(0)
  vecs <- list()
  for (a in seq_len(k)) {
    v <- as.numeric(((seq_len(n) - 1L) %% 7L) + 1L)
    v <- v / sqrt(.morie_spx_dot(v, v))
    for (it in seq_len(iters)) {
      u <- .morie_spx_matvec(M, v)
      s <- sqrt(.morie_spx_dot(u, u))
      if (s < 1e-300) break
      v <- u / s
    }
    lam <- .morie_spx_dot(v, .morie_spx_matvec(M, v))
    v <- .morie_spx_fixsign(v)
    vals <- c(vals, lam)
    vecs[[a]] <- v
    M <- M - lam * outer(v, v)
  }
  list(values = vals, vectors = vecs)
}

#' X_k = sum_u x_u exp(-i w_k u), u and k running from 0
#'
#' Part of the sp_fill implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @return A list with \code{re}, \code{im}.
#' @export
.morie_spx_dft <- function(x) {
  # X_k = sum_u x_u exp(-i w_k u), u and k running from 0.
  n <- length(x)
  idx <- seq_len(n) - 1L
  re <- vapply(idx, function(k) {
    .morie_fsum(x * cos(-2 * pi * k * idx / n))
  }, numeric(1))
  im <- vapply(idx, function(k) {
    .morie_fsum(x * sin(-2 * pi * k * idx / n))
  }, numeric(1))
  list(re = re, im = im)
}

#' .morie_spx_idftre
#'
#' Part of the sp_fill implementation; see the file header for the
#' source it follows.
#'
#' @param re See Usage.
#' @param im See Usage.
#' @return A vector, from \code{vapply}.
#' @export
.morie_spx_idftre <- function(re, im) {
  n <- length(re)
  idx <- seq_len(n) - 1L
  vapply(idx, function(i) {
    ang <- 2 * pi * i * idx / n
    .morie_fsum(re * cos(ang) - im * sin(ang)) / n
  }, numeric(1))
}

#' .morie_spx_median
#'
#' Part of the sp_fill implementation; see the file header for the
#' source it follows.
#'
#' @param v See Usage.
#' @return One of two values, depending on the branch taken.
#' @export
.morie_spx_median <- function(v) {
  s <- sort(v)
  n <- length(s)
  if (!n) stop("the median of an empty vector is undefined")
  h <- n %/% 2L
  if (n %% 2L == 1L) s[h + 1L] else 0.5 * (s[h] + s[h + 1L])
}

#' .morie_spx_dist
#'
#' Part of the sp_fill implementation; see the file header for the
#' source it follows.
#'
#' @param a See Usage.
#' @param b See Usage.
#' @return A numeric value.
#' @export
.morie_spx_dist <- function(a, b) sqrt(.morie_fsum((a - b)^2))

#' .morie_spx_p2
#'
#' Part of the sp_fill implementation; see the file header for the
#' source it follows.
#'
#' @param z See Usage.
#' @return A numeric value.
#' @export
.morie_spx_p2 <- function(z) 2 * (1 - pnorm(abs(z)))

#' .morie_spx_chkw
#'
#' Part of the sp_fill implementation; see the file header for the
#' source it follows.
#'
#' @param w See Usage.
#' @param n See Usage.
#' @param zero_diag Defaults to \code{TRUE}.
#' @return The value of \code{W}, as built in the body.
#' @export
.morie_spx_chkw <- function(w, n, zero_diag = TRUE) {
  W <- as.matrix(w)
  if (nrow(W) != ncol(W)) stop("`w` must be square")
  if (!is.null(n) && nrow(W) != n) {
    stop(sprintf("`w` must be %d by %d", n, n))
  }
  if (any(!is.finite(W))) stop("`w` must be finite")
  if (zero_diag && any(diag(W) != 0)) {
    stop("`w` must have a zero diagonal; a site is not its own neighbour")
  }
  W
}

#' .morie_spx_chkv
#'
#' Part of the sp_fill implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param name Defaults to \code{"x"}.
#' @return The value of \code{v}, as built in the body.
#' @export
.morie_spx_chkv <- function(x, name = "x") {
  v <- as.numeric(x)
  if (!length(v)) stop(sprintf("`%s` must contain at least one value", name))
  if (any(!is.finite(v))) stop(sprintf("`%s` must be finite", name))
  v
}

# --- Ch 1: autocorrelation, Mantel, Moran, LISA -----------------------------

#' Empirical correlogram R(h) = C(h)/C(0), Schabenberger & Gotway (2005)
#'
#' Sec. 1.4.2 and Chapter problem 1.14.  C(0) uses the 1/n divisor.
#'
#' @param coords See Usage.
#' @param z See Usage.
#' @param bins Defaults to \code{NULL}.
#' @param cutoff Defaults to \code{NULL}.
#' @return A list with \code{lags}, \code{centres}, \code{cov}, \code{acf}, \code{c0}, \code{npairs}, \code{n}, \code{incomplete_description_of_second_order_structure}, \code{method}.
#' @export
SpAcf <- function(coords, z, bins = NULL, cutoff = NULL) {
  # Empirical correlogram R(h) = C(h)/C(0), Schabenberger & Gotway (2005)
  # Sec. 1.4.2 and Chapter problem 1.14.  C(0) uses the 1/n divisor.
  zz <- .morie_spx_chkv(z, "z")
  cc <- as.matrix(coords)
  n <- length(zz)
  if (nrow(cc) != n) {
    stop(sprintf("`coords` has %d rows but `z` has %d values", nrow(cc), n))
  }
  if (n < 3L) stop("at least 3 sites are needed for a lag class")
  d <- zz - .morie_fsum(zz) / n
  c0 <- .morie_fsum(d * d) / n
  if (c0 <= 0) stop("`z` is constant; C(0) is zero and R(h) undefined")
  h <- numeric(0)
  prod <- numeric(0)
  for (i in seq_len(n - 1L)) {
    for (j in (i + 1L):n) {
      h <- c(h, .morie_spx_dist(cc[i, ], cc[j, ]))
      prod <- c(prod, d[i] * d[j])
    }
  }
  hmax <- max(h)
  if (!is.null(cutoff)) {
    hmax <- as.numeric(cutoff)
    if (hmax <= 0) stop("`cutoff` must be positive")
  }
  if (is.null(bins)) {
    edges <- hmax * seq_len(10L) / 10
  } else if (length(bins) == 1L) {
    nb <- as.integer(bins)
    if (nb < 1L) stop("`bins` must be at least 1")
    edges <- hmax * seq_len(nb) / nb
  } else {
    edges <- as.numeric(bins)
    if (any(diff(edges) <= 0)) stop("`bins` edges must increase")
  }
  lo <- 0
  cov <- numeric(0)
  acf <- numeric(0)
  npairs <- numeric(0)
  centres <- numeric(0)
  for (e in edges) {
    keep <- which(h > lo & h <= e)
    npairs <- c(npairs, length(keep))
    centres <- c(centres, 0.5 * (lo + e))
    if (length(keep)) {
      cv <- .morie_fsum(prod[keep]) / length(keep)
      cov <- c(cov, cv)
      acf <- c(acf, cv / c0)
    } else {
      cov <- c(cov, NaN)
      acf <- c(acf, NaN)
    }
    lo <- e
  }
  list(
    lags = edges, centres = centres, cov = cov, acf = acf, c0 = c0,
    npairs = npairs, n = n,
    incomplete_description_of_second_order_structure = TRUE,
    method = paste(
      "Empirical correlogram R(h)=C(h)/C(0);",
      "Schabenberger & Gotway (2005) Sec. 1.4.2 and",
      "Chapter problem 1.14"
    )
  )
}

#' Eq (1.17), Sec. 1.3.3.  sum_i I(s_i) = w.. I is checked, not assumed:
#'
#' a non-zero gap means the weights or the scaling are wrong.
#'
#' @param x See Usage.
#' @param w See Usage.
#' @return A list with \code{local}, \code{expectation}, \code{lagged}, \code{global_i}, \code{s0}, \code{sum_identity_gap}, \code{n}, \code{method}.
#' @export
LisaI <- function(x, w) {
  # eq (1.17), Sec. 1.3.3.  sum_i I(s_i) = w.. I is checked, not assumed:
  # a non-zero gap means the weights or the scaling are wrong.
  z <- .morie_spx_chkv(x, "x")
  n <- length(z)
  if (n < 3L) stop("at least 3 sites are needed")
  W <- .morie_spx_chkw(w, n)
  m <- .morie_fsum(z) / n
  d <- z - m
  ss <- .morie_fsum(d * d)
  if (ss <= 0) stop("`x` is constant; local Moran's I is undefined")
  s0 <- .morie_fsum(as.numeric(W))
  if (s0 <= 0) stop("total weight w.. must be positive")
  lagged <- vapply(seq_len(n), function(i) .morie_fsum(W[i, ] * d), numeric(1))
  local <- n * d * lagged / ss
  expect <- vapply(
    seq_len(n), function(i) -.morie_fsum(W[i, ]) / (n - 1),
    numeric(1)
  )
  gi <- n * .morie_fsum(as.numeric(W) * as.numeric(outer(d, d))) / (s0 * ss)
  list(
    local = local, expectation = expect, lagged = lagged, global_i = gi,
    s0 = s0, sum_identity_gap = .morie_fsum(local) - s0 * gi, n = n,
    method = paste(
      "Local Moran's I, Schabenberger & Gotway (2005)",
      "eq (1.17), Sec. 1.3.3; after Anselin (1995)"
    )
  )
}

#' Eqs (1.4) and (1.5) with the book\'s own default choices,
#'
#' W_ij = ||s_i - s_j|| and U_ij = |Z_i - Z_j|, plus the regression
#' slope beta = M2 / sum sum W_ij^2 displayed in Sec. 1.3.1.
#'
#' @param coords See Usage.
#' @param x See Usage.
#' @param w Defaults to \code{NULL}.
#' @param u Defaults to \code{NULL}.
#' @return A list with \code{m1}, \code{m2}, \code{beta}, \code{sw2}, \code{s0}, \code{mean_attribute}, \code{n}, \code{method}.
#' @export
MantelM2 <- function(coords, x, w = NULL, u = NULL) {
  # eqs (1.4) and (1.5) with the book's own default choices,
  # W_ij = ||s_i - s_j|| and U_ij = |Z_i - Z_j|, plus the regression slope
  # beta = M2 / sum sum W_ij^2 displayed in Sec. 1.3.1.
  z <- .morie_spx_chkv(x, "x")
  n <- length(z)
  if (n < 2L) stop("at least 2 sites are needed for a pair")
  if (is.null(w)) {
    cc <- as.matrix(coords)
    if (nrow(cc) != n) {
      stop(sprintf("`coords` has %d rows but `x` has %d values", nrow(cc), n))
    }
    W <- matrix(0, n, n)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (i != j) W[i, j] <- .morie_spx_dist(cc[i, ], cc[j, ])
      }
    }
  } else {
    W <- .morie_spx_chkw(w, n)
  }
  if (is.null(u)) {
    U <- abs(outer(z, z, "-"))
    diag(U) <- 0
  } else {
    U <- .morie_spx_chkw(u, n)
  }
  m1 <- .morie_fsum(as.numeric(W[upper.tri(W)] * U[upper.tri(U)]))
  m2 <- .morie_fsum(as.numeric(W) * as.numeric(U))
  sw2 <- .morie_fsum(as.numeric(W) * as.numeric(W))
  if (sw2 <= 0) stop("all spatial proximities are zero; beta undefined")
  list(
    m1 = m1, m2 = m2, beta = m2 / sw2, sw2 = sw2,
    s0 = .morie_fsum(as.numeric(W)),
    mean_attribute = .morie_fsum(z) / n, n = n,
    method = paste(
      "Mantel statistics M1 and M2, Schabenberger & Gotway",
      "(2005) eqs (1.4)-(1.5), Sec. 1.3.1; Mantel (1967)"
    )
  )
}

#' Gaussian Z-test of Sec. 1.3.1 with U of eq (1.10).  The book states
#'
#' the approach but does not print Eg[M2] or Varg[M2]; both are derived
#' from the quadratic-form moments and are stated in the Python
#' docstring.  Only the SYMMETRIC part of W contributes.
#'
#' @param coords See Usage.
#' @param x See Usage.
#' @param w See Usage.
#' @param u Defaults to \code{NULL}.
#' @return A list with \code{m2}, \code{expectation}, \code{variance}, \code{z}, \code{p_value}, \code{sigma2}, \code{n}, \code{gaussian_moments_apply}, \code{method}.
#' @export
MantelZ <- function(coords, x, w, u = NULL) {
  # Gaussian Z-test of Sec. 1.3.1 with U of eq (1.10).  The book states
  # the approach but does not print Eg[M2] or Varg[M2]; both are derived
  # from the quadratic-form moments and are stated in the Python
  # docstring.  Only the SYMMETRIC part of W contributes.
  z <- .morie_spx_chkv(x, "x")
  n <- length(z)
  if (n < 3L) stop("at least 3 sites are needed")
  W <- .morie_spx_chkw(w, n)
  m <- .morie_fsum(z) / n
  d <- z - m
  ss <- .morie_fsum(d * d)
  if (ss <= 0) stop("`x` is constant; the Mantel statistic is degenerate")
  if (!is.null(u)) {
    U <- .morie_spx_chkw(u, n)
    return(list(
      m2 = .morie_fsum(as.numeric(W) * as.numeric(U)),
      expectation = NA_real_, variance = NA_real_,
      z = NA_real_, p_value = NA_real_,
      sigma2 = ss / (n - 1), n = n,
      gaussian_moments_apply = FALSE,
      method = paste(
        "Mantel M2 with a user-supplied U; the",
        "Gaussian Z-test of Schabenberger & Gotway",
        "Sec. 1.3.1 needs U of eq (1.10) and is not",
        "reported"
      )
    ))
  }
  m2 <- .morie_fsum(as.numeric(W) * as.numeric(outer(d, d)))
  A <- 0.5 * (W + t(W))
  P <- diag(n) - matrix(1 / n, n, n)
  AM <- .morie_spx_matmul(A, P)
  s2 <- ss / (n - 1)
  ex <- s2 * .morie_spx_trace(AM)
  vr <- 2 * s2 * s2 * .morie_spx_trace(.morie_spx_matmul(AM, AM))
  if (vr <= 0) {
    stop(paste(
      "the null variance of M2 is not positive;",
      "the weight matrix carries no information"
    ))
  }
  zz <- (m2 - ex) / sqrt(vr)
  list(
    m2 = m2, expectation = ex, variance = vr, z = zz,
    p_value = .morie_spx_p2(zz), sigma2 = s2, n = n,
    gaussian_moments_apply = TRUE,
    method = paste(
      "Standardized Mantel z_M, Gaussian Z-test of",
      "Schabenberger & Gotway (2005) Sec. 1.3.1 with U of",
      "eq (1.10); the moments are derived, the book states",
      "only the approach"
    )
  )
}

#' Eq (1.16).  Eg[Ires] = n tr[MW] / {(n-k) w..} is the book\'s own
#'
#' formula, Sec. 1.3.2, and is reproduced term for term.  The variance
#' is derived from the exact moments of a ratio of quadratic forms in
#' the same Gaussian projection.
#'
#' @param residuals See Usage.
#' @param w See Usage.
#' @param x Defaults to \code{NULL}.
#' @return A list with \code{i}, \code{expectation}, \code{variance}, \code{z}, \code{p_value}, \code{s0}, \code{tr_mw}, \code{k}, \code{n}, \code{not_minus_one_over_n_minus_one}, \code{method}.
#' @export
MoranRes <- function(residuals, w, x = NULL) {
  # eq (1.16).  Eg[Ires] = n tr[MW] / {(n-k) w..} is the book's own
  # formula, Sec. 1.3.2, and is reproduced term for term.  The variance is
  # derived from the exact moments of a ratio of quadratic forms in the
  # same Gaussian projection.
  e <- .morie_spx_chkv(residuals, "residuals")
  n <- length(e)
  if (n < 4L) stop("at least 4 sites are needed")
  W <- .morie_spx_chkw(w, n)
  s0 <- .morie_fsum(as.numeric(W))
  if (s0 <= 0) stop("total weight w.. must be positive")
  if (is.null(x)) {
    k <- 1L
    P <- diag(n) - matrix(1 / n, n, n)
  } else {
    X <- as.matrix(x)
    if (nrow(X) != n) {
      stop(sprintf(
        "`x` has %d rows but `residuals` has %d values",
        nrow(X), n
      ))
    }
    k <- ncol(X)
    if (n - k < 3L) stop("need n - k >= 3 residual degrees of freedom")
    G <- .morie_spx_matmul(t(X), X)
    inv <- matrix(0, k, k)
    for (cc in seq_len(k)) {
      ec <- as.numeric(seq_len(k) == cc)
      inv[, cc] <- .morie_spx_solve(G, ec)
    }
    P <- diag(n) - .morie_spx_matmul(.morie_spx_matmul(X, inv), t(X))
  }
  ee <- .morie_fsum(e * e)
  if (ee <= 0) stop("the residuals are all zero; Ires is undefined")
  ewe <- .morie_fsum(as.numeric(W) * as.numeric(outer(e, e)))
  ires <- n * ewe / (s0 * ee)
  A <- 0.5 * (W + t(W))
  B <- .morie_spx_matmul(.morie_spx_matmul(P, A), P)
  trb <- .morie_spx_trace(B)
  trb2 <- .morie_spx_trace(.morie_spx_matmul(B, B))
  df <- as.numeric(n - k)
  scale <- n / s0
  et <- trb / df
  et2 <- (2 * trb2 + trb * trb) / (df * (df + 2))
  ex <- scale * et
  vr <- scale * scale * (et2 - et * et)
  if (vr <= 0) stop("the null variance of Ires is not positive")
  zz <- (ires - ex) / sqrt(vr)
  list(
    i = ires, expectation = ex, variance = vr, z = zz,
    p_value = .morie_spx_p2(zz), s0 = s0, tr_mw = trb,
    k = k, n = n, not_minus_one_over_n_minus_one = TRUE,
    method = paste(
      "Moran's I on OLS residuals, Schabenberger & Gotway",
      "(2005) eq (1.16) with Eg[Ires] as printed in",
      "Sec. 1.3.2; the variance is derived"
    )
  )
}

# --- Ch 3: point patterns --------------------------------------------------

#' R(h) = K\'(h) / (2 h pi), Sec. 3.4.1, with Khat of Sec. 3.4.2 and the
#'
#' intensity of eq (3.8).  The naive estimator is NEGATIVELY BIASED (the
#' book says so outright), hence the border correction by default.
#'
#' @param points See Usage.
#' @param region Defaults to \code{NULL}.
#' @param r Defaults to \code{NULL}.
#' @param correction Defaults to \code{"border"}.
#' @return A list with \code{r}, \code{k}, \code{pcf}, \code{lambda}, \code{area}, \code{csr_k}, \code{csr_pcf_is_one}, \code{correction}, \code{n}, \code{method}.
#' @export
Pcf <- function(points, region = NULL, r = NULL, correction = "border") {
  # R(h) = K'(h) / (2 h pi), Sec. 3.4.1, with Khat of Sec. 3.4.2 and the
  # intensity of eq (3.8).  The naive estimator is NEGATIVELY BIASED (the
  # book says so outright), hence the border correction by default.
  P <- as.matrix(points)
  if (ncol(P) < 2L) stop("`points` must have at least two coordinate columns")
  n <- nrow(P)
  if (n < 3L) stop("at least 3 events are needed")
  if (is.null(region)) {
    reg <- rbind(range(P[, 1L]), range(P[, 2L]))
  } else {
    reg <- as.matrix(region)
    if (nrow(reg) != 2L || ncol(reg) != 2L) {
      stop("`region` must be ((xlo, xhi), (ylo, yhi))")
    }
  }
  wid <- reg[1L, 2L] - reg[1L, 1L]
  hgt <- reg[2L, 2L] - reg[2L, 1L]
  if (wid <= 0 || hgt <= 0) {
    stop("`region` must have positive width and height")
  }
  area <- wid * hgt
  lam <- n / area
  if (is.null(r)) {
    top <- 0.25 * min(wid, hgt)
    rr <- top * seq_len(10L) / 10
  } else {
    rr <- as.numeric(r)
    if (any(rr <= 0)) stop("`r` must be positive")
    if (any(diff(rr) <= 0)) stop("`r` must increase")
  }
  if (length(rr) < 2L) stop("at least 2 radii are needed to difference K")
  D <- matrix(0, n, n)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      D[i, j] <- .morie_spx_dist(P[i, 1:2], P[j, 1:2])
    }
  }
  bd <- vapply(seq_len(n), function(i) {
    min(
      P[i, 1L] - reg[1L, 1L], reg[1L, 2L] - P[i, 1L],
      P[i, 2L] - reg[2L, 1L], reg[2L, 2L] - P[i, 2L]
    )
  }, numeric(1))
  kv <- numeric(0)
  for (h in rr) {
    if (identical(correction, "none")) {
      cnt <- 0
      for (i in seq_len(n)) {
        for (j in seq_len(n)) {
          if (i != j && D[i, j] <= h) cnt <- cnt + 1
        }
      }
      kv <- c(kv, (cnt / n) / lam)
    } else if (identical(correction, "border")) {
      keep <- which(bd > h)
      if (!length(keep)) {
        kv <- c(kv, NaN)
        next
      }
      cnt <- 0
      for (i in seq_len(n)) {
        for (j in keep) {
          if (i != j && D[i, j] <= h) cnt <- cnt + 1
        }
      }
      kv <- c(kv, (cnt / length(keep)) / lam)
    } else {
      stop("`correction` must be \"border\" or \"none\"")
    }
  }
  m <- length(rr)
  g <- numeric(m)
  for (k in seq_len(m)) {
    der <- if (k == 1L) {
      (kv[2L] - kv[1L]) / (rr[2L] - rr[1L])
    } else if (k == m) {
      (kv[m] - kv[m - 1L]) / (rr[m] - rr[m - 1L])
    } else {
      (kv[k + 1L] - kv[k - 1L]) / (rr[k + 1L] - rr[k - 1L])
    }
    g[k] <- der / (2 * pi * rr[k])
  }
  list(
    r = rr, k = kv, pcf = g, lambda = lam, area = area,
    csr_k = pi * rr * rr, csr_pcf_is_one = TRUE,
    correction = correction, n = n,
    method = paste(
      "Pair correlation R(h)=K'(h)/(2 pi h), Schabenberger",
      "& Gotway (2005) Sec. 3.4.1, with Khat of Sec. 3.4.2",
      "and eq (3.8)"
    )
  )
}

# --- Ch 4: semivariogram and periodogram -----------------------------------

#' Gamma(h) = c0 + c{3h/(2a) - (1/2)(h/a)^3} on 0 < h <= a, eq (4.15)
#'
#' plus the nugget of Sec. 4.3.6; covariance eq (4.14).  gamma(0) = 0
#' ALWAYS -- the discontinuity at the origin IS the nugget, and dropping
#' the h = 0 case is the usual way to lose it.
#'
#' @param h See Usage.
#' @param c0 Defaults to \code{0}.
#' @param c Defaults to \code{1}.
#' @param a Defaults to \code{1}.
#' @return A list with \code{h}, \code{gamma}, \code{cov}, \code{nugget}, \code{psill}, \code{sill}, \code{range}, \code{true_range}, \code{n}, \code{method}.
#' @export
SphVario <- function(h, c0 = 0, c = 1, a = 1) {
  # gamma(h) = c0 + c{3h/(2a) - (1/2)(h/a)^3} on 0 < h <= a, eq (4.15)
  # plus the nugget of Sec. 4.3.6; covariance eq (4.14).  gamma(0) = 0
  # ALWAYS -- the discontinuity at the origin IS the nugget, and dropping
  # the h = 0 case is the usual way to lose it.
  hh <- .morie_spx_chkv(h, "h")
  if (any(hh < 0)) stop("`h` must be non-negative")
  c0 <- as.numeric(c0)
  c <- as.numeric(c)
  a <- as.numeric(a)
  if (a <= 0) stop("`a` (the range) must be positive")
  if (c0 < 0) stop("`c0` (the nugget) must be non-negative")
  if (c < 0) stop("`c` (the partial sill) must be non-negative")
  gam <- numeric(length(hh))
  cov <- numeric(length(hh))
  for (i in seq_along(hh)) {
    t <- hh[i]
    if (t == 0) {
      gam[i] <- 0
      cov[i] <- c0 + c
    } else if (t <= a) {
      u <- t / a
      s <- 1.5 * u - 0.5 * u * u * u
      gam[i] <- c0 + c * s
      cov[i] <- c * (1 - s)
    } else {
      gam[i] <- c0 + c
      cov[i] <- 0
    }
  }
  list(
    h = hh, gamma = gam, cov = cov, nugget = c0, psill = c,
    sill = c0 + c, range = a, true_range = TRUE, n = length(hh),
    method = paste(
      "Spherical semivariogram, Schabenberger & Gotway",
      "(2005) eq (4.15) with the nugget of Sec. 4.3.6;",
      "covariance eq (4.14)"
    )
  )
}

#' Eq (4.57) specialised to R^1 in Sec. 4.7.1.1, checked against the
#'
#' covariance form of eq (4.58).  THE ZERO FREQUENCY IS EXCLUDED: the
#' derivation of (4.58) turns on sum_u cos(w_j u) = 0, true at every
#' Fourier frequency EXCEPT w = 0.
#'
#' @param y See Usage.
#' @return A list with \code{omega}, \code{periodogram}, \code{from_covariance}, \code{max_difference}, \code{acov}, \code{zero_frequency_excluded}, \code{n}, \code{method}.
#' @export
Pgram <- function(y) {
  # eq (4.57) specialised to R^1 in Sec. 4.7.1.1, checked against the
  # covariance form of eq (4.58).  THE ZERO FREQUENCY IS EXCLUDED: the
  # derivation of (4.58) turns on sum_u cos(w_j u) = 0, true at every
  # Fourier frequency EXCEPT w = 0.
  z <- .morie_spx_chkv(y, "y")
  r <- length(z)
  if (r < 4L) stop("at least 4 lattice sites are needed")
  m <- .morie_fsum(z) / r
  d <- z - m
  if (.morie_fsum(d * d) <= 0) {
    stop("`y` is constant; the periodogram is identically 0")
  }
  lo <- -((r - 1L) %/% 2L)
  hi <- r %/% 2L
  js <- setdiff(lo:hi, 0L)
  omega <- 2 * pi * js / r
  u <- seq_len(r)
  direct <- vapply(omega, function(w) {
    re <- .morie_fsum(d * cos(w * u))
    im <- .morie_fsum(-d * sin(w * u))
    (re * re + im * im) / (2 * pi * r)
  }, numeric(1))
  acov <- vapply(0:(r - 1L), function(k) {
    .morie_fsum(d[seq_len(r - k)] * d[seq_len(r - k) + k]) / r
  }, numeric(1))
  viacov <- vapply(
    omega, function(w) {
      (acov[1L] + 2 * .morie_fsum(cos(w * seq_len(r - 1L)) *
        acov[seq_len(r - 1L) + 1L])) / (2 * pi)
    },
    numeric(1)
  )
  list(
    omega = omega, periodogram = direct, from_covariance = viacov,
    max_difference = max(abs(direct - viacov)), acov = acov,
    zero_frequency_excluded = TRUE, n = r,
    method = paste(
      "Periodogram, Schabenberger & Gotway (2005) eq (4.57)",
      "specialised to R^1 in Sec. 4.7.1.1, checked against",
      "eq (4.58)"
    )
  )
}

#' Daniell (equal-weight) smoothing of the eq (4.57) periodogram.  NOT
#' in
#'
#' Schabenberger & Gotway: "Daniell" and "smoothed periodogram" are
#' absent from the book, whose Sec. 4.7.2 fits a parametric spectral
#' density instead.  See Bloomfield (2000), Fourier Analysis of Time
#' Series, 2nd edn, Ch. 8.  The window is CIRCULAR; a truncating window
#' would bias both ends.
#'
#' @param y See Usage.
#' @param span Defaults to \code{3L}.
#' @return A list with \code{omega}, \code{smoothed}, \code{raw}, \code{span}, \code{equivalent_df}, \code{circular_window}, \code{n}, \code{method}.
#' @export
SmPgram <- function(y, span = 3L) {
  # Daniell (equal-weight) smoothing of the eq (4.57) periodogram.  NOT in
  # Schabenberger & Gotway: "Daniell" and "smoothed periodogram" are
  # absent from the book, whose Sec. 4.7.2 fits a parametric spectral
  # density instead.  See Bloomfield (2000), Fourier Analysis of Time
  # Series, 2nd edn, Ch. 8.  The window is CIRCULAR; a truncating window
  # would bias both ends.
  span <- as.integer(span)
  if (span < 1L || span %% 2L == 0L) {
    stop("`span` must be an odd positive integer")
  }
  base <- Pgram(y)
  raw <- base$periodogram
  m <- length(raw)
  if (span > m) {
    stop(sprintf(
      "`span` (%d) exceeds the number of Fourier ordinates (%d)",
      span, m
    ))
  }
  half <- span %/% 2L
  sm <- vapply(seq_len(m), function(k) {
    idx <- ((k - 1L + (-half):half) %% m) + 1L
    .morie_fsum(raw[idx]) / span
  }, numeric(1))
  list(
    omega = base$omega, smoothed = sm, raw = raw, span = span,
    equivalent_df = 2 * span, circular_window = TRUE, n = base$n,
    method = paste(
      "Daniell-smoothed periodogram; periodogram from",
      "Schabenberger & Gotway (2005) eq (4.57), the",
      "smoother is NOT in that book (see Bloomfield 2000,",
      "Ch. 8)"
    )
  )
}

# --- Ch 6: spatial autoregression ------------------------------------------

#' The SAR bound |rho| < 1/rho(W) comes from the non-singularity
#'
#' condition in the PROSE of Sec. 6.2.2.1, p. 336 (1/theta_min < rho <
#' 1/theta_max, after Haining 1990 p. 82) -- NOT from eq (6.48), which
#' is the CAR information matrix.  Anything in this package citing
#' (6.48) for the rho interval is miscited.
#'
#' @param g See Usage.
#' @param iters Defaults to \code{400L}.
#' @return A list with \code{rho}, \code{dominant_eigenvalue}, \code{eigenvector}, \code{sar_rho_bound}, \code{symmetric}, \code{iterations}, \code{n}, \code{method}.
#' @export
SpecRad <- function(g, iters = 400L) {
  # The SAR bound |rho| < 1/rho(W) comes from the non-singularity
  # condition in the PROSE of Sec. 6.2.2.1, p. 336 (1/theta_min < rho <
  # 1/theta_max, after Haining 1990 p. 82) -- NOT from eq (6.48), which is
  # the CAR information matrix.  Anything in this package citing (6.48)
  # for the rho interval is miscited.
  W <- .morie_spx_chkw(g, NULL, zero_diag = FALSE)
  n <- nrow(W)
  if (n < 2L) stop("`g` must be at least 2 by 2")
  iters <- as.integer(iters)
  if (iters < 1L) stop("`iters` must be positive")
  if (any(abs(W - t(W)) > 1e-12)) {
    stop(paste(
      "`g` must be symmetric; power iteration on a non-symmetric",
      "matrix can converge to a complex pair and report a modulus",
      "that is not the spectral radius"
    ))
  }
  v <- as.numeric(((seq_len(n) - 1L) %% 7L) + 1L)
  v <- v / sqrt(.morie_spx_dot(v, v))
  for (it in seq_len(iters)) {
    u <- .morie_spx_matvec(W, v)
    s <- sqrt(.morie_spx_dot(u, u))
    if (s < 1e-300) {
      stop(paste(
        "`g` is numerically zero; the spectral radius is 0 and",
        "no eigenvector is defined"
      ))
    }
    v <- u / s
  }
  lam <- .morie_spx_dot(v, .morie_spx_matvec(W, v))
  rho <- abs(lam)
  if (rho <= 0) stop("the spectral radius is 0; `g` has no edges")
  v <- .morie_spx_fixsign(v)
  list(
    rho = rho, dominant_eigenvalue = lam, eigenvector = v,
    sar_rho_bound = 1 / rho, symmetric = TRUE, iterations = iters, n = n,
    method = paste(
      "Spectral radius by power iteration (Golub & Van Loan",
      "2013, Sec. 7.3); the SAR bound |rho| < 1/rho(W) is",
      "Schabenberger & Gotway (2005) Sec. 6.2.2.1, p. 336",
      "-- NOT eq (6.48)"
    )
  )
}

#' .morie_spx_sarneg2
#'
#' Part of the sp_fill implementation; see the file header for the
#' source it follows.
#'
#' @param y See Usage.
#' @param X See Usage.
#' @param W See Usage.
#' @param rho See Usage.
#' @return A list with \code{v}, \code{b}, \code{s2}.
#' @export
.morie_spx_sarneg2 <- function(y, X, W, rho) {
  n <- length(y)
  A <- diag(n) - rho * W
  sl <- .morie_spx_logabsdet(A)
  if (sl[1L] <= 0 || !is.finite(sl[2L])) {
    return(list(v = Inf, b = NULL, s2 = NaN))
  }
  ys <- .morie_spx_matvec(A, y)
  Xs <- .morie_spx_matmul(A, X)
  b <- .morie_spx_lstsq(Xs, ys)
  r <- ys - .morie_spx_matvec(Xs, b)
  s2 <- .morie_spx_dot(r, r) / n
  if (s2 <= 0) {
    return(list(v = Inf, b = NULL, s2 = NaN))
  }
  list(v = n * log(2 * pi * s2) + n - 2 * sl[2L], b = b, s2 = s2)
}

#' SAR ERROR model, eqs (6.35)-(6.37) of Sec. 6.2.2.1 -- NOT the
#' spatially
#'
#' lagged model of eq (6.38).  Whitening by A = I - rho W profiles beta
#' and sigma^2 out, so only a ONE-dimensional search in rho remains; the
#' -2 log|A| Jacobian is what makes naive OLS-in-rho wrong.
#'
#' @param x See Usage.
#' @param y See Usage.
#' @param w See Usage.
#' @param n_grid Defaults to \code{201L}.
#' @param refine Defaults to \code{60L}.
#' @return A list with \code{rho}, \code{beta}, \code{sigma2}, \code{neg2loglik}, \code{rho_bounds}, \code{ols_beta}, \code{spectral_radius}, \code{is_error_model_not_lag_model}, \code{k}, \code{n}, \code{method}.
#' @export
SpErrMod <- function(x, y, w, n_grid = 201L, refine = 60L) {
  # SAR ERROR model, eqs (6.35)-(6.37) of Sec. 6.2.2.1 -- NOT the spatially
  # lagged model of eq (6.38).  Whitening by A = I - rho W profiles beta
  # and sigma^2 out, so only a ONE-dimensional search in rho remains; the
  # -2 log|A| Jacobian is what makes naive OLS-in-rho wrong.
  yy <- .morie_spx_chkv(y, "y")
  n <- length(yy)
  X <- as.matrix(x)
  if (nrow(X) != n) {
    stop(sprintf("`x` has %d rows but `y` has %d values", nrow(X), n))
  }
  k <- ncol(X)
  if (n <= k + 1L) stop("need n > k + 1 observations")
  W <- .morie_spx_chkw(w, n)
  if (any(abs(W - t(W)) > 1e-12)) {
    stop(paste(
      "`w` must be symmetric for the eigenvalue bound of",
      "Sec. 6.2.2.1 to reduce to the spectral radius"
    ))
  }
  n_grid <- as.integer(n_grid)
  if (n_grid < 5L) stop("`n_grid` must be at least 5")
  v <- as.numeric(((seq_len(n) - 1L) %% 7L) + 1L)
  v <- v / sqrt(.morie_spx_dot(v, v))
  for (it in seq_len(400L)) {
    u <- .morie_spx_matvec(W, v)
    s <- sqrt(.morie_spx_dot(u, u))
    if (s < 1e-300) stop("`w` is numerically zero; no neighbours")
    v <- u / s
  }
  srad <- abs(.morie_spx_dot(v, .morie_spx_matvec(W, v)))
  if (srad <= 0) stop("`w` has spectral radius 0; rho is unidentified")
  hi <- (1 / srad) * (1 - 1e-6)
  lo <- -hi
  bestv <- Inf
  bestr <- lo
  for (gi in seq_len(n_grid)) {
    rho <- lo + (hi - lo) * (gi - 1L) / (n_grid - 1L)
    f <- .morie_spx_sarneg2(yy, X, W, rho)
    if (f$v < bestv) {
      bestv <- f$v
      bestr <- rho
    }
  }
  step <- (hi - lo) / (n_grid - 1L)
  a <- max(lo, bestr - step)
  b <- min(hi, bestr + step)
  inv <- 0.6180339887498949
  cc <- b - inv * (b - a)
  dd <- a + inv * (b - a)
  fc <- .morie_spx_sarneg2(yy, X, W, cc)$v
  fd <- .morie_spx_sarneg2(yy, X, W, dd)$v
  for (it in seq_len(as.integer(refine))) {
    if (fc < fd) {
      b <- dd
      dd <- cc
      fd <- fc
      cc <- b - inv * (b - a)
      fc <- .morie_spx_sarneg2(yy, X, W, cc)$v
    } else {
      a <- cc
      cc <- dd
      fc <- fd
      dd <- a + inv * (b - a)
      fd <- .morie_spx_sarneg2(yy, X, W, dd)$v
    }
  }
  rho <- 0.5 * (a + b)
  f <- .morie_spx_sarneg2(yy, X, W, rho)
  if (is.null(f$b)) {
    stop(paste(
      "the likelihood is undefined at the optimum; check that",
      "`w` admits a non-singular I - rho W"
    ))
  }
  list(
    rho = rho, beta = f$b, sigma2 = f$s2, neg2loglik = f$v,
    rho_bounds = c(lo, hi), ols_beta = .morie_spx_lstsq(X, yy),
    spectral_radius = srad, is_error_model_not_lag_model = TRUE,
    k = k, n = n,
    method = paste(
      "SAR error model by ML, Schabenberger & Gotway (2005)",
      "eqs (6.35)-(6.37), Sec. 6.2.2.1; concentrated",
      "likelihood, grid scan + golden section"
    )
  )
}

# --- methods NOT in Schabenberger & Gotway ---------------------------------

#' Cohen\'s kappa (Cohen 1960) scored over NEIGHBOUR pairs rather than
#'
#' same-site pairs; the pairing is Mantel\'s M2, eq (1.5), with U_ij =
#' I{x_i = y_j}.  The kappa coefficient itself is NOT in the book.
#'
#' @param x See Usage.
#' @param y See Usage.
#' @param w See Usage.
#' @return A list with \code{kappa}, \code{p_observed}, \code{p_expected}, \code{categories}, \code{s0}, \code{compares_neighbours_not_same_site}, \code{n}, \code{method}.
#' @export
SpKappa <- function(x, y, w) {
  # Cohen's kappa (Cohen 1960) scored over NEIGHBOUR pairs rather than
  # same-site pairs; the pairing is Mantel's M2, eq (1.5), with
  # U_ij = I{x_i = y_j}.  The kappa coefficient itself is NOT in the book.
  xv <- .morie_spx_chkv(x, "x")
  yv <- .morie_spx_chkv(y, "y")
  n <- length(xv)
  if (length(yv) != n) stop("`x` and `y` must have the same length")
  if (n < 2L) stop("at least 2 sites are needed")
  xi <- round(xv)
  yi <- round(yv)
  if (any(abs(xv - xi) > 1e-9)) stop("`x` must hold integer category codes")
  if (any(abs(yv - yi) > 1e-9)) stop("`y` must hold integer category codes")
  W <- .morie_spx_chkw(w, n)
  if (any(W < 0)) {
    stop("`w` must be non-negative for kappa to be a proportion")
  }
  s0 <- .morie_fsum(as.numeric(W))
  if (s0 <= 0) stop("total weight w.. must be positive")
  cats <- sort(unique(c(xi, yi)))
  agree <- outer(xi, yi, "==")
  po <- .morie_fsum(as.numeric(W) * as.numeric(agree)) / s0
  rows <- vapply(seq_len(n), function(i) .morie_fsum(W[i, ]), numeric(1))
  cols <- vapply(seq_len(n), function(j) .morie_fsum(W[, j]), numeric(1))
  pe <- .morie_fsum(vapply(
    cats, function(cv) {
      (.morie_fsum(rows[xi == cv]) / s0) * (.morie_fsum(cols[yi == cv]) / s0)
    },
    numeric(1)
  ))
  if (abs(1 - pe) < 1e-12) {
    stop(paste(
      "expected agreement is 1; kappa is undefined (both maps are",
      "effectively constant)"
    ))
  }
  list(
    kappa = (po - pe) / (1 - pe), p_observed = po, p_expected = pe,
    categories = as.numeric(cats), s0 = s0,
    compares_neighbours_not_same_site = TRUE, n = n,
    method = paste(
      "Cohen's kappa (Cohen 1960) over the neighbour pairs",
      "of Mantel's M2, Schabenberger & Gotway (2005)",
      "eq (1.5); the kappa coefficient is NOT in that book"
    )
  )
}

#' Local Moran eq (1.17) with EXACT conditional-randomization moments
#'
#' (simple random sampling without replacement of the other n-1
#' deviations into the neighbour slots).  The HH/LL/HL/LH labels are
#' Anselin (1996)\'s Moran scatterplot, NOT in Schabenberger & Gotway; a
#' fixed-string search of the book for "quadrant" and "Moran scatter"
#' finds only an unrelated kriging search neighbourhood.
#'
#' @param x See Usage.
#' @param w See Usage.
#' @param alpha Defaults to \code{0.05}.
#' @return A list with \code{labels}, \code{local}, \code{z}, \code{p_value}, \code{lagged_mean}, \code{counts}, \code{alpha}, \code{conditional_randomization}, \code{hl_and_lh_are_outliers_not_clusters}, \code{n}, \code{method}.
#' @export
LisaClust <- function(x, w, alpha = 0.05) {
  # Local Moran eq (1.17) with EXACT conditional-randomization moments
  # (simple random sampling without replacement of the other n-1
  # deviations into the neighbour slots).  The HH/LL/HL/LH labels are
  # Anselin (1996)'s Moran scatterplot, NOT in Schabenberger & Gotway;
  # a fixed-string search of the book for "quadrant" and "Moran scatter"
  # finds only an unrelated kriging search neighbourhood.
  z <- .morie_spx_chkv(x, "x")
  n <- length(z)
  if (n < 4L) {
    stop("at least 4 sites are needed; the conditional variance divides by n-2")
  }
  W <- .morie_spx_chkw(w, n)
  alpha <- as.numeric(alpha)
  if (!(alpha > 0 && alpha < 1)) {
    stop("`alpha` must lie strictly between 0 and 1")
  }
  m <- .morie_fsum(z) / n
  d <- z - m
  ss <- .morie_fsum(d * d)
  if (ss <= 0) stop("`x` is constant; local Moran's I is undefined")
  labels <- character(n)
  local <- numeric(n)
  zs <- numeric(n)
  ps <- numeric(n)
  lagm <- numeric(n)
  for (i in seq_len(n)) {
    li <- .morie_fsum(W[i, ] * d)
    local[i] <- n * d[i] * li / ss
    others <- d[-i]
    mb <- .morie_fsum(others) / (n - 1)
    v <- .morie_fsum((others - mb)^2) / (n - 1)
    s1 <- .morie_fsum(W[i, ])
    s2 <- .morie_fsum(W[i, ] * W[i, ])
    varl <- v * (s2 - s1 * s1 / (n - 1)) * (n - 1) / (n - 2)
    nb <- sum(W[i, ] != 0)
    lagm[i] <- if (nb > 0) li / nb else 0
    if (varl <= 0) {
      zs[i] <- NaN
      ps[i] <- 1
      labels[i] <- "NS"
      next
    }
    zi <- (li - mb * s1) / sqrt(varl)
    pv <- .morie_spx_p2(zi)
    zs[i] <- zi
    ps[i] <- pv
    labels[i] <- if (pv >= alpha) {
      "NS"
    } else if (d[i] >= 0 && lagm[i] >= 0) {
      "HH"
    } else if (d[i] < 0 && lagm[i] < 0) {
      "LL"
    } else if (d[i] >= 0) "HL" else "LH"
  }
  counts <- vapply(
    c("HH", "LL", "HL", "LH", "NS"),
    function(k) sum(labels == k), numeric(1)
  )
  list(
    labels = labels, local = local, z = zs, p_value = ps,
    lagged_mean = lagm, counts = counts, alpha = alpha,
    conditional_randomization = TRUE,
    hl_and_lh_are_outliers_not_clusters = TRUE, n = n,
    method = paste(
      "Local Moran eq (1.17) of Schabenberger & Gotway",
      "(2005) Sec. 1.3.3 with exact",
      "conditional-randomization moments; the HH/LL/HL/LH",
      "labels are Anselin (1996), not in that book"
    )
  )
}

#' Tukey (1977), Exploratory Data Analysis.  NOT in Schabenberger &
#'
#' Gotway -- a fixed-string search for "polish" returns nothing; the
#' book\'s trend removal is the OLS trend surface of Sec. 5.3.1.  Median
#' polish is resistant to outliers, which is why the geostatistical
#' literature reaches for it first.  A sweep must run row-then-column in
#' a FIXED order; median polish is not order-invariant.
#'
#' @param values See Usage.
#' @param grid Defaults to \code{NULL}.
#' @param iters Defaults to \code{10L}.
#' @return A list with \code{overall}, \code{row}, \code{col}, \code{residuals}, \code{fitted}, \code{abs_residual_sum}, \code{sweeps}, \code{resistant_to_outliers}, \code{nrow}, \code{ncol}, \code{n}, \code{method}.
#' @export
MedPolish <- function(values, grid = NULL, iters = 10L) {
  # Tukey (1977), Exploratory Data Analysis.  NOT in Schabenberger &
  # Gotway -- a fixed-string search for "polish" returns nothing; the
  # book's trend removal is the OLS trend surface of Sec. 5.3.1.  Median
  # polish is resistant to outliers, which is why the geostatistical
  # literature reaches for it first.  A sweep must run row-then-column in
  # a FIXED order; median polish is not order-invariant.
  if (!is.null(grid)) {
    flat <- as.numeric(values)
    g <- as.integer(grid)
    if (length(g) != 2L || g[1L] < 1L || g[2L] < 1L) {
      stop("`grid` must be (nrow, ncol), both positive")
    }
    if (length(flat) != g[1L] * g[2L]) {
      stop(sprintf(
        "`values` has %d entries but `grid` asks for %d",
        length(flat), g[1L] * g[2L]
      ))
    }
    Y <- matrix(flat, nrow = g[1L], ncol = g[2L], byrow = TRUE)
  } else {
    Y <- as.matrix(values)
  }
  nr <- nrow(Y)
  nc <- ncol(Y)
  if (nr < 2L || nc < 2L) stop("median polish needs at least a 2 by 2 grid")
  iters <- as.integer(iters)
  if (iters < 1L) stop("`iters` must be positive")
  res <- Y
  row <- numeric(nr)
  col <- numeric(nc)
  overall <- 0
  for (it in seq_len(iters)) {
    for (i in seq_len(nr)) {
      d <- .morie_spx_median(res[i, ])
      row[i] <- row[i] + d
      res[i, ] <- res[i, ] - d
    }
    d <- .morie_spx_median(col)
    overall <- overall + d
    col <- col - d
    for (j in seq_len(nc)) {
      d <- .morie_spx_median(res[, j])
      col[j] <- col[j] + d
      res[, j] <- res[, j] - d
    }
    d <- .morie_spx_median(row)
    overall <- overall + d
    row <- row - d
  }
  fitted <- outer(row, col, "+") + overall
  list(
    overall = overall, row = row, col = col, residuals = res,
    fitted = fitted,
    abs_residual_sum = .morie_fsum(abs(as.numeric(res))),
    sweeps = iters, resistant_to_outliers = TRUE,
    nrow = nr, ncol = nc, n = nr * nc,
    method = paste(
      "Median polish (Tukey 1977, Exploratory Data",
      "Analysis); NOT in Schabenberger & Gotway, whose",
      "trend removal is the OLS trend surface of",
      "Sec. 5.3.1"
    )
  )
}

#' Thetahat_j = ybar.. + (1 - lambda_j)(ybar_j - ybar..),
#'
#' lambda_j = sigma2_e / (sigma2_e + n_j sigma2_u).  lambda depends on
#' the CLUSTER\'S OWN SIZE; a common lambda over-shrinks the large
#' clusters. Stein (1956); Morris (1983) JASA 78:47-55.  NOT in
#' Schabenberger & Gotway -- a fixed-string search for "shrinkage"
#' returns nothing.
#'
#' @param y See Usage.
#' @param cluster See Usage.
#' @param sigma2_u See Usage.
#' @param sigma2_e See Usage.
#' @return A list with \code{clusters}, \code{shrunk}, \code{raw}, \code{lambda}, \code{sizes}, \code{grand_mean}, \code{sigma2_u}, \code{sigma2_e}, \code{shrinkage_depends_on_cluster_size}, \code{n}, \code{method}.
#' @export
ShrinkPred <- function(y, cluster, sigma2_u, sigma2_e) {
  # thetahat_j = ybar.. + (1 - lambda_j)(ybar_j - ybar..),
  # lambda_j = sigma2_e / (sigma2_e + n_j sigma2_u).  lambda depends on the
  # CLUSTER'S OWN SIZE; a common lambda over-shrinks the large clusters.
  # Stein (1956); Morris (1983) JASA 78:47-55.  NOT in Schabenberger &
  # Gotway -- a fixed-string search for "shrinkage" returns nothing.
  yy <- .morie_spx_chkv(y, "y")
  cv <- .morie_spx_chkv(cluster, "cluster")
  n <- length(yy)
  if (length(cv) != n) stop("`y` and `cluster` must have the same length")
  ci <- round(cv)
  if (any(abs(cv - ci) > 1e-9)) stop("`cluster` must hold integer codes")
  su <- as.numeric(sigma2_u)
  se <- as.numeric(sigma2_e)
  if (su < 0) stop("`sigma2_u` must be non-negative")
  if (se <= 0) stop("`sigma2_e` must be positive")
  keys <- sort(unique(ci))
  if (length(keys) < 2L) {
    stop("at least 2 clusters are needed for shrinkage to mean anything")
  }
  grand <- .morie_fsum(yy) / n
  sizes <- numeric(0)
  raw <- numeric(0)
  lam <- numeric(0)
  shrunk <- numeric(0)
  for (cval in keys) {
    vals <- yy[ci == cval]
    nj <- length(vals)
    mj <- .morie_fsum(vals) / nj
    lj <- se / (se + nj * su)
    sizes <- c(sizes, nj)
    raw <- c(raw, mj)
    lam <- c(lam, lj)
    shrunk <- c(shrunk, grand + (1 - lj) * (mj - grand))
  }
  list(
    clusters = as.numeric(keys), shrunk = shrunk, raw = raw,
    lambda = lam, sizes = sizes, grand_mean = grand,
    sigma2_u = su, sigma2_e = se,
    shrinkage_depends_on_cluster_size = TRUE, n = n,
    method = paste(
      "Level-2 shrinkage / empirical-Bayes predictor",
      "(Stein 1956; Morris 1983); NOT in Schabenberger &",
      "Gotway"
    )
  )
}

#' SparseVector
#'
#' Part of the sp_fill implementation; see the file header for the
#' source it follows.
#'
#' @param queries See Usage.
#' @param threshold See Usage.
#' @param c Defaults to \code{1L}.
#' @param epsilon Defaults to \code{1}.
#' @param threshold_noise Defaults to \code{0}.
#' @param query_noise Defaults to \code{NULL}.
#' @return A list with \code{above}, \code{released}, \code{halted_at}, \code{n_above}, \code{noisy_threshold}, \code{noise_scales}, \code{epsilon_split}, \code{epsilon}, \code{c}, \code{cost_scales_with_c_not_with_m}, \code{answered}, \code{n}, \code{method}.
#' @export
SparseVector <- function(queries, threshold, c = 1L, epsilon = 1,
                         threshold_noise = 0, query_noise = NULL) {
  # AboveThreshold / sparse vector (Dwork & Roth 2014, Alg. 2; Hardt &
  # Rothblum 2010).  NOT in Schabenberger & Gotway -- differential
  # privacy, not spatial statistics.  THE NOISE IS AN ARGUMENT: R and
  # Python generators do not share a stream, so a function that samples
  # its own Laplace noise cannot be compared across arms.
  q <- .morie_spx_chkv(queries, "queries")
  m <- length(q)
  t <- as.numeric(threshold)
  cc <- as.integer(c)
  eps <- as.numeric(epsilon)
  if (cc < 1L) stop("`c` must be at least 1")
  if (cc > m) {
    stop(sprintf("`c` (%d) exceeds the number of queries (%d)", cc, m))
  }
  if (eps <= 0) stop("`epsilon` must be positive")
  qn <- if (is.null(query_noise)) {
    rep(0, m)
  } else {
    .morie_spx_chkv(query_noise, "query_noise")
  }
  if (length(qn) != m) stop("`query_noise` must have one entry per query")
  tn <- t + as.numeric(threshold_noise)
  above <- rep(NA, m)
  released <- rep(NA_real_, m)
  hits <- 0L
  halted <- m
  for (i in seq_len(m)) {
    if (hits >= cc) {
      halted <- i - 1L
      break
    }
    if (q[i] + qn[i] >= tn) {
      above[i] <- TRUE
      released[i] <- q[i] + qn[i]
      hits <- hits + 1L
    } else {
      above[i] <- FALSE
    }
  }
  list(
    above = above, released = released, halted_at = halted,
    n_above = hits, noisy_threshold = tn,
    noise_scales = c(threshold = 2 / eps, query = 2 * cc / eps),
    epsilon_split = c(threshold = eps / 2, queries = eps / 2),
    epsilon = eps, c = cc,
    cost_scales_with_c_not_with_m = TRUE,
    answered = sum(!is.na(above)), n = m,
    method = paste(
      "Sparse vector / AboveThreshold (Dwork & Roth 2014,",
      "Alg. 2; Hardt & Rothblum 2010) with",
      "caller-supplied noise; NOT in Schabenberger &",
      "Gotway"
    )
  )
}

#' Alpha = sum_x min(p,q) = 1 - TV(p,q); E[tokens] =
#'
#' (1 - alpha^(gamma+1))/(1 - alpha), capped at gamma+1 (a rejected
#' token is resampled from the residual and still counts).  Leviathan,
#' Kalman & Matias (2023).  NOT in Schabenberger & Gotway.
#' Deterministic: the expectation, not a sampled run.
#'
#' @param draft See Usage.
#' @param target See Usage.
#' @param gamma Defaults to \code{4L}.
#' @return A list with \code{alpha}, \code{tv_distance}, \code{expected_tokens}, \code{gamma}, \code{max_tokens}, \code{deterministic_expectation_not_a_sampled_run}, \code{n}, \code{method}.
#' @export
SpecDec <- function(draft, target, gamma = 4L) {
  # alpha = sum_x min(p,q) = 1 - TV(p,q); E[tokens] =
  # (1 - alpha^(gamma+1))/(1 - alpha), capped at gamma+1 (a rejected token
  # is resampled from the residual and still counts).  Leviathan, Kalman &
  # Matias (2023).  NOT in Schabenberger & Gotway.  Deterministic: the
  # expectation, not a sampled run.
  q <- .morie_spx_chkv(draft, "draft")
  p <- .morie_spx_chkv(target, "target")
  if (length(q) != length(p)) {
    stop("`draft` and `target` must cover the same vocabulary")
  }
  if (length(q) < 2L) stop("a vocabulary of at least 2 tokens is needed")
  if (any(q < 0) || any(p < 0)) stop("probabilities must be non-negative")
  if (abs(.morie_fsum(q) - 1) > 1e-9) {
    stop(sprintf("`draft` must sum to 1 (got %.12g)", .morie_fsum(q)))
  }
  if (abs(.morie_fsum(p) - 1) > 1e-9) {
    stop(sprintf("`target` must sum to 1 (got %.12g)", .morie_fsum(p)))
  }
  g <- as.integer(gamma)
  if (g < 1L) stop("`gamma` must be at least 1")
  alpha <- .morie_fsum(pmin(p, q))
  tv <- 1 - alpha
  expect <- if (tv <= 1e-15) {
    as.numeric(g + 1L)
  } else {
    (1 - alpha^(g + 1L)) / (1 - alpha)
  }
  list(
    alpha = alpha, tv_distance = tv, expected_tokens = expect,
    gamma = as.numeric(g), max_tokens = as.numeric(g + 1L),
    deterministic_expectation_not_a_sampled_run = TRUE,
    n = length(p),
    method = paste(
      "Speculative decoding acceptance rate and expected",
      "token yield (Leviathan, Kalman & Matias 2023); NOT",
      "in Schabenberger & Gotway"
    )
  )
}

#' Raw cross-periodogram S_xy(w) = X(w) conj(Y(w)) / (2 pi n) on
#'
#' MEAN-REMOVED records (Brillinger 2001, Ch. 7).  NOT in Schabenberger
#' & Gotway -- a fixed-string search for "cross-spectr" finds one
#' bibliography entry and no method.  Zero frequency dropped.
#'
#' @param x See Usage.
#' @param y See Usage.
#' @return A list with \code{omega}, \code{cospectrum}, \code{quadrature}, \code{amplitude}, \code{phase}, \code{means_removed}, \code{raw_not_consistent}, \code{n}, \code{method}.
#' @export
CrossSpec <- function(x, y) {
  # Raw cross-periodogram S_xy(w) = X(w) conj(Y(w)) / (2 pi n) on
  # MEAN-REMOVED records (Brillinger 2001, Ch. 7).  NOT in Schabenberger &
  # Gotway -- a fixed-string search for "cross-spectr" finds one
  # bibliography entry and no method.  Zero frequency dropped.
  xv <- .morie_spx_chkv(x, "x")
  yv <- .morie_spx_chkv(y, "y")
  n <- length(xv)
  if (length(yv) != n) stop("`x` and `y` must have the same length")
  if (n < 4L) stop("at least 4 observations are needed")
  dx <- xv - .morie_fsum(xv) / n
  dy <- yv - .morie_fsum(yv) / n
  if (.morie_fsum(dx * dx) <= 0 || .morie_fsum(dy * dy) <= 0) {
    stop("`x` and `y` must not be constant")
  }
  fx <- .morie_spx_dft(dx)
  fy <- .morie_spx_dft(dy)
  scale <- 2 * pi * n
  ks <- seq_len(n %/% 2L)
  omega <- 2 * pi * ks / n
  re <- (fx$re[ks + 1L] * fy$re[ks + 1L] + fx$im[ks + 1L] * fy$im[ks + 1L]) /
    scale
  im <- (fx$im[ks + 1L] * fy$re[ks + 1L] - fx$re[ks + 1L] * fy$im[ks + 1L]) /
    scale
  list(
    omega = omega, cospectrum = re, quadrature = -im,
    amplitude = sqrt(re * re + im * im), phase = atan2(im, re),
    means_removed = TRUE, raw_not_consistent = TRUE, n = n,
    method = paste(
      "Raw cross-periodogram (Brillinger 2001, Ch. 7);",
      "NOT in Schabenberger & Gotway"
    )
  )
}

#' C_xy = |S_xy|^2 / (S_xx S_yy) by Welch averaging (Bendat & Piersol
#'
#' 2010, Ch. 5).  NOT in Schabenberger & Gotway -- a fixed-string search
#' for "coherence" returns nothing.  AVERAGING IS NOT OPTIONAL: on a
#' single segment the coherence of ANY two records is exactly 1, so
#' fewer than two segments raises.
#'
#' @param x See Usage.
#' @param y See Usage.
#' @param nperseg Defaults to \code{NULL}.
#' @param overlap Defaults to \code{0.5}.
#' @return A list with \code{omega}, \code{coherence}, \code{sxx}, \code{syy}, \code{n_segments}, \code{nperseg}, \code{step}, \code{single_segment_coherence_is_identically_one}, \code{n}, \code{method}.
#' @export
MsCoh <- function(x, y, nperseg = NULL, overlap = 0.5) {
  # C_xy = |S_xy|^2 / (S_xx S_yy) by Welch averaging (Bendat & Piersol
  # 2010, Ch. 5).  NOT in Schabenberger & Gotway -- a fixed-string search
  # for "coherence" returns nothing.  AVERAGING IS NOT OPTIONAL: on a
  # single segment the coherence of ANY two records is exactly 1, so
  # fewer than two segments raises.
  xv <- .morie_spx_chkv(x, "x")
  yv <- .morie_spx_chkv(y, "y")
  n <- length(xv)
  if (length(yv) != n) stop("`x` and `y` must have the same length")
  m <- if (is.null(nperseg)) max(8L, n %/% 4L) else as.integer(nperseg)
  if (m < 8L) stop("`nperseg` must be at least 8")
  if (m > n) {
    stop(sprintf("`nperseg` (%d) exceeds the record length (%d)", m, n))
  }
  overlap <- as.numeric(overlap)
  if (!(overlap >= 0 && overlap < 1)) stop("`overlap` must lie in [0, 1)")
  step <- max(1L, as.integer(round(m * (1 - overlap))))
  starts <- seq.int(0L, n - m, by = step)
  if (length(starts) < 2L) {
    stop(paste(
      "fewer than 2 segments: coherence would be identically 1",
      "and would mean nothing; shorten `nperseg` or lengthen the",
      "records"
    ))
  }
  win <- 0.5 - 0.5 * cos(2 * pi * (seq_len(m) - 1L) / (m - 1))
  ks <- seq_len(m %/% 2L)
  sxx <- rep(0, length(ks))
  syy <- rep(0, length(ks))
  cre <- rep(0, length(ks))
  cim <- rep(0, length(ks))
  for (s in starts) {
    sx <- xv[(s + 1L):(s + m)]
    sy <- yv[(s + 1L):(s + m)]
    wx <- (sx - .morie_fsum(sx) / m) * win
    wy <- (sy - .morie_fsum(sy) / m) * win
    fx <- .morie_spx_dft(wx)
    fy <- .morie_spx_dft(wy)
    xr <- fx$re[ks + 1L]
    xi <- fx$im[ks + 1L]
    yr <- fy$re[ks + 1L]
    yi <- fy$im[ks + 1L]
    sxx <- sxx + xr * xr + xi * xi
    syy <- syy + yr * yr + yi * yi
    cre <- cre + xr * yr + xi * yi
    cim <- cim + xi * yr - xr * yi
  }
  nseg <- length(starts)
  den <- sxx * syy
  coh <- ifelse(den <= 0, NaN, (cre^2 + cim^2) / den)
  list(
    omega = 2 * pi * ks / m, coherence = coh,
    sxx = sxx / nseg, syy = syy / nseg,
    n_segments = nseg, nperseg = m, step = step,
    single_segment_coherence_is_identically_one = TRUE, n = n,
    method = paste(
      "Magnitude-squared coherence by Welch averaging",
      "(Bendat & Piersol 2010, Ch. 5); NOT in",
      "Schabenberger & Gotway"
    )
  )
}

#' Spectral residual saliency (Hou & Zhang 2007).  NOT in Schabenberger
#' &
#'
#' Gotway.  THE PHASE IS KEPT: rebuilding from the residual amplitude
#' with the ORIGINAL phase is the whole mechanism.  The moving average
#' is circular, matching the periodicity of the DFT.
#'
#' @param x See Usage.
#' @param q Defaults to \code{3L}.
#' @return A list with \code{saliency}, \code{peak}, \code{peak_index}, \code{residual}, \code{log_amplitude}, \code{floored}, \code{phase_is_preserved}, \code{q}, \code{n}, \code{method}.
#' @export
SpecAnom <- function(x, q = 3L) {
  # Spectral residual saliency (Hou & Zhang 2007).  NOT in Schabenberger &
  # Gotway.  THE PHASE IS KEPT: rebuilding from the residual amplitude
  # with the ORIGINAL phase is the whole mechanism.  The moving average is
  # circular, matching the periodicity of the DFT.
  v <- .morie_spx_chkv(x, "x")
  n <- length(v)
  if (n < 8L) stop("at least 8 samples are needed")
  q <- as.integer(q)
  if (q < 1L || q %% 2L == 0L) stop("`q` must be an odd positive integer")
  if (q > n) {
    stop(sprintf("`q` (%d) exceeds the record length (%d)", q, n))
  }
  f <- .morie_spx_dft(v)
  amp <- sqrt(f$re * f$re + f$im * f$im)
  floored <- sum(amp < 1e-300)
  lg <- ifelse(amp < 1e-300, log(1e-300), log(amp))
  half <- q %/% 2L
  sm <- vapply(seq_len(n), function(k) {
    idx <- ((k - 1L + (-half):half) %% n) + 1L
    .morie_fsum(lg[idx]) / q
  }, numeric(1))
  res <- lg - sm
  ph <- atan2(f$im, f$re)
  rec <- .morie_spx_idftre(exp(res) * cos(ph), exp(res) * sin(ph))
  sal <- rec * rec
  pk <- max(sal)
  list(
    saliency = sal, peak = pk, peak_index = which.max(sal) - 1L,
    residual = res, log_amplitude = lg, floored = floored,
    phase_is_preserved = TRUE, q = q, n = n,
    method = paste(
      "Spectral residual saliency (Hou & Zhang 2007); NOT",
      "in Schabenberger & Gotway"
    )
  )
}

#' L_sym = I - D^-1/2 A D^-1/2.  The clustering lives in the SMALLEST
#'
#' eigenvalues, so power iteration runs on 2I - L_sym and the values are
#' mapped back; running it on L_sym and taking the top vectors gets this
#' exactly backwards.  Ng, Jordan & Weiss (2001).  NOT in Schabenberger
#' & Gotway.  A zero-degree node RAISES rather than being quietly
#' assigned.
#'
#' @param a See Usage.
#' @param k Defaults to \code{2L}.
#' @return A list with \code{labels}, \code{sizes}, \code{eigenvalues}, \code{fiedler}, \code{degree}, \code{smallest_eigenvalues_not_largest}, \code{k}, \code{n}, \code{method}.
#' @export
SpecClust <- function(a, k = 2L) {
  # L_sym = I - D^-1/2 A D^-1/2.  The clustering lives in the SMALLEST
  # eigenvalues, so power iteration runs on 2I - L_sym and the values are
  # mapped back; running it on L_sym and taking the top vectors gets this
  # exactly backwards.  Ng, Jordan & Weiss (2001).  NOT in Schabenberger &
  # Gotway.  A zero-degree node RAISES rather than being quietly assigned.
  W <- .morie_spx_chkw(a, NULL)
  n <- nrow(W)
  k <- as.integer(k)
  if (k < 2L || k > n) {
    stop("`k` must lie between 2 and the number of nodes")
  }
  if (any(W < 0)) stop("`a` must be non-negative")
  if (any(abs(W - t(W)) > 1e-12)) stop("`a` must be symmetric")
  deg <- vapply(seq_len(n), function(i) .morie_fsum(W[i, ]), numeric(1))
  if (any(deg <= 0)) {
    stop(sprintf(
      "node %d has degree 0; an isolated node belongs to no cluster",
      which(deg <= 0)[1L] - 1L
    ))
  }
  ds <- 1 / sqrt(deg)
  lsym <- diag(n) - (ds * W) * rep(ds, each = n)
  shifted <- diag(2, n) - lsym
  te <- .morie_spx_topeigs(shifted, min(k, n))
  eig <- 2 - te$values
  fied <- if (length(te$vectors) > 1L) te$vectors[[2L]] else te$vectors[[1L]]
  if (k == 2L) {
    labels <- as.numeric(fied >= 0)
  } else {
    srt <- sort(fied)
    cen <- vapply(seq_len(k), function(cv) {
      srt[round((n - 1) * (cv - 0.5) / k) + 1L]
    }, numeric(1))
    labels <- rep(0, n)
    for (it in seq_len(50L)) {
      for (i in seq_len(n)) {
        best <- 1L
        for (cv in seq_len(k)) {
          if (abs(fied[i] - cen[cv]) < abs(fied[i] - cen[best])) best <- cv
        }
        labels[i] <- best - 1L
      }
      for (cv in seq_len(k)) {
        mem <- fied[labels == cv - 1L]
        if (length(mem)) cen[cv] <- .morie_fsum(mem) / length(mem)
      }
    }
  }
  sizes <- vapply(seq_len(k), function(cv) sum(labels == cv - 1L), numeric(1))
  list(
    labels = labels, sizes = sizes, eigenvalues = eig, fiedler = fied,
    degree = deg, smallest_eigenvalues_not_largest = TRUE,
    k = k, n = n,
    method = paste(
      "Normalized spectral clustering (Ng, Jordan & Weiss",
      "2001) with deterministic order-statistic starts;",
      "NOT in Schabenberger & Gotway"
    )
  )
}

#' MULTISPATI: diagonalise H = (1/n) X\' ((W + W\')/2) X on the centred,
#'
#' unit-variance X, so an axis is scored by SPATIAL covariance, not
#' variance.  Eigenvalues may be NEGATIVE -- that is a local-contrast
#' axis, which ordinary PCA cannot express -- and are returned signed. W
#' is symmetrised first: a row-standardised W is ASYMMETRIC and a
#' symmetric eigensolver would read one triangle only.  Dray, Said &
#' Debias (2008).  NOT in Schabenberger & Gotway.
#'
#' @param x See Usage.
#' @param w See Usage.
#' @param naxes Defaults to \code{2L}.
#' @return A list with \code{eigenvalues}, \code{loadings}, \code{scores}, \code{lagged_scores}, \code{total_variance}, \code{eigenvalues_may_be_negative}, \code{weights_symmetrised}, \code{naxes}, \code{n}, \code{method}.
#' @export
SpatialPca <- function(x, w, naxes = 2L) {
  # MULTISPATI: diagonalise H = (1/n) X' ((W + W')/2) X on the centred,
  # unit-variance X, so an axis is scored by SPATIAL covariance, not
  # variance.  Eigenvalues may be NEGATIVE -- that is a local-contrast
  # axis, which ordinary PCA cannot express -- and are returned signed.
  # W is symmetrised first: a row-standardised W is ASYMMETRIC and a
  # symmetric eigensolver would read one triangle only.  Dray, Said &
  # Debias (2008).  NOT in Schabenberger & Gotway.
  X <- as.matrix(x)
  n <- nrow(X)
  p <- ncol(X)
  if (n < 3L) stop("at least 3 sites are needed")
  naxes <- as.integer(naxes)
  if (naxes < 1L || naxes > p) {
    stop("`naxes` must lie between 1 and the number of columns")
  }
  W <- .morie_spx_chkw(w, n)
  Z <- matrix(0, n, p)
  for (j in seq_len(p)) {
    cj <- as.numeric(X[, j])
    d <- cj - .morie_fsum(cj) / n
    s <- sqrt(.morie_fsum(d * d) / n)
    if (s <= 0) {
      stop("a column of `x` is constant and cannot be scaled to unit variance")
    }
    Z[, j] <- d / s
  }
  sym <- 0.5 * (W + t(W))
  H <- .morie_spx_matmul(t(Z), .morie_spx_matmul(sym, Z)) / n
  H <- 0.5 * (H + t(H))
  te <- .morie_spx_topeigs(H, naxes)
  scores <- lapply(seq_len(naxes), function(a) {
    vapply(seq_len(n), function(i) {
      .morie_fsum(Z[i, ] * te$vectors[[a]])
    }, numeric(1))
  })
  lagged <- lapply(scores, function(s) .morie_spx_matvec(sym, s))
  list(
    eigenvalues = te$values, loadings = te$vectors, scores = scores,
    lagged_scores = lagged, total_variance = as.numeric(p),
    eigenvalues_may_be_negative = TRUE, weights_symmetrised = TRUE,
    naxes = naxes, n = n,
    method = paste(
      "MULTISPATI spatial PCA (Dray, Said & Debias 2008);",
      "NOT in Schabenberger & Gotway"
    )
  )
}

#' Thin-plate spline eta(r) = r^2 log r plus linear covariates, solved
#' as
#'
#' the saddle-point system [K + n lam I, T; T\', 0].  T = [1, s1, s2, X]
#' spans the null space of the penalty and must NOT be shrunk; dropping
#' the T\'c = 0 block leaves the system singular.  Duchon (1977); Wood
#' (2006) Ch. 4.  NOT in Schabenberger & Gotway, whose parametric
#' analogue is Sec. 5.3.1.
#'
#' @param y See Usage.
#' @param x See Usage.
#' @param coords See Usage.
#' @param lam Defaults to \code{0}.
#' @return A list with \code{fitted}, \code{residuals}, \code{coef}, \code{spline_weights}, \code{rss}, \code{penalty}, \code{lam}, \code{null_space_is_unpenalised}, \code{n}, \code{method}.
#' @export
SpGam <- function(y, x, coords, lam = 0) {
  # Thin-plate spline eta(r) = r^2 log r plus linear covariates, solved as
  # the saddle-point system [K + n lam I, T; T', 0].  T = [1, s1, s2, X]
  # spans the null space of the penalty and must NOT be shrunk; dropping
  # the T'c = 0 block leaves the system singular.  Duchon (1977); Wood
  # (2006) Ch. 4.  NOT in Schabenberger & Gotway, whose parametric
  # analogue is Sec. 5.3.1.
  yv <- .morie_spx_chkv(y, "y")
  n <- length(yv)
  cc <- as.matrix(coords)
  if (nrow(cc) != n) {
    stop(sprintf("`coords` has %d rows but `y` has %d values", nrow(cc), n))
  }
  if (ncol(cc) < 2L) {
    stop("`coords` must have two columns for a 2-D thin-plate spline")
  }
  lam <- as.numeric(lam)
  if (lam < 0) stop("`lam` must be non-negative")
  Tm <- if (is.null(x)) {
    cbind(1, cc[, 1L], cc[, 2L])
  } else {
    cbind(1, cc[, 1L], cc[, 2L], as.matrix(x))
  }
  m <- ncol(Tm)
  if (n <= m) {
    stop(sprintf("need more sites than null-space columns (%d)", m))
  }
  K <- matrix(0, n, n)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i != j) {
        r <- .morie_spx_dist(cc[i, 1:2], cc[j, 1:2])
        K[i, j] <- if (r > 0) r * r * log(r) else 0
      }
    }
  }
  size <- n + m
  A <- matrix(0, size, size)
  A[seq_len(n), seq_len(n)] <- K
  diag(A)[seq_len(n)] <- diag(A)[seq_len(n)] + n * lam
  A[seq_len(n), n + seq_len(m)] <- Tm
  A[n + seq_len(m), seq_len(n)] <- t(Tm)
  sol <- .morie_spx_solve(A, c(yv, rep(0, m)))
  cw <- sol[seq_len(n)]
  d <- sol[n + seq_len(m)]
  fitted <- vapply(seq_len(n), function(i) {
    .morie_fsum(K[i, ] * cw) + .morie_fsum(Tm[i, ] * d)
  }, numeric(1))
  resid <- yv - fitted
  list(
    fitted = fitted, residuals = resid, coef = d, spline_weights = cw,
    rss = .morie_fsum(resid * resid),
    penalty = .morie_fsum(vapply(seq_len(n), function(i) {
      cw[i] * .morie_fsum(K[i, ] * cw)
    }, numeric(1))),
    lam = lam, null_space_is_unpenalised = TRUE, n = n,
    method = paste(
      "Thin-plate spline surface plus linear covariates",
      "(Duchon 1977; Wood 2006, Ch. 4); NOT in",
      "Schabenberger & Gotway, whose parametric analogue",
      "is Sec. 5.3.1"
    )
  )
}

#' I = H(R) - H(R|S), bits.  H_noise is the STIMULUS-WEIGHTED average of
#'
#' the per-stimulus entropies; weighting them equally inflates I
#' whenever the classes are unbalanced.  Equal-COUNT bins, not
#' equal-width: on a skewed count distribution equal-width bins report I
#' near 0 regardless of the truth.  Strong et al. (1998); biased upward
#' at small n, no correction applied.  NOT in Schabenberger & Gotway.
#'
#' @param spike See Usage.
#' @param stim See Usage.
#' @param nbins Defaults to \code{2L}.
#' @return A list with \code{information}, \code{h_total}, \code{h_noise}, \code{n_stimuli}, \code{nbins}, \code{n_per_cell}, \code{bits}, \code{biased_upward_at_small_n}, \code{equal_count_bins}, \code{n}, \code{method}.
#' @export
SpikeInfo <- function(spike, stim, nbins = 2L) {
  # I = H(R) - H(R|S), bits.  H_noise is the STIMULUS-WEIGHTED average of
  # the per-stimulus entropies; weighting them equally inflates I whenever
  # the classes are unbalanced.  Equal-COUNT bins, not equal-width: on a
  # skewed count distribution equal-width bins report I near 0 regardless
  # of the truth.  Strong et al. (1998); biased upward at small n, no
  # correction applied.  NOT in Schabenberger & Gotway.
  r <- .morie_spx_chkv(spike, "spike")
  s <- .morie_spx_chkv(stim, "stim")
  n <- length(r)
  if (length(s) != n) stop("`spike` and `stim` must have the same length")
  if (n < 4L) stop("at least 4 trials are needed")
  nbins <- as.integer(nbins)
  if (nbins < 2L) stop("`nbins` must be at least 2")
  if (nbins > n) {
    stop(sprintf("`nbins` (%d) exceeds the number of trials (%d)", nbins, n))
  }
  si <- round(s)
  if (any(abs(s - si) > 1e-9)) stop("`stim` must hold integer class labels")
  keys <- sort(unique(si))
  if (length(keys) < 2L) stop("at least 2 stimulus classes are needed")
  srt <- sort(r)
  edges <- vapply(seq_len(nbins - 1L), function(b) {
    srt[round(n * b / nbins)]
  }, numeric(1))
  binof <- function(v) {
    for (b in seq_len(nbins - 1L)) if (v <= edges[b]) {
      return(b - 1L)
    }
    nbins - 1L
  }
  code <- vapply(r, binof, numeric(1))
  ent <- function(codes) {
    mm <- length(codes)
    h <- 0
    for (b in seq_len(nbins) - 1L) {
      cnt <- sum(codes == b)
      if (cnt) {
        p <- cnt / mm
        h <- h - p * log(p, 2)
      }
    }
    h
  }
  htot <- ent(code)
  hnoise <- 0
  for (cv in keys) {
    sub <- code[si == cv]
    hnoise <- hnoise + (length(sub) / n) * ent(sub)
  }
  list(
    information = htot - hnoise, h_total = htot, h_noise = hnoise,
    n_stimuli = length(keys), nbins = nbins,
    n_per_cell = n / (nbins * length(keys)), bits = TRUE,
    biased_upward_at_small_n = TRUE, equal_count_bins = TRUE, n = n,
    method = paste(
      "Direct-method spike-train information (Strong et al.",
      "1998), no bias correction; NOT in Schabenberger &",
      "Gotway"
    )
  )
}

#' Psi = E[ {g(A - delta | H) / g(A | H)} Y ] -- the density ratio at
#' the
#'
#' BACK-shifted exposure.  Forward-shifting is the sign error this
#' estimand invites.  Gaussian working model A|H ~ N(H\'gamma, tau^2),
#' so w = exp{(delta/tau^2)(A - H\'gamma - delta/2)}.  Diaz & van der
#' Laan (2012, 2018).  NOT in Schabenberger & Gotway.
#'
#' @param y See Usage.
#' @param a See Usage.
#' @param h See Usage.
#' @param delta Defaults to \code{1}.
#' @param trim Defaults to \code{NULL}.
#' @return A list with \code{psi}, \code{naive_mean}, \code{weights}, \code{max_weight}, \code{mean_weight}, \code{tau2}, \code{gamma}, \code{delta}, \code{weight_uses_back_shifted_density}, \code{gaussian_working_model}, \code{n}, \code{method}.
#' @export
ShiftInt <- function(y, a, h, delta = 1, trim = NULL) {
  # psi = E[ {g(A - delta | H) / g(A | H)} Y ] -- the density ratio at the
  # BACK-shifted exposure.  Forward-shifting is the sign error this
  # estimand invites.  Gaussian working model A|H ~ N(H'gamma, tau^2), so
  # w = exp{(delta/tau^2)(A - H'gamma - delta/2)}.  Diaz & van der Laan
  # (2012, 2018).  NOT in Schabenberger & Gotway.
  yv <- .morie_spx_chkv(y, "y")
  av <- .morie_spx_chkv(a, "a")
  n <- length(yv)
  if (length(av) != n) stop("`y` and `a` must have the same length")
  if (n < 4L) stop("at least 4 observations are needed")
  D <- if (is.null(h)) matrix(1, n, 1L) else cbind(1, as.matrix(h))
  if (nrow(D) != n) {
    stop(sprintf("`h` has %d rows but `y` has %d values", nrow(D), n))
  }
  k <- ncol(D)
  if (n <= k) stop("need more observations than covariates + 1")
  d <- as.numeric(delta)
  gam <- .morie_spx_lstsq(D, av)
  res <- av - .morie_spx_matvec(D, gam)
  tau2 <- .morie_fsum(res * res) / (n - k)
  if (tau2 <= 0) {
    stop("the exposure is perfectly predicted by `h`; no shift is identified")
  }
  w <- exp((d / tau2) * (res - 0.5 * d))
  if (!is.null(trim)) {
    cap <- as.numeric(trim)
    if (cap <= 0) stop("`trim` must be positive")
    w <- pmin(w, cap)
  }
  list(
    psi = .morie_fsum(w * yv) / n, naive_mean = .morie_fsum(yv) / n,
    weights = w, max_weight = max(w), mean_weight = .morie_fsum(w) / n,
    tau2 = tau2, gamma = gam, delta = d,
    weight_uses_back_shifted_density = TRUE,
    gaussian_working_model = TRUE, n = n,
    method = paste(
      "Shifted-intervention IPW psi =",
      "E[g(A-delta|H)/g(A|H) Y] with a Gaussian exposure",
      "model (Diaz & van der Laan 2012, 2018); NOT in",
      "Schabenberger & Gotway"
    )
  )
}

# pre-policy spellings kept as aliases
morie_schabenberger_autocorrelation_function <- SpAcf
morie_schabenberger_lisa <- LisaI
morie_schabenberger_mantel_test <- MantelM2
morie_schabenberger_mantel_standard <- MantelZ
morie_schabenberger_moran_i_residuals <- MoranRes
morie_schabenberger_pair_correlation <- Pcf
morie_spherical_variogram_model <- SphVario
# morie_spectral_density and morie_coherence are ALREADY TAKEN in this
# package (R/specf.R, exported in NAMESPACE; R/cohrc.R and R/rgcoh.R).
# Rebinding them here would silently change what those exports mean, so
# these two carry an sp_ qualifier instead.
morie_sp_spectral_density <- Pgram
morie_spectral_smoothed <- SmPgram
morie_spectral_radius <- SpecRad
morie_schabenberger_spatial_error_model <- SpErrMod
morie_spatial_concordance_kappa <- SpKappa
morie_spatial_cluster_lisa <- LisaClust
morie_spatial_detrending <- MedPolish
morie_shrinkage_predictor_level2 <- ShrinkPred
morie_sparse_vector <- SparseVector
morie_speculative_decoding <- SpecDec
morie_cross_spectrum <- CrossSpec
morie_sp_coherence <- MsCoh
morie_spectral_anomaly <- SpecAnom
morie_spectral_clustering <- SpecClust
morie_spatial_pca <- SpatialPca
morie_spatial_gams <- SpGam
morie_spike_information <- SpikeInfo
morie_spsm_shifted_intervention <- ShiftInt
