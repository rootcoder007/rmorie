# SPDX-License-Identifier: AGPL-3.0-or-later
# Private numeric helpers shared by the slice-s03 function files.
#
# This is the mirror of morie.fn._s03core on the Python side.  Every
# routine performs the same floating-point operations in the same order
# as its Python counterpart, which is what lets the three-way parity
# harness assert agreement at 1e-9 rather than at some looser tolerance.
#
# Two rules are enforced throughout:
#   * no pseudo-random numbers -- anything that would classically be a
#     draw is either supplied by the caller or replaced by a
#     deterministic low-discrepancy / fixed-index construction;
#   * no library linear algebra where a hand-written loop will do,
#     because LAPACK and a native kernel need not round identically.
#
# Nothing here is exported.

.s03vec <- function(x) {
  if (is.null(x)) return(numeric(0))
  as.numeric(unlist(x, use.names = FALSE))
}

.s03mat <- function(x) {
  if (is.null(x)) return(matrix(numeric(0), 0, 0))
  if (is.matrix(x)) {
    storage.mode(x) <- "double"
    return(x)
  }
  if (is.list(x)) {
    return(do.call(rbind, lapply(x, function(r) as.numeric(r))))
  }
  matrix(as.numeric(x), ncol = 1L)
}

.s03matmul <- function(A, B) {
  n <- nrow(A); k <- nrow(B); m <- ncol(B)
  out <- matrix(0, n, m)
  for (i in seq_len(n)) {
    for (j in seq_len(m)) {
      s <- 0
      for (p in seq_len(k)) s <- s + A[i, p] * B[p, j]
      out[i, j] <- s
    }
  }
  out
}

.s03matvec <- function(A, v) {
  n <- nrow(A); out <- numeric(n)
  for (i in seq_len(n)) {
    s <- 0
    for (p in seq_along(v)) s <- s + A[i, p] * v[p]
    out[i] <- s
  }
  out
}

.s03crossprod <- function(A) .s03matmul(t(A), A)

.s03chol <- function(A) {
  n <- nrow(A)
  L <- matrix(0, n, n)
  for (i in seq_len(n)) {
    for (j in seq_len(i)) {
      s <- 0
      if (j > 1L) for (p in seq_len(j - 1L)) s <- s + L[i, p] * L[j, p]
      if (i == j) {
        d <- A[i, i] - s
        L[i, j] <- if (d > 0) sqrt(d) else 0
      } else {
        L[i, j] <- if (L[j, j] != 0) (A[i, j] - s) / L[j, j] else 0
      }
    }
  }
  L
}

.s03cholsolve <- function(A, b) {
  n <- nrow(A)
  L <- .s03chol(A)
  y <- numeric(n)
  for (i in seq_len(n)) {
    s <- b[i]
    if (i > 1L) for (p in seq_len(i - 1L)) s <- s - L[i, p] * y[p]
    y[i] <- if (L[i, i] != 0) s / L[i, i] else 0
  }
  x <- numeric(n)
  for (i in seq(n, 1L)) {
    s <- y[i]
    if (i < n) for (p in seq(i + 1L, n)) s <- s - L[p, i] * x[p]
    x[i] <- if (L[i, i] != 0) s / L[i, i] else 0
  }
  x
}

.s03ridgesolve <- function(A, b, ridge = 1e-10) {
  n <- nrow(A)
  M <- A
  for (i in seq_len(n)) M[i, i] <- M[i, i] + ridge
  .s03cholsolve(M, b)
}

.s03lstsq <- function(X, y, ridge = 1e-10) {
  XtX <- .s03crossprod(X)
  Xty <- .s03matvec(t(X), y)
  .s03ridgesolve(XtX, Xty, ridge)
}

# Symmetric eigenproblem by cyclic Jacobi.  Values ascending; each vector
# sign-fixed so its largest-magnitude entry is positive, because the
# eigenproblem does not determine the sign and the arms must agree.
.s03jacobi <- function(A, sweeps = 60L) {
  n <- nrow(A)
  M <- matrix(as.numeric(A), n, n)
  V <- diag(1, n)
  for (sw in seq_len(sweeps)) {
    off <- 0
    for (i in seq_len(n)) {
      if (i < n) for (j in seq(i + 1L, n)) off <- off + M[i, j] * M[i, j]
    }
    if (off <= 1e-30) break
    if (n > 1L) for (p in seq_len(n - 1L)) {
      for (q in seq(p + 1L, n)) {
        if (abs(M[p, q]) <= 1e-300) next
        theta <- (M[q, q] - M[p, p]) / (2 * M[p, q])
        tt <- (if (theta >= 0) 1 else -1) / (abs(theta) + sqrt(theta * theta + 1))
        cc <- 1 / sqrt(tt * tt + 1)
        ss <- tt * cc
        for (k in seq_len(n)) {
          mkp <- M[k, p]; mkq <- M[k, q]
          M[k, p] <- cc * mkp - ss * mkq
          M[k, q] <- ss * mkp + cc * mkq
        }
        for (k in seq_len(n)) {
          mpk <- M[p, k]; mqk <- M[q, k]
          M[p, k] <- cc * mpk - ss * mqk
          M[q, k] <- ss * mpk + cc * mqk
        }
        for (k in seq_len(n)) {
          vkp <- V[k, p]; vkq <- V[k, q]
          V[k, p] <- cc * vkp - ss * vkq
          V[k, q] <- ss * vkp + cc * vkq
        }
      }
    }
  }
  vals <- diag(M)
  ord <- order(vals, seq_len(n))
  ev <- vals[ord]
  vecs <- V[, ord, drop = FALSE]
  for (j in seq_len(n)) {
    big <- 1L
    for (r in seq_len(n)) if (abs(vecs[r, j]) > abs(vecs[big, j]) + 1e-15) big <- r
    if (vecs[big, j] < 0) vecs[, j] <- -vecs[, j]
  }
  list(values = ev, vectors = vecs)
}

.s03sigmoid <- function(z) {
  if (z >= 0) 1 / (1 + exp(-z)) else { e <- exp(z); e / (1 + e) }
}

# Exact GELU, x * Phi(x) (Hendrycks and Gimpel 2016).
# erf(z/sqrt(2)) = 2 pnorm(z) - 1, so 0.5 z (1 + erf(z/sqrt 2)) = z Phi(z).
.s03gelu <- function(z) z * pnorm(z)

# Swish_beta(x) = x sigma(beta x) (Ramachandran et al. 2017).
.s03swish <- function(z, beta = 1) z * .s03sigmoid(beta * z)

.s03relu <- function(z) if (z > 0) z else 0

.s03softmax <- function(v) {
  if (length(v) == 0L) return(numeric(0))
  m <- max(v)
  e <- exp(v - m)
  s <- 0
  for (x in e) s <- s + x
  e / s
}

.s03logsumexp <- function(v) {
  if (length(v) == 0L) return(-Inf)
  m <- max(v)
  if (m == -Inf) return(m)
  s <- 0
  for (x in v) s <- s + exp(x - m)
  m + log(s)
}

.s03mean <- function(v) {
  n <- length(v)
  if (n == 0L) return(NaN)
  s <- 0
  for (x in v) s <- s + x
  s / n
}

.s03var <- function(v, ddof = 1L) {
  n <- length(v)
  if (n - ddof <= 0L) return(NaN)
  m <- .s03mean(v)
  s <- 0
  for (x in v) s <- s + (x - m) * (x - m)
  s / (n - ddof)
}

.s03sd <- function(v, ddof = 1L) sqrt(.s03var(v, ddof))

.s03median <- function(v) {
  s <- sort(v)
  n <- length(s)
  if (n == 0L) return(NaN)
  h <- n %/% 2L
  if (n %% 2L == 1L) s[h + 1L] else 0.5 * (s[h] + s[h + 1L])
}

.s03mad <- function(v, constant = 1.4826) {
  m <- .s03median(v)
  constant * .s03median(abs(v - m))
}

# Type-7 quantile, the default of R's quantile().
.s03quantile7 <- function(v, p) {
  s <- sort(v)
  n <- length(s)
  if (n == 0L) return(NaN)
  if (n == 1L) return(s[1L])
  h <- (n - 1) * p
  lo <- floor(h)
  hi <- if (lo + 1 < n) lo + 1 else n - 1
  s[lo + 1L] + (h - lo) * (s[hi + 1L] - s[lo + 1L])
}

.s03rank <- function(v) {
  n <- length(v)
  ord <- order(v, seq_len(n))
  r <- numeric(n)
  i <- 1L
  while (i <= n) {
    j <- i
    while (j + 1L <= n && v[ord[j + 1L]] == v[ord[i]]) j <- j + 1L
    avg <- (i - 1L + j - 1L) / 2 + 1
    for (k in seq(i, j)) r[ord[k]] <- avg
    i <- j + 1L
  }
  r
}

.s03corr <- function(x, y) {
  n <- length(x)
  if (n < 2L) return(NaN)
  mx <- .s03mean(x); my <- .s03mean(y)
  sxy <- 0; sxx <- 0; syy <- 0
  for (i in seq_len(n)) {
    dx <- x[i] - mx; dy <- y[i] - my
    sxy <- sxy + dx * dy; sxx <- sxx + dx * dx; syy <- syy + dy * dy
  }
  d <- sqrt(sxx * syy)
  if (d > 0) sxy / d else NaN
}

# Van der Corput point -- the deterministic stand-in for a uniform draw.
.s03vdc <- function(i, base = 2L) {
  f <- 1; r <- 0
  k <- as.integer(i) + 1L
  while (k > 0L) {
    f <- f / base
    r <- r + f * (k %% base)
    k <- k %/% base
  }
  r
}

.s03unif <- function(n, base = 2L) vapply(seq_len(n) - 1L, .s03vdc, 0, base = base)

# R's qnorm IS Wichura AS 241, the same algorithm the Python arm codes.
.s03qnorm <- function(p) qnorm(p)

.s03pnorm <- function(z) pnorm(z)

.s03normdraws <- function(n, base = 2L) qnorm(.s03unif(n, base))

.s03lgamma <- function(x) lgamma(x)

# Same recurrence + asymptotic series as the Python arm, so the two agree
# term for term rather than relying on R's digamma matching a Python series.
.s03digamma <- function(x) {
  r <- 0
  while (x < 6) { r <- r - 1 / x; x <- x + 1 }
  f <- 1 / (x * x)
  r + log(x) - 0.5 / x +
    f * (-1 / 12 + f * (1 / 120 + f * (-1 / 252 + f * (1 / 240 + f * (-1 / 132)))))
}

# Modified Bessel K_nu(x), x > 0: series for small x, Hankel asymptotic
# for large x.  Written out rather than deferring to R's besselK so the
# Python mirror matches term for term.
.s03besselk <- function(nu, x, terms = 160L) {
  if (x <= 0) return(Inf)
  if (x < 2) {
    if (abs(nu - round(nu)) < 1e-12) nu <- round(nu) + 1e-8
    bessel_i <- function(order, z) {
      s <- 0
      for (k in seq_len(terms) - 1L) {
        s <- s + exp((2 * k + order) * log(z / 2) - lgamma(k + 1) - lgamma(k + order + 1))
      }
      s
    }
    return(0.5 * pi * (bessel_i(-nu, x) - bessel_i(nu, x)) / sin(nu * pi))
  }
  mu <- 4 * nu * nu
  term <- 1; s <- 1
  for (k in seq_len(23L)) {
    term <- term * (mu - (2 * k - 1)^2) / (8 * k * x)
    s <- s + term
  }
  sqrt(pi / (2 * x)) * exp(-x) * s
}
