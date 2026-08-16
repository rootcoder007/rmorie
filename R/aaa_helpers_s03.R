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

#' .s03vec
#'
#' A step of the helpers_s03 implementation. Called by \code{.bkw_influence}, \code{.ch_ols_se}, \code{.icc_balanced} and 343 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x See Usage.
#' @return A vector, from \code{as.numeric}.
#' @export
.s03vec <- function(x) {
  if (is.null(x)) return(numeric(0))
  as.numeric(unlist(x, use.names = FALSE))
}

#' .s03mat
#'
#' A step of the helpers_s03 implementation. Called by \code{.bkw_influence}, \code{.cfa_cov}, \code{.ch_ols_se} and 236 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Optional; may be \code{NULL}. A matrix; the body checks with \code{is.matrix}.
#' @return A matrix, from \code{matrix}.
#' @export
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

#' .s03matmul
#'
#' A step of the helpers_s03 implementation. Called by \code{.s03crossprod}, \code{Fevdc}, \code{Fnlm} and 11 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A A matrix; indexed by row and column.
#' @param B A matrix; indexed by row and column.
#' @return The value of \code{out}, as built in the body.
#' @export
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

#' .s03matvec
#'
#' A step of the helpers_s03 implementation. Called by \code{.bkw_influence}, \code{.ch_ols_se}, \code{.jnt_lmm_ri} and 36 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A A matrix; indexed by row and column.
#' @param v A vector; its length is taken and its elements indexed.
#' @return The value of \code{out}, as built in the body.
#' @export
.s03matvec <- function(A, v) {
  n <- nrow(A); out <- numeric(n)
  for (i in seq_len(n)) {
    s <- 0
    for (p in seq_along(v)) s <- s + A[i, p] * v[p]
    out[i] <- s
  }
  out
}

#' .s03crossprod
#'
#' A step of the helpers_s03 implementation. Called by \code{.bkw_influence}, \code{.btres_xtxinv}, \code{.ch_ols_se} and 7 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A A matrix; passed to \code{t}.
#' @return The value of \code{.s03matmul}.
#' @export
.s03crossprod <- function(A) .s03matmul(t(A), A)

#' .s03chol
#'
#' A step of the helpers_s03 implementation. Called by \code{.cfa_logdet}, \code{.s03cholsolve}, \code{Fevdc} and 14 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A A matrix; indexed by row and column.
#' @return The value of \code{L}, as built in the body.
#' @export
.s03chol <- function(A) {
  n <- nrow(A)
  L <- matrix(0, n, n)
  for (i in seq_len(n)) {
    for (j in seq_len(i)) {
      s <- 0
      if (j > 1L) for (p in seq_len(j - 1L)) s <- s + L[i, p] * L[j, p]
      if (i == j) {
        d <- A[i, i] - s
        if (!(d > 0)) {
          # A Cholesky factor exists only for a positive-definite matrix.
          # Returning 0 here used to make the solve hand back the ZERO
          # VECTOR, which a Newton step on a log-likelihood Hessian
          # (negative definite) accepted silently: beta stayed at 0 and
          # three-way parity passed at 1e-9 because both arms did the same
          # thing. Solve against -H, or ridge the matrix into positive
          # definiteness, but do not accept a wrong answer.
          stop(sprintf(paste("chol: matrix is not positive definite",
                             "(pivot %d is %.17g); a Cholesky factor does",
                             "not exist. If this is a log-likelihood",
                             "Hessian, solve against -H."), i, d),
               call. = FALSE)
        }
        L[i, j] <- sqrt(d)
      } else {
        L[i, j] <- if (L[j, j] != 0) (A[i, j] - s) / L[j, j] else 0
      }
    }
  }
  L
}

#' .s03cholsolve
#'
#' A step of the helpers_s03 implementation. Called by \code{.bkw_influence}, \code{.cfa_inv}, \code{.ch_ols_se} and 35 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A A matrix; passed to \code{nrow}.
#' @param b A vector; indexed elementwise.
#' @return The value of \code{x}, as built in the body.
#' @export
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

#' .s03ridgesolve
#'
#' A step of the helpers_s03 implementation. Called by \code{.btres_xtxinv}, \code{.cfa_em}, \code{.htprd_ridge_cv} and 25 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A A matrix; passed to \code{nrow}.
#' @param b Passed to \code{.s03cholsolve}.
#' @param ridge Numeric; combined arithmetically in the body. Defaults to \code{1e-10}.
#' @return The value of \code{.s03cholsolve}.
#' @export
.s03ridgesolve <- function(A, b, ridge = 1e-10) {
  n <- nrow(A)
  M <- A
  for (i in seq_len(n)) M[i, i] <- M[i, i] + ridge
  .s03cholsolve(M, b)
}

#' .s03lstsq
#'
#' A step of the helpers_s03 implementation. Called by \code{.btnpqr_fit}, \code{.btsieve_arfit}, \code{.dssoot_ols} and 36 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; passed to \code{t}.
#' @param y Passed to \code{.s03matvec}.
#' @param ridge Passed to \code{.s03ridgesolve}. Defaults to \code{1e-10}.
#' @return The value of \code{.s03ridgesolve}.
#' @export
.s03lstsq <- function(X, y, ridge = 1e-10) {
  XtX <- .s03crossprod(X)
  Xty <- .s03matvec(t(X), y)
  .s03ridgesolve(XtX, Xty, ridge)
}

# Symmetric eigenproblem by cyclic Jacobi.  Values ascending; each vector
# sign-fixed so its largest-magnitude entry is positive, because the
# eigenproblem does not determine the sign and the arms must agree.
#' Symmetric eigenproblem by cyclic Jacobi.  Values ascending; each
#' vector
#'
#' sign-fixed so its largest-magnitude entry is positive, because the
#' eigenproblem does not determine the sign and the arms must agree.
#'
#' @param A A matrix; passed to \code{nrow}.
#' @param sweeps A count; the body uses it as \code{seq_len(...)}. Defaults to \code{60L}.
#' @return A list with \code{values}, \code{vectors}.
#' @export
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

#' .s03sigmoid
#'
#' A step of the helpers_s03 implementation. Called by \code{.dnnact}, \code{.dw_skipgram}, \code{.s03swish} and 21 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param z Numeric; passed to \code{exp}.
#' @return One of two values, depending on the branch taken.
#' @export
.s03sigmoid <- function(z) {
  if (z >= 0) 1 / (1 + exp(-z)) else { e <- exp(z); e / (1 + e) }
}

# Exact GELU, x * Phi(x) (Hendrycks and Gimpel 2016).
# erf(z/sqrt(2)) = 2 pnorm(z) - 1, so 0.5 z (1 + erf(z/sqrt 2)) = z Phi(z).
#' Exact GELU, x * Phi(x) (Hendrycks and Gimpel 2016)
#'
#' erf(z/sqrt(2)) = 2 pnorm(z) - 1, so 0.5 z (1 + erf(z/sqrt 2)) = z
#' Phi(z).
#'
#' @param z Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.s03gelu <- function(z) z * pnorm(z)

# Swish_beta(x) = x sigma(beta x) (Ramachandran et al. 2017).
#' Swish_beta(x) = x sigma(beta x) (Ramachandran et al. 2017)
#'
#' A step of the helpers_s03 implementation. Called by \code{Llamablock}, \code{Mbconv}, \code{Swiglu}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param z Numeric; combined arithmetically in the body.
#' @param beta Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @return A numeric value.
#' @export
.s03swish <- function(z, beta = 1) z * .s03sigmoid(beta * z)

#' .s03relu
#'
#' A step of the helpers_s03 implementation. Called by \code{Autoint}, \code{DeepF}, \code{Reglu}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param z See Usage.
#' @return One of two values, depending on the branch taken.
#' @export
.s03relu <- function(z) if (z > 0) z else 0

#' .s03softmax
#'
#' A step of the helpers_s03 implementation. Called by \code{Autoint}, \code{Bertrec}, \code{Deitkd} and 7 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v A vector; its length is taken.
#' @return A numeric value.
#' @export
.s03softmax <- function(v) {
  if (length(v) == 0L) return(numeric(0))
  m <- max(v)
  e <- exp(v - m)
  s <- 0
  for (x in e) s <- s + x
  e / s
}

#' .s03logsumexp
#'
#' A step of the helpers_s03 implementation. Called by \code{Dpgmm}, \code{Hdpgmm}, \code{Hdplda} and 4 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v A vector; its length is taken.
#' @return A numeric value.
#' @export
.s03logsumexp <- function(v) {
  if (length(v) == 0L) return(-Inf)
  m <- max(v)
  if (m == -Inf) return(m)
  s <- 0
  for (x in v) s <- s + exp(x - m)
  m + log(s)
}

#' .s03mean
#'
#' A step of the helpers_s03 implementation. Called by \code{.btsieve_arfit}, \code{.s03corr}, \code{.s03var} and 42 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v A vector; its length is taken.
#' @return A numeric value.
#' @export
.s03mean <- function(v) {
  n <- length(v)
  if (n == 0L) return(NaN)
  s <- 0
  for (x in v) s <- s + x
  s / n
}

#' .s03var
#'
#' A step of the helpers_s03 implementation. Called by \code{.s03sd}, \code{Btsubs}, \code{Dic} and 3 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v A vector; its length is taken.
#' @param ddof Numeric; combined arithmetically in the body. Defaults to \code{1L}.
#' @return A numeric value.
#' @export
.s03var <- function(v, ddof = 1L) {
  n <- length(v)
  if (n - ddof <= 0L) return(NaN)
  m <- .s03mean(v)
  s <- 0
  for (x in v) s <- s + (x - m) * (x - m)
  s / (n - ddof)
}

#' .s03sd
#'
#' A step of the helpers_s03 implementation. Called by \code{Btbayes}, \code{Btcbb}, \code{Btcicor} and 28 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v Passed to \code{.s03var}.
#' @param ddof Passed to \code{.s03var}. Defaults to \code{1L}.
#' @return A numeric value.
#' @export
.s03sd <- function(v, ddof = 1L) sqrt(.s03var(v, ddof))

#' .s03median
#'
#' A step of the helpers_s03 implementation. Called by \code{.dnnheadweights}, \code{.s03mad}, \code{Epicur} and 4 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v Numeric; passed to \code{sort}.
#' @return One of two values, depending on the branch taken.
#' @export
.s03median <- function(v) {
  s <- sort(v)
  n <- length(s)
  if (n == 0L) return(NaN)
  h <- n %/% 2L
  if (n %% 2L == 1L) s[h + 1L] else 0.5 * (s[h] + s[h + 1L])
}

#' .s03mad
#'
#' A step of the helpers_s03 implementation. Called by \code{Irlsfn}, \code{Ogkcv}, \code{Ramsw}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v Numeric; combined arithmetically in the body.
#' @param constant Numeric; combined arithmetically in the body. Defaults to \code{1.4826}.
#' @return A numeric value.
#' @export
.s03mad <- function(v, constant = 1.4826) {
  m <- .s03median(v)
  constant * .s03median(abs(v - m))
}

# Type-7 quantile, the default of R's quantile().
#' Type-7 quantile, the default of R\'s quantile()
#'
#' A step of the helpers_s03 implementation. Called by \code{.cstat_uno}, \code{.dnnheadweights}, \code{.ot_quantiles} and 38 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v Numeric; passed to \code{sort}.
#' @param p Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
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

#' .s03rank
#'
#' A step of the helpers_s03 implementation. Called by \code{.hrz3_u01}, \code{CnsRos}, \code{Evangia} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v A vector; its length is taken and its elements indexed.
#' @return The value of \code{r}, as built in the body.
#' @export
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

#' .s03corr
#'
#' A step of the helpers_s03 implementation. Called by \code{Btcicor}, \code{Cv1gn}, \code{Hetero} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x A vector; its length is taken and its elements indexed.
#' @param y A vector; indexed elementwise.
#' @return One of two values, depending on the branch taken.
#' @export
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
#' Van der Corput point -- the deterministic stand-in for a uniform draw
#'
#' A step of the helpers_s03 implementation. Called by \code{.bt_counts}, \code{.rfcand}, \code{.s03mammen} and 22 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param i Coerced to integer by the body, with \code{as.integer}.
#' @param base Numeric; combined arithmetically in the body. Defaults to \code{2L}.
#' @return The value of \code{r}, as built in the body.
#' @export
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

#' .s03unif
#'
#' A step of the helpers_s03 implementation. Called by \code{.s03normdraws}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n A count; the body uses it as \code{seq_len(...)}.
#' @param base Iterated over elementwise, with \code{vapply}. Defaults to \code{2L}.
#' @return A vector, from \code{vapply}.
#' @export
.s03unif <- function(n, base = 2L) vapply(seq_len(n) - 1L, .s03vdc, 0, base = base)

# R's qnorm IS Wichura AS 241, the same algorithm the Python arm codes.
#' R\'s qnorm IS Wichura AS 241, the same algorithm the Python arm codes
#'
#' A step of the helpers_s03 implementation. Called by \code{.drbsze_tquant}, \code{Btbca}, \code{Btcicor} and 20 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p See Usage.
#' @return The value of \code{qnorm}.
#' @export
.s03qnorm <- function(p) qnorm(p)

#' .s03pnorm
#'
#' A step of the helpers_s03 implementation. Called by \code{.huber_k}, \code{Augmn}, \code{Btbca} and 10 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param z See Usage.
#' @return The value of \code{pnorm}.
#' @export
.s03pnorm <- function(z) pnorm(z)

#' .s03normdraws
#'
#' A step of the helpers_s03 implementation. Called by \code{.ot_directions}, \code{.vitdraw}, \code{MedCI} and 2 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n Passed to \code{.s03unif}.
#' @param base Passed to \code{.s03unif}. Defaults to \code{2L}.
#' @return The value of \code{qnorm}.
#' @export
.s03normdraws <- function(n, base = 2L) qnorm(.s03unif(n, base))

#' .s03lgamma
#'
#' A step of the helpers_s03 implementation. Called by \code{.sgflrt_corr}, \code{Vbnpc}, \code{Vinfer}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x See Usage.
#' @return The value of \code{lgamma}.
#' @export
.s03lgamma <- function(x) lgamma(x)

# Same recurrence + asymptotic series as the Python arm, so the two agree
# term for term rather than relying on R's digamma matching a Python series.
#' Same recurrence + asymptotic series as the Python arm, so the two
#' agree
#'
#' term for term rather than relying on R\'s digamma matching a Python
#' series.
#'
#' @param x A vector; its length is taken.
#' @return A numeric value.
#' @export
.s03digamma <- function(x) {
  # Vectorised: the recurrence below is scalar (while (x < 6) on a vector
  # is an error in modern R), and this helper is SHARED, so every caller
  # that passes a vector -- lda, limmav and anything else using digamma --
  # broke on it.
  if (length(x) > 1L) return(vapply(x, .s03digamma, numeric(1)))
  r <- 0
  while (x < 6) { r <- r - 1 / x; x <- x + 1 }
  f <- 1 / (x * x)
  r + log(x) - 0.5 / x +
    f * (-1 / 12 + f * (1 / 120 + f * (-1 / 252 + f * (1 / 240 + f * (-1 / 132)))))
}

# Modified Bessel K_nu(x) for x > 0 and any real nu > 0, from the integral
# representation K_nu(x) = Int_0^inf exp(-x cosh t) cosh(nu t) dt (Watson
# 1944, section 6.22), by the trapezoidal rule on a fixed grid, step 0.01
# out to t = 25.  The integrand is smooth, even in t and exponentially
# decaying, so the trapezoidal rule converges geometrically; the grid is
# fixed rather than adaptive, and the sum is accumulated in an explicit
# loop rather than by sum(), so that the Python mirror performs the
# identical arithmetic in the identical order.  Each term is formed in
# logs and dropped once it underflows, which keeps cosh(nu t) from
# overflowing at large nu before exp(-x cosh t) has annihilated it.
#
# This replaced an earlier small-x branch that evaluated
# pi/2 (I_-nu - I_nu) / sin(nu pi).  That form has a removable pole at
# every integer nu, and stepping nu by 1e-8 to dodge it does not work:
# sin((1 + eps) pi) is NEGATIVE, so the old code returned K_1 with the
# wrong sign, and near the pole the I difference cancels to nothing.  The
# integral has no pole, no branch and no cancellation.  Checked against
# base R besselK at nu = 0.5, 1, 2, 2.5, 4, 12 and x from 0.001 to 6:
# 14 significant figures throughout.
#
# terms is retained for backward compatibility and is not used.
#' Terms is retained for backward compatibility and is not used
#'
#' A step of the helpers_s03 implementation. Called by \code{.s03maternk}, \code{.sgflrt_corr}, \code{Maternvg}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param nu Numeric; combined arithmetically in the body.
#' @param x Numeric; combined arithmetically in the body.
#' @param terms Accepted by the signature and not used anywhere in the body. Defaults to \code{160L}.
#' @return A numeric value.
#' @export
.s03besselk <- function(nu, x, terms = 160L) {
  if (x <= 0) return(Inf)
  h <- 0.01
  n <- 2500L
  s <- 0
  for (i in seq_len(n + 1L) - 1L) {
    t <- i * h
    az <- abs(nu * t)
    lg <- -x * cosh(t) + az + log1p(exp(-2 * az)) - log(2)
    term <- if (lg > -740) exp(lg) else 0
    s <- s + term * (if (i == 0L || i == n) 0.5 else 1)
  }
  s * h
}


# ---------------------------------------------- regression workhorses
#
# Written out rather than delegating to glm()/lm() so that the Python
# mirror performs the identical arithmetic in the identical order.

# Logistic regression by IRLS: Newton-Raphson on the log-likelihood,
# which for the canonical link is exactly IRLS,
# beta <- beta + (X' W X)^-1 X' (y - p), W = diag(p (1 - p)).
#' Logistic regression by IRLS: Newton-Raphson on the log-likelihood,
#'
#' which for the canonical link is exactly IRLS, beta <- beta + (X\' W
#' X)^-1 X\' (y - p), W = diag(p (1 - p)).
#'
#' @param X A matrix; indexed by row and column.
#' @param y A vector; indexed elementwise.
#' @param iters A count; the body uses it as \code{seq_len(...)}. Defaults to \code{60L}.
#' @param ridge Passed to \code{.s03ridgesolve}. Defaults to \code{1e-10}.
#' @param tol Defaults to \code{1e-13}.
#' @return The value of \code{beta}, as built in the body.
#' @export
.s03logit <- function(X, y, iters = 60L, ridge = 1e-10, tol = 1e-13) {
  n <- nrow(X); p <- ncol(X)
  beta <- numeric(p)
  for (it in seq_len(iters)) {
    eta <- .s03matvec(X, beta)
    mu <- vapply(eta, .s03sigmoid, 0)
    w <- mu * (1 - mu)
    XtWX <- matrix(0, p, p); Xtr <- numeric(p)
    for (i in seq_len(n)) {
      r <- y[i] - mu[i]
      for (a in seq_len(p)) {
        Xtr[a] <- Xtr[a] + X[i, a] * r
        for (b in seq_len(p)) XtWX[a, b] <- XtWX[a, b] + X[i, a] * w[i] * X[i, b]
      }
    }
    step <- .s03ridgesolve(XtWX, Xtr, ridge)
    mx <- 0
    for (a in seq_len(p)) {
      beta[a] <- beta[a] + step[a]
      if (abs(step[a]) > mx) mx <- abs(step[a])
    }
    if (mx < tol) break
  }
  beta
}

#' .s03design
#'
#' A step of the helpers_s03 implementation. Called by \code{.s03drdid}, \code{.s03tmle}, \code{.tmlcic_hier_cluster_arm} and 16 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X Optional; may be \code{NULL}. Passed to \code{.s03mat}.
#' @param n A count; the body uses it as \code{matrix(...)}.
#' @return The value of \code{cbind}.
#' @export
.s03design <- function(X, n) {
  if (is.null(X)) return(matrix(1, n, 1))
  rows <- .s03mat(X)
  if (nrow(rows) == 0L) return(matrix(1, n, 1))
  cbind(1, rows)
}

# Doubly robust DiD for panel data, Sant'Anna and Zhao (2020) eq. (2.6):
#   tau = E[(w1(D) - w0(D, X; pi)) (dY - mu_0(X))]
#   w1  = D / E[D]
#   w0  = [pi(X)(1-D)/(1-pi(X))] / E[pi(X)(1-D)/(1-pi(X))]
#' Doubly robust DiD for panel data, Sant\'Anna and Zhao (2020) eq.
#' (2.6):
#'
#' tau = E[(w1(D) - w0(D, X; pi)) (dY - mu_0(X))] w1 = D / E[D] w0 =
#' [pi(X)(1-D)/(1-pi(X))] / E[pi(X)(1-D)/(1-pi(X))]
#'
#' @param dy Passed to \code{.s03vec}.
#' @param D Passed to \code{.s03vec}.
#' @param X Passed to \code{.s03design}.
#' @param weights Optional; may be \code{NULL}. Passed to \code{.s03vec}.
#' @return A list with \code{tau}, \code{inf}, \code{se}, \code{pi}, \code{mu0}, \code{w1}, \code{w0}, \code{gamma}, \code{beta0}.
#' @export
.s03drdid <- function(dy, D, X = NULL, weights = NULL) {
  dyv <- .s03vec(dy); d <- .s03vec(D); n <- length(dyv)
  Z <- .s03design(X, n)
  w <- if (!is.null(weights)) .s03vec(weights) else rep(1, n)
  gam <- .s03logit(Z, d, 60L)
  # bounded away from 0 and 1: with a covariate that separates D the IRLS
  # fit diverges and 1 - pi underflows to zero, which is a positivity
  # violation, not an arithmetic accident.
  pi_ <- pmin(pmax(vapply(.s03matvec(Z, gam), .s03sigmoid, 0), 1e-12), 1 - 1e-12)
  keep <- which(d < 0.5)
  Z0 <- Z[keep, , drop = FALSE]; y0 <- dyv[keep]
  b0 <- if (length(keep)) .s03lstsq(Z0, y0) else numeric(ncol(Z))
  mu0 <- .s03matvec(Z, b0)
  s1 <- 0; s0 <- 0
  for (i in seq_len(n)) {
    s1 <- s1 + w[i] * d[i]
    s0 <- s0 + w[i] * pi_[i] * (1 - d[i]) / (1 - pi_[i])
  }
  w1 <- numeric(n); w0 <- numeric(n)
  for (i in seq_len(n)) {
    w1[i] <- if (s1 > 0) w[i] * d[i] / s1 else 0
    w0[i] <- if (s0 > 0) w[i] * pi_[i] * (1 - d[i]) / (1 - pi_[i]) / s0 else 0
  }
  tau <- 0
  for (i in seq_len(n)) tau <- tau + (w1[i] - w0[i]) * (dyv[i] - mu0[i])
  inf <- numeric(n)
  for (i in seq_len(n)) inf[i] <- n * (w1[i] - w0[i]) * (dyv[i] - mu0[i]) - tau
  v <- 0
  for (x in inf) v <- v + x * x
  se <- if (n) sqrt(v / (n * n)) else NaN
  list(tau = tau, inf = inf, se = se, pi = pi_, mu0 = mu0, w1 = w1, w0 = w0,
       gamma = gam, beta0 = b0)
}

# Mammen's two-point multiplier at a van der Corput point: mean 1,
# variance 1, third moment 1, and deterministic, so both arms agree.
#' Mammen\'s two-point multiplier at a van der Corput point: mean 1,
#'
#' variance 1, third moment 1, and deterministic, so both arms agree.
#'
#' @param i Passed to \code{.s03vdc}.
#' @return One of two values, depending on the branch taken.
#' @export
.s03mammen <- function(i) {
  r5 <- sqrt(5)
  p <- (r5 + 1) / (2 * r5)
  if (.s03vdc(i, 2L) < p) (1 - r5) / 2 else (1 + r5) / 2
}

# Targeted maximum likelihood for the ATE (van der Laan and Rubin 2006,
# Int. J. Biostatistics 2(1), art. 11).  The initial Qbar is fluctuated
# along the logistic submodel whose score is the clever covariate
# H = D/g - (1-D)/(1-g); eps solves the score equation by Newton, then
# psi = mean(Q*(1,X) - Q*(0,X)).  y is scaled to [0, 1] so the logistic
# fluctuation is valid for continuous outcomes (Gruber and van der Laan
# 2010).
#' Targeted maximum likelihood for the ATE (van der Laan and Rubin 2006,
#'
#' Int. J. Biostatistics 2(1), art. 11).  The initial Qbar is fluctuated
#' along the logistic submodel whose score is the clever covariate H =
#' D/g - (1-D)/(1-g); eps solves the score equation by Newton, then psi
#' = mean(Q*(1,X) - Q*(0,X)).  y is scaled to [0, 1] so the logistic
#' fluctuation is valid for continuous outcomes (Gruber and van der Laan
#' 2010).
#'
#' @param y Passed to \code{.s03vec}.
#' @param D Passed to \code{.s03vec}.
#' @param X Passed to \code{.s03design}.
#' @param trim Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.
#' @param link Accepted by the signature and not used anywhere in the body. Defaults to \code{"logit"}.
#' @return A list with \code{psi}, \code{se}, \code{eps}, \code{g}, \code{q1}, \code{q0}, \code{inf}, \code{ey1}, \code{ey0}, \code{scale}, \code{shift}.
#' @export
.s03tmle <- function(y, D, X = NULL, trim = 0, link = "logit") {
  yv <- .s03vec(y); d <- .s03vec(D); n <- length(yv)
  Z <- .s03design(X, n)
  g <- vapply(.s03matvec(Z, .s03logit(Z, d, 60L)), .s03sigmoid, 0)
  t <- as.numeric(trim)
  if (t > 0) g <- pmin(pmax(g, t), 1 - t)
  lo <- min(yv); hi <- max(yv)
  rng <- if (hi > lo) hi - lo else 1
  ys <- (yv - lo) / rng
  Q <- cbind(1, d, Z[, -1, drop = FALSE])
  bq <- .s03lstsq(Q, ys)
  q1 <- numeric(n); q0 <- numeric(n); qa <- numeric(n)
  for (i in seq_len(n)) {
    row1 <- c(1, 1, Z[i, -1]); row0 <- c(1, 0, Z[i, -1])
    s1 <- 0; s0 <- 0
    for (j in seq_along(bq)) { s1 <- s1 + bq[j] * row1[j]; s0 <- s0 + bq[j] * row0[j] }
    q1[i] <- min(max(s1, 1e-8), 1 - 1e-8)
    q0[i] <- min(max(s0, 1e-8), 1 - 1e-8)
    qa[i] <- if (d[i] > 0.5) q1[i] else q0[i]
  }
  H <- d / g - (1 - d) / (1 - g)
  eps <- 0
  for (it in seq_len(80L)) {
    num <- 0; den <- 0
    for (i in seq_len(n)) {
      z <- log(qa[i] / (1 - qa[i])) + eps * H[i]
      p <- .s03sigmoid(z)
      num <- num + H[i] * (ys[i] - p)
      den <- den + H[i] * H[i] * p * (1 - p)
    }
    if (den <= 0) break
    step <- num / den
    eps <- eps + step
    if (abs(step) < 1e-13) break
  }
  q1s <- numeric(n); q0s <- numeric(n)
  for (i in seq_len(n)) {
    q1s[i] <- .s03sigmoid(log(q1[i] / (1 - q1[i])) + eps / g[i])
    q0s[i] <- .s03sigmoid(log(q0[i] / (1 - q0[i])) - eps / (1 - g[i]))
  }
  psi_s <- 0
  for (i in seq_len(n)) psi_s <- psi_s + (q1s[i] - q0s[i]) / n
  psi <- psi_s * rng
  m1 <- 0; m0 <- 0
  for (i in seq_len(n)) { m1 <- m1 + q1s[i] / n; m0 <- m0 + q0s[i] / n }
  inf <- numeric(n)
  for (i in seq_len(n)) {
    qas <- if (d[i] > 0.5) q1s[i] else q0s[i]
    inf[i] <- rng * (H[i] * (ys[i] - qas) + (q1s[i] - q0s[i]) - psi_s)
  }
  v <- 0
  for (x in inf) v <- v + x * x
  se <- if (n) sqrt(v / (n * n)) else NaN
  list(psi = psi, se = se, eps = eps, g = g, q1 = q1s, q0 = q0s, inf = inf,
       ey1 = lo + rng * m1, ey0 = lo + rng * m0, scale = rng, shift = lo)
}
# ---------------------------------------------------------------- JSON
# The four functions the package calls, over the full mapping in
# jsonlt_native.R. That file is the implementation and the place to read
# about the options; these are the historical entry points.
#
# The defaults here are the ones the call sites were written against and
# differ from jsonlite's in two places on purpose. NULL serialises as
# null, not as {}. And an array of objects parses back as a list, not as
# a data.frame -- jsonlite simplifies by default, but 28 call sites here
# index the result as a list and simplification would silently change
# what they read.
#
# digits: NULL keeps every significant digit, which is the only choice
# that round-trips a double. A number rounds to that many DECIMAL places,
# reproducing jsonlite::toJSON exactly -- pass digits = 4 for its default.

#' .s03json_toJSON
#'
#' A step of the helpers_s03 implementation. Called by \code{.morie_to_json}, \code{.s03json_write}, \code{jsonlite_toJSON_or_stub} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Passed to \code{morie_jsonlt_to_json}.
#' @param auto_unbox Passed to \code{morie_jsonlt_to_json}. Defaults to \code{TRUE}.
#' @param digits Passed to \code{morie_jsonlt_to_json}.
#' @param pretty Passed to \code{morie_jsonlt_to_json}. Defaults to \code{FALSE}.
#' @param ... Passed through.
#' @return The value of \code{morie_jsonlt_to_json}.
#' @export
.s03json_toJSON <- function(x, auto_unbox = TRUE, digits = NULL,
                            pretty = FALSE, ...)
  morie_jsonlt_to_json(x, pretty = pretty, auto_unbox = auto_unbox,
                       digits = digits, na = "null", null = "null", ...)

#' .s03json_pretty
#'
#' A step of the helpers_s03 implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param txt Passed to \code{morie_jsonlt_prettify}.
#' @param indent Passed to \code{morie_jsonlt_prettify}. Defaults to \code{2L}.
#' @return The value of \code{morie_jsonlt_prettify}.
#' @export
.s03json_pretty <- function(txt, indent = 2L)
  morie_jsonlt_prettify(txt, indent)

#' .s03json_fromJSON
#'
#' A step of the helpers_s03 implementation. Called by \code{.morie_datasette_get_json}, \code{.morie_from_json}, \code{.siu_panel_extract} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param txt Passed to \code{morie_jsonlt_from_json}.
#' @param ... Passed through.
#' @return The value of \code{morie_jsonlt_from_json}.
#' @export
.s03json_fromJSON <- function(txt, ...)
  morie_jsonlt_from_json(txt, simplifyDataFrame = FALSE,
                         simplifyMatrix = FALSE)

#' .s03json_write
#'
#' A step of the helpers_s03 implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Passed to \code{.s03json_toJSON}.
#' @param path See Usage.
#' @param auto_unbox Passed to \code{.s03json_toJSON}. Defaults to \code{TRUE}.
#' @param digits Passed to \code{.s03json_toJSON}.
#' @param pretty Passed to \code{.s03json_toJSON}. Defaults to \code{FALSE}.
#' @param ... Passed through.
#' @return Invisibly,the value of \code{path}, as built in the body.
#' @export
.s03json_write <- function(x, path, auto_unbox = TRUE, digits = NULL,
                           pretty = FALSE, ...) {
  writeLines(.s03json_toJSON(x, auto_unbox, digits, pretty), path)
  invisible(path)
}

# newline-delimited JSON: one object per line
#' Newline-delimited JSON: one object per line
#'
#' A step of the helpers_s03 implementation. Called by \code{morie_dataset_load}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param con See Usage.
#' @param ... Passed through.
#' @return The value of \code{lapply}.
#' @export
.s03json_stream_in <- function(con, ...) {
  lines <- readLines(con, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]
  lapply(lines, .s03json_fromJSON)
}
