# SPDX-License-Identifier: AGPL-3.0-or-later
# Private helpers shared by the Rousseeuw high-breakdown function files.
#
# This is the mirror of morie.fn._rousscore on the Python side.  Every routine
# performs the same floating-point operations in the same order as its Python
# counterpart, which is what lets the parity harness assert agreement at 1e-9.
# In particular the LU factorisation pivots on the same rule in both arms
# (largest magnitude, ties broken by the lowest row index), because a different
# pivot order gives a different last digit and the two arms would then disagree
# on a near-singular scatter matrix.  base::det and base::solve are deliberately
# NOT used here for that reason.
#
# Nothing here draws a random number.  Where the published algorithms say "draw
# random subsets", these helpers enumerate subsets in lexicographic order, so
# both arms visit the same candidates in the same sequence and land on the same
# optimum rather than merely on an optimum of the same quality.
#
# Nothing here is exported.

# Indices that sort v ascending, ties broken by the lower index.  Written out
# rather than delegated to order() because the C-step selects "the h smallest
# distances": which of two tied points is taken changes the subset.
#' Indices that sort v ascending, ties broken by the lower index.
#' Written out
#'
#' rather than delegated to order() because the C-step selects "the h
#' smallest distances": which of two tied points is taken changes the
#' subset.
#'
#' @param v A vector; its length is taken and its elements indexed.
#' @return The value of \code{idx}, as built in the body.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .rsosort(v = x)
#' res
.rsosort <- function(v) {
  idx <- seq_along(v)
  n <- length(idx)
  if (n < 2L) {
    return(idx)
  }
  for (i in 2:n) {
    j <- i
    while (j > 1L && (v[idx[j - 1L]] > v[idx[j]] ||
      (v[idx[j - 1L]] == v[idx[j]] && idx[j - 1L] > idx[j]))) {
      tmp <- idx[j - 1L]
      idx[j - 1L] <- idx[j]
      idx[j] <- tmp
      j <- j - 1L
    }
  }
  idx
}

# Relative tolerance for the pivot-is-zero test.  An EXACT test (bv == 0) was
# the original spelling and it was wrong in a way that mattered: when a subset
# of points lies exactly on a line its covariance is singular, but the
# elimination leaves a pivot of order 1e-13 rather than 0, so the matrix was
# declared non-singular, the "determinant" came out NEGATIVE (-1.6e-12 for a
# covariance matrix, which is impossible), and the Mahalanobis distances
# computed through it were noise.  Downstream the C-step then cycled with
# period three and its determinant chain INCREASED -- a direct violation of the
# Rousseeuw and Van Driessen theorem the iteration relies on.  The paper is
# explicit that |S| = 0 means the minimum is already attained and the iteration
# must stop, so detecting it correctly is not a nicety.
.RS_SINGULAR_TOL <- 1e-12

# LU with partial pivoting.  Returns list(M, piv, sign, singular).  Singularity
# is judged RELATIVE to the largest entry of A, not against exact zero.
#' LU with partial pivoting.  Returns list(M, piv, sign, singular).
#' Singularity
#'
#' is judged RELATIVE to the largest entry of A, not against exact zero.
#'
#' @param A A matrix; passed to \code{nrow}.
#' @return A list with \code{M}, \code{piv}, \code{sign}, \code{singular}.
#' @export
#' @examples
#' A <- matrix(c(4, 1, 0.5, 1, 3, 0.8, 0.5, 0.8, 2), nrow = 3)
#' res <- .rslufactor(A = A)
#' res
.rslufactor <- function(A) {
  n <- nrow(A)
  M <- matrix(as.numeric(A), n, n)
  amax <- 0
  for (i in seq_len(n)) for (j in seq_len(n)) if (abs(M[i, j]) > amax) amax <- abs(M[i, j])
  thresh <- .RS_SINGULAR_TOL * amax
  piv <- seq_len(n)
  sgn <- 1
  singular <- FALSE
  for (cc in seq_len(n)) {
    best <- cc
    bv <- abs(M[cc, cc])
    if (cc < n) {
      for (r in seq(cc + 1L, n)) {
        if (abs(M[r, cc]) > bv) {
          bv <- abs(M[r, cc])
          best <- r
        }
      }
    }
    if (bv <= thresh) {
      singular <- TRUE
      next
    }
    if (best != cc) {
      tmp <- M[cc, ]
      M[cc, ] <- M[best, ]
      M[best, ] <- tmp
      tp <- piv[cc]
      piv[cc] <- piv[best]
      piv[best] <- tp
      sgn <- -sgn
    }
    if (cc < n) {
      for (r in seq(cc + 1L, n)) {
        f <- M[r, cc] / M[cc, cc]
        M[r, cc] <- f
        if (cc < n) for (j in seq(cc + 1L, n)) M[r, j] <- M[r, j] - f * M[cc, j]
      }
    }
  }
  list(M = M, piv = piv, sign = sgn, singular = singular)
}

# Determinant of a COVARIANCE matrix, which cannot be negative.  .rsludet is a
# general determinant and may return a small negative number for a
# positive-semidefinite matrix that is singular to working precision.  Every
# objective in this shelf is minimised over such determinants, so an unclamped
# negative rounding artefact wins the minimisation outright and the search
# returns whichever subset happened to round furthest below zero.  Clamping at
# zero makes the comparison pick the FIRST exactly-degenerate subset instead,
# which is both correct and identical in the two language arms.
#' Determinant of a COVARIANCE matrix, which cannot be negative.
#' .rsludet is a
#'
#' general determinant and may return a small negative number for a
#' positive-semidefinite matrix that is singular to working precision.
#' Every objective in this shelf is minimised over such determinants, so
#' an unclamped negative rounding artefact wins the minimisation
#' outright and the search returns whichever subset happened to round
#' furthest below zero.  Clamping at zero makes the comparison pick the
#' FIRST exactly-degenerate subset instead, which is both correct and
#' identical in the two language arms.
#'
#' @param S Passed to \code{.rsludet}.
#' @return One of two values, depending on the branch taken.
#' @export
.rscovdet <- function(S) {
  d <- .rsludet(S)
  if (d > 0) d else 0
}

#' .rsludet
#'
#' A step of the helpers_rouss implementation. Called by \code{.rscovdet}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A A matrix; passed to \code{nrow}.
#' @return The value of \code{d}, as built in the body.
#' @export
#' @examples
#' A <- matrix(c(4, 1, 0.5, 1, 3, 0.8, 0.5, 0.8, 2), nrow = 3)
#' res <- .rsludet(A = A)
#' res
.rsludet <- function(A) {
  n <- nrow(A)
  if (n == 0L) {
    return(1)
  }
  f <- .rslufactor(A)
  if (f$singular) {
    return(0)
  }
  d <- f$sign
  for (i in seq_len(n)) d <- d * f$M[i, i]
  d
}

# Solve A x = b; NULL when A is singular.
#' Solve A x = b; NULL when A is singular
#'
#' A step of the helpers_rouss implementation. Called by \code{.rsltsfit}, \code{Lmsreg},
#' \code{Ltsreg} and 2 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A A matrix; passed to \code{nrow}.
#' @param b A vector; indexed elementwise.
#' @return The value of \code{x}, as built in the body.
#' @export
#' @examples
#' A <- matrix(c(4, 1, 0.5, 1, 3, 0.8, 0.5, 0.8, 2), nrow = 3)
#' b <- c(1.5, 2.5, 3.5)
#' res <- .rslusolve(A = A, b = b)
#' res
.rslusolve <- function(A, b) {
  n <- nrow(A)
  f <- .rslufactor(A)
  if (f$singular) {
    return(NULL)
  }
  M <- f$M
  piv <- f$piv
  y <- numeric(n)
  for (i in seq_len(n)) {
    s <- b[piv[i]]
    if (i > 1L) for (j in seq_len(i - 1L)) s <- s - M[i, j] * y[j]
    y[i] <- s
  }
  x <- numeric(n)
  for (i in seq(n, 1L)) {
    s <- y[i]
    if (i < n) for (j in seq(i + 1L, n)) s <- s - M[i, j] * x[j]
    x[i] <- s / M[i, i]
  }
  x
}

# Mean vector and covariance matrix (divisor |idx| - 1) of a subset.
#' Mean vector and covariance matrix (divisor |idx| - 1) of a subset
#'
#' A step of the helpers_rouss implementation. Called by \code{.rscstep}, \code{Fastm},
#' \code{Mcdcv} and 2 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; indexed by row and column.
#' @param idx A vector; its length is taken.
#' @return A list with \code{mu}, \code{S}.
#' @export
.rsmeancov <- function(X, idx) {
  m <- length(idx)
  p <- ncol(X)
  mu <- numeric(p)
  for (i in idx) for (j in seq_len(p)) mu[j] <- mu[j] + X[i, j]
  mu <- mu / m
  S <- matrix(0, p, p)
  for (i in idx) {
    for (a in seq_len(p)) {
      da <- X[i, a] - mu[a]
      for (b in seq_len(p)) S[a, b] <- S[a, b] + da * (X[i, b] - mu[b])
    }
  }
  den <- if (m > 1L) m - 1 else 1
  S <- S / den
  list(mu = mu, S = S)
}

# Squared Mahalanobis distances of every row of X; NULL if S is singular.
#' Squared Mahalanobis distances of every row of X; NULL if S is
#' singular
#'
#' A step of the helpers_rouss implementation. Called by \code{.rscstep}, \code{Fastm},
#' \code{Mvedet}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; indexed by row and column.
#' @param mu A vector; its length is taken.
#' @param S Passed to \code{.rslufactor}.
#' @return The value of \code{out}, as built in the body.
#' @export
.rsmahal2 <- function(X, mu, S) {
  n <- nrow(X)
  p <- length(mu)
  f <- .rslufactor(S)
  if (f$singular) {
    return(NULL)
  }
  M <- f$M
  piv <- f$piv
  out <- numeric(n)
  for (i in seq_len(n)) {
    b <- X[i, ] - mu
    y <- numeric(p)
    for (a in seq_len(p)) {
      s <- b[piv[a]]
      if (a > 1L) for (j in seq_len(a - 1L)) s <- s - M[a, j] * y[j]
      y[a] <- s
    }
    z <- numeric(p)
    for (a in seq(p, 1L)) {
      s <- y[a]
      if (a < p) for (j in seq(a + 1L, p)) s <- s - M[a, j] * z[j]
      z[a] <- s / M[a, a]
    }
    d <- 0
    for (j in seq_len(p)) d <- d + b[j] * z[j]
    out[i] <- d
  }
  out
}

#' .rsnchoosek
#'
#' A step of the helpers_rouss implementation. Called by \code{.rscombosstride},
#' \code{Lmsreg}, \code{Ltsreg} and 2 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n Numeric; combined arithmetically in the body.
#' @param k Numeric; passed to \code{min}.
#' @return A numeric value.
#' @export
#' @examples
#' res <- .rsnchoosek(n = 3L, k = 3L)
#' res
.rsnchoosek <- function(n, k) {
  if (k < 0L || k > n) {
    return(0)
  }
  k <- min(k, n - k)
  r <- 1
  if (k > 0L) for (i in 0:(k - 1L)) r <- r * (n - i) / (i + 1)
  round(r)
}

# Lexicographic k-subsets of 1..n, at most cap of them.  Returns a list.
#' Lexicographic k-subsets of 1..n, at most cap of them.  Returns a list
#'
#' A step of the helpers_rouss implementation. Called by \code{.rscombosstride},
#' \code{Lmsreg}, \code{Ltsreg} and 2 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n Numeric; combined arithmetically in the body.
#' @param k A count; the body uses it as \code{seq_len(...)}.
#' @param cap Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @return The value of \code{repeat}.
#' @export
#' @examples
#' res <- .rscombos(n = 3L, k = 3L)
#' res
.rscombos <- function(n, k, cap = NULL) {
  out <- list()
  if (k > n || k < 0L) {
    return(out)
  }
  cvec <- seq_len(k)
  repeat {
    out[[length(out) + 1L]] <- cvec
    if (!is.null(cap) && length(out) >= cap) {
      return(out)
    }
    i <- k
    while (i >= 1L && cvec[i] == i + n - k) i <- i - 1L
    if (i < 1L) {
      return(out)
    }
    cvec[i] <- cvec[i] + 1L
    if (i < k) for (j in seq(i + 1L, k)) cvec[j] <- cvec[j - 1L] + 1L
  }
}

# One C-step of Rousseeuw and Van Driessen (1999).  Theorem, restated in
# Hubert, Debruyne and Rousseeuw (2018), arXiv 1709.07045, "COMPUTATION": from
# H1 of size h with mean mu1 and covariance S1, taking H2 to be the h
# observations with the smallest distances d(x_i, mu1, S1) gives |S2| <= |S1|,
# with equality iff mu2 = mu1 and S2 = S1.  The determinant therefore never
# increases, which is what makes the iteration terminate.
# Returns list(idx, det) or NULL when S1 is singular, in which case the
# objective is already zero and the subset lies on a hyperplane.
#' One C-step of Rousseeuw and Van Driessen (1999).  Theorem, restated
#' in
#'
#' Hubert, Debruyne and Rousseeuw (2018), arXiv 1709.07045,
#' "COMPUTATION": from H1 of size h with mean mu1 and covariance S1,
#' taking H2 to be the h observations with the smallest distances d(x_i,
#' mu1, S1) gives |S2| <= |S1|, with equality iff mu2 = mu1 and S2 = S1.
#' The determinant therefore never increases, which is what makes the
#' iteration terminate. Returns list(idx, det) or NULL when S1 is
#' singular, in which case the objective is already zero and the subset
#' lies on a hyperplane.
#'
#' @param X Passed to \code{.rsmeancov}.
#' @param idx Passed to \code{.rsmeancov}.
#' @param h A count; the body uses it as \code{seq_len(...)}.
#' @return A list with \code{idx}, \code{det}.
#' @export
.rscstep <- function(X, idx, h) {
  mc <- .rsmeancov(X, idx)
  d0 <- .rscovdet(mc$S)
  dd <- .rsmahal2(X, mc$mu, mc$S)
  if (is.null(dd)) {
    return(NULL)
  }
  ord <- .rsosort(dd)
  list(idx = sort(ord[seq_len(h)]), det = d0)
}

# The maximal-breakdown h of Rousseeuw (1984) Remark 1, [n/2] + [(p+1)/2].
#' The maximal-breakdown h of Rousseeuw (1984) Remark 1, \[n/2\] +
#' \[(p+1)/2\]
#'
#' A step of the helpers_rouss implementation. Called by \code{Ltsreg}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n Numeric; combined arithmetically in the body.
#' @param p Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
#' @examples
#' res <- .rstrimmedh(n = 3L, p = 0.5)
#' res
.rstrimmedh <- function(n, p) n %/% 2L + (p + 1L) %/% 2L

# The most robust MCD subset size, [(n + p + 1) / 2].
#' The most robust MCD subset size, \[(n + p + 1) / 2\]
#'
#' A step of the helpers_rouss implementation. Called by \code{Fastm}, \code{Mcdcv},
#' \code{Mcdv} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n Numeric; combined arithmetically in the body.
#' @param p Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
#' @examples
#' res <- .rsmcdh(n = 3L, p = 0.5)
#' res
.rsmcdh <- function(n, p) (n + p + 1L) %/% 2L

# Shortest window of h points in a sorted univariate sample.  Rousseeuw (1984)
# Theorem 2, p. 873: in one dimension the LMS location is the midpoint of the
# shortest half, found as the smallest of y_{h:n} - y_{1:n}, ...,
# y_{n:n} - y_{n-h+1:n}.  The same contiguity argument gives the univariate MCD
# and MVE subsets.  Returns list(start, width, sorted) with a 1-based start.
#' Shortest window of h points in a sorted univariate sample.  Rousseeuw
#' (1984)
#'
#' Theorem 2, p. 873: in one dimension the LMS location is the midpoint
#' of the shortest half, found as the smallest of y_\{h:n\} - y_\{1:n\},
#' ..., y_\{n:n\} - y_\{n-h+1:n\}.  The same contiguity argument gives the
#' univariate MCD and MVE subsets.  Returns list(start, width, sorted)
#' with a 1-based start.
#'
#' @param v Numeric; passed to \code{sort}.
#' @param h Numeric; combined arithmetically in the body.
#' @return A list with \code{start}, \code{width}, \code{sorted}.
#' @export
.rsshortesthalf <- function(v, h) {
  s <- sort(v)
  n <- length(s)
  best <- 1L
  bw <- s[h] - s[1]
  if (n > h) {
    for (a in 2:(n - h + 1L)) {
      w <- s[a + h - 1L] - s[a]
      if (w < bw) {
        bw <- w
        best <- a
      }
    }
  }
  list(start = best, width = bw, sorted = s)
}

# c0 = alpha / F_chi2_{p+2}(q_alpha), alpha = h/n, q_alpha the chi2_p quantile.
# Hubert, Debruyne and Rousseeuw (2018), arXiv 1709.07045, "Definition".
# Pchisq and Qchisq are the package's own native mirrors, not stats::.
#' C0 = alpha / F_chi2_\{p+2\}(q_alpha), alpha = h/n, q_alpha the chi2_p
#' quantile
#'
#' Hubert, Debruyne and Rousseeuw (2018), arXiv 1709.07045,
#' "Definition". Pchisq and Qchisq are the package\'s own native
#' mirrors, not stats::.
#'
#' @param h Numeric; combined arithmetically in the body.
#' @param n Numeric; combined arithmetically in the body.
#' @param p Numeric; combined arithmetically in the body.
#' @return One of two values, depending on the branch taken.
#' @export
#' @examples
#' res <- .rsconsistency(h = 0.5, n = 3L, p = 0.5)
#' res
.rsconsistency <- function(h, n, p) {
  alpha <- h / n
  if (alpha >= 1) {
    return(1)
  }
  q <- Qchisq(alpha, p)
  f <- Pchisq(q, p + 2)
  if (f > 0) alpha / f else 1
}

# med_i r_i^2, the objective of Rousseeuw (1984) equation (1.8).
#' Med_i r_i^2, the objective of Rousseeuw (1984) equation (1.8)
#'
#' A step of the helpers_rouss implementation. Called by \code{Lmsreg}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param r Numeric; combined arithmetically in the body.
#' @return One of two values, depending on the branch taken.
#' @export
.rsmedsq <- function(r) {
  sq <- sort(r * r)
  n <- length(sq)
  if (n %% 2L == 1L) sq[(n %/% 2L) + 1L] else 0.5 * (sq[n %/% 2L] + sq[n %/% 2L + 1L])
}

# Index of the first all-ones design column, or 0 when there is none.
#' Index of the first all-ones design column, or 0 when there is none
#'
#' A step of the helpers_rouss implementation. Called by \code{Lmsreg}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param Xm A matrix; indexed by row and column.
#' @param n A count; the body uses it as \code{seq_len(...)}.
#' @param p A count; the body uses it as \code{seq_len(...)}.
#' @return A numeric value.
#' @export
.rsintercept <- function(Xm, n, p) {
  for (j in seq_len(p)) {
    allone <- TRUE
    for (i in seq_len(n)) {
      if (Xm[i, j] != 1) {
        allone <- FALSE
        break
      }
    }
    if (allone) {
      return(j)
    }
  }
  0L
}

# Ordinary least squares on a subset, by the normal equations.
#' Ordinary least squares on a subset, by the normal equations
#'
#' A step of the helpers_rouss implementation. Called by \code{Ltsreg}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param Xm A matrix; indexed by row and column.
#' @param yy A vector; indexed elementwise.
#' @param idx See Usage.
#' @param p A count; the body uses it as \code{seq_len(...)}.
#' @return The value of \code{.rslusolve}.
#' @export
.rsltsfit <- function(Xm, yy, idx, p) {
  A <- matrix(0, p, p)
  b <- numeric(p)
  for (i in idx) {
    for (a in seq_len(p)) {
      for (cc in seq_len(p)) A[a, cc] <- A[a, cc] + Xm[i, a] * Xm[i, cc]
      b[a] <- b[a] + Xm[i, a] * yy[i]
    }
  }
  .rslusolve(A, b)
}

# The h smallest squared residuals, their sum, and their indices.
#' The h smallest squared residuals, their sum, and their indices
#'
#' A step of the helpers_rouss implementation. Called by \code{Ltsreg}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param Xm A matrix; indexed by row and column.
#' @param yy A vector; indexed elementwise.
#' @param th A vector; indexed elementwise.
#' @param n A count; the body uses it as \code{seq_len(...)}.
#' @param p A count; the body uses it as \code{seq_len(...)}.
#' @param h A count; the body uses it as \code{seq_len(...)}.
#' @return A list with \code{tot}, \code{idx}, \code{sq}.
#' @export
.rsltsobj <- function(Xm, yy, th, n, p, h) {
  sq <- numeric(n)
  for (i in seq_len(n)) {
    s <- yy[i]
    for (j in seq_len(p)) s <- s - th[j] * Xm[i, j]
    sq[i] <- s * s
  }
  ord <- .rsosort(sq)
  idx <- sort(ord[seq_len(h)])
  tot <- 0
  for (i in idx) tot <- tot + sq[i]
  list(tot = tot, idx = idx, sq = sq)
}

# `want` k-subsets spread evenly across the whole lexicographic order.
#
# This exists because of a defect a confusion matrix caught.  The published
# FastMCD draws its elemental subsets AT RANDOM; replacing that with "the first
# `want` subsets in lexicographic order" is not a neutral substitution, because
# the lexicographic prefix is drawn almost entirely from the LOWEST indices --
# the first 300 of the 1140 triples of 1..20 never mention an observation past
# index 10.  On a fixture whose outliers sat in the upper half, every start was
# seeded inside the clean block's neighbours, the C-steps converged on the wrong
# concentration, and the estimator excluded four clean points and kept all four
# outliers: TP=0 FP=4 TN=12 FN=4, a perfectly inverted decision that the
# determinant alone could not reveal.
#
# Striding keeps the determinism -- both arms visit the same subsets in the same
# order -- while spreading the seeds over the whole index range the way random
# draws would.
#' Striding keeps the determinism -- both arms visit the same subsets in
#' the same
#'
#' order -- while spreading the seeds over the whole index range the way
#' random draws would.
#'
#' @param n Numeric; combined arithmetically in the body.
#' @param k A count; the body uses it as \code{seq_len(...)}.
#' @param want Numeric; combined arithmetically in the body.
#' @param max_walk Passed to \code{>}. Defaults to \code{5e+06}.
#' @return The value of \code{repeat}.
#' @export
.rscombosstride <- function(n, k, want, max_walk = 5000000) {
  total <- .rsnchoosek(n, k)
  if (total == 0) {
    return(list())
  }
  if (total <= want) {
    return(.rscombos(n, k))
  }
  stride <- total %/% want
  out <- list()
  cvec <- seq_len(k)
  i <- 0
  walked <- 0
  repeat {
    if (i %% stride == 0) {
      out[[length(out) + 1L]] <- cvec
      if (length(out) >= want) {
        return(out)
      }
    }
    walked <- walked + 1
    if (walked > max_walk) {
      return(out)
    }
    j <- k
    while (j >= 1L && cvec[j] == j + n - k) j <- j - 1L
    if (j < 1L) {
      return(out)
    }
    cvec[j] <- cvec[j] + 1L
    if (j < k) for (m in seq(j + 1L, k)) cvec[m] <- cvec[m - 1L] + 1L
    i <- i + 1
  }
}
