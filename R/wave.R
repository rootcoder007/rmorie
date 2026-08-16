# SPDX-License-Identifier: AGPL-3.0-or-later
#' Daubechies orthonormal wavelet basis for a periodic signal
#'
#' SOURCE. Daubechies, I. (1988), "Orthonormal bases of compactly
#' supported wavelets", Communications on Pure and Applied Mathematics
#' 41(7):909-996, doi:10.1002/cpa.3160410705.
#'
#' The scaling filter h of length L = 2N is what the paper constructs:
#' the compactly supported solution of the two-scale relation whose
#' defining conditions are sum h_k = sqrt(2), sum_k h_k h_{k+2m} =
#' delta_{m0}, and sum_k (-1)^k k^m h_k = 0 for m = 0 ... N-1. The
#' wavelet filter is the quadrature mirror g_k = (-1)^k h_{L-1-k}. Those
#' conditions determine h up to reflection and are checked numerically
#' here rather than taken on trust: \code{orthonormality},
#' \code{double_shift} and \code{vanishing_moments} are the residuals.
#'
#' FILTERS. db1 (Haar), db2 and db3, each from its exact radical closed
#' form rather than a decimal table: db1 h = (1,1)/sqrt(2); db2
#' h = (1+r3, 3+r3, 3-r3, 1-r3)/(4 sqrt(2)) with r3 = sqrt(3); db3
#' h = c/sqrt(2) with 16c = (1+a+b, 5+a+3b, 10-2a+2b, 10-2a-2b, 5+a-3b,
#' 1+a-b), a = sqrt(10), b = sqrt(5+2a). Longer filters have no compact
#' radical form and are NOT provided; that is this implementation's scope
#' choice, stated rather than attributed.
#'
#' BASIS. For length N = 2^J the periodic pyramid transform is
#' orthonormal; its matrix W is built by transforming the standard basis
#' vectors, so W W' = I is a real check on the pyramid code.
#'
#' @param y Signal of length 2^J, J at least 1.
#' @param wavelet "db1", "db2" or "db3".
#' @param level Pyramid depth; default the full J levels.
#' @return List with \code{basis}, \code{coefficients}, \code{h},
#'   \code{g}, \code{orthonormality}, \code{normalisation},
#'   \code{double_shift}, \code{vanishing_moments}, \code{gram_error},
#'   \code{energy_in}, \code{energy_out}, \code{n}, \code{level},
#'   \code{filter_length}.
#' @references Daubechies, I. (1988). Communications on Pure and Applied
#'   Mathematics 41(7):909-996. doi:10.1002/cpa.3160410705.
#' @examples
#' Wave(c(1, 2, 3, 4), "db1")$coefficients
#' @export
Wave <- function(y, wavelet = "db2", level = NULL) {
  x <- .s03vec(y)
  n <- length(x)
  J <- .dbpow2(n)
  if (J < 1L) stop("wavelet_basis: length of y must be a power of two, at least 2")
  h <- .dbfilter(wavelet)
  L <- length(h)
  g <- .dbmirror(h)
  lev <- if (is.null(level)) J else as.integer(level)
  if (is.na(lev) || lev < 1L || lev > J) {
    stop("wavelet_basis: level must lie in 1 .. log2(n)")
  }
  if (L > n %/% (2^(lev - 1L))) {
    stop("wavelet_basis: filter is longer than the coarsest level")
  }
  orth <- abs(sum(h * h) - 1)
  ds <- 0
  m <- 1L
  while (2L * m < L) {
    s <- 0
    for (k in seq_len(L - 2L * m)) s <- s + h[k] * h[k + 2L * m]
    ds <- max(ds, abs(s))
    m <- m + 1L
  }
  vm <- 0
  for (p in seq_len(L %/% 2L) - 1L) {
    s <- 0
    for (k in seq_len(L) - 1L) {
      s <- s + (-1)^k * (if (p > 0L) k^p else 1) * h[k + 1L]
    }
    vm <- max(vm, abs(s))
  }
  nrm <- abs(sum(h) - sqrt(2))
  Wt <- matrix(0, n, n)
  for (i in seq_len(n)) {
    e <- numeric(n)
    e[i] <- 1
    Wt[i, ] <- .dbforward(e, h, g, lev)
  }
  W <- t(Wt)
  gram <- 0
  for (i in seq_len(n)) for (j in seq_len(n)) {
    s <- 0
    for (k in seq_len(n)) s <- s + W[i, k] * W[j, k]
    gram <- max(gram, abs(s - (if (i == j) 1 else 0)))
  }
  co <- .s03matvec(W, x)
  .t1_result(estimate = sum(co * co), basis = W, coefficients = co, h = h,
             g = g, orthonormality = orth, normalisation = nrm,
             double_shift = ds, vanishing_moments = vm, gram_error = gram,
             energy_in = sum(x * x), energy_out = sum(co * co), n = n,
             level = lev, filter_length = L,
             method = paste("Daubechies (1988) orthonormal wavelet basis,",
                            "periodic pyramid"))
}

# Scaling filter from the exact radical closed form.
#' Scaling filter from the exact radical closed form
#'
#' A step of the wave implementation. Called by \code{Wave}, \code{Wvltdb}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param name Coerced to character by the body, with \code{as.character}.
#' @return Nothing; this branch always raises.
#' @export
.dbfilter <- function(name) {
  key <- tolower(trimws(as.character(name)[1L]))
  r2 <- sqrt(2)
  if (key %in% c("db1", "haar", "d2", "1")) return(c(1, 1) / r2)
  if (key %in% c("db2", "d4", "2")) {
    r3 <- sqrt(3)
    s <- 4 * r2
    return(c((1 + r3) / s, (3 + r3) / s, (3 - r3) / s, (1 - r3) / s))
  }
  if (key %in% c("db3", "d6", "3")) {
    a <- sqrt(10)
    b <- sqrt(5 + 2 * a)
    cc <- c((1 + a + b) / 16, (5 + a + 3 * b) / 16, (10 - 2 * a + 2 * b) / 16,
            (10 - 2 * a - 2 * b) / 16, (5 + a - 3 * b) / 16, (1 + a - b) / 16)
    return(cc / r2)
  }
  stop(sprintf("wavelet_basis: unknown wavelet '%s' (db1, db2, db3)", key))
}

#' .dbmirror
#'
#' A step of the wave implementation. Called by \code{Wave}, \code{Wvltdb}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param h A vector; its length is taken and its elements indexed.
#' @return The value of \code{out}, as built in the body.
#' @export
.dbmirror <- function(h) {
  L <- length(h)
  out <- numeric(L)
  for (k in seq_len(L) - 1L) out[k + 1L] <- (-1)^k * h[L - k]
  out
}

#' .dbstep
#'
#' A step of the wave implementation. Called by \code{.dbforward}, \code{Wvltdb}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param a A vector; its length is taken and its elements indexed.
#' @param h A vector; its length is taken and its elements indexed.
#' @param g A vector; indexed elementwise.
#' @return A list with \code{a}, \code{d}.
#' @export
.dbstep <- function(a, h, g) {
  n <- length(a)
  m <- n %/% 2L
  L <- length(h)
  ap <- numeric(m)
  de <- numeric(m)
  for (i in seq_len(m) - 1L) {
    sa <- 0
    sd <- 0
    for (k in seq_len(L) - 1L) {
      v <- a[(2L * i + k) %% n + 1L]
      sa <- sa + h[k + 1L] * v
      sd <- sd + g[k + 1L] * v
    }
    ap[i + 1L] <- sa
    de[i + 1L] <- sd
  }
  list(a = ap, d = de)
}

#' .dbforward
#'
#' A step of the wave implementation. Called by \code{Wave}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param x See Usage.
#' @param h Passed to \code{.dbstep}.
#' @param g Passed to \code{.dbstep}.
#' @param level A count; the body uses it as \code{seq_len(...)}.
#' @return The value of \code{out}, as built in the body.
#' @export
.dbforward <- function(x, h, g, level) {
  a <- x
  coeffs <- vector("list", level)
  for (j in seq_len(level)) {
    st <- .dbstep(a, h, g)
    a <- st$a
    coeffs[[j]] <- st$d
  }
  out <- a
  for (j in seq(level, 1L)) out <- c(out, coeffs[[j]])
  out
}

#' .dbpow2
#'
#' A step of the wave implementation. Called by \code{Wave}, \code{Wvltdb}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param n Passed to \code{>=}.
#' @return One of two values, depending on the branch taken.
#' @export
.dbpow2 <- function(n) {
  k <- 0L
  m <- n
  while (m > 1) {
    if (m %% 2 != 0) return(-1L)
    m <- m %/% 2
    k <- k + 1L
  }
  if (n >= 1) k else -1L
}
