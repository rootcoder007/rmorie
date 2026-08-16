# Performer: FAVOR+ kernel attention with positive random features.
# Reference: Choromanski et al. (2021) "Rethinking Attention with
# Performers", arXiv:2009.14794.

.perfat_EPS <- 1e-9

.dot <- function(a, b) sum(a * b)
.norm2 <- function(a) sum(a * a)

#' draw_projections
#'
#' Part of the perfat_native implementation; see the file header for the
#' source it follows.
#'
#' @param m See Usage.
#' @param d See Usage.
#' @param seed Defaults to \code{0L}.
#' @param orthogonal Defaults to \code{TRUE}.
#' @return The value of \code{[}.
#' @export
draw_projections <- function(m, d, seed = 0L, orthogonal = TRUE) {
  m <- as.integer(m); d <- as.integer(d)
  if (m < 1L || d < 1L)
    stop(sprintf("perfat: need m >= 1 and d >= 1, got %d and %d", m, d))
  set.seed(as.integer(seed))
  rows <- matrix(stats::rnorm(m * d), nrow = m, ncol = d)
  if (!isTRUE(orthogonal)) return(rows)
  out <- matrix(0, nrow = m, ncol = d)
  i_out <- 1L
  for (start in seq(0L, m - 1L, by = d)) {
    end <- min(start + d, m)
    block <- rows[(start + 1L):end, , drop = FALSE]
    nb <- end - start
    basis <- matrix(0, nrow = nb, ncol = d)
    for (t in seq_len(nb)) {
      u <- block[t, ]
      for (b in seq_len(t - 1L)) {
        p <- .dot(u, basis[b, ])
        u <- u - p * basis[b, ]
      }
      nrm <- sqrt(.norm2(u))
      if (nrm < 1e-10) {
        basis[t, ] <- 0
        basis[t, min(t, d)] <- 1
        next
      }
      basis[t, ] <- u / nrm
    }
    for (t in seq_len(nb)) {
      length_ <- sqrt(.norm2(block[t, ]))
      out[i_out, ] <- basis[t, ] * length_
      i_out <- i_out + 1L
      if (i_out > m) break
    }
    if (i_out > m) break
  }
  out[seq_len(m), , drop = FALSE]
}

#' favor_features
#'
#' Part of the perfat_native implementation; see the file header for the
#' source it follows.
#'
#' @param X See Usage.
#' @param omegas See Usage.
#' @param kind Defaults to \code{"positive"}.
#' @param eps Defaults to \code{1e-06}.
#' @return The value of \code{out}, as built in the body.
#' @export
favor_features <- function(X, omegas, kind = "positive", eps = 1e-6) {
  if (!kind %in% c("positive", "trig"))
    stop(sprintf("perfat: kind must be positive or trig, got %s", kind))
  Xm <- as.matrix(X)
  m <- nrow(omegas)
  out <- matrix(0, nrow = nrow(Xm), ncol = if (kind == "positive") m else 2L * m)
  for (i in seq_len(nrow(Xm))) {
    x <- Xm[i, ]
    nx <- .norm2(x)
    proj <- as.numeric(omegas %*% x)
    if (kind == "positive") {
      mx <- if (length(proj)) max(proj) else 0
      scale <- exp(-0.5 * nx + mx) / sqrt(m)
      out[i, ] <- scale * exp(proj - mx) + eps
    } else {
      scale <- exp(0.5 * nx) / sqrt(m)
      out[i, seq_len(m)] <- scale * sin(proj)
      out[i, m + seq_len(m)] <- scale * cos(proj)
    }
  }
  out
}

#' kernel_estimate
#'
#' Part of the perfat_native implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param y See Usage.
#' @param omegas See Usage.
#' @param kind Defaults to \code{"positive"}.
#' @return The value of \code{.dot}.
#' @export
kernel_estimate <- function(x, y, omegas, kind = "positive") {
  f <- favor_features(rbind(x, y), omegas, kind = kind)
  .dot(f[1L, ], f[2L, ])
}

#' softmax_attention
#'
#' Part of the perfat_native implementation; see the file header for the
#' source it follows.
#'
#' @param Q See Usage.
#' @param K See Usage.
#' @param V See Usage.
#' @param causal Defaults to \code{FALSE}.
#' @return The value of \code{out}, as built in the body.
#' @export
softmax_attention <- function(Q, K, V, causal = FALSE) {
  Qm <- as.matrix(Q); Km <- as.matrix(K); Vm <- as.matrix(V)
  L <- nrow(Qm); d <- ncol(Qm); dv <- ncol(Vm)
  out <- matrix(0, nrow = L, ncol = dv)
  for (i in seq_len(L)) {
    lim <- if (isTRUE(causal)) i else L
    s <- as.numeric(Qm[i, ] %*% t(Km[seq_len(lim), , drop = FALSE]))
    mx <- max(s)
    w <- exp(s - mx)
    out[i, ] <- as.numeric(t(w) %*% Vm[seq_len(lim), , drop = FALSE]) / sum(w)
  }
  out
}

#' favor_attention
#'
#' Part of the perfat_native implementation; see the file header for the
#' source it follows.
#'
#' @param Q See Usage.
#' @param K See Usage.
#' @param V See Usage.
#' @param n_features Defaults to \code{128L}.
#' @param seed Defaults to \code{0L}.
#' @param kind Defaults to \code{"positive"}.
#' @param orthogonal Defaults to \code{TRUE}.
#' @param causal Defaults to \code{FALSE}.
#' @return A list with \code{estimate}, \code{output}, \code{n_features}, \code{kind}, \code{orthogonal}, \code{causal}, \code{L}, \code{d}, \code{d_v}, \code{method}.
#' @export
favor_attention <- function(Q, K, V, n_features = 128L, seed = 0L,
                            kind = "positive", orthogonal = TRUE,
                            causal = FALSE) {
  Qm <- as.matrix(Q); Km <- as.matrix(K); Vm <- as.matrix(V)
  L <- nrow(Qm); d <- ncol(Qm); dv <- ncol(Vm)
  if (nrow(Km) != nrow(Vm))
    stop(sprintf("perfat: %d keys but %d values", nrow(Km), nrow(Vm)))
  if (nrow(Km) != L && !isTRUE(causal))
    stop(sprintf("perfat: %d queries but %d keys", L, nrow(Km)))
  if (d == 0L || ncol(Km) != d)
    stop("perfat: query and key dimensions differ")
  om <- draw_projections(as.integer(n_features), d, seed = seed,
                         orthogonal = orthogonal)
  Qf <- favor_features(Qm, om, kind = kind)
  Kf <- favor_features(Km, om, kind = kind)
  mf <- ncol(Qf)
  out <- matrix(0, nrow = L, ncol = dv)
  if (!isTRUE(causal)) {
    KV <- t(Kf) %*% Vm
    Ksum <- colSums(Kf)
    for (i in seq_len(L)) {
      num <- as.numeric(Qf[i, , drop = FALSE] %*% KV)
      den <- .dot(Qf[i, ], Ksum)
      if (abs(den) < .perfat_EPS)
        stop(sprintf("perfat: a renormaliser vanished at query %d; this is what the trig map does and Lemma 1 prevents", i))
      out[i, ] <- num / den
    }
  } else {
    KV <- matrix(0, nrow = mf, ncol = dv)
    Ksum <- numeric(mf)
    for (i in seq_len(L)) {
      for (a in seq_len(mf)) {
        Ksum[a] <- Ksum[a] + Kf[i, a]
        for (c in seq_len(dv))
          KV[a, c] <- KV[a, c] + Kf[i, a] * Vm[i, c]
      }
      num <- as.numeric(Qf[i, , drop = FALSE] %*% KV)
      den <- .dot(Qf[i, ], Ksum)
      if (abs(den) < .perfat_EPS)
        stop(sprintf("perfat: a renormaliser vanished at query %d", i))
      out[i, ] <- num / den
    }
  }
  list(estimate = out, output = out,
       n_features = as.integer(n_features), kind = kind,
       orthogonal = isTRUE(orthogonal), causal = isTRUE(causal),
       L = L, d = d, d_v = dv,
       method = "FAVOR+ linear attention, Choromanski et al. (2021) Lemma 1")
}

favorattention <- favor_attention
performer_favor_attention <- favor_attention

# house entry point: the package exports one morie_<module>
morie_perfat <- favor_attention
