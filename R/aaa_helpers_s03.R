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
# Native JSON, so the package does not call out for it. RFC 8259.
#
# jsonlite was the last runtime dependency doing real work in the R arms,
# which contradicted the package's own claim that nothing else is called at
# runtime. These cover the whole surface that was in use: parse, serialise,
# write, and newline-delimited streams.
#
# Two jsonlite behaviours are deliberately reproduced because call sites
# rely on them: a length-one vector serialises as a bare scalar when
# auto_unbox is TRUE, and a data.frame serialises row-wise as an array of
# objects. Two are deliberately NOT: no digits rounding by default (the
# full double is written, since silently shortening numbers is how parity
# gets lost), and no factor coercion.

.s03json_esc <- function(s) {
  s <- gsub("\\\\", "\\\\\\\\", s)
  s <- gsub("\"", "\\\\\"", s)
  s <- gsub("\b", "\\\\b", s, fixed = TRUE)
  s <- gsub("\f", "\\\\f", s, fixed = TRUE)
  s <- gsub("\n", "\\\\n", s, fixed = TRUE)
  s <- gsub("\r", "\\\\r", s, fixed = TRUE)
  s <- gsub("\t", "\\\\t", s, fixed = TRUE)
  # control characters must be \u escaped, not passed through
  # from 1, not 0: R strings cannot contain NUL, so rawToChar(as.raw(0))
  # is the empty string and gsub rejects a zero-length pattern
  for (cc in c(1:7, 11, 14:31)) {
    ch <- rawToChar(as.raw(cc))
    if (grepl(ch, s, fixed = TRUE))
      s <- gsub(ch, sprintf("\\\\u%04x", cc), s, fixed = TRUE)
  }
  s
}

.s03json_num <- function(x, digits = NULL) {
  if (is.na(x)) return("null")
  if (is.infinite(x)) return(if (x > 0) "1e999" else "-1e999")
  if (!is.null(digits)) x <- round(x, as.integer(digits))
  if (x == as.integer(round(x)) && abs(x) < 2147483647)
    return(format(as.integer(round(x))))
  if (!is.null(digits))
    return(format(x, nsmall = 0L, scientific = FALSE, trim = TRUE))
  # 17 significant digits round-trips a double exactly, which is what the
  # default has to be: parity is checked on the written value, and a
  # shortened number that still LOOKS right is the worst kind of wrong.
  format(x, digits = 17, scientific = FALSE, trim = TRUE)
}

.s03json_write_value <- function(x, auto_unbox = TRUE, digits = NULL) {
  if (is.null(x)) return("null")
  if (is.data.frame(x)) {
    rows <- vapply(seq_len(nrow(x)), function(i) {
      parts <- vapply(names(x), function(nm)
        paste0("\"", .s03json_esc(nm), "\":",
               .s03json_write_value(x[[nm]][i], TRUE, digits)), character(1))
      paste0("{", paste(parts, collapse = ","), "}")
    }, character(1))
    return(paste0("[", paste(rows, collapse = ","), "]"))
  }
  if (is.list(x)) {
    nm <- names(x)
    if (!is.null(nm) && all(nzchar(nm))) {
      parts <- vapply(seq_along(x), function(i)
        paste0("\"", .s03json_esc(nm[i]), "\":",
               .s03json_write_value(x[[i]], auto_unbox, digits)), character(1))
      return(paste0("{", paste(parts, collapse = ","), "}"))
    }
    parts <- vapply(x, function(v) .s03json_write_value(v, auto_unbox, digits),
                    character(1))
    return(paste0("[", paste(parts, collapse = ","), "]"))
  }
  if (is.matrix(x)) {
    rows <- vapply(seq_len(nrow(x)), function(i)
      .s03json_write_value(as.vector(x[i, ]), FALSE, digits), character(1))
    return(paste0("[", paste(rows, collapse = ","), "]"))
  }
  n <- length(x)
  one <- function(v) {
    if (is.na(v)) return("null")
    if (is.logical(v)) return(if (v) "true" else "false")
    if (is.numeric(v)) return(.s03json_num(as.numeric(v), digits))
    paste0("\"", .s03json_esc(as.character(v)), "\"")
  }
  if (n == 1L && auto_unbox) return(one(x))
  paste0("[", paste(vapply(seq_len(n), function(i) one(x[i]), character(1)),
                    collapse = ","), "]")
}

# Indent a compact JSON string. Structural braces/brackets and commas only
# -- anything inside a string literal is copied through untouched, so a
# comma in a value never becomes a line break.
.s03json_pretty <- function(txt, indent = 2L) {
  ch <- strsplit(txt, "", fixed = TRUE)[[1]]
  out <- character(0); depth <- 0L; instr <- FALSE; esc <- FALSE
  pad <- function(d) paste(rep(" ", d * indent), collapse = "")
  for (c0 in ch) {
    if (instr) {
      out <- c(out, c0)
      if (esc) esc <- FALSE
      else if (c0 == "\\") esc <- TRUE
      else if (c0 == "\"") instr <- FALSE
      next
    }
    if (c0 == "\"") { instr <- TRUE; out <- c(out, c0); next }
    if (c0 == "{" || c0 == "[") {
      depth <- depth + 1L
      out <- c(out, c0, "\n", pad(depth))
    } else if (c0 == "}" || c0 == "]") {
      depth <- depth - 1L
      out <- c(out, "\n", pad(depth), c0)
    } else if (c0 == ",") {
      out <- c(out, ",", "\n", pad(depth))
    } else if (c0 == ":") {
      out <- c(out, ": ")
    } else out <- c(out, c0)
  }
  paste(out, collapse = "")
}

# digits: NULL keeps every significant digit (the default, and the only
# choice that round-trips a double). A number rounds to that many DECIMAL
# places, which is what jsonlite::toJSON does -- pass digits = 4 to
# reproduce its default exactly.
.s03json_toJSON <- function(x, auto_unbox = TRUE, digits = NULL,
                            pretty = FALSE, ...) {
  s <- .s03json_write_value(x, auto_unbox, digits)
  if (isTRUE(pretty)) .s03json_pretty(s) else s
}

# ---- parser: recursive descent over the character vector
.s03json_parse <- function(txt) {
  s <- paste(txt, collapse = "\n")
  ch <- strsplit(s, "", fixed = TRUE)[[1]]
  n <- length(ch)
  i <- 1L

  skip <- function() {
    while (i <= n && (ch[i] == " " || ch[i] == "\t" || ch[i] == "\n" ||
                      ch[i] == "\r")) i <<- i + 1L
  }
  value <- NULL

  str_ <- function() {
    i <<- i + 1L                       # opening quote
    out <- character(0)
    while (i <= n && ch[i] != "\"") {
      if (ch[i] == "\\") {
        i <<- i + 1L
        e <- ch[i]
        out <- c(out,
                 if (e == "n") "\n" else if (e == "t") "\t"
                 else if (e == "r") "\r" else if (e == "b") "\b"
                 else if (e == "f") "\f"
                 else if (e == "u") {
                   hex <- paste(ch[(i + 1L):(i + 4L)], collapse = "")
                   i <<- i + 4L
                   intToUtf8(strtoi(hex, 16L))
                 } else e)
      } else out <- c(out, ch[i])
      i <<- i + 1L
    }
    i <<- i + 1L                       # closing quote
    paste(out, collapse = "")
  }

  num_ <- function() {
    st <- i
    while (i <= n && (grepl("[0-9+.eE-]", ch[i]))) i <<- i + 1L
    as.numeric(paste(ch[st:(i - 1L)], collapse = ""))
  }

  value <- function() {
    skip()
    if (i > n) return(NULL)
    c0 <- ch[i]
    if (c0 == "{") {
      i <<- i + 1L
      out <- list()
      skip()
      if (i <= n && ch[i] == "}") { i <<- i + 1L; return(out) }
      repeat {
        skip()
        key <- str_()
        skip()
        i <<- i + 1L                   # colon
        out[[key]] <- value()
        skip()
        if (i <= n && ch[i] == ",") { i <<- i + 1L; next }
        if (i <= n && ch[i] == "}") { i <<- i + 1L; break }
        break
      }
      return(out)
    }
    if (c0 == "[") {
      i <<- i + 1L
      out <- list()
      skip()
      if (i <= n && ch[i] == "]") { i <<- i + 1L; return(out) }
      repeat {
        out[[length(out) + 1L]] <- value()
        skip()
        if (i <= n && ch[i] == ",") { i <<- i + 1L; next }
        if (i <= n && ch[i] == "]") { i <<- i + 1L; break }
        break
      }
      # simplify a flat array of one atomic type, as jsonlite does
      if (length(out) && all(vapply(out, function(v)
          is.atomic(v) && length(v) == 1L, logical(1))))
        return(unlist(out))
      return(out)
    }
    if (c0 == "\"") return(str_())
    if (substr(s, i, i + 3L) == "true") { i <<- i + 4L; return(TRUE) }
    if (substr(s, i, i + 4L) == "false") { i <<- i + 5L; return(FALSE) }
    if (substr(s, i, i + 3L) == "null") { i <<- i + 4L; return(NULL) }
    num_()
  }

  value()
}

.s03json_fromJSON <- function(txt, ...) {
  if (length(txt) == 1L && !grepl("[{\\[]", substr(txt, 1L, 1L)) &&
      file.exists(txt))
    txt <- readLines(txt, warn = FALSE)
  .s03json_parse(txt)
}

.s03json_write <- function(x, path, auto_unbox = TRUE, digits = NULL,
                           pretty = FALSE, ...) {
  writeLines(.s03json_toJSON(x, auto_unbox, digits, pretty), path)
  invisible(path)
}

# newline-delimited JSON: one object per line
.s03json_stream_in <- function(con, ...) {
  lines <- if (inherits(con, "connection")) readLines(con, warn = FALSE)
           else readLines(con, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]
  lapply(lines, .s03json_parse)
}
