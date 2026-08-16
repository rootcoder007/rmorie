# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Information-theory shelf -- R mirror of the Python modules acvent,
# cmutif, cndent, dpitst, jntent, kchnls, pcmpr1 (Cover & Thomas 2006)
# and aitsdv, crsent, mutinf, redund, shanen, surprl (Shannon 1948).
#
# Sources consulted, not recalled:
#   Cover, T.M. & Thomas, J.A. (2006). Elements of Information Theory,
#   2nd ed. Wiley.  Eq. (2.1), (2.10), (2.12), (2.15), (2.26), (2.28),
#   (2.60), (2.65), (2.67), (8.1); Theorems 2.8.1 and 5.4.3.
#   Shannon, C.E. (1948). A Mathematical Theory of Communication.
#   Bell System Technical Journal 27:379-423, 623-656.  Sections 6-7.
#
# Everything is closed form -- no RNG, no tolerance-driven early exit --
# so this arm reproduces the Python arm to machine precision.
#
# Collision scan: b2info.R and all thirteen exported names were free in
# both R trees (morie/r-package/morie and r-morie-oss) and in
# _lazy_map.json at the time of writing.

#' .b2logb
#'
#' Part of the b2info implementation; see the file header for the source
#' it follows.
#'
#' @param v See Usage.
#' @param base Defaults to \code{2}.
#' @return A numeric value.
#' @export
.b2logb <- function(v, base = 2) {
  out <- log(as.numeric(v))
  if (is.null(base)) return(out)
  b <- as.numeric(base)
  if (b <= 0 || b == 1) stop("base must be positive and not 1", call. = FALSE)
  out / log(b)
}

#' .b2pnorm
#'
#' Part of the b2info implementation; see the file header for the source
#' it follows.
#'
#' @param p See Usage.
#' @return A numeric value.
#' @export
.b2pnorm <- function(p) {
  p <- as.numeric(p)
  if (any(p < 0)) stop("probabilities must be non-negative", call. = FALSE)
  tot <- sum(p)
  if (!(tot > 0)) stop("total mass must be positive", call. = FALSE)
  p / tot
}

#' .b2xlogx
#'
#' Part of the b2info implementation; see the file header for the source
#' it follows.
#'
#' @param p See Usage.
#' @param base Defaults to \code{2}.
#' @return The value of \code{ifelse}.
#' @export
.b2xlogx <- function(p, base = 2) {
  p <- as.numeric(p)
  ifelse(p > 0, p * .b2logb(ifelse(p > 0, p, 1), base), 0)
}

#' .b2ent
#'
#' Part of the b2info implementation; see the file header for the source
#' it follows.
#'
#' @param p See Usage.
#' @param base Defaults to \code{2}.
#' @return A numeric value.
#' @export
.b2ent <- function(p, base = 2) -sum(.b2xlogx(p, base))

#' .b2kl
#'
#' Part of the b2info implementation; see the file header for the source
#' it follows.
#'
#' @param p See Usage.
#' @param q See Usage.
#' @param base Defaults to \code{2}.
#' @return A numeric value.
#' @export
.b2kl <- function(p, q, base = 2) {
  p <- as.numeric(p)
  q <- as.numeric(q)
  if (any(p > 0 & q <= 0)) return(Inf)
  sum(ifelse(p > 0,
             p * (.b2logb(ifelse(p > 0, p, 1), base) -
                    .b2logb(ifelse(q > 0, q, 1), base)), 0))
}

#' Differential entropy of a density given on a grid
#'
#' \deqn{h(X) = -\int f(x)\log f(x)\,dx}{h(X) = -int f(x) log f(x) dx}
#' Cover & Thomas (2006), eq. (8.1), p. 243.  Trapezoid rule on the
#' supplied grid -- a fixed node set, never adapted.
#'
#' @param density Non-negative density values at the grid points.
#' @param x Grid; defaults to `seq(0, 1, length.out = length(density))`.
#' @param base Log base; 2 gives bits, `NULL` gives nats.
#' @return Named list with `estimate`, `mass`, `n`, `base`, `method`.
#' @references Cover & Thomas (2006), eq. (8.1).
#' @examples
#' Difent(c(1, 1, 1))
#' @export
Difent <- function(density, x = NULL, base = 2) {
  f <- as.numeric(density)
  n <- length(f)
  if (n < 2) stop("density needs at least two grid points", call. = FALSE)
  grid <- if (is.null(x)) seq(0, 1, length.out = n) else as.numeric(x)
  if (length(grid) != n) stop("x and density must have the same length", call. = FALSE)
  if (any(f < 0)) stop("density must be non-negative", call. = FALSE)
  trap <- function(v, g) sum(diff(g) * (v[-length(v)] + v[-1]) / 2)
  list(estimate = trap(-.b2xlogx(f, base), grid), mass = trap(f, grid),
       n = n, base = base,
       method = "Differential entropy h(X) -- Cover & Thomas (2006) eq. (8.1)")
}

#' Conditional mutual information of a 3-D joint pmf
#'
#' \deqn{I(X;Y|Z) = H(X,Z) + H(Y,Z) - H(X,Y,Z) - H(Z)}
#' Cover & Thomas (2006), eq. (2.60)-(2.61), p. 23.
#'
#' @param pxyz Joint pmf as a 3-D array or nested list; normalised here.
#' @param base Log base; 2 gives bits.
#' @return Named list with `estimate`, `hxz`, `hyz`, `hxyz`, `hz`, `n`, `method`.
#' @references Cover & Thomas (2006), eq. (2.60).
#' @export
Cndmi <- function(pxyz, base = 2) {
  a <- .b2as3d(pxyz)
  a <- a / sum(a)
  hxyz <- .b2ent(as.numeric(a), base)
  hxz <- .b2ent(as.numeric(apply(a, c(1, 3), sum)), base)
  hyz <- .b2ent(as.numeric(apply(a, c(2, 3), sum)), base)
  hz <- .b2ent(as.numeric(apply(a, 3, sum)), base)
  list(estimate = hxz + hyz - hxyz - hz, hxz = hxz, hyz = hyz,
       hxyz = hxyz, hz = hz, n = length(a),
       method = "Conditional mutual information I(X;Y|Z) -- Cover & Thomas (2006) eq. (2.60)")
}

#' .b2as3d
#'
#' Part of the b2info implementation; see the file header for the source
#' it follows.
#'
#' @param p See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
.b2as3d <- function(p) {
  if (is.array(p) && length(dim(p)) == 3L) {
    storage.mode(p) <- "double"
    if (any(p < 0)) stop("pmf must be non-negative", call. = FALSE)
    if (!(sum(p) > 0)) stop("pmf must have positive total mass", call. = FALSE)
    return(p)
  }
  nx <- length(p)
  ny <- length(p[[1]])
  nz <- length(p[[1]][[1]])
  out <- array(0, dim = c(nx, ny, nz))
  for (i in seq_len(nx)) {
    if (length(p[[i]]) != ny) stop("ragged 3-D array", call. = FALSE)
    for (j in seq_len(ny)) {
      if (length(p[[i]][[j]]) != nz) stop("ragged 3-D array", call. = FALSE)
      out[i, j, ] <- as.numeric(p[[i]][[j]])
    }
  }
  if (any(out < 0)) stop("pmf must be non-negative", call. = FALSE)
  if (!(sum(out) > 0)) stop("pmf must have positive total mass", call. = FALSE)
  out
}

#' .b2as2d
#'
#' Part of the b2info implementation; see the file header for the source
#' it follows.
#'
#' @param p See Usage.
#' @return The value of \code{m}, as built in the body.
#' @export
.b2as2d <- function(p) {
  m <- if (is.matrix(p)) p else do.call(rbind, lapply(p, as.numeric))
  storage.mode(m) <- "double"
  if (any(m < 0)) stop("pmf must be non-negative", call. = FALSE)
  if (!(sum(m) > 0)) stop("pmf must have positive total mass", call. = FALSE)
  m
}

#' Conditional entropy of a 2-D joint pmf
#'
#' \deqn{H(Y|X) = H(X,Y) - H(X)}
#' Cover & Thomas (2006), eq. (2.10)/(2.12), p. 17.
#'
#' @param pxy Joint pmf as a matrix or list of rows; normalised here.
#' @param base Log base; 2 gives bits.
#' @return Named list with `estimate`, `hxy`, `hx`, `hy`, `n`, `method`.
#' @references Cover & Thomas (2006), eq. (2.12).
#' @examples
#' Cndent(matrix(c(0.5, 0.125, 0.25, 0.125), 2, 2))
#' @export
Cndent <- function(pxy, base = 2) {
  m <- .b2as2d(pxy)
  m <- m / sum(m)
  hxy <- .b2ent(as.numeric(m), base)
  hx <- .b2ent(rowSums(m), base)
  hy <- .b2ent(colSums(m), base)
  list(estimate = hxy - hx, hxy = hxy, hx = hx, hy = hy, n = length(m),
       method = "Conditional entropy H(Y|X) -- Cover & Thomas (2006) eq. (2.12)")
}

#' Joint entropy of a 2-D joint pmf
#'
#' \deqn{H(X,Y) = -\sum p(x,y)\log p(x,y)}{H(X,Y) = -sum p log p}
#' Cover & Thomas (2006), eq. (2.15), p. 17.
#'
#' @param pxy Joint pmf as a matrix or list of rows; normalised here.
#' @param base Log base; 2 gives bits.
#' @return Named list with `estimate`, `hx`, `hy`, `n`, `method`.
#' @references Cover & Thomas (2006), eq. (2.15).
#' @export
Jntent <- function(pxy, base = 2) {
  m <- .b2as2d(pxy)
  m <- m / sum(m)
  list(estimate = .b2ent(as.numeric(m), base), hx = .b2ent(rowSums(m), base),
       hy = .b2ent(colSums(m), base), n = length(m),
       method = "Joint entropy H(X,Y) -- Cover & Thomas (2006) eq. (2.15)")
}

#' Chain rule for relative entropy
#'
#' \deqn{D(p(x,y)\|q(x,y)) = D(p(x)\|q(x)) + D(p(y|x)\|q(y|x))}
#' Cover & Thomas (2006), Theorem 2.5.3, eq. (2.67), p. 24, with the
#' conditional relative entropy of eq. (2.65).
#'
#' @param pxy,qxy Joint pmfs of the same shape; each normalised here.
#' @param base Log base; 2 gives bits.
#' @return Named list with `estimate`, `marginal`, `conditional`,
#'   `residual`, `n`, `method`.  `residual` is zero up to rounding.
#' @references Cover & Thomas (2006), eq. (2.65), (2.67).
#' @export
Klchain <- function(pxy, qxy, base = 2) {
  p <- .b2as2d(pxy)
  q <- .b2as2d(qxy)
  if (!identical(dim(p), dim(q))) stop("pxy and qxy must have the same shape", call. = FALSE)
  p <- p / sum(p)
  q <- q / sum(q)
  joint <- .b2kl(as.numeric(p), as.numeric(q), base)
  px <- rowSums(p)
  qx <- rowSums(q)
  marginal <- .b2kl(px, qx, base)
  cond <- 0
  for (i in seq_len(nrow(p))) {
    if (px[i] <= 0) next
    if (qx[i] <= 0) { cond <- Inf; break }
    term <- .b2kl(p[i, ] / px[i], q[i, ] / qx[i], base)
    if (is.infinite(term)) { cond <- Inf; break }
    cond <- cond + px[i] * term
  }
  resid <- if (is.infinite(joint) || is.infinite(cond) || is.infinite(marginal)) {
    NaN
  } else {
    joint - marginal - cond
  }
  list(estimate = joint, marginal = marginal, conditional = cond,
       residual = resid, n = length(p),
       method = "Chain rule for relative entropy -- Cover & Thomas (2006) eq. (2.67)")
}

#' Expected code length of a predictive model (the wrong-code bound)
#'
#' \deqn{H(p) + D(p\|q) \le E_p\,l(X) < H(p) + D(p\|q) + 1}
#' Cover & Thomas (2006), Theorem 5.4.3, eq. (5.42), p. 115.
#'
#' @param model Predictive pmf over an alphabet of size K; normalised here.
#' @param data Observed symbols as 0-based indices into that alphabet
#'   (0-based, matching the Python arm).
#' @param base Log base; 2 gives bits per symbol.
#' @return Named list with `estimate`, `entropy`, `kl`, `upper`, `n`, `method`.
#' @references Cover & Thomas (2006), Theorem 5.4.3.
#' @export
Predcomp <- function(model, data, base = 2) {
  q <- .b2pnorm(model)
  k <- length(q)
  idx <- as.integer(data)
  n <- length(idx)
  if (n == 0L) stop("data must be non-empty", call. = FALSE)
  if (any(idx < 0L | idx >= k)) stop("data index outside the model alphabet", call. = FALSE)
  counts <- tabulate(idx + 1L, nbins = k)
  phat <- counts / n
  qi <- q[idx + 1L]
  rate <- if (any(qi <= 0)) Inf else -sum(.b2logb(qi, base)) / n
  hp <- .b2ent(phat, base)
  d <- .b2kl(phat, q, base)
  list(estimate = rate, entropy = hp, kl = d,
       upper = if (is.infinite(d)) Inf else hp + d + 1, n = n,
       method = "Wrong-code expected length H(p)+D(p||q) -- Cover & Thomas (2006) Thm 5.4.3")
}

#' Data-processing inequality for a Markov chain X -> Y -> Z
#'
#' \deqn{X \to Y \to Z \Rightarrow I(X;Y) \ge I(X;Z)}
#' Cover & Thomas (2006), Section 2.8, Theorem 2.8.1, p. 34.  A
#' deterministic identity, not a hypothesis test.
#'
#' @param pxyz Joint pmf as a 3-D array or nested list; normalised here.
#' @param cdf Accepted and ignored; kept so older call sites keep working.
#' @param base Log base; 2 gives bits.
#' @return Named list with `estimate` (the gap), `ixy`, `ixz`,
#'   `markov_gap`, `holds`, `n`, `method`.
#' @references Cover & Thomas (2006), Theorem 2.8.1.
#' @export
Dpineq <- function(pxyz, cdf = NULL, base = 2) {
  a <- .b2as3d(pxyz)
  a <- a / sum(a)
  hx <- .b2ent(as.numeric(apply(a, 1, sum)), base)
  hy <- .b2ent(as.numeric(apply(a, 2, sum)), base)
  hz <- .b2ent(as.numeric(apply(a, 3, sum)), base)
  hxy <- .b2ent(as.numeric(apply(a, c(1, 2), sum)), base)
  hxz <- .b2ent(as.numeric(apply(a, c(1, 3), sum)), base)
  hyz <- .b2ent(as.numeric(apply(a, c(2, 3), sum)), base)
  hxyz <- .b2ent(as.numeric(a), base)
  ixy <- hx + hy - hxy
  ixz <- hx + hz - hxz
  list(estimate = ixy - ixz, ixy = ixy, ixz = ixz,
       markov_gap = hxy + hyz - hxyz - hy, holds = (ixy - ixz >= -1e-12),
       n = length(a),
       method = "Data-processing inequality I(X;Y) >= I(X;Z) -- Cover & Thomas (2006) Thm 2.8.1")
}

#' Shannon entropy of a composition
#'
#' \deqn{H(x) = -\sum x_i \log x_i}{H(x) = -sum x_i log x_i}
#' Shannon (1948), Sections 6-7; Cover & Thomas (2006), eq. (2.1).  The
#' input is closed to unit total first and the closure constant is
#' reported, which is the only difference from `Shanent`.
#'
#' @param x Non-negative parts of a composition.
#' @param base Log base; 2 gives bits.
#' @return Named list with `estimate`, `closure`, `evenness`, `n`, `method`.
#' @references Shannon (1948), Sections 6-7.
#' @export
Compshan <- function(x, base = 2) {
  v <- as.numeric(x)
  n <- length(v)
  if (n < 1L) stop("x must be non-empty", call. = FALSE)
  closure <- sum(v)
  p <- .b2pnorm(v)
  h <- .b2ent(p, base)
  hmax <- .b2logb(n, base)
  list(estimate = h, closure = closure,
       evenness = if (hmax > 0) h / hmax else NaN, n = n,
       method = "Shannon entropy of a composition -- Shannon (1948) Sec. 6")
}

#' Cross entropy of p relative to q
#'
#' \deqn{-\sum p \log q = H(p) + D(p\|q)}{-sum p log q = H(p) + D(p||q)}
#' Cover & Thomas (2006), eq. (2.1) and (2.26).
#'
#' @param p,q Non-negative vectors of the same length; each closed here.
#' @param base Log base; 2 gives bits.
#' @return Named list with `estimate`, `entropy`, `kl`, `n`, `method`.
#' @references Cover & Thomas (2006), eq. (2.26).
#' @export
Crsent <- function(p, q, base = 2) {
  pv <- .b2pnorm(p)
  qv <- .b2pnorm(q)
  if (length(pv) != length(qv)) stop("p and q must have the same length", call. = FALSE)
  ce <- if (any(pv > 0 & qv <= 0)) {
    Inf
  } else {
    -sum(ifelse(pv > 0, pv * .b2logb(ifelse(qv > 0, qv, 1), base), 0))
  }
  h <- .b2ent(pv, base)
  list(estimate = ce, entropy = h, kl = if (is.infinite(ce)) Inf else ce - h,
       n = length(pv),
       method = "Cross entropy H(p) + D(p||q) -- Cover & Thomas (2006) eq. (2.26)")
}

#' Mutual information of a 2-D joint pmf
#'
#' \deqn{I(X;Y) = \sum p(x,y)\log\frac{p(x,y)}{p(x)p(y)}}{I(X;Y) = sum p log p/(pp)}
#' Cover & Thomas (2006), eq. (2.28)-(2.30), p. 20; Shannon (1948),
#' Section 12.
#'
#' @param pxy Joint pmf as a matrix or list of rows; normalised here.
#' @param base Log base; 2 gives bits.
#' @return Named list with `estimate`, `hx`, `hy`, `hxy`, `n`, `method`.
#' @references Cover & Thomas (2006), eq. (2.28).
#' @export
Mutinf <- function(pxy, base = 2) {
  m <- .b2as2d(pxy)
  m <- m / sum(m)
  hx <- .b2ent(rowSums(m), base)
  hy <- .b2ent(colSums(m), base)
  hxy <- .b2ent(as.numeric(m), base)
  list(estimate = hx + hy - hxy, hx = hx, hy = hy, hxy = hxy, n = length(m),
       method = "Mutual information I(X;Y) -- Cover & Thomas (2006) eq. (2.28)")
}

#' Redundancy of a source
#'
#' \deqn{R = 1 - H(X)/\log|A|}{R = 1 - H(X)/log|A|}
#' Shannon (1948), Section 7: "The ratio of the entropy of a source to
#' the maximum value it could have while still restricted to the same
#' symbols will be called its relative entropy ... One minus the
#' relative entropy is the redundancy."
#'
#' @param p Non-negative source pmf; closed to unit sum here.
#' @param base Log base for the reported entropies; the redundancy is a
#'   ratio and does not depend on it.
#' @return Named list with `estimate`, `entropy`, `hmax`, `relative`,
#'   `n`, `method`.
#' @references Shannon (1948), Section 7.
#' @examples
#' Redund(c(0.5, 0.5))
#' @export
Redund <- function(p, base = 2) {
  v <- .b2pnorm(p)
  n <- length(v)
  if (n < 2L) stop("redundancy needs an alphabet of at least two symbols", call. = FALSE)
  h <- .b2ent(v, base)
  hmax <- .b2logb(n, base)
  list(estimate = 1 - h / hmax, entropy = h, hmax = hmax,
       relative = h / hmax, n = n,
       method = "Redundancy 1 - H/Hmax -- Shannon (1948) Sec. 7")
}

#' Shannon entropy of a discrete distribution
#'
#' \deqn{H(X) = -\sum_x p(x)\log p(x)}{H(X) = -sum p log p}
#' Shannon (1948), Section 6; Cover & Thomas (2006), eq. (2.1).  Counts
#' are accepted as well as probabilities: the input is closed first.
#'
#' @param y Non-negative probabilities or counts.
#' @param base Log base; 2 gives bits (Shannon's own unit).
#' @return Named list with `estimate`, `hmax`, `evenness`, `n`, `method`.
#' @references Shannon (1948), Section 6.
#' @examples
#' Shanent(c(1, 1, 1, 1))
#' @export
Shanent <- function(y, base = 2) {
  v <- .b2pnorm(y)
  n <- length(v)
  h <- .b2ent(v, base)
  hmax <- if (n > 1L) .b2logb(n, base) else 0
  list(estimate = h, hmax = hmax,
       evenness = if (hmax > 0) h / hmax else NaN, n = n,
       method = "Shannon entropy of a discrete distribution -- Shannon (1948) Sec. 6")
}

#' Surprisal (self-information) of one or more outcomes
#'
#' \deqn{I(x) = -\log p(x)}{I(x) = -log p(x)}
#' Shannon (1948), Section 6; Cover & Thomas (2006), p. 14, where the
#' entropy is the expectation of -log p(X).
#'
#' @param p Non-negative pmf; closed to unit sum here.
#' @param x Outcome index or indices, 0-based to match the Python arm.
#' @param base Log base; 2 gives bits.
#' @return Named list with `estimate` (mean surprisal), `values`,
#'   `entropy`, `n`, `method`.
#' @references Shannon (1948), Section 6.
#' @export
Surpris <- function(p, x, base = 2) {
  v <- .b2pnorm(p)
  k <- length(v)
  idx <- as.integer(x)
  if (length(idx) == 0L) stop("x must be non-empty", call. = FALSE)
  if (any(idx < 0L | idx >= k)) stop("outcome index outside the alphabet", call. = FALSE)
  pi_ <- v[idx + 1L]
  vals <- ifelse(pi_ > 0, -.b2logb(ifelse(pi_ > 0, pi_, 1), base), Inf)
  list(estimate = if (any(is.infinite(vals))) Inf else mean(vals),
       values = vals, entropy = .b2ent(v, base), n = length(idx),
       method = "Surprisal -log p(x) -- Shannon (1948) Sec. 6")
}

# CANONICAL TEST
# stopifnot(abs(Shanent(c(1, 1, 1, 1))$estimate - 2) < 1e-12)
# stopifnot(abs(Redund(c(0.5, 0.5))$estimate) < 1e-12)
# stopifnot(abs(Difent(c(1, 1, 1))$estimate) < 1e-12)
