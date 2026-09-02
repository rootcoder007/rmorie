# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Classical toolkit shelf -- R mirror of the Python modules precK,
# colMet, hitsR, mapMet, diophs, diopT, contFr, conti, frmlD, resaln,
# zscoreA, detrnd, rbfk, renent.
#
# These name no owning source.  Triage confirmed each is a classical
# or standard definition -- ranking metrics, elementary number theory,
# polynomial algebra, elementary preprocessing -- and no citation is
# manufactured for any of them.  The single exception carries its own:
#
#   Renyi, A. (1961). On measures of entropy and information. Proc.
#   4th Berkeley Symp. Math. Statist. Prob. 1:547-561.  The Project
#   Euclid scan has no text layer, so the definition was taken from
#   the standard mathematical references, not from the page.
#
# Everything is closed form and integer-exact where the mathematics is
# integer-exact (extended Euclid, the Farey recurrence, Bareiss
# elimination), so this arm reproduces the Python arm to machine
# precision.
#
# Collision scan: classictk.R and all fourteen exported names were
# free in both R trees and in _lazy_map.json at the time of writing.

#' Precision at k
#'
#' \deqn{P@k = |rel \cap top_k| / k}{P@k = |rel intersect top-k| / k}
#' The denominator is k itself, so a short result list is penalized.
#'
#' @param pred_rank Ranked item ids, best first.
#' @param relevant Relevant item ids.
#' @param k Cutoff rank.
#' @return Named list with `estimate`, `hits`, `k`, `n_relevant`, `method`.
#' @references Standard IR metric; no single owning source.
#' @examples
#' PrecK(c(1, 2, 3, 4, 5), c(2, 4, 9), 3)
#' @export
PrecK <- function(pred_rank, relevant, k) {
  kk <- as.integer(k)
  if (kk <= 0) stop("k must be positive", call. = FALSE)
  rel <- unique(relevant)
  top <- head(pred_rank, kk)
  hits <- sum(top %in% rel)
  list(estimate = hits / kk, hits = hits, k = kk,
       n_relevant = length(rel), method = "precision at k")
}

#' Recall at k
#'
#' \deqn{R@k = |rel \cap top_k| / |rel|}{R@k = |rel intersect top-k| / |rel|}
#' Saturates at 1; unlike precision it cannot be raised by asking for
#' a longer list than there are relevant items.
#'
#' @param pred_rank Ranked item ids, best first.
#' @param relevant Relevant item ids.
#' @param k Cutoff rank.
#' @return Named list with `estimate`, `hits`, `k`, `n_relevant`, `method`.
#' @references Standard IR metric; no single owning source.
#' @examples
#' ColMet(c(1, 2, 3, 4, 5), c(2, 4, 9), 3)
#' @export
ColMet <- function(pred_rank, relevant, k) {
  kk <- as.integer(k)
  if (kk <= 0) stop("k must be positive", call. = FALSE)
  rel <- unique(relevant)
  if (!length(rel)) stop("recall is undefined with no relevant items",
                         call. = FALSE)
  top <- head(pred_rank, kk)
  hits <- sum(top %in% rel)
  list(estimate = hits / length(rel), hits = hits, k = kk,
       n_relevant = length(rel), method = "recall at k")
}

#' Hit rate at k
#'
#' 1 if any relevant item falls in the top k, 0 otherwise.  The
#' coarsest cutoff metric: it ignores how many hits there were and
#' where they sat.
#'
#' @param pred_rank Ranked item ids, best first.
#' @param relevant Relevant item ids.
#' @param k Cutoff rank.
#' @return Named list with `estimate`, `hits`, `hit`, `k`,
#'   `n_relevant`, `method`.
#' @references Standard IR metric; no single owning source.
#' @examples
#' HitsR(c(1, 2, 3), c(2, 9), 2)
#' @export
HitsR <- function(pred_rank, relevant, k) {
  kk <- as.integer(k)
  if (kk <= 0) stop("k must be positive", call. = FALSE)
  rel <- unique(relevant)
  top <- head(pred_rank, kk)
  hits <- sum(top %in% rel)
  list(estimate = if (hits > 0) 1 else 0, hits = hits, hit = hits > 0,
       k = kk, n_relevant = length(rel), method = "hit rate at k")
}

#' Mean average precision at k
#'
#' The mean of the precisions measured at each rank holding a relevant
#' item, divided by \eqn{\min(|rel|, k)}.  The competing convention
#' divides by \eqn{|rel|}; it is returned as `ap_over_nrel` so the two
#' are never silently swapped.
#'
#' @param pred_rank A ranking, or a list of rankings.
#' @param relevant Relevant ids, or a list of such vectors.
#' @param k Cutoff rank.
#' @return Named list with `estimate`, `ap`, `ap_over_nrel`,
#'   `n_queries`, `k`, `method`.
#' @references Standard IR metric; no single owning source.
#' @examples
#' MapMet(c(1, 2, 3, 4, 5), c(2, 4, 9), 5)
#' @export
MapMet <- function(pred_rank, relevant, k) {
  kk <- as.integer(k)
  if (kk <= 0) stop("k must be positive", call. = FALSE)
  ranks <- if (is.list(pred_rank)) pred_rank else list(pred_rank)
  rels <- if (is.list(relevant)) relevant else list(relevant)
  if (length(rels) != length(ranks))
    stop("need one relevant set per ranking", call. = FALSE)
  aps <- numeric(length(ranks)); apn <- numeric(length(ranks))
  for (q in seq_along(ranks)) {
    rl <- unique(rels[[q]])
    if (!length(rl)) stop("average precision needs relevant items",
                          call. = FALSE)
    top <- head(ranks[[q]], kk)
    hits <- 0; s <- 0
    for (i in seq_along(top)) {
      if (top[i] %in% rl) { hits <- hits + 1; s <- s + hits / i }
    }
    aps[q] <- s / min(length(rl), kk)
    apn[q] <- s / length(rl)
  }
  list(estimate = sum(aps) / length(aps), ap = aps, ap_over_nrel = apn,
       n_queries = length(aps), k = kk,
       method = "mean average precision at k")
}

#' Solve a linear Diophantine equation ax + by = c
#'
#' Solvable iff \eqn{\gcd(a,b) \mid c}.  The extended Euclidean
#' algorithm gives one solution; every other is
#' \eqn{x_0 + t b/g,\ y_0 - t a/g}, so the steps are returned too.
#'
#' @param a,b,c Integer coefficients; a and b not both zero.
#' @return Named list with `estimate`, `solvable`, `x`, `y`, `gcd`,
#'   `x_step`, `y_step`, `method`.
#' @references Classical number theory; no single owning source.
#' @examples
#' Diophs(6, 9, 21)
#' @export
Diophs <- function(a, b, c) {
  ai <- as.integer(a); bi <- as.integer(b); ci <- as.integer(c)
  if (ai == 0 && bi == 0) stop("a and b cannot both be zero", call. = FALSE)
  old_r <- abs(ai); r <- abs(bi)
  old_s <- 1; s <- 0; old_t <- 0; t <- 1
  while (r != 0) {
    q <- old_r %/% r
    tmp <- old_r - q * r; old_r <- r; r <- tmp
    tmp <- old_s - q * s; old_s <- s; s <- tmp
    tmp <- old_t - q * t; old_t <- t; t <- tmp
  }
  g <- old_r; x <- old_s; y <- old_t
  if (ai < 0) x <- -x
  if (bi < 0) y <- -y
  solvable <- (ci %% g) == 0
  if (solvable) {
    m <- ci %/% g
    x0 <- x * m; y0 <- y * m
    xs <- bi %/% g; ys <- -(ai %/% g)
  } else x0 <- y0 <- xs <- ys <- NULL
  list(estimate = if (solvable) 1 else 0, solvable = solvable,
       x = x0, y = y0, gcd = g, x_step = xs, y_step = ys,
       method = "linear Diophantine equation (extended Euclid)")
}

#' Farey sequence of order n
#'
#' Every reduced fraction in \eqn{\[0,1\]} with denominator at most n, in
#' ascending order, generated by the neighbour recurrence
#' \eqn{k = \lfloor (n+b)/d \rfloor} rather than by enumerate-and-sort.
#'
#' @param n Order, at least 1.
#' @return Named list with `estimate` (the length), `terms` (a two
#'   column numerator/denominator matrix), `values`, `n`, `method`.
#' @references Classical number theory; no single owning source.
#' @examples
#' DiopT(5)
#' @export
DiopT <- function(n) {
  nn <- as.integer(n)
  if (nn < 1) stop("order must be at least 1", call. = FALSE)
  a <- 0; b <- 1; cc <- 1; d <- nn
  num <- c(a); den <- c(b)
  while (cc <= nn) {
    num <- c(num, cc); den <- c(den, d)
    k <- (nn + b) %/% d
    nc <- k * cc - a; nd <- k * d - b
    a <- cc; b <- d; cc <- nc; d <- nd
  }
  list(estimate = length(num), terms = cbind(num, den),
       values = num / den, n = nn,
       method = "Farey sequence of order n")
}

#' Continued fraction expansion of a real number
#'
#' \eqn{a_0 + 1/(a_1 + 1/(a_2 + \cdots))}.  The walk stops as soon as
#' the remainder falls below the rounding floor of the value it came
#' from: continuing past that emits a partial quotient made entirely
#' of floating point residue.
#'
#' @param x Number to expand.
#' @param n Maximum number of partial quotients, at most 20.
#' @return Named list with `estimate`, `terms`, `convergents`,
#'   `reliable_terms`, `residual`, `n`, `method`.
#' @references Classical number theory; no single owning source.
#' @examples
#' ContFr(pi, 5)
#' @export
ContFr <- function(x, n) {
  nn <- as.integer(n)
  if (nn < 1) stop("need at least one term", call. = FALSE)
  if (nn > 20) stop("a double supports at most 20 partial quotients",
                    call. = FALSE)
  v <- as.numeric(x); r <- v
  terms <- integer(0); reliable <- 0L
  for (i in seq_len(nn)) {
    a <- floor(r)
    terms <- c(terms, as.integer(a))
    r <- r - a
    if (abs(r) <= 1e-12 * max(1, abs(v))) break
    reliable <- i
    r <- 1 / r
  }
  hm1 <- 1; hm2 <- 0; km1 <- 0; km2 <- 1
  hs <- numeric(0); ks <- numeric(0)
  for (a in terms) {
    h <- a * hm1 + hm2; kk <- a * km1 + km2
    hs <- c(hs, h); ks <- c(ks, kk)
    hm2 <- hm1; hm1 <- h; km2 <- km1; km1 <- kk
  }
  h <- hs[length(hs)]; kk <- ks[length(ks)]
  list(estimate = h / kk, terms = terms,
       convergents = cbind(hs, ks), reliable_terms = reliable,
       residual = v - h / kk, n = length(terms),
       method = "simple continued fraction expansion")
}

#' Continued fraction convergents of pi
#'
#' The leading partial quotients \[3; 7, 15, 1, 292, ...\] are tabulated
#' rather than derived: a double cannot supply them past the tenth, so
#' asking for more than fifteen is refused instead of answered with
#' noise.  The early convergents are the classical 22/7, 333/106 and
#' 355/113.
#'
#' @param n Number of partial quotients, 1 to 15.
#' @return Named list with `estimate`, `terms`, `convergents`,
#'   `numerator`, `denominator`, `error`, `n`, `method`.
#' @references Classical; no single owning source.
#' @examples
#' Conti(4)
#' @export
Conti <- function(n) {
  pit <- c(3, 7, 15, 1, 292, 1, 1, 1, 2, 1, 3, 1, 14, 2, 1)
  nn <- as.integer(n)
  if (nn < 1 || nn > length(pit))
    stop(sprintf("n must be between 1 and %d", length(pit)), call. = FALSE)
  terms <- pit[seq_len(nn)]
  hm1 <- 1; hm2 <- 0; km1 <- 0; km2 <- 1
  hs <- numeric(0); ks <- numeric(0)
  for (a in terms) {
    h <- a * hm1 + hm2; kk <- a * km1 + km2
    hs <- c(hs, h); ks <- c(ks, kk)
    hm2 <- hm1; hm1 <- h; km2 <- km1; km1 <- kk
  }
  h <- hs[length(hs)]; kk <- ks[length(ks)]
  val <- h / kk
  list(estimate = val, terms = terms, convergents = cbind(hs, ks),
       numerator = h, denominator = kk,
       error = val - 3.141592653589793, n = nn,
       method = "continued fraction convergents of pi")
}

#' Formal derivative of a polynomial
#'
#' \deqn{\frac{d}{dx}\sum_i a_i x^i = \sum_i i a_i x^{i-1}}{d/dx sum a_i x^i = sum i a_i x^(i-1)}
#' Coefficients are lowest degree first, matching `Resaln`.  Nothing
#' analytic is used, so the rule is equally valid over a ring.
#'
#' @param poly Coefficients, lowest degree first.
#' @return Named list with `estimate`, `coefficients`, `degree`,
#'   `method`.
#' @references Classical algebra; no single owning source.
#' @examples
#' FrmlD(c(5, 3, 2, 1))
#' @export
FrmlD <- function(poly) {
  a <- as.numeric(poly)
  if (!length(a)) stop("polynomial needs at least one coefficient",
                       call. = FALSE)
  d <- if (length(a) > 1) seq_len(length(a) - 1) * a[-1] else 0
  if (!length(d)) d <- 0
  deg <- length(d) - 1
  while (deg > 0 && d[deg + 1] == 0) deg <- deg - 1
  list(estimate = d[deg + 1], coefficients = d, degree = deg,
       method = "formal derivative of a polynomial")
}

#' Resultant of two polynomials
#'
#' The determinant of the Sylvester matrix, taken with the Bareiss
#' fraction-free algorithm so integer coefficients give an exact
#' integer answer.  It vanishes exactly when the two polynomials share
#' a root, which is what makes it the standard elimination tool.
#'
#' @param p,q Coefficients, lowest degree first.
#' @return Named list with `estimate`, `resultant`, `sylvester`,
#'   `deg_p`, `deg_q`, `share_root`, `method`.
#' @references Classical algebra; no single owning source.
#' @examples
#' Resaln(c(2, -3, 1), c(6, -5, 1))
#' @export
Resaln <- function(p, q) {
  trim <- function(cc) {
    a <- as.numeric(cc)
    while (length(a) > 1 && a[length(a)] == 0) a <- a[-length(a)]
    a
  }
  a <- trim(p); b <- trim(q)
  m <- length(a) - 1; n <- length(b) - 1
  if (m < 1 && n < 1)
    stop("at least one polynomial must be non-constant", call. = FALSE)
  ah <- rev(a); bh <- rev(b)
  size <- m + n
  S <- matrix(0, size, size)
  if (n > 0) for (i in seq_len(n)) S[i, i + seq_along(ah) - 1] <- ah
  if (m > 0) for (i in seq_len(m)) S[n + i, i + seq_along(bh) - 1] <- bh
  A <- S; sgn <- 1; prev <- 1
  if (size > 1) {
    for (k in seq_len(size - 1)) {
      if (A[k, k] == 0) {
        sw <- which(A[(k + 1):size, k] != 0)
        if (!length(sw)) return(list(estimate = 0, resultant = 0,
                                     sylvester = S, deg_p = m, deg_q = n,
                                     share_root = TRUE,
                                     method = "resultant via the Sylvester matrix (Bareiss)"))
        sw <- k + sw[1]
        tmp <- A[k, ]; A[k, ] <- A[sw, ]; A[sw, ] <- tmp
        sgn <- -sgn
      }
      for (i in (k + 1):size) {
        for (j in (k + 1):size) {
          A[i, j] <- (A[i, j] * A[k, k] - A[i, k] * A[k, j]) / prev
        }
        A[i, k] <- 0
      }
      prev <- A[k, k]
    }
  }
  res <- sgn * A[size, size]
  list(estimate = res, resultant = res, sylvester = S,
       deg_p = m, deg_q = n, share_root = abs(res) < 1e-12,
       method = "resultant via the Sylvester matrix (Bareiss)")
}

#' Z-score anomaly flagging
#'
#' Flags \eqn{|x_i - \mu|/\sigma > k}.  Both the centre and the scale
#' come from the series being screened, so a large outlier inflates
#' sigma and masks itself -- this is a screen, not a test.
#'
#' @param x Series to screen.
#' @param k Threshold in standard deviations.
#' @param ddof Delta degrees of freedom of the standard deviation.
#' @return Named list with `estimate`, `z`, `flags`, `indices`,
#'   `mean`, `sd`, `k`, `n`, `method`.
#' @references Basic descriptive screening; no single owning source.
#' @examples
#' ZscoreA(c(1, 2, 3, 4, 50), 2)
#' @export
ZscoreA <- function(x, k, ddof = 1) {
  v <- as.numeric(x); n <- length(v)
  if (n < 2) stop("need at least two observations", call. = FALSE)
  kk <- as.numeric(k)
  mu <- sum(v) / n
  den <- n - ddof
  sdv <- if (den > 0) sqrt(sum((v - mu)^2) / den) else 0
  z <- if (sdv > 0) abs(v - mu) / sdv else rep(0, n)
  flags <- z > kk
  list(estimate = sum(flags), z = z, flags = flags,
       indices = which(flags) - 1L, mean = mu, sd = sdv, k = kk,
       n = n, method = "z-score anomaly flagging")
}

#' Linear detrending of a series
#'
#' Removes the least squares straight line, leaving
#' \eqn{x_i - (a + b t_i)}.  `t` defaults to 0, 1, 2, ..., which is
#' right only for evenly spaced observations.
#'
#' @param x Series.
#' @param t Optional times; 0..n-1 by default.
#' @return Named list with `estimate` (the slope), `detrended`,
#'   `fitted`, `intercept`, `slope`, `n`, `method`.
#' @references Standard preprocessing; no single owning source.
#' @examples
#' Detrnd(c(1, 3.1, 5, 7.2, 9.1))
#' @export
Detrnd <- function(x, t = NULL) {
  v <- as.numeric(x); n <- length(v)
  if (n < 2) stop("need at least two observations", call. = FALSE)
  tv <- if (!is.null(t)) as.numeric(t) else seq_len(n) - 1
  if (length(tv) != n) stop("t and x must have the same length",
                            call. = FALSE)
  tb <- sum(tv) / n; xb <- sum(v) / n
  stt <- sum((tv - tb)^2)
  if (stt == 0) stop("t must not be constant", call. = FALSE)
  b <- sum((tv - tb) * (v - xb)) / stt
  a <- xb - b * tb
  fit <- a + b * tv
  list(estimate = b, detrended = v - fit, fitted = fit,
       intercept = a, slope = b, n = n,
       method = "linear detrending by least squares")
}

#' Radial basis (Gaussian) kernel
#'
#' \deqn{k(x,y) = \exp(-\|x-y\|^2/(2\sigma^2))}{k = exp(-||x-y||^2/(2 sigma^2))}
#' This takes the bandwidth, not the rate; the equivalent
#' \eqn{\gamma = 1/(2\sigma^2)} is returned so the two conventions
#' cannot be confused.
#'
#' @param x,y Vectors of the same length.
#' @param sigma Bandwidth, positive.
#' @return Named list with `estimate`, `value`, `sq_distance`,
#'   `distance`, `sigma`, `gamma`, `method`.
#' @references Standard kernel method; no single owning source.
#' @examples
#' Rbfk(c(0, 0), c(3, 4), 2)
#' @export
Rbfk <- function(x, y, sigma) {
  a <- as.numeric(x); b <- as.numeric(y)
  if (length(a) != length(b))
    stop("x and y must have the same length", call. = FALSE)
  s <- as.numeric(sigma)
  if (s <= 0) stop("sigma must be positive", call. = FALSE)
  d2 <- sum((a - b)^2)
  g <- 1 / (2 * s * s)
  list(estimate = exp(-d2 * g), value = exp(-d2 * g),
       sq_distance = d2, distance = sqrt(d2), sigma = s, gamma = g,
       method = "radial basis (Gaussian) kernel")
}

#' Renyi entropy of order alpha
#'
#' \deqn{H_\alpha(p) = \frac{1}{1-\alpha}\log\sum_k p_k^\alpha}{H_alpha = (1/(1-alpha)) log sum p^alpha}
#' with the removable cases taken as limits: \eqn{\alpha \to 1} gives
#' Shannon entropy, \eqn{\alpha \to \infty} the min-entropy
#' \eqn{-\log\max_k p_k}, and \eqn{\alpha = 0} the log support size.
#' \eqn{H_\alpha} is non-increasing in alpha.
#'
#' @param y Counts or probabilities.
#' @param alpha Order, non-negative; `Inf` gives the min-entropy.
#' @param base Log base; 2 gives bits, `NULL` gives nats.
#' @return Named list with `estimate`, `alpha`, `base`,
#'   `probabilities`, `support`, `method`.
#' @references Renyi (1961) Proc. 4th Berkeley Symp. 1:547-561.
#' @examples
#' Renent(c(0.5, 0.25, 0.25), 2)
#' @export
Renent <- function(y, alpha = 2, base = 2) {
  p <- as.numeric(y)
  if (any(p < 0)) stop("probabilities must be non-negative", call. = FALSE)
  tot <- sum(p)
  if (!(tot > 0)) stop("total mass must be positive", call. = FALSE)
  p <- p / tot
  a <- as.numeric(alpha)
  if (a < 0) stop("alpha must be non-negative", call. = FALSE)
  lb <- if (is.null(base)) NULL else as.numeric(base)
  if (!is.null(lb) && (lb <= 0 || lb == 1))
    stop("base must be positive and not 1", call. = FALSE)
  lg <- function(v) if (is.null(lb)) log(v) else log(v) / log(lb)
  pos <- p[p > 0]
  sup <- length(pos)
  h <- if (is.infinite(a)) {
    -lg(max(p))
  } else if (a == 1) {
    -sum(pos * lg(pos))
  } else if (a == 0) {
    lg(sup)
  } else {
    lg(sum(pos^a)) / (1 - a)
  }
  list(estimate = h, alpha = a, base = lb, probabilities = p,
       support = sup,
       method = "Renyi entropy of order alpha (Renyi 1961)")
}
