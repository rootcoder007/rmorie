# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Information-theory / Bayesian-inference shelf -- R mirror of
# morie/fn/_itila.py and the twenty-eight information_theory_mackay*
# modules.
#
# Spec: MacKay, D.J.C. (2003), Information Theory, Inference, and
# Learning Algorithms, Cambridge University Press. Each function
# names the printed equation and page it implements; page numbers are
# the printed book pages of the CUP edition.
#
# Collision scan: info_mackay.R and all twenty-eight exported names
# were free in both R trees (morie/r-package/morie and r-morie-oss).
#
# Everything here is closed form or a fixed-iteration recurrence -- no
# RNG, no tolerance-driven early exit -- so this arm reproduces the
# Python arm to machine precision.

#' .morie_mk_pinv
#'
#' A step of the info_mackay implementation. Called by \code{morie_linevid}, \code{morie_postgapx}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a A matrix; passed to \code{dim}.
#' @return The value of \code{%*%}.
#' @export
.morie_mk_pinv <- function(a) {
  s <- svd(a)
  tol <- max(dim(a)) * max(s$d) * .Machine$double.eps
  dinv <- ifelse(s$d > tol, 1 / s$d, 0)
  s$v %*% (dinv * t(s$u))
}

# --- ch. 1: the repetition codes R3 / RN (pp. 5-20) -------------------

#' R3 repetition-code bit posterior (MacKay 2003 eq. 1.18, p. 9)
#' @param r length-3 integer vector of received bits
#' @param f binary-symmetric-channel flip probability, in (0, 1)
#' @return list(p0, p1, decoded, gamma, evidence)
#' @export
#' @examples
#' morie_r3post(r = c(1L, 1L, 0L), f = 0.1)
morie_r3post <- function(r, f) {
  r <- as.integer(r)
  if (length(r) != 3L || any(!(r %in% c(0L, 1L)))) {
    stop("r must be three bits.", call. = FALSE)
  }
  f <- as.numeric(f)
  if (!(f > 0 && f < 1)) stop("f must lie strictly in (0, 1).", call. = FALSE)
  lik <- vapply(c(0L, 1L), function(s) prod(ifelse(r != s, f, 1 - f)), numeric(1))
  tot <- lik[1] + lik[2]
  list(p0 = lik[1] / tot, p1 = lik[2] / tot,
       decoded = if (lik[2] > lik[1]) 1L else 0L,
       gamma = (1 - f) / f, evidence = tot / 2)
}

#' Gaussian approximation to the central binomial (eq. 1.40, p. 17)
#' @param n blocklength
#' @return list(approx, exact, relerr, logapprox)
#' @export
#' @examples
#' morie_cbcapx(n = 5L)
morie_cbcapx <- function(n) {
  n <- as.integer(n)
  if (n < 1L) stop("n must be at least 1.", call. = FALSE)
  approx <- 2^n / sqrt(2 * pi * n / 4)
  # ponytail: round() makes R agree with Python exact integer arithmetic
  # while the value fits a double; above 2^53 neither arm is exact.
  exact <- round(choose(n, n %/% 2L))
  list(approx = approx, exact = exact, relerr = approx / exact - 1,
       logapprox = n * log(2) - 0.5 * log(2 * pi * n / 4))
}

#' Gaussian sum behind the central binomial approximation (eq. 1.41, p. 17)
#'
#' The book writes sigma = sqrt(N/4) in the running text but then uses
#' sqrt(2 pi sigma); only the reading sigma = N/4 (the VARIANCE) makes
#' eq. (1.41) agree with eq. (1.40). Both quantities are returned so
#' the discrepancy stays visible.
#' @param n blocklength
#' @return list(var, sd, gsum, total, cbcapprox)
#' @export
#' @examples
#' morie_binsumga(n = 5L)
morie_binsumga <- function(n) {
  n <- as.integer(n)
  if (n < 1L) stop("n must be at least 1.", call. = FALSE)
  v <- n / 4
  gsum <- sqrt(2 * pi * v)
  exact <- round(choose(n, n %/% 2L))
  list(var = v, sd = sqrt(v), gsum = gsum,
       total = 2^(-n) * exact * gsum, cbcapprox = 2^n / gsum)
}

#' Block error probability of the RN repetition code (eq. 1.42-1.43, p. 17)
#' @param n odd blocklength
#' @param f channel flip probability, in (0, 1)
#' @return list(leading, approx1, approx2, logapprox2)
#' @export
#' @examples
#' morie_repcpb(n = 61, f = 0.1)
morie_repcpb <- function(n, f) {
  n <- as.integer(n)
  f <- as.numeric(f)
  if (n < 1L || n %% 2L == 0L) stop("n must be a positive odd integer.", call. = FALSE)
  if (!(f > 0 && f < 1)) stop("f must lie strictly in (0, 1).", call. = FALSE)
  k <- (n + 1L) %/% 2L
  half <- (n - 1) / 2
  list(leading = round(choose(n, k)) * f^k * (1 - f)^(n - k),
       approx1 = (2^n / sqrt(pi * n / 2)) * f * (f * (1 - f))^half,
       approx2 = (1 / sqrt(pi * n / 8)) * f * (4 * f * (1 - f))^half,
       logapprox2 = log((1 / sqrt(pi * n / 8)) * f * (4 * f * (1 - f))^half) / log(2))
}

#' Blocklength reaching a target error rate (eq. 1.44-1.45, p. 17)
#'
#' A fixed number of sweeps of the iteration printed under eq. (1.44),
#' started from the book value N-hat_1 = 68. There is no convergence
#' test, so both language arms take identical steps.
#' @param pb target block error probability
#' @param f channel flip probability, in (0, 0.5)
#' @param n0 starting blocklength
#' @param iters number of sweeps
#' @return list(n, half, denom, iters)
#' @export
#' @examples
#' morie_repcn(pb = 1e-15, f = 0.1)
morie_repcn <- function(pb, f, n0 = 68, iters = 3L) {
  pb <- as.numeric(pb)
  f <- as.numeric(f)
  if (!(pb > 0 && pb < 1)) stop("pb must lie strictly in (0, 1).", call. = FALSE)
  if (!(f > 0 && f < 0.5)) stop("f must lie strictly in (0, 0.5).", call. = FALSE)
  n <- as.numeric(n0)
  denom <- log10(4 * f * (1 - f))
  half <- NA_real_
  for (i in seq_len(as.integer(iters))) {
    half <- (log10(pb) + log10(sqrt(pi * n / 8) / f)) / denom
    n <- 2 * half + 1
  }
  list(n = n, half = half, denom = denom, iters = as.integer(iters))
}

# --- ch. 2: inferring which urn (pp. 27-31) ---------------------------

#' Posterior over a discrete urn hypothesis (eq. 2.25-2.26, p. 28)
#' @param nb number of black balls drawn
#' @param ntot number of draws
#' @param nurns number of urns minus one; urn u holds u black in nurns
#' @return list(posterior, evidence, map, prior)
#' @export
#' @examples
#' morie_urnpost(nb = 5L, ntot = 5L)
morie_urnpost <- function(nb, ntot, nurns = 10L) {
  nb <- as.integer(nb); ntot <- as.integer(ntot); nurns <- as.integer(nurns)
  if (nurns < 1L || ntot < 0L || nb < 0L || nb > ntot) {
    stop("need 0 <= nb <= ntot and nurns >= 1.", call. = FALSE)
  }
  prior <- 1 / (nurns + 1)
  u <- 0:nurns
  fu <- u / nurns
  joint <- prior * round(choose(ntot, nb)) * fu^nb * (1 - fu)^(ntot - nb)
  evid <- sum(joint)
  post <- joint / evid
  list(posterior = post, evidence = evid,
       map = as.integer(which.max(post) - 1L), prior = prior)
}

#' Posterior predictive probability for the next draw (eq. 2.29-2.31, p. 29)
#' @inheritParams morie_urnpost
#' @return list(p, pnot, pmap)
#' @export
#' @examples
#' morie_urnpred(nb = 5L, ntot = 5L)
morie_urnpred <- function(nb, ntot, nurns = 10L) {
  nurns <- as.integer(nurns)
  post <- morie_urnpost(nb, ntot, nurns)$posterior
  u <- 0:nurns
  p <- sum((u / nurns) * post)
  list(p = p, pnot = 1 - p, pmap = (which.max(post) - 1L) / nurns)
}

# --- ch. 3: the bent coin and model comparison (pp. 50-53, 63) --------

#' Bent-coin likelihood (eq. 3.8, p. 51)
#' @param pa probability of outcome a, in \[0, 1\]
#' @param fa,fb counts of outcomes a and b
#' @return list(likelihood, loglik, fa, fb)
#' @export
#' @examples
#' morie_bcoinlik(pa = 0.3, fa = 3, fb = 7)
morie_bcoinlik <- function(pa, fa, fb) {
  pa <- as.numeric(pa); fa <- as.integer(fa); fb <- as.integer(fb)
  if (pa < 0 || pa > 1 || fa < 0L || fb < 0L) {
    stop("need pa in [0, 1] and non-negative counts.", call. = FALSE)
  }
  list(likelihood = pa^fa * (1 - pa)^fb,
       loglik = (if (fa > 0L) fa * log(pa) else 0) +
         (if (fb > 0L) fb * log1p(-pa) else 0),
       fa = fa, fb = fb)
}

#' Uniform prior density on the bent-coin bias (eq. 3.9, p. 51)
#' @param pa candidate bias
#' @return list(density, inside, logdensity)
#' @export
#' @examples
#' morie_bcoinpri(pa = 5L)
morie_bcoinpri <- function(pa) {
  pa <- as.numeric(pa)
  inside <- pa >= 0 && pa <= 1
  list(density = if (inside) 1 else 0, inside = inside,
       logdensity = if (inside) 0 else -Inf)
}

#' Rule of succession (eq. 3.16, p. 52)
#' @param fa,fb counts of outcomes a and b
#' @return list(p, pnot, mle)
#' @export
#' @examples
#' morie_sucrule(fa = 5L, fb = 5L)
morie_sucrule <- function(fa, fb) {
  fa <- as.integer(fa); fb <- as.integer(fb)
  if (fa < 0L || fb < 0L) stop("counts must be non-negative.", call. = FALSE)
  p <- (fa + 1) / (fa + fb + 2)
  list(p = p, pnot = 1 - p,
       mle = if ((fa + fb) > 0L) fa / (fa + fb) else NA_real_)
}

#' Total evidence over a set of models (eq. 3.19, p. 53)
#' @param evidences per-model evidences
#' @param priors per-model prior probabilities
#' @return list(evidence, terms, posterior)
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_evidmix(V, V)
morie_evidmix <- function(evidences, priors) {
  ev <- as.numeric(evidences); pr <- as.numeric(priors)
  if (length(ev) != length(pr) || length(ev) == 0L) {
    stop("evidences and priors must be non-empty and equal length.", call. = FALSE)
  }
  terms <- ev * pr
  tot <- sum(terms)
  list(evidence = tot, terms = terms, posterior = terms / tot)
}

#' Posterior odds from a likelihood ratio and prior odds (eq. 3.21, p. 53)
#' @param lik1,lik0 evidences for the two models
#' @param prior1,prior0 prior probabilities of the two models
#' @return list(odds, logodds, p1, bayesfactor)
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_postodds(V, V)
morie_postodds <- function(lik1, lik0, prior1 = 0.5, prior0 = 0.5) {
  lik1 <- as.numeric(lik1); lik0 <- as.numeric(lik0)
  odds <- (lik1 * as.numeric(prior1)) / (lik0 * as.numeric(prior0))
  list(odds = odds, logodds = log(odds), p1 = odds / (1 + odds),
       bayesfactor = lik1 / lik0)
}

#' Free-bias against fixed-bias coin (eq. 3.12, 3.20, 3.22, pp. 52-53)
#' @param fa,fb counts of outcomes a and b
#' @param p0 the fixed bias asserted by the simpler model
#' @return list(evidence1, evidence0, ratio, logratio)
#' @export
#' @examples
#' morie_bcoinbf(fa = 5L, fb = 5L)
morie_bcoinbf <- function(fa, fb, p0 = 1 / 6) {
  fa <- as.integer(fa); fb <- as.integer(fb)
  if (fa < 0L || fb < 0L) stop("counts must be non-negative.", call. = FALSE)
  p0 <- as.numeric(p0)
  lge1 <- lgamma(fa + 1) + lgamma(fb + 1) - lgamma(fa + fb + 2)
  lge0 <- (if (fa > 0L) fa * log(p0) else 0) +
    (if (fb > 0L) fb * log1p(-p0) else 0)
  list(evidence1 = exp(lge1), evidence0 = exp(lge0),
       ratio = exp(lge1 - lge0), logratio = lge1 - lge0)
}

#' Posterior odds as a running product of ratios (eq. 3.31, p. 63)
#' @param num,den per-observation probabilities under the two hypotheses
#' @return list(ratio, p1, logratio, n)
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_lrprod(V, V)
morie_lrprod <- function(num, den) {
  num <- as.numeric(num); den <- as.numeric(den)
  if (length(num) != length(den) || length(num) == 0L) {
    stop("num and den must be non-empty and equal length.", call. = FALSE)
  }
  ratio <- 1
  for (i in seq_along(num)) ratio <- ratio * (num[i] / den[i])
  list(ratio = ratio, p1 = ratio / (1 + ratio), logratio = log(ratio),
       n = length(num))
}

# --- ch. 4: the typical set (p. 80) -----------------------------------

#' Typical-set membership test (eq. 4.29, p. 80)
#' @param p probability of the string x
#' @param n string length
#' @param h entropy per symbol, in bits
#' @param beta typicality tolerance
#' @return list(info, rate, deviation, member)
#' @export
#' @examples
#' morie_typset(p = 0.5, n = 5L, h = 0.5, beta = 0.5)
morie_typset <- function(p, n, h, beta) {
  p <- as.numeric(p); n <- as.integer(n)
  if (!(p > 0 && p <= 1) || n < 1L) {
    stop("need 0 < p <= 1 and n >= 1.", call. = FALSE)
  }
  info <- -log(p) / log(2)
  rate <- info / n
  dev <- rate - as.numeric(h)
  list(info = info, rate = rate, deviation = dev,
       member = abs(dev) < as.numeric(beta))
}

# --- ch. 11: the Gaussian channel (p. 182) ----------------------------

#' Posterior over the input of a Gaussian channel (eq. 11.27-11.29, p. 182)
#' @param y observed channel output
#' @param v prior variance of the input
#' @param s2 channel noise variance
#' @return list(mean, var, sd, wdata, marginalvar)
#' @export
#' @examples
#' morie_gchpost(y = c(1, 2, 3, 4, 5, 6, 7, 8), v = 5L, s2 = 5L)
morie_gchpost <- function(y, v, s2) {
  y <- as.numeric(y); v <- as.numeric(v); s2 <- as.numeric(s2)
  if (v <= 0 || s2 <= 0) stop("v and s2 must be positive.", call. = FALSE)
  vr <- 1 / (1 / v + 1 / s2)
  list(mean = v / (v + s2) * y, var = vr, sd = sqrt(vr),
       wdata = (1 / s2) / (1 / v + 1 / s2), marginalvar = v + s2)
}

# --- ch. 19: why have sex? (pp. 271-273) ------------------------------

.morie_mk_eta <- sqrt(2 / (pi + 2))

#' Dynamic-equilibrium fitness-variance factor (eq. 19.7, p. 271)
#' @param gamma variance-reduction factor of selection, in [0, 1)
#' @return list(onepbeta, beta, gamma)
#' @export
#' @examples
#' morie_sexbeta(gamma = 0.5)
morie_sexbeta <- function(gamma) {
  gamma <- as.numeric(gamma)
  if (!(gamma >= 0 && gamma < 1)) stop("gamma must lie in [0, 1).", call. = FALSE)
  onep <- 1 / (1 - gamma)
  list(onepbeta = onep, beta = onep - 1, gamma = gamma)
}

#' Mean-fitness growth rate under recombination (eq. 19.13, p. 273)
#' @param f normalized fitness, in \[0, 1\]
#' @param g genome size
#' @param eta rate constant; default sqrt(2/(pi + 2)) as in the book
#' @return list(dfbardt, eta, g)
#' @export
#' @examples
#' morie_sexdfdt(f = 0.6, g = 1000)
morie_sexdfdt <- function(f, g, eta = NULL) {
  f <- as.numeric(f); g <- as.numeric(g)
  if (f < 0 || f > 1 || g <= 0) stop("need f in [0, 1] and G > 0.", call. = FALSE)
  eta <- if (is.null(eta)) .morie_mk_eta else as.numeric(eta)
  list(dfbardt = eta * sqrt(f * (1 - f) * g), eta = eta, g = g)
}

#' Closed-form fitness trajectory under recombination (eq. 19.14, p. 273)
#'
#' The book states c = asin(2 f(0) - 1), which does NOT satisfy its own
#' f(0) = f0 unless the sine argument is read as eta t/sqrt(G) + c. The
#' default c here is the self-consistent one, sqrt(G)/eta times
#' asin(2 f0 - 1); the printed value is returned as cbook.
#' @param t time, in generations
#' @param g genome size
#' @param f0 normalized fitness at t = 0
#' @param eta rate constant; default sqrt(2/(pi + 2))
#' @param c integration constant; default the self-consistent one
#' @return list(f, c, cbook, tperfect)
#' @export
#' @examples
#' morie_sexfsol(t = 10, g = 1000, f0 = 0.5)
morie_sexfsol <- function(t, g, f0, eta = NULL, c = NULL) {
  t <- as.numeric(t); g <- as.numeric(g); f0 <- as.numeric(f0)
  if (g <= 0 || f0 < 0 || f0 > 1) stop("need G > 0 and f0 in [0, 1].", call. = FALSE)
  eta <- if (is.null(eta)) .morie_mk_eta else as.numeric(eta)
  cbook <- asin(2 * f0 - 1)
  cc <- if (is.null(c)) (sqrt(g) / eta) * cbook else as.numeric(c)
  list(f = 0.5 * (1 + sin(eta * (t + cc) / sqrt(g))), c = cc, cbook = cbook,
       tperfect = (pi / eta) * sqrt(g))
}

# --- ch. 24: exact marginalization in Gaussians (pp. 319-320) ---------

#' Gaussian log likelihood in sufficient statistics (eq. 24.5-24.6, p. 319)
#' @param xbar sample mean
#' @param s sum of squared deviations from xbar
#' @param n sample size
#' @param mu candidate mean
#' @param sigma candidate noise level
#' @return list(loglik, n, s, sigman)
#' @export
#' @examples
#' morie_gllsuff(xbar = c(1, 2, 3, 4, 5, 6, 7, 8), s = c(1, 2, 3, 4, 5, 6, 7, 8), n = 5L, mu = c(1, 2, 3, 4, 5, 6, 7, 8), sigma = 0.5)
morie_gllsuff <- function(xbar, s, n, mu, sigma) {
  n <- as.integer(n); sigma <- as.numeric(sigma); s <- as.numeric(s)
  if (n < 1L || sigma <= 0) stop("need n >= 1 and sigma > 0.", call. = FALSE)
  ll <- -n * log(sqrt(2 * pi) * sigma) -
    (n * (as.numeric(mu) - as.numeric(xbar))^2 + s) / (2 * sigma^2)
  list(loglik = ll, n = n, s = s, sigman = sqrt(s / n))
}

#' Posterior over the mean at known noise level (eq. 24.9-24.11, p. 320)
#' @param xbar sample mean
#' @param n sample size
#' @param sigma known noise level
#' @return list(mean, var, se, n)
#' @export
#' @examples
#' morie_mupostsg(xbar = c(1, 2, 3, 4, 5, 6, 7, 8), n = 5L, sigma = 0.5)
morie_mupostsg <- function(xbar, n, sigma) {
  n <- as.integer(n); sigma <- as.numeric(sigma)
  if (n < 1L || sigma <= 0) stop("need n >= 1 and sigma > 0.", call. = FALSE)
  v <- sigma^2 / n
  list(mean = as.numeric(xbar), var = v, se = sqrt(v), n = n)
}

#' Log evidence for the noise level (eq. 24.13, p. 320)
#' @param s sum of squared deviations from the sample mean
#' @param n sample size
#' @param sigma candidate noise level
#' @param sigmamu width of the top-hat prior on the mean
#' @return list(logevidence, bestfit, logoccam)
#' @export
#' @examples
#' morie_sigevid(s = c(1, 2, 3, 4, 5, 6, 7, 8), n = 5L, sigma = 0.5)
morie_sigevid <- function(s, n, sigma, sigmamu = 1) {
  n <- as.integer(n); sigma <- as.numeric(sigma); sigmamu <- as.numeric(sigmamu)
  if (n < 1L || sigma <= 0 || sigmamu <= 0) {
    stop("need n >= 1, sigma > 0, sigmamu > 0.", call. = FALSE)
  }
  s <- as.numeric(s)
  bestfit <- -n * log(sqrt(2 * pi) * sigma) - s / (2 * sigma^2)
  occam <- log(sqrt(2 * pi) * sigma / sqrt(n) / sigmamu)
  list(logevidence = bestfit + occam, bestfit = bestfit, logoccam = occam)
}

# --- ch. 28: model comparison and the razor (pp. 344-352) -------------

#' Quadratic approximation to a posterior (eq. 28.5, p. 344)
#' @param dw parameter offset from the posterior mode
#' @param a Hessian of the negative log posterior at the mode
#' @return list(quadform, logratio, ratio, errorbars)
#' @export
#' @examples
#' A <- rbind(c(2, 0.3), c(0.3, 1.5))
#' morie_postgapx(dw = c(0.2, -0.1), a = A)
morie_postgapx <- function(dw, a) {
  dw <- as.numeric(dw)
  a <- as.matrix(a)
  k <- length(dw)
  if (nrow(a) != k || ncol(a) != k) {
    stop("A must be square and match the length of dw.", call. = FALSE)
  }
  quad <- as.numeric(dw %*% (a %*% dw))
  cov <- .morie_mk_pinv(a)
  list(quadform = quad, logratio = -0.5 * quad, ratio = exp(-0.5 * quad),
       errorbars = sqrt(abs(diag(cov))))
}

#' Posterior model ratio as a product of penalties (eq. 28.13-28.14, p. 351)
#' @param factors the per-parameter penalty factors of the richer model
#' @return list(ratio, product, logratio, n)
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_evratio(V)
morie_evratio <- function(factors) {
  factors <- as.numeric(factors)
  if (length(factors) == 0L || any(factors <= 0)) {
    stop("factors must be positive and non-empty.", call. = FALSE)
  }
  prod_ <- 1
  for (i in seq_along(factors)) prod_ <- prod_ * factors[i]
  list(ratio = 1 / prod_, product = prod_, logratio = -log(prod_),
       n = length(factors))
}

#' Message length and its probability, in bits (eq. 28.15, p. 352)
#' @param p probability of the event; give exactly one of p or length
#' @param length message length in bits
#' @return list(length, p, nats)
#' @export
#' @examples
#' morie_msglen(p = 1 / 8)
morie_msglen <- function(p = NULL, length = NULL) {
  if (is.null(p) == is.null(length)) {
    stop("give exactly one of p or length.", call. = FALSE)
  }
  if (!is.null(p)) {
    p <- as.numeric(p)
    if (!(p > 0 && p <= 1)) stop("p must lie in (0, 1].", call. = FALSE)
    len <- -log(p) / log(2)
  } else {
    len <- as.numeric(length)
    p <- 2^(-len)
  }
  list(length = len, p = p, nats = len * log(2))
}

#' Two-part minimum-description-length message length (eq. 28.16-28.17, p. 352)
#' @param ph prior probability of the model
#' @param pdh density of the data under the model
#' @param deltad the pre-arranged precision to which the data are sent
#' @return list(total, model, data)
#' @export
#' @examples
#' morie_mdlpost(ph = 0.25, pdh = 0.05, deltad = 0.01)
morie_mdlpost <- function(ph, pdh, deltad = 1) {
  ph <- as.numeric(ph); pdh <- as.numeric(pdh); deltad <- as.numeric(deltad)
  if (!(ph > 0 && ph <= 1) || pdh <= 0 || deltad <= 0) {
    stop("need 0 < ph <= 1 and pdh, deltad > 0.", call. = FALSE)
  }
  model <- -log(ph) / log(2)
  data <- -log(pdh * deltad) / log(2)
  list(total = model + data, model = model, data = data)
}

#' Marginal likelihood of a straight-line model (eq. 28.22, Ex. 28.2, p. 352)
#'
#' slope = FALSE is the horizontal-line model H1 (w1 = 0); slope = TRUE
#' is H2 with w1 free. Closed form: t is Normal(0, priorsd^2 X X' +
#' sigma^2 I).
#' @param x predictor values chosen by the experimenter
#' @param t observed responses
#' @param sigma known noise level
#' @param slope whether the slope is a free parameter
#' @param priorsd prior standard deviation on each weight
#' @return list(logevidence, evidence, quadform, logdet, k)
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_linevid(V, V)
morie_linevid <- function(x, t, sigma = 1, slope = TRUE, priorsd = 1) {
  x <- as.numeric(x); t <- as.numeric(t)
  n <- length(x)
  if (length(t) != n || n < 1L) {
    stop("x and t must be non-empty and the same length.", call. = FALSE)
  }
  sigma <- as.numeric(sigma); priorsd <- as.numeric(priorsd)
  if (sigma <= 0 || priorsd <= 0) {
    stop("sigma and priorsd must be positive.", call. = FALSE)
  }
  xmat <- if (isTRUE(slope)) cbind(rep(1, n), x) else cbind(rep(1, n))
  cov <- priorsd^2 * (xmat %*% t(xmat)) + sigma^2 * diag(n)
  logdet <- as.numeric(determinant(cov, logarithm = TRUE)$modulus)
  quad <- as.numeric(t %*% (.morie_mk_pinv(cov) %*% t))
  lev <- -0.5 * (n * log(2 * pi) + logdet + quad)
  list(logevidence = lev, evidence = exp(lev), quadform = quad,
       logdet = logdet, k = ncol(xmat))
}

# --- ch. 29: why uniform sampling fails (p. 366) ----------------------

#' Uniform draws needed to hit the typical set once (eq. 29.19, p. 366)
#' @param n number of binary variables
#' @param h entropy of the target distribution, in bits
#' @return list(log2rmin, log10rmin, rmin)
#' @export
#' @examples
#' morie_rminsamp(n = 5L, h = 0.5)
morie_rminsamp <- function(n, h) {
  n <- as.numeric(n); h <- as.numeric(h)
  if (n <= 0 || h < 0 || h > n) stop("need 0 < n and 0 <= h <= n.", call. = FALSE)
  log2r <- n - h
  list(log2rmin = log2r, log10rmin = log2r * log(2) / log(10),
       rmin = if (log2r < 1000) 2^log2r else Inf)
}
