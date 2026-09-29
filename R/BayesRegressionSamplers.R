.br_stream <- function(seed) {
  env <- new.env()
  env$seed <- seed
  env$block <- 0
  env$u <- numeric(0)
  env$k <- 0
  env
}

.br_unif <- function(st) {
  if (st$k >= length(st$u)) {
    st$u <- .morie_random_uniform(4096, seed = st$seed, stream = st$block)
    st$block <- st$block + 1
    st$k <- 0
  }
  st$k <- st$k + 1
  st$u[st$k]
}

.br_norm <- function(st) stats::qnorm(.br_unif(st))

.br_gamma <- function(st, shape) {
  if (shape < 1) {
    g <- .br_gamma(st, shape + 1)
    return(g * .br_unif(st)^(1 / shape))
  }
  d <- shape - 1 / 3
  cc <- 1 / sqrt(9 * d)
  repeat {
    x <- .br_norm(st)
    v <- (1 + cc * x)^3
    if (v <= 0) next
    u <- .br_unif(st)
    if (log(u) < 0.5 * x^2 + d - d * v + d * log(v)) return(d * v)
  }
}

.br_summ <- function(keep) list(mean = colMeans(keep), sd = apply(keep, 2, stats::sd))

#' Gibbs samplers for Bayesian regression, mixtures and genomic prediction
#'
#' \code{BayesLinearHalfcauchy}: normal regression with a half-Cauchy prior on
#' sigma. \code{FiniteMixtureGibbs}: finite normal mixture.
#' \code{ContaminatedNormalOutliers}: Box-Tiao outlier probabilities.
#' \code{BayesA} and \code{BayesB}: marker-effect models of genomic
#' prediction. \code{GenomicReliability}: 1 - PEV / sigma_a^2. The Philox
#' stream and Marsaglia-Tsang gamma draws reproduce the Python arm
#' \code{morie.fn.bayesreg}.
#'
#' @param y Response.
#' @param X Design matrix including the intercept.
#' @param prior_var Prior variance of the coefficients.
#' @param scale Half-Cauchy scale.
#' @param ndraw,burn_in Retained and burn-in draws.
#' @param seed Philox seed.
#' @param K Number of mixture components.
#' @param alpha Dirichlet concentration.
#' @param eps,k Contamination probability and scale inflation.
#' @param M Marker matrix (individuals by markers).
#' @param nu,s2 Scaled inverse chi-square degrees of freedom and scale.
#' @param pi Proportion of markers without effect.
#' @param pev Prediction error variances.
#' @param sigma2_a Additive genetic variance.
#' @return A list.
#' @references Gelman, A. (2006). Prior distributions for variance
#'   parameters in hierarchical models. Bayesian Analysis 1, 515-534.
#'
#'   Diebolt, J. and Robert, C. P. (1994). Estimation of finite mixture
#'   distributions through Bayesian sampling. Journal of the Royal
#'   Statistical Society B 56, 363-375.
#'
#'   Box, G. E. P. and Tiao, G. C. (1968). A Bayesian approach to some outlier
#'   problems. Biometrika 55, 119-129.
#'
#'   Meuwissen, T. H. E., Hayes, B. J. and Goddard, M. E. (2001). Prediction of
#'   total genetic value using genome-wide dense marker maps. Genetics 157,
#'   1819-1829.
#' @examples
#' GenomicReliability(c(0.2, 0.5), 0.8)$reliability
#' @export
BayesLinearHalfcauchy <- function(y, X, prior_var = 1e6, scale = 25, ndraw = 2000, burn_in = 500, seed = 0) {
  X <- as.matrix(X)
  n <- nrow(X)
  k <- ncol(X)
  st <- .br_stream(seed)
  xtx <- crossprod(X)
  xty <- as.vector(crossprod(X, y))
  s2 <- 1
  a <- 1
  keep <- matrix(0, ndraw, k + 1)
  for (it in seq_len(ndraw + burn_in)) {
    Mv <- solve(xtx / s2 + diag(1 / prior_var, k))
    mu <- as.vector(Mv %*% (xty / s2))
    L <- t(chol(Mv))
    z <- vapply(seq_len(k), function(i) .br_norm(st), 0)
    beta <- mu + as.vector(L %*% z)
    sse <- sum((y - X %*% beta)^2)
    s2 <- (sse / 2 + 1 / a) / .br_gamma(st, (n + 1) / 2)
    a <- (1 / s2 + 1 / scale^2) / .br_gamma(st, 1)
    if (it > burn_in) keep[it - burn_in, ] <- c(beta, s2)
  }
  s <- .br_summ(keep)
  list(beta = s$mean[1:k], beta_sd = s$sd[1:k], sigma2 = s$mean[k + 1], draws = keep)
}

#' @rdname BayesLinearHalfcauchy
#' @export
FiniteMixtureGibbs <- function(y, K, ndraw = 2000, burn_in = 500, seed = 0, alpha = 1) {
  n <- length(y)
  m0 <- sum(y) / n
  s2y <- sum((y - m0)^2) / (n - 1)
  s0 <- 10 * s2y
  st <- .br_stream(seed)
  srt <- sort(y)
  mu <- srt[floor(((seq_len(K) - 1) + 0.5) * n / K) + 1]
  sig <- rep(s2y, K)
  pi <- rep(1 / K, K)
  keep <- matrix(0, ndraw, 3 * K)
  for (it in seq_len(ndraw + burn_in)) {
    z <- integer(n)
    for (i in seq_len(n)) {
      w <- pi * exp(-0.5 * (y[i] - mu)^2 / sig) / sqrt(sig)
      t <- .br_unif(st) * sum(w)
      cz <- which(cumsum(w) >= t)
      z[i] <- if (length(cz)) cz[1] else K
    }
    cnt <- tabulate(z, K)
    g <- vapply(seq_len(K), function(k) .br_gamma(st, alpha + cnt[k]), 0)
    pi <- g / sum(g)
    for (k in seq_len(K)) {
      yk <- y[z == k]
      prec <- 1 / s0 + cnt[k] / sig[k]
      mk <- (m0 / s0 + sum(yk) / sig[k]) / prec
      mu[k] <- mk + .br_norm(st) / sqrt(prec)
      sig[k] <- (s2y + 0.5 * sum((yk - mu[k])^2)) / .br_gamma(st, 2 + cnt[k] / 2)
    }
    o <- order(mu, seq_len(K))
    if (it > burn_in) keep[it - burn_in, ] <- c(mu[o], sig[o], pi[o])
  }
  s <- colMeans(keep)
  list(mu = s[1:K], sigma2 = s[K + 1:K], pi = s[2 * K + 1:K], draws = keep)
}

#' @rdname BayesLinearHalfcauchy
#' @export
ContaminatedNormalOutliers <- function(y, eps = 0.05, k = 5, ndraw = 2000, burn_in = 500, seed = 0) {
  n <- length(y)
  st <- .br_stream(seed)
  mu <- sort(y)[n %/% 2 + 1]
  s2 <- sum((y - mu)^2) / n
  prob <- numeric(n)
  keep <- matrix(0, ndraw, 2)
  for (it in seq_len(ndraw + burn_in)) {
    a <- (1 - eps) * exp(-0.5 * (y - mu)^2 / s2)
    b <- eps / k * exp(-0.5 * (y - mu)^2 / (k^2 * s2))
    pr <- b / (a + b)
    delta <- vapply(seq_len(n), function(i) .br_unif(st) < pr[i], TRUE)
    w <- ifelse(delta, 1 / k^2, 1)
    sw <- sum(w)
    mu <- sum(w * y) / sw + .br_norm(st) * sqrt(s2 / sw)
    s2 <- sum(w * (y - mu)^2) / (2 * .br_gamma(st, n / 2))
    if (it > burn_in) {
      keep[it - burn_in, ] <- c(mu, s2)
      prob <- prob + pr
    }
  }
  list(outlier_prob = prob / ndraw, mu = mean(keep[, 1]), sigma2 = mean(keep[, 2]))
}

.br_ab <- function(y, M, nu, s2, incl, ndraw, burn_in, seed) {
  M <- as.matrix(M)
  n <- nrow(M)
  p <- ncol(M)
  mm <- colSums(M^2)
  my <- sum(y) / n
  vy <- sum((y - my)^2) / (n - 1)
  S2 <- if (is.null(s2)) vy * (nu - 2) / (nu * p * incl) else s2
  st <- .br_stream(seed)
  u <- numeric(p)
  sj <- rep(S2, p)
  delta <- rep(1, p)
  mu <- my
  se <- vy / 2
  e <- y - mu
  keep <- matrix(0, ndraw, p + 2)
  keepd <- matrix(0, ndraw, p)
  for (it in seq_len(ndraw + burn_in)) {
    e <- e + mu
    mu <- sum(e) / n + .br_norm(st) * sqrt(se / n)
    e <- e - mu
    for (j in seq_len(p)) {
      cj <- M[, j]
      rhs <- sum(cj * e) + mm[j] * u[j]
      if (incl < 1) {
        cand <- if (delta[j] == 1) sj[j] else nu * S2 / (2 * .br_gamma(st, nu / 2))
        lr <- -0.5 * log(1 + mm[j] * cand / se) + rhs^2 * cand / (2 * se * (se + mm[j] * cand))
        p1 <- if (lr > -700) incl / (incl + (1 - incl) * exp(-lr)) else 0
        delta[j] <- if (.br_unif(st) < p1) 1 else 0
        if (delta[j] == 1) sj[j] <- cand
      } else {
        .br_unif(st)
      }
      if (delta[j] == 1) {
        prec <- mm[j] / se + 1 / sj[j]
        new <- rhs / se / prec + .br_norm(st) / sqrt(prec)
        sj[j] <- (nu * S2 + new^2) / (2 * .br_gamma(st, (nu + 1) / 2))
      } else {
        new <- 0
        .br_unif(st)
        .br_unif(st)
      }
      e <- e + cj * (u[j] - new)
      u[j] <- new
    }
    se <- sum(e^2) / (2 * .br_gamma(st, (n - 2) / 2))
    if (it > burn_in) {
      keep[it - burn_in, ] <- c(u, mu, se)
      keepd[it - burn_in, ] <- delta
    }
  }
  s <- .br_summ(keep)
  list(effects = s$mean[1:p], effects_sd = s$sd[1:p], mu = s$mean[p + 1], sigma2_e = s$mean[p + 2],
       inclusion = colMeans(keepd))
}

#' @rdname BayesLinearHalfcauchy
#' @export
BayesA <- function(y, M, nu = 4.012, s2 = NULL, ndraw = 2000, burn_in = 500, seed = 0) {
  .br_ab(y, M, nu, s2, 1, ndraw, burn_in, seed)
}

#' @rdname BayesLinearHalfcauchy
#' @export
BayesB <- function(y, M, pi = 0.95, nu = 4.012, s2 = NULL, ndraw = 2000, burn_in = 500, seed = 0) {
  .br_ab(y, M, nu, s2, 1 - pi, ndraw, burn_in, seed)
}

#' @rdname BayesLinearHalfcauchy
#' @export
GenomicReliability <- function(pev, sigma2_a) {
  rel <- 1 - pev / sigma2_a
  list(reliability = rel, accuracy = sqrt(pmax(rel, 0)))
}
