# ABC with a Gaussian-process surrogate.
# Sources: Wilkinson, R. D. (2014) AISTATS PMLR 33, 1015-1023 (the
# default route: GP on log GABC likelihood, Sec. 2.1, with sequential
# history matching, Sec. 3); Meeds, E. & Welling, M. (2014) UAI 593-602
# (Algorithms 1-3; eq. (9) synthetic likelihood, eq. (11) sampling
# distribution of the mean, eq. (12)-(16) decision error); Wood, S. N.
# (2010) Nature 466, 1102-1104 (synthetic likelihood); Craig, P. S. et
# al. (1997) Case Studies in Bayesian Statistics III, 37-93 (history
# matching); Sobol, I. M. (1967) USSR Comp. Math. Math. Phys. 7(4),
# 86-112, with direction numbers of Bratley & Fox (1988) ACM TOMS 14(1),
# 88-100; Rasmussen & Williams (2006) GPML, MIT Press.
#
# Native implementation mirroring morie.fn.abcgp: same Sobol design, the
# same log-sum-exp shift in the GABC likelihood, the same conjugate-GP
# posterior with the quadratic mean, the same Wilkinson implausibility
# rule m + 3 sigma < max l - 10, and the same Meeds & Welling decision
# rule tau = median(alpha), E(alpha) < xi.

.abcgp.sobol_poly <- list(
  NULL,
  list(degree = 1L, coeff = 0L,  m_init = c(1L)),
  list(degree = 2L, coeff = 1L,  m_init = c(1L, 3L)),
  list(degree = 3L, coeff = 1L,  m_init = c(1L, 3L, 1L)),
  list(degree = 3L, coeff = 2L,  m_init = c(1L, 1L, 1L)),
  list(degree = 4L, coeff = 1L,  m_init = c(1L, 1L, 3L, 3L)),
  list(degree = 4L, coeff = 4L,  m_init = c(1L, 3L, 5L, 13L)),
  list(degree = 5L, coeff = 2L,  m_init = c(1L, 1L, 5L, 5L, 17L))
)

#' .abcgp.lse
#'
#' A step of the abcgp_native implementation. Called by \code{.abcgp.gabc_log_likelihood}, \code{morie_abcgp}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param values A vector; indexed elementwise.
#' @return A numeric value.
#' @export
.abcgp.lse <- function(values) {
  vals <- values[!is.na(values)]
  if (length(vals) == 0L) return(-Inf)
  a <- max(vals)
  if (a == -Inf) return(-Inf)
  a + log(sum(exp(vals - a)))
}

#' .abcgp.sobol_dir
#'
#' A step of the abcgp_native implementation. Called by \code{.abcgp.sobol_sequence}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param dim A count; the body uses it as \code{seq_len(...)}.
#' @param bits A count; the body uses it as \code{seq_len(...)}.
#' @return The value of \code{out}, as built in the body.
#' @export
.abcgp.sobol_dir <- function(dim, bits) {
  out <- vector("list", dim)
  for (d in seq_len(dim)) {
    entry <- .abcgp.sobol_poly[[d]]
    if (is.null(entry)) {
      m <- rep(1L, bits)
    } else {
      m <- as.integer(entry$m_init)
      deg <- entry$degree; coeff <- entry$coeff
      for (k in seq.int(deg, bits - 1L)) {
        val <- m[[k - deg + 1L]]
        term <- bitwXor(val, bitwShiftL(val, deg))
        for (j in seq_len(deg - 1L)) {
          bit <- bitwAnd(bitwShiftR(coeff, deg - 1L - j), 1L)
          if (bit == 1L) term <- bitwXor(term, bitwShiftL(m[[k - j + 1L]], j))
        }
        m[[k + 1L]] <- term
      }
    }
    out[[d]] <- as.integer(m) * bitwShiftL(1L, bits - seq_len(bits))
  }
  out
}

#' .abcgp.sobol_sequence
#'
#' A step of the abcgp_native implementation. Called by \code{.abcgp.design_from_prior}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n A count; the body uses it as \code{matrix(...)}.
#' @param dim A count; the body uses it as \code{seq_len(...)}.
#' @param skip Coerced to integer by the body, with \code{as.integer}. Defaults to \code{0}.
#' @return The value of \code{out}, as built in the body.
#' @export
.abcgp.sobol_sequence <- function(n, dim, skip = 0) {
  n <- as.integer(n); dim <- as.integer(dim)
  if (n < 1L) stop("sobol_sequence: n must be at least 1")
  if (dim < 1L || dim > length(.abcgp.sobol_poly))
    stop("sobol_sequence: dim must be between 1 and ", length(.abcgp.sobol_poly))
  total <- n + as.integer(skip)
  bits <- max(1L, as.integer(ceiling(log(total + 1, 2))) + 1L)
  v <- .abcgp.sobol_dir(dim, bits)
  out <- matrix(0, nrow = n, ncol = dim)
  x <- integer(dim)
  for (i in 0:(total - 1L)) {
    if (i >= as.integer(skip)) out[i - as.integer(skip) + 1L, ] <- x / (2 ^ bits)
    c <- 0L; value <- i
    while (bitwAnd(value, 1L) == 1L) { value <- bitwShiftR(value, 1L); c <- c + 1L }
    for (d in seq_len(dim)) x[d] <- bitwXor(x[d], v[[d]][c + 1L])
  }
  out
}

#' .abcgp.summarise
#'
#' A step of the abcgp_native implementation. Called by \code{.abcgp.gabc_log_likelihood}, \code{.abcgp.mw_sampler}, \code{.abcgp.synthetic_log_likelihood}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Passed to \code{summary}.
#' @param summary Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @return A vector, from \code{as.numeric}.
#' @export
.abcgp.summarise <- function(x, summary) {
  v <- if (is.null(summary)) x else summary(x)
  as.numeric(v)
}

#' .abcgp.chol
#'
#' A step of the abcgp_native implementation. Called by \code{.abcgp.draw_mean}, \code{.abcgp.gp_fit}, \code{.abcgp.mvn_logpdf} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a A matrix; indexed by row and column.
#' @param jitter Defaults to \code{1e-12}.
#' @return The value of \code{L}, as built in the body.
#' @export
.abcgp.chol <- function(a, jitter = 1e-12) {
  n <- length(a)
  L <- matrix(0, n, n)
  for (i in seq_len(n)) for (j in seq_len(i)) {
    s <- a[i, j] - sum(L[i, seq_len(j - 1L)] * L[j, seq_len(j - 1L)])
    if (i == j) {
      if (s <= 0) s <- jitter
      L[i, j] <- sqrt(s)
    } else L[i, j] <- s / L[j, j]
  }
  L
}

#' .abcgp.chol_solve
#'
#' A step of the abcgp_native implementation. Called by \code{.abcgp.gp_fit}, \code{.abcgp.gp_predict}, \code{.abcgp.mvn_logpdf} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param L A matrix; indexed by row and column.
#' @param b A vector; its length is taken and its elements indexed.
#' @return The value of \code{x}, as built in the body.
#' @export
.abcgp.chol_solve <- function(L, b) {
  n <- length(b); y <- numeric(n)
  for (i in seq_len(n))
    y[i] <- (b[i] - sum(L[i, seq_len(i - 1L)] * y[seq_len(i - 1L)])) / L[i, i]
  x <- numeric(n)
  for (i in n:1L)
    x[i] <- (y[i] - sum(L[seq.int(i + 1L, n), i] * x[seq.int(i + 1L, n)])) / L[i, i]
  x
}

#' .abcgp.mvn_logpdf
#'
#' A step of the abcgp_native implementation. Called by \code{.abcgp.mw_sampler}, \code{.abcgp.synthetic_log_likelihood}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y A vector; its length is taken.
#' @param mu Numeric; combined arithmetically in the body.
#' @param cov Passed to \code{.abcgp.chol}.
#' @return A numeric value.
#' @export
.abcgp.mvn_logpdf <- function(y, mu, cov) {
  n <- length(y)
  L <- .abcgp.chol(cov)
  diff <- y - mu
  alpha <- .abcgp.chol_solve(L, diff)
  quad <- sum(diff * alpha)
  logdet <- 2 * sum(log(diag(L)))
  -0.5 * (quad + logdet + n * log(2 * pi))
}

#' .abcgp.corr
#'
#' A step of the abcgp_native implementation. Called by \code{.abcgp.gp_fit}, \code{.abcgp.gp_predict}, \code{.abcgp.profile_nll}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a Numeric; combined arithmetically in the body.
#' @param b Numeric; combined arithmetically in the body.
#' @param lengthscale Numeric; combined arithmetically in the body.
#' @param kernel One of \code{"matern32"}, \code{"sqexp"}.
#' @return A numeric value.
#' @export
.abcgp.corr <- function(a, b, lengthscale, kernel) {
  r2 <- sum(((a - b) / lengthscale) ^ 2)
  if (kernel == "sqexp") return(exp(-0.5 * r2))
  r <- sqrt(r2)
  if (kernel == "matern32") { s <- sqrt(3) * r; return((1 + s) * exp(-s)) }
  s <- sqrt(5) * r; (1 + s + s * s / 3) * exp(-s)
}

#' .abcgp.basis
#'
#' A step of the abcgp_native implementation. Called by \code{.abcgp.gp_predict}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param theta Coerced to numeric by the body, with \code{as.numeric}.
#' @return A vector, from \code{c}.
#' @export
.abcgp.basis <- function(theta)
  c(1, as.numeric(theta), as.numeric(theta) ^ 2)

#' .abcgp.as_nugget
#'
#' A step of the abcgp_native implementation. Called by \code{.abcgp.gp_fit}, \code{.abcgp.mle_lengthscale}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param nugget Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param n A count; the body uses it as \code{rep(...)}.
#' @return The value of \code{pmax}.
#' @export
.abcgp.as_nugget <- function(nugget, n) {
  if (is.null(nugget)) return(rep(1e-8, n))
  v <- as.numeric(nugget)
  if (length(v) == 1L) v <- rep(v, n)
  if (length(v) != n) stop("gp_fit: nugget length does not match n")
  pmax(v, 1e-12)
}

#' .abcgp.profile_nll
#'
#' A step of the abcgp_native implementation. Called by \code{.abcgp.mle_lengthscale}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; indexed by row and column.
#' @param y Numeric; combined arithmetically in the body.
#' @param ls Passed to \code{.abcgp.corr}.
#' @param nug A vector; indexed elementwise.
#' @param kernel Passed to \code{.abcgp.corr}.
#' @return A numeric value.
#' @export
.abcgp.profile_nll <- function(X, y, ls, nug, kernel) {
  n <- nrow(X)
  A <- matrix(0, n, n)
  for (i in seq_len(n)) for (j in seq_len(n))
    A[i, j] <- .abcgp.corr(X[i, ], X[j, ], ls, kernel) +
      (if (i == j) nug[i] else 0)
  L <- .abcgp.chol(A)
  H <- t(apply(X, 1, .abcgp.basis))
  q <- ncol(H)
  if (n <= q) return(Inf)
  Ainv_y <- .abcgp.chol_solve(L, y)
  Ainv_H <- matrix(0, q, n)
  for (k in seq_len(q)) Ainv_H[k, ] <- .abcgp.chol_solve(L, H[, k])
  HtAinvH <- H %*% t(Ainv_H)
  HtAinvy <- as.numeric(H %*% Ainv_y)
  beta <- tryCatch(.abcgp.chol_solve(.abcgp.chol(HtAinvH), HtAinvy),
                   error = function(e) NULL)
  if (is.null(beta)) return(Inf)
  resid <- y - as.numeric(H %*% beta)
  Ainv_r <- .abcgp.chol_solve(L, resid)
  s2 <- sum(resid * Ainv_r) / (n - q)
  if (s2 <= 0) return(Inf)
  logdet <- 2 * sum(log(diag(L)))
  0.5 * (logdet + (n - q) * log(s2))
}

#' .abcgp.mle_lengthscale
#'
#' A step of the abcgp_native implementation. Called by \code{.abcgp.gp_fit}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; passed to \code{nrow}.
#' @param y Passed to \code{.abcgp.profile_nll}.
#' @param nugget Passed to \code{.abcgp.as_nugget}.
#' @param kernel Passed to \code{.abcgp.profile_nll}.
#' @return The value of \code{ls}, as built in the body.
#' @export
.abcgp.mle_lengthscale <- function(X, y, nugget, kernel) {
  n <- nrow(X); p <- ncol(X)
  nug <- .abcgp.as_nugget(nugget, n)
  spans <- apply(X, 2, function(c) { s <- diff(range(c)); if (s > 0) s else 1 })
  ls <- 0.5 * spans
  best <- .abcgp.profile_nll(X, y, ls, nug, kernel)
  grid <- c(0.05, 0.1, 0.2, 0.35, 0.5, 0.75, 1, 1.5, 2, 3)
  for (iter in 1:3) {
    improved <- FALSE
    for (j in seq_len(p)) for (g in grid) {
      trial <- ls; trial[j] <- g * spans[j]
      val <- .abcgp.profile_nll(X, y, trial, nug, kernel)
      if (val < best - 1e-12) { best <- val; ls <- trial; improved <- TRUE }
    }
    if (!improved) break
  }
  ls
}

#' .abcgp.gp_fit
#'
#' A step of the abcgp_native implementation. Called by \code{.abcgp.history_match}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param design A matrix; passed to \code{as.matrix}.
#' @param values Coerced to numeric by the body, with \code{as.numeric}.
#' @param nugget Passed to \code{.abcgp.mle_lengthscale}.
#' @param lengthscale Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param kernel Passed to \code{.abcgp.mle_lengthscale}. Defaults to \code{"sqexp"}.
#' @param tau2 Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{design}, \code{values}, \code{beta}, \code{tau2}, \code{lengthscale}, \code{kernel}, \code{nugget}, \code{chol}, \code{Ainv_r}, \code{Ainv_H}, \code{H}, \code{HtAinvH_chol}, \code{n}, \code{q}, \code{dim}.
#' @export
.abcgp.gp_fit <- function(design, values, nugget = NULL, lengthscale = NULL,
                          kernel = "sqexp", tau2 = NULL) {
  kernels <- c("sqexp", "matern32", "matern52")
  if (!(kernel %in% kernels))
    stop("gp_fit: kernel must be one of sqexp/matern32/matern52")
  X <- as.matrix(design); y <- as.numeric(values)
  n <- nrow(X)
  if (n != length(y)) stop("gp_fit: design and values length mismatch")
  if (n < 3) stop("gp_fit: need at least 3 design points")
  p <- ncol(X)
  if (is.null(lengthscale)) lengthscale <- .abcgp.mle_lengthscale(X, y, nugget, kernel)
  ls <- as.numeric(lengthscale)
  if (length(ls) == 1L) ls <- rep(ls, p)
  if (any(ls <= 0)) stop("gp_fit: length-scales must be positive")
  nug <- .abcgp.as_nugget(nugget, n)
  A <- matrix(0, n, n)
  for (i in seq_len(n)) for (j in seq_len(n))
    A[i, j] <- .abcgp.corr(X[i, ], X[j, ], ls, kernel) +
      (if (i == j) nug[i] else 0)
  L <- .abcgp.chol(A)
  H <- t(apply(X, 1, .abcgp.basis))
  q <- ncol(H)
  if (n <= q) stop("gp_fit: not enough points for quadratic mean")
  Ainv_y <- .abcgp.chol_solve(L, y)
  Ainv_H <- matrix(0, q, n)
  for (k in seq_len(q)) Ainv_H[k, ] <- .abcgp.chol_solve(L, H[, k])
  HtAinvH <- H %*% t(Ainv_H)
  HtAinvy <- as.numeric(H %*% Ainv_y)
  Lh <- .abcgp.chol(HtAinvH)
  beta <- .abcgp.chol_solve(Lh, HtAinvy)
  resid <- y - as.numeric(H %*% beta)
  Ainv_r <- .abcgp.chol_solve(L, resid)
  if (is.null(tau2)) tau2 <- sum(resid * Ainv_r) / (n - q)
  list(design = X, values = y, beta = beta, tau2 = as.numeric(tau2),
       lengthscale = ls, kernel = kernel, nugget = nug, chol = L,
       Ainv_r = Ainv_r, Ainv_H = Ainv_H, H = H, HtAinvH_chol = Lh,
       n = n, q = q, dim = p)
}

#' .abcgp.gp_predict
#'
#' A step of the abcgp_native implementation. Called by \code{.abcgp.implausible}, \code{morie_abcgp}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param fit A list; the body reads \code{$Ainv_r}, \code{$beta}, \code{$chol}, \code{$design}, \code{$dim}, \code{$H}, \code{$HtAinvH_chol}, \code{$kernel}, \code{$lengthscale}, \code{$n}, \code{$tau2} from it.
#' @param theta Coerced to numeric by the body, with \code{as.numeric}.
#' @return A vector, from \code{c}.
#' @export
.abcgp.gp_predict <- function(fit, theta) {
  t <- as.numeric(theta)
  if (length(t) != fit$dim)
    stop("gp_predict: theta has wrong length")
  k <- numeric(fit$n)
  for (i in seq_len(fit$n))
    k[i] <- .abcgp.corr(t, fit$design[i, ], fit$lengthscale, fit$kernel)
  h <- .abcgp.basis(t)
  mean <- sum(h * fit$beta) + sum(k * fit$Ainv_r)
  Ainv_k <- .abcgp.chol_solve(fit$chol, k)
  var <- 1 - sum(k * Ainv_k)
  hh <- h - as.numeric(fit$H %*% Ainv_k)
  w <- .abcgp.chol_solve(fit$HtAinvH_chol, hh)
  var <- var + sum(hh * w)
  var <- fit$tau2 * max(var, 0)
  c(mean, sqrt(var))
}

#' .abcgp.implausible
#'
#' A step of the abcgp_native implementation. Called by \code{.abcgp.history_match}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param fit A list; the body reads \code{$values} from it.
#' @param theta Passed to \code{.abcgp.gp_predict}.
#' @param threshold Numeric; combined arithmetically in the body. Defaults to \code{10}.
#' @param n_sd Numeric; combined arithmetically in the body. Defaults to \code{3}.
#' @return A logical value.
#' @export
.abcgp.implausible <- function(fit, theta, threshold = 10, n_sd = 3) {
  pr <- .abcgp.gp_predict(fit, theta)
  pr[1] + n_sd * pr[2] < max(fit$values) - threshold
}

#' .abcgp.design_from_prior
#'
#' A step of the abcgp_native implementation. Called by \code{.abcgp.history_match}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n A count; the body uses it as \code{seq_len(...)}.
#' @param prior_ppf A vector; its length is taken and its elements indexed.
#' @param dim Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @param skip Passed to \code{.abcgp.sobol_sequence}. Defaults to \code{1}.
#' @return The value of \code{out}, as built in the body.
#' @export
.abcgp.design_from_prior <- function(n, prior_ppf, dim = NULL, skip = 1) {
  if (is.numeric(prior_ppf) || is.matrix(prior_ppf)) {
    prior_ppf <- list(prior_ppf)  # treat as (lo, hi) below
  }
  if (is.list(prior_ppf) && length(prior_ppf) == 2 &&
      !is.function(prior_ppf[[1]])) {
    lo <- as.numeric(prior_ppf[[1]]); hi <- as.numeric(prior_ppf[[2]])
    if (length(lo) != length(hi))
      stop("design_from_prior: lo and hi differ in length")
    u <- .abcgp.sobol_sequence(n, length(lo), skip = skip)
    out <- matrix(0, n, length(lo))
    for (i in seq_len(n)) out[i, ] <- lo + (hi - lo) * u[i, ]
    return(out)
  }
  fns <- prior_ppf
  if (!is.null(dim) && as.integer(dim) != length(fns))
    stop("design_from_prior: dim mismatch")
  u <- .abcgp.sobol_sequence(n, length(fns), skip = skip)
  out <- matrix(0, n, length(fns))
  for (i in seq_len(n)) for (j in seq_along(fns)) out[i, j] <- fns[[j]](u[i, j])
  out
}

#' .abcgp.gabc_log_likelihood
#'
#' A step of the abcgp_native implementation. Called by \code{.abcgp.history_match}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param sim Accepted by the signature and not used anywhere in the body.
#' @param obs Passed to \code{.abcgp.summarise}.
#' @param theta Passed to \code{sim}.
#' @param n_sim A count; the body uses it as \code{seq_len(...)}. Defaults to \code{50}.
#' @param epsilon Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @param summary Passed to \code{.abcgp.summarise}.
#' @param kernel One of \code{"gaussian"}, \code{"uniform"}. Defaults to \code{"gaussian"}.
#' @param seed Passed to \code{.ghc_rng}. Defaults to \code{0}.
#' @param bootstrap Coerced to integer by the body, with \code{as.integer}. Defaults to \code{25}.
#' @return A vector, from \code{c}.
#' @export
.abcgp.gabc_log_likelihood <- function(sim, obs, theta, n_sim = 50, epsilon = 1,
                                      summary = NULL, kernel = "gaussian",
                                      seed = 0, bootstrap = 25) {
  if (!(kernel %in% c("gaussian", "uniform")))
    stop("gabc_log_likelihood: kernel must be 'gaussian' or 'uniform'")
  if (epsilon <= 0) stop("gabc_log_likelihood: epsilon must be positive")
  d_obs <- .abcgp.summarise(obs, summary)
  e <- .ghc_rng(seed)
  terms <- numeric(n_sim)
  for (k in seq_len(n_sim)) {
    x <- sim(theta, e)
    s <- .abcgp.summarise(x, summary)
    if (length(s) != length(d_obs))
      stop("gabc_log_likelihood: simulator summary length mismatch")
    rho <- sqrt(sum((s - d_obs) ^ 2))
    terms[k] <- if (kernel == "uniform")
      if (rho <= epsilon) 0 else -Inf
    else -0.5 * (rho / epsilon) ^ 2
  }
  m <- length(terms)
  log_lik <- .abcgp.lse(terms) - log(m)
  reps <- as.integer(bootstrap)
  if (reps < 2 || m < 2) return(c(log_lik, 0))
  boot <- numeric(reps)
  for (b in seq_len(reps)) {
    idx <- pmin(as.integer(.ghc_unif(e, m) * m), m - 1L) + 1L
    boot[b] <- .abcgp.lse(terms[idx]) - log(m)
  }
  finite <- boot[boot > -Inf]
  if (length(finite) < 2) return(c(log_lik, 0))
  mu <- mean(finite)
  var <- sum((finite - mu) ^ 2) / (length(finite) - 1)
  c(log_lik, var)
}

#' .abcgp.synthetic_log_likelihood
#'
#' A step of the abcgp_native implementation. Called by \code{.abcgp.mw_sampler}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param draws Iterated over elementwise, with \code{lapply}.
#' @param obs Passed to \code{.abcgp.summarise}.
#' @param epsilon Numeric; combined arithmetically in the body. Defaults to \code{0}.
#' @param summary Passed to \code{.abcgp.summarise}.
#' @return A list with \code{log_lik}, \code{mu}, \code{cov}.
#' @export
.abcgp.synthetic_log_likelihood <- function(draws, obs, epsilon = 0,
                                            summary = NULL) {
  rows <- lapply(draws, function(x) .abcgp.summarise(x, summary))
  S <- length(rows)
  if (S < 2) stop("synthetic_log_likelihood: need at least 2 simulations")
  J <- length(rows[[1]])
  y <- .abcgp.summarise(obs, summary)
  if (length(y) != J)
    stop("synthetic_log_likelihood: summary length mismatch")
  mat <- do.call(rbind, rows)
  mu <- colMeans(mat)
  cov <- cov(mat)
  e2 <- epsilon ^ 2
  diag(cov) <- diag(cov) + e2
  list(log_lik = .abcgp.mvn_logpdf(y, mu, cov), mu = mu, cov = cov)
}

#' .abcgp.history_match
#'
#' A step of the abcgp_native implementation. Called by \code{morie_abcgp}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param sim Passed to \code{.abcgp.gabc_log_likelihood}.
#' @param obs Passed to \code{.abcgp.gabc_log_likelihood}.
#' @param prior_ppf Passed to \code{.abcgp.design_from_prior}.
#' @param n_waves Coerced to integer by the body, with \code{as.integer}. Defaults to \code{3}.
#' @param n_design Coerced to integer by the body, with \code{as.integer}. Defaults to \code{32}.
#' @param n_sim Passed to \code{.abcgp.gabc_log_likelihood}. Defaults to \code{50}.
#' @param epsilon Passed to \code{.abcgp.gabc_log_likelihood}. Defaults to \code{1}.
#' @param summary Passed to \code{.abcgp.gabc_log_likelihood}.
#' @param threshold Passed to \code{.abcgp.implausible}. Defaults to \code{10}.
#' @param n_sd Passed to \code{.abcgp.implausible}. Defaults to \code{3}.
#' @param kernel Passed to \code{.abcgp.gp_fit}. Defaults to \code{"sqexp"}.
#' @param accept_kernel Passed to \code{.abcgp.gabc_log_likelihood}. Defaults to \code{"gaussian"}.
#' @param seed Coerced to integer by the body, with \code{as.integer}. Defaults to \code{0}.
#' @return A list with \code{fit}, \code{waves}.
#' @export
.abcgp.history_match <- function(sim, obs, prior_ppf, n_waves = 3,
                                 n_design = 32, n_sim = 50, epsilon = 1,
                                 summary = NULL, threshold = 10, n_sd = 3,
                                 kernel = "sqexp", accept_kernel = "gaussian",
                                 seed = 0) {
  ex <- list(); ey <- numeric(0); ev <- numeric(0)
  waves <- list(); fit <- NULL
  for (w in seq_len(as.integer(n_waves))) {
    cand <- .abcgp.design_from_prior(as.integer(n_design) * 4L, prior_ppf,
                                     skip = 1L + (w - 1L) * as.integer(n_design) * 4L)
    if (!is.null(fit)) {
      keep <- apply(cand, 1, function(r)
        !.abcgp.implausible(fit, r, threshold, n_sd))
      ruled <- sum(!keep)
      rows <- cand[keep, , drop = FALSE]
      if (nrow(rows) == 0L) rows <- cand[seq_len(as.integer(n_design)), , drop = FALSE]
    } else { ruled <- 0; rows <- cand }
    rows <- rows[seq_len(min(nrow(rows), as.integer(n_design))), , drop = FALSE]
    for (i in seq_len(nrow(rows))) {
      out <- .abcgp.gabc_log_likelihood(sim, obs, rows[i, ],
                                        n_sim = n_sim, epsilon = epsilon,
                                        summary = summary,
                                        kernel = accept_kernel,
                                        seed = as.integer(seed) + 1000L * (w - 1L) + (i - 1L))
      if (out[1] > -Inf) {
        ex[[length(ex) + 1L]] <- rows[i, ]
        ey <- c(ey, out[1]); ev <- c(ev, out[2])
      }
    }
    if (length(ex) < 3L)
      stop("history_match: too many rejections, epsilon may be too small")
    fit <- .abcgp.gp_fit(do.call(rbind, ex), ey, nugget = ev, kernel = kernel)
    waves[[length(waves) + 1L]] <- list(wave = w - 1L,
                                        n_ensemble = length(ex),
                                        ruled_implausible = ruled,
                                        max_log_lik = max(ey))
  }
  list(fit = fit, waves = waves)
}

#' .abcgp.alpha_terms
#'
#' A step of the abcgp_native implementation. Called by \code{.abcgp.mw_sampler}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param log_prior Accepted by the signature and not used anywhere in the body.
#' @param theta Passed to \code{log_prior}.
#' @param theta_p Passed to \code{log_prior}.
#' @param ll Numeric; combined arithmetically in the body.
#' @param ll_p Numeric; combined arithmetically in the body.
#' @param log_q Numeric; combined arithmetically in the body.
#' @param log_q_p Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.abcgp.alpha_terms <- function(log_prior, theta, theta_p, ll, ll_p,
                               log_q, log_q_p)
  min(0, (log_prior(theta_p) + ll_p + log_q_p) - (log_prior(theta) + ll + log_q))

#' .abcgp.expected_error
#'
#' A step of the abcgp_native implementation. Called by \code{.abcgp.mw_sampler}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param alphas A vector; its length is taken.
#' @param tau Passed to \code{<=}.
#' @param n_grid A count; the body uses it as \code{seq_len(...)}. Defaults to \code{101}.
#' @return A numeric value.
#' @export
.abcgp.expected_error <- function(alphas, tau, n_grid = 101) {
  M <- length(alphas); total <- 0
  for (i in seq_len(n_grid)) {
    u <- (i - 0.5) / n_grid
    if (u <= tau) err <- sum(alphas < u) / M
    else err <- sum(alphas >= u) / M
    total <- total + err
  }
  total / n_grid
}

#' .abcgp.median
#'
#' A step of the abcgp_native implementation. Called by \code{.abcgp.mw_sampler}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v Numeric; passed to \code{sort}.
#' @return One of two values, depending on the branch taken.
#' @export
.abcgp.median <- function(v) {
  s <- sort(v); n <- length(s)
  if (n %% 2L == 1L) s[(n + 1L) %/% 2L] else 0.5 * (s[n %/% 2L] + s[n %/% 2L + 1L])
}

#' .abcgp.draw_mean
#'
#' A step of the abcgp_native implementation. Called by \code{.abcgp.mw_sampler}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param mu A vector; its length is taken.
#' @param cov Numeric; combined arithmetically in the body.
#' @param S Numeric; combined arithmetically in the body.
#' @param e Passed to \code{.ghc_norm}.
#' @return A numeric value.
#' @export
.abcgp.draw_mean <- function(mu, cov, S, e) {
  n <- length(mu)
  scaled <- cov / S
  L <- .abcgp.chol(scaled)
  z <- .ghc_norm(e, n)
  as.numeric(mu) + L %*% z
}

#' .abcgp.synthetic_abc
#'
#' A step of the abcgp_native implementation. Called by \code{morie_abcgp}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param sim Passed to \code{.abcgp.mw_sampler}.
#' @param obs Passed to \code{.abcgp.mw_sampler}.
#' @param log_prior Passed to \code{.abcgp.mw_sampler}.
#' @param theta0 Passed to \code{.abcgp.mw_sampler}.
#' @param n_iter Passed to \code{.abcgp.mw_sampler}. Defaults to \code{200}.
#' @param n_sim Passed to \code{.abcgp.mw_sampler}. Defaults to \code{20}.
#' @param epsilon Passed to \code{.abcgp.mw_sampler}. Defaults to \code{0}.
#' @param proposal_sd Passed to \code{.abcgp.mw_sampler}. Defaults to \code{0.5}.
#' @param summary Passed to \code{.abcgp.mw_sampler}.
#' @param seed Passed to \code{.abcgp.mw_sampler}. Defaults to \code{0}.
#' @return The value of \code{.abcgp.mw_sampler}.
#' @export
.abcgp.synthetic_abc <- function(sim, obs, log_prior, theta0, n_iter = 200,
                                 n_sim = 20, epsilon = 0, proposal_sd = 0.5,
                                 summary = NULL, seed = 0)
  .abcgp.mw_sampler(sim, obs, log_prior, theta0, n_iter, n_sim, epsilon,
                    proposal_sd, summary, seed, FALSE, NULL, 0, 0)

#' .abcgp.gps_abc
#'
#' A step of the abcgp_native implementation. Called by \code{morie_abcgp}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param sim Passed to \code{.abcgp.mw_sampler}.
#' @param obs Passed to \code{.abcgp.mw_sampler}.
#' @param log_prior Passed to \code{.abcgp.mw_sampler}.
#' @param theta0 Passed to \code{.abcgp.mw_sampler}.
#' @param n_iter Passed to \code{.abcgp.mw_sampler}. Defaults to \code{200}.
#' @param n_sim Passed to \code{.abcgp.mw_sampler}. Defaults to \code{10}.
#' @param epsilon Passed to \code{.abcgp.mw_sampler}. Defaults to \code{0}.
#' @param proposal_sd Passed to \code{.abcgp.mw_sampler}. Defaults to \code{0.5}.
#' @param summary Passed to \code{.abcgp.mw_sampler}.
#' @param seed Passed to \code{.abcgp.mw_sampler}. Defaults to \code{0}.
#' @param xi Passed to \code{.abcgp.mw_sampler}. Defaults to \code{0.05}.
#' @param delta_s Passed to \code{.abcgp.mw_sampler}. Defaults to \code{10}.
#' @param n_alpha Passed to \code{.abcgp.mw_sampler}. Defaults to \code{64}.
#' @param max_sim Passed to \code{.abcgp.mw_sampler}. Defaults to \code{400}.
#' @return The value of \code{.abcgp.mw_sampler}.
#' @export
.abcgp.gps_abc <- function(sim, obs, log_prior, theta0, n_iter = 200,
                           n_sim = 10, epsilon = 0, proposal_sd = 0.5,
                           summary = NULL, seed = 0, xi = 0.05, delta_s = 10,
                           n_alpha = 64, max_sim = 400)
  .abcgp.mw_sampler(sim, obs, log_prior, theta0, n_iter, n_sim, epsilon,
                    proposal_sd, summary, seed, TRUE, xi, delta_s, n_alpha,
                    max_sim)

#' .abcgp.mw_sampler
#'
#' A step of the abcgp_native implementation. Called by \code{.abcgp.gps_abc}, \code{.abcgp.synthetic_abc}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param sim Accepted by the signature and not used anywhere in the body.
#' @param obs Passed to \code{.abcgp.synthetic_log_likelihood}.
#' @param log_prior Passed to \code{.abcgp.alpha_terms}.
#' @param theta0 Coerced to numeric by the body, with \code{as.numeric}.
#' @param n_iter Numeric; combined arithmetically in the body.
#' @param n_sim Coerced to integer by the body, with \code{as.integer}.
#' @param epsilon Passed to \code{.abcgp.synthetic_log_likelihood}.
#' @param proposal_sd Coerced to numeric by the body, with \code{as.numeric}.
#' @param summary Passed to \code{.abcgp.synthetic_log_likelihood}.
#' @param seed Passed to \code{.ghc_rng}.
#' @param adaptive A flag; the body branches on it.
#' @param xi Passed to \code{<}.
#' @param delta_s Coerced to integer by the body, with \code{as.integer}.
#' @param n_alpha Coerced to integer by the body, with \code{as.integer}.
#' @param max_sim Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{chain}, \code{acceptance_rate}, \code{n_simulations}, \code{unresolved_steps}.
#' @export
.abcgp.mw_sampler <- function(sim, obs, log_prior, theta0, n_iter, n_sim,
                             epsilon, proposal_sd, summary, seed,
                             adaptive, xi, delta_s, n_alpha, max_sim = NULL) {
  e <- .ghc_rng(seed)
  theta <- as.numeric(theta0)
  p <- length(theta)
  sd <- as.numeric(proposal_sd)
  if (length(sd) == 1L) sd <- rep(sd, p)
  chain <- list(theta)
  n_accept <- 0L; unresolved <- 0L; sims_used <- 0L
  for (it in seq_len(as.integer(n_iter))) {
    prop <- theta + sd * .ghc_norm(e, p)
    if (log_prior(prop) == -Inf) { chain[[length(chain) + 1L]] <- theta; next }
    S <- as.integer(n_sim)
    repeat {
      cur <- replicate(S, sim(theta, e), simplify = FALSE)
      new <- replicate(S, sim(prop, e), simplify = FALSE)
      sims_used <- sims_used + 2L * S
      lc <- .abcgp.synthetic_log_likelihood(cur, obs, epsilon, summary)
      lp <- .abcgp.synthetic_log_likelihood(new, obs, epsilon, summary)
      if (!adaptive) {
        loga <- .abcgp.alpha_terms(log_prior, theta, prop, lc$log_lik,
                                   lp$log_lik, 0, 0)
        tau <- exp(loga)
        break
      }
      y <- .abcgp.summarise(obs, summary)
      alphas <- numeric(as.integer(n_alpha))
      for (j in seq_along(alphas)) {
        mc <- .abcgp.draw_mean(lc$mu, lc$cov, S, e)
        mp <- .abcgp.draw_mean(lp$mu, lp$cov, S, e)
        a <- min(0, (log_prior(prop) + .abcgp.mvn_logpdf(y, mp, lp$cov)) -
                   (log_prior(theta) + .abcgp.mvn_logpdf(y, mc, lc$cov)))
        alphas[j] <- exp(a)
      }
      tau <- .abcgp.median(alphas)
      err <- .abcgp.expected_error(alphas, tau)
      if (err < xi) break
      if (!is.null(max_sim) && S >= as.integer(max_sim)) { unresolved <- unresolved + 1L; break }
      S <- S + as.integer(delta_s)
    }
    if (.ghc_unif(e, 1L) <= tau) { theta <- prop; n_accept <- n_accept + 1L }
    chain[[length(chain) + 1L]] <- theta
  }
  list(chain = chain, acceptance_rate = n_accept / n_iter,
       n_simulations = sims_used, unresolved_steps = unresolved)
}

#' ABC with a Gaussian-process surrogate
#'
#' Four published routes share this name. \code{method="wilkinson"} (the
#' default) emulates the log GABC likelihood of Wilkinson (2014) eq. (1)
#' on a Sobol design pushed through the prior, runs a wave of
#' history-matching to rule out implausible parameter values, and
#' returns the emulator's posterior on a grid. \code{method="gps"} and
#' \code{method="adaptive"} are Meeds & Welling (2014) Algorithms 2 and
#' 3: a Metropolis-Hastings sampler in which the accept probability is
#' itself a random variable, drawn from the posterior on the synthetic
#' likelihood's mean (their eq. (11)), with tau = median(alpha) and the
#' step repeated until E(alpha) < xi (their eq. (16)).
#' \code{method="synthetic"} is their fixed-S Algorithm 1.
#'
#' @param sim Simulator \code{function(theta, e)} where \code{e} is the
#'   shared generator environment.
#' @param obs Observed summary.
#' @param X_grid Grid of theta to evaluate the emulator on. If NULL the
#'   design is reused.
#' @param kernel GP kernel ("sqexp"/"matern32"/"matern52").
#' @param method One of "wilkinson", "gps", "adaptive", "synthetic".
#' @param prior_ppf For wilkinson: (lo, hi) or list of quantile fns.
#' @param log_prior,theta0,n_sim,epsilon,summary For the MH routes.
#' @param n_waves,n_design,threshold,n_sd,accept_kernel Wilkinson knobs.
#' @param n_iter,proposal_sd,xi,delta_s,n_alpha Meeds & Welling knobs.
#' @param seed Seed.
#' @return List mirroring the Python RichResult payload.
#' @references Wilkinson (2014); Meeds & Welling (2014); Wood (2010);
#'   Craig et al. (1997); Sobol (1967); Bratley & Fox (1988);
#'   Rasmussen & Williams (2006).
#' @export
morie_abcgp <- function(sim, obs, X_grid = NULL, kernel = "sqexp",
                        method = "wilkinson", prior_ppf = NULL,
                        log_prior = NULL, theta0 = NULL, n_sim = 50,
                        epsilon = 1, summary = NULL, n_waves = 3,
                        n_design = 32, threshold = 10, n_sd = 3,
                        accept_kernel = "gaussian", n_iter = 200,
                        proposal_sd = 0.5, xi = 0.05, delta_s = 10,
                        n_alpha = 64, seed = 0) {
  if (!(method %in% c("wilkinson", "gps", "adaptive", "synthetic")))
    stop("method must be wilkinson/gps/adaptive/synthetic")
  if (!is.function(sim)) stop("sim must be a callable simulator")
  if (method == "wilkinson") {
    if (is.null(prior_ppf))
      stop("method='wilkinson' needs prior_ppf")
    hm <- .abcgp.history_match(sim, obs, prior_ppf, n_waves, n_design,
                               n_sim, epsilon, summary, threshold, n_sd,
                               kernel, accept_kernel, seed)
    fit <- hm$fit
    if (is.null(X_grid)) grid <- fit$design else {
      grid <- as.matrix(X_grid); storage.mode(grid) <- "double"
    }
    means <- sapply(seq_len(nrow(grid)),
                    function(i) .abcgp.gp_predict(fit, grid[i, ])[1])
    sds   <- sapply(seq_len(nrow(grid)),
                    function(i) .abcgp.gp_predict(fit, grid[i, ])[2])
    top <- which.max(means)
    lse <- .abcgp.lse(means)
    post <- exp(means - lse)
    list(estimate = grid[top, ], grid = grid,
         log_likelihood = means, log_likelihood_sd = sds,
         posterior = post, waves = hm$waves, ensemble_size = fit$n,
         lengthscale = fit$lengthscale, tau2 = fit$tau2,
         beta = fit$beta,
         n_simulations = sum(sapply(hm$waves, function(w) w$n_ensemble)) *
           as.integer(n_sim),
         method = "ABC GP emulator, Wilkinson (2014) with sequential history matching")
  } else {
    if (is.null(log_prior) || is.null(theta0))
      stop("method MH routes need log_prior and theta0")
    if (method == "synthetic") {
      out <- .abcgp.synthetic_abc(sim, obs, log_prior, theta0, n_iter,
                                  n_sim, epsilon, proposal_sd, summary, seed)
      label <- "synthetic-likelihood ABC-MH, Meeds & Welling Algorithm 1"
    } else {
      out <- .abcgp.gps_abc(sim, obs, log_prior, theta0, n_iter, n_sim,
                            epsilon, proposal_sd, summary, seed, xi,
                            delta_s, n_alpha)
      label <- paste0("GPS-ABC adaptive MH, Meeds & Welling (2014) ",
                      "Algorithm 2, eqs. 11-16")
    }
    chain <- do.call(rbind, out$chain)
    p <- ncol(chain); burn <- nrow(chain) %/% 2L
    kept <- chain[(burn + 1L):nrow(chain), , drop = FALSE]
    est <- colMeans(kept)
    c(out, list(estimate = est, posterior_mean = est, burn_in = burn,
                method = label))
  }
}
