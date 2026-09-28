.stm_logdet <- function(A) as.numeric(determinant(A, logarithm = TRUE)$modulus)

#' Spatio-temporal model building blocks
#'
#' \code{WishartMoments}: means, inverse means and expected log-determinant
#' of the Wishart or inverse-Wishart distribution. \code{PartitionedCovariance}:
#' Bartlett decomposition of a partitioned covariance and Gaussian
#' conditioning. \code{SeparablePrewhiten}: temporal prewhitening of a
#' separable matrix-normal field. \code{NormalGammaDlm}: West-Harrison
#' normal-gamma dynamic linear model filter with unknown observation variance.
#' \code{HarmonicOzoneDlm}: Huerta et al. multi-site harmonic DLM (Kalman
#' filter). \code{CarrollStCorrelation}: Carroll et al. space-time correlation
#' with its smallest eigenvalue. \code{ProcessConvolutionCovariance}: Higdon
#' process-convolution covariance with location-specific Gaussian kernels.
#' \code{ArmaAcf}: theoretical ARMA autocorrelation function. Identical to the
#' Python arm \code{morie.fn.stmodels}.
#'
#' @param scale Scale matrix.
#' @param df Degrees of freedom.
#' @param inverse_wishart Moments of the inverse Wishart instead.
#' @param sigma Covariance matrix.
#' @param k Size of the first block.
#' @param x2 Optional values of the second block.
#' @param mean Optional mean vector.
#' @param z Sites by times data matrix.
#' @param rho_t Temporal correlation matrix.
#' @param y Observations (vector, or times by sites matrix).
#' @param F Observation vector.
#' @param G Evolution matrix.
#' @param W Scale-free evolution covariance.
#' @param m0,C0,n0,S0 Prior mean, scale-free covariance, degrees of freedom and
#'   variance estimate.
#' @param times Times (hours for the ozone model).
#' @param coords Site coordinates, one row per site or point.
#' @param a Harmonic coefficients (ozone model) or the three log-psi
#'   coefficients (Carroll).
#' @param lam Range of the exponential measurement-error correlation.
#' @param sigma2 Variance.
#' @param w Evolution variances of the level and the two amplitude sets.
#' @param c0 Prior variance of the state.
#' @param b The three log-phi coefficients.
#' @param kernels One kernel covariance matrix per location (array) or one
#'   matrix.
#' @param normalize Return correlations.
#' @param ar,ma AR and MA coefficients.
#' @param lag_max Largest lag.
#' @return A list, or a numeric vector for \code{ArmaAcf}.
#' @references Shaddick, G., Zidek, J. V. and Schmidt, A. M. (2023).
#'   Spatio-Temporal Methods in Environmental Epidemiology with R, 2nd edn.
#'   CRC Press.
#'
#'   West, M. and Harrison, J. (1997). Bayesian Forecasting and Dynamic
#'   Models, 2nd edn. Springer.
#'
#'   Huerta, G., Sanso, B. and Stroud, J. R. (2004). A spatiotemporal model
#'   for Mexico City ozone levels. Applied Statistics 53, 231-248.
#'
#'   Higdon, D., Swall, J. and Kern, J. (1999). Non-stationary spatial
#'   modeling. Bayesian Statistics 6, 761-768.
#' @examples
#' WishartMoments(matrix(c(2, 0.5, 0.5, 1), 2), 6)$mean_logdet
#' ArmaAcf(ar = c(0.5, 0.2), lag_max = 3)
#' @export
WishartMoments <- function(scale, df, inverse_wishart = FALSE) {
  S <- as.matrix(scale)
  p <- nrow(S)
  if (df <= p - 1) stop("df must exceed p - 1")
  Si <- solve(S)
  ld <- .stm_logdet(S)
  dig <- sum(digamma((df - seq_len(p) + 1) / 2))
  ok <- df - p - 1 > 0
  if (!inverse_wishart) {
    list(mean = df * S, mean_inverse = if (ok) Si / (df - p - 1) else NULL,
         mean_logdet = p * log(2) + dig + ld)
  } else {
    list(mean = if (ok) S / (df - p - 1) else NULL, mean_inverse = df * Si,
         mean_logdet = -p * log(2) - dig + ld)
  }
}

#' @rdname WishartMoments
#' @export
PartitionedCovariance <- function(sigma, k, x2 = NULL, mean = NULL) {
  S <- as.matrix(sigma)
  n <- nrow(S)
  if (k <= 0 || k >= n) stop("k must split the matrix into two non-empty blocks")
  i1 <- seq_len(k)
  i2 <- (k + 1):n
  tau <- S[i1, i2, drop = FALSE] %*% solve(S[i2, i2, drop = FALSE])
  cond <- S[i1, i1, drop = FALSE] - tau %*% t(S[i1, i2, drop = FALSE])
  Tm <- diag(n)
  Tm[i1, i2] <- tau
  D <- matrix(0, n, n)
  D[i1, i1] <- cond
  D[i2, i2] <- S[i2, i2]
  out <- list(sigma_cond = cond, tau = tau, T = Tm, Delta = D, reconstructed = Tm %*% D %*% t(Tm))
  if (!is.null(x2)) {
    mu <- if (is.null(mean)) numeric(n) else mean
    out$cond_mean <- as.vector(mu[i1] + tau %*% (x2 - mu[i2]))
  }
  out
}

#' @rdname WishartMoments
#' @export
SeparablePrewhiten <- function(z, rho_t) {
  e <- eigen(as.matrix(rho_t), symmetric = TRUE)
  if (min(e$values) <= 0) stop("rho_t must be positive definite")
  rm <- e$vectors %*% diag(1 / sqrt(e$values), length(e$values)) %*% t(e$vectors)
  list(z_star = as.matrix(z) %*% rm, rho_t_inv_sqrt = rm)
}

#' @rdname WishartMoments
#' @export
NormalGammaDlm <- function(y, F, G, W, m0, C0, n0, S0) {
  G <- as.matrix(G)
  W <- as.matrix(W)
  m <- m0
  C <- as.matrix(C0)
  n <- n0
  S <- S0
  tt <- length(y)
  ms <- vector("list", tt)
  Cs <- vector("list", tt)
  fs <- numeric(tt)
  Qs <- numeric(tt)
  Ss <- numeric(tt)
  ns <- numeric(tt)
  ll <- 0
  for (t in seq_len(tt)) {
    a <- as.vector(G %*% m)
    R <- G %*% C %*% t(G) + W
    f <- sum(F * a)
    RF <- as.vector(R %*% F)
    Q <- 1 + sum(F * RF)
    e <- y[t] - f
    sc <- S * Q
    ll <- ll + lgamma((n + 1) / 2) - lgamma(n / 2) - 0.5 * log(n * pi * sc) -
      (n + 1) / 2 * log(1 + e^2 / (n * sc))
    A <- RF / Q
    nS <- n * S + e^2 / Q
    n <- n + 1
    S <- nS / n
    m <- a + A * e
    C <- R - outer(A, A) * Q
    ms[[t]] <- m
    Cs[[t]] <- S * C
    fs[t] <- f
    Qs[t] <- Q
    Ss[t] <- S
    ns[t] <- n
  }
  list(m = ms, C = Cs, f = fs, Q = Qs, S = Ss, n = ns, loglik = ll)
}

#' @rdname WishartMoments
#' @export
HarmonicOzoneDlm <- function(y, times, coords, a, lam, sigma2, w, m0 = NULL, c0 = 1e6) {
  Y <- as.matrix(y)
  P <- as.matrix(coords)
  ns <- nrow(P)
  q <- 1 + 2 * ns
  V <- sigma2 * exp(-as.matrix(stats::dist(P)) / lam)
  Wd <- c(w[1], rep(w[2], ns), rep(w[3], ns))
  m <- if (is.null(m0)) numeric(q) else m0
  C <- diag(c0, q)
  means <- vector("list", nrow(Y))
  ll <- 0
  for (i in seq_len(nrow(Y))) {
    t <- times[i]
    s1 <- cos(pi * t / 12) + a[1] * sin(pi * t / 12)
    s2 <- cos(pi * 2 * t / 12) + a[2] * sin(pi * 2 * t / 12)
    Fm <- cbind(1, diag(s1, ns), diag(s2, ns))
    R <- C + diag(Wd, q)
    e <- Y[i, ] - as.vector(Fm %*% m)
    FR <- Fm %*% R
    Qm <- FR %*% t(Fm) + V
    Qi <- solve(Qm)
    ll <- ll - 0.5 * (ns * log(2 * pi) + .stm_logdet(Qm) + sum(e * (Qi %*% e)))
    K <- t(FR) %*% Qi
    m <- as.vector(m + K %*% e)
    C <- R - K %*% FR
    means[[i]] <- m
  }
  list(means = means, loglik = ll, final_cov = C)
}

#' @rdname WishartMoments
#' @export
CarrollStCorrelation <- function(coords, times, a, b) {
  P <- as.matrix(coords)
  d <- as.matrix(stats::dist(P))
  v <- abs(outer(times, times, "-"))
  psi <- exp(a[1] + a[2] * v + a[3] * v^2)
  phi <- exp(b[1] + b[2] * v + b[3] * v^2)
  R <- phi * psi^d
  R[d == 0 & v == 0] <- 1
  ev <- min(eigen(R, symmetric = TRUE, only.values = TRUE)$values)
  list(correlation = R, min_eigenvalue = ev, positive_definite = ev > 0)
}

#' @rdname WishartMoments
#' @export
ProcessConvolutionCovariance <- function(coords, kernels, sigma2 = 1, normalize = FALSE) {
  P <- as.matrix(coords)
  n <- nrow(P)
  d <- ncol(P)
  ks <- if (length(dim(kernels)) == 3) lapply(seq_len(n), function(i) kernels[i, , ]) else
    rep(list(as.matrix(kernels)), n)
  C <- matrix(0, n, n)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      S <- ks[[i]] + ks[[j]]
      h <- P[i, ] - P[j, ]
      q <- sum(h * solve(S, h))
      C[i, j] <- sigma2 * (2 * pi)^(-d / 2) * exp(-0.5 * .stm_logdet(S) - q / 2)
    }
  }
  if (normalize) C <- C / sqrt(outer(diag(C), diag(C)))
  list(covariance = C)
}

#' @rdname WishartMoments
#' @export
ArmaAcf <- function(ar = numeric(0), ma = numeric(0), lag_max = NULL) {
  p <- length(ar)
  q <- length(ma)
  if (p == 0 && q == 0) stop("empty model")
  r <- max(p, q + 1)
  if (is.null(lag_max)) lag_max <- r
  ph <- c(ar, rep(0, r - p))
  theta <- c(1, ma)
  psi <- 1
  for (j in seq_len(q)) psi <- c(psi, theta[j + 1] + sum(ph[seq_len(min(j, r))] * psi[j - seq_len(min(j, r)) + 1]))
  rhs <- vapply(0:r, function(k) if (k <= q) sum(theta[(k:q) + 1] * psi[(k:q) - k + 1]) else 0, numeric(1))
  A <- diag(r + 1)
  for (k in 0:r) for (j in seq_len(r)) A[k + 1, abs(k - j) + 1] <- A[k + 1, abs(k - j) + 1] - ph[j]
  g <- solve(A, rhs)
  while (length(g) <= lag_max) {
    k <- length(g)
    g <- c(g, sum(ph * g[k - seq_len(r) + 1]))
  }
  g[seq_len(lag_max + 1)] / g[1]
}
