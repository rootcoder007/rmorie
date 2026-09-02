# SPDX-License-Identifier: AGPL-3.0-or-later
#' Anomaly detection by VAE reconstruction probability
#'
#' SOURCE. An, J. and Cho, S. (2015), "Variational Autoencoder based
#' Anomaly Detection using Reconstruction Probability", Special Lecture
#' on IE 2:1-18, SNU Data Mining Center.
#'
#' Algorithm 4 is the whole method. For each point x_i: encode to
#' (mu_z, sigma_z); draw z^(1)...z^(L) from N(mu_z, diag(sigma_z^2));
#' decode each to (mu_x, sigma_x); the reconstruction probability is
#' (1/L) sum_l p(x_i | mu_x^(l), sigma_x^(l)); flag x_i when it falls
#' below a threshold. Their Section 4 point is that this is a
#' probability, not a reconstruction error: the decoder variance makes
#' the score comparable across dimensions of different scale.
#'
#' MODEL. A reference implementation cannot ship trained weights and an
#' untrained autoencoder detects nothing, so the default encoder/decoder
#' are the LINEAR-GAUSSIAN instance, whose optimum is closed form: the
#' ML decoder subspace of a linear-Gaussian latent model is the principal
#' subspace and the residual variance is the mean discarded eigenvalue --
#' Tipping, M.E. and Bishop, C.M. (1999), "Probabilistic Principal
#' Component Analysis", JRSS-B 61(3):611-622,
#' doi:10.1111/1467-9868.00196, Section 3.2. So W = top-k eigenvectors of
#' the sample covariance, mu_z(x) = W'(x - xbar), mu_x(z) = W z + xbar.
#' Using the closed-form optimum in place of stochastic training is this
#' implementation's choice, stated rather than attributed.
#'
#' z^(l) comes from the shared deterministic normal stream, so both arms
#' hold the same draws.
#'
#' THRESHOLD. \code{alpha} is a tail fraction: the cut is the type-7
#' alpha-quantile of the log reconstruction probabilities unless
#' \code{threshold} is given.
#'
#' ANCHOR. With \code{latent_dim} = d and \code{encoder_sd} = 0 the map
#' W W' is the identity, reconstruction is exact, and the log
#' reconstruction probability is exactly -d/2 log(2 pi s^2) everywhere.
#'
#' @param X n-by-d data matrix.
#' @param vae list with \code{W} and optionally \code{center}, or NULL.
#' @param latent_dim k in 1..d, used only when \code{vae} is NULL.
#' @param n_samples L, latent draws per point.
#' @param alpha Tail fraction in \[0, 1\] for the threshold quantile.
#' @param encoder_sd sigma_z, non-negative.
#' @param decoder_scale sigma_x; NULL uses the Tipping-Bishop residual.
#' @param threshold Explicit cut on the log reconstruction probability.
#' @param skip Offset into the shared deterministic stream.
#' @return List with \code{reconstruction_probability}, \code{log_rp},
#'   \code{anomaly}, \code{threshold}, \code{n_anomalies}, \code{W},
#'   \code{center}, \code{decoder_scale}, \code{eigenvalues}, \code{n},
#'   \code{d}, \code{latent_dim}, \code{n_samples}.
#' @references An, J. and Cho, S. (2015). Special Lecture on IE 2:1-18.
#'   Tipping, M.E. and Bishop, C.M. (1999). JRSS-B 61(3):611-622.
#'   doi:10.1111/1467-9868.00196.
#' @examples
#' Vaean(cbind(c(1, 2, 3, 9), c(1, 2, 3, 1)), latent_dim = 1)$n_anomalies
#' @export
Vaean <- function(X, vae = NULL, latent_dim = 1, n_samples = 32, alpha = 0.1,
                   encoder_sd = 0, decoder_scale = NULL, threshold = NULL,
                   skip = 0) {
  A <- .s03mat(X)
  n <- nrow(A)
  if (n == 0L) stop("vae_anomaly: X is empty")
  d <- ncol(A)
  L <- as.integer(n_samples)
  if (is.na(L) || L < 1L) stop("vae_anomaly: n_samples must be positive")
  alpha <- as.numeric(alpha)
  if (!(alpha >= 0 && alpha <= 1)) stop("vae_anomaly: alpha must lie in [0, 1]")
  esd <- as.numeric(encoder_sd)
  if (esd < 0) stop("vae_anomaly: encoder_sd must be non-negative")
  skip <- as.integer(skip)
  if (is.na(skip) || skip < 0L) stop("vae_anomaly: skip must be non-negative")
  cen <- numeric(d)
  for (j in seq_len(d)) {
    s <- 0
    for (i in seq_len(n)) s <- s + A[i, j]
    cen[j] <- s / n
  }
  C <- matrix(0, d, d)
  for (a in seq_len(d)) for (b in seq_len(d)) {
    s <- 0
    for (i in seq_len(n)) s <- s + (A[i, a] - cen[a]) * (A[i, b] - cen[b])
    C[a, b] <- s / n
  }
  je <- .s03jacobi(C)
  ev <- je$values
  evec <- je$vectors
  if (is.null(vae)) {
    k <- as.integer(latent_dim)
    if (is.na(k) || k < 1L || k > d) stop("vae_anomaly: latent_dim must lie in 1 .. d")
    W <- matrix(0, d, k)
    for (i in seq_len(d)) for (t in seq_len(k)) W[i, t] <- evec[i, d - t + 1L]
  } else {
    W <- .s03mat(vae$W)
    if (nrow(W) != d) stop("vae_anomaly: vae W must have d rows")
    k <- ncol(W)
    if (!is.null(vae$center)) {
      cen <- .s03vec(vae$center)
      if (length(cen) != d) stop("vae_anomaly: vae center must have length d")
    }
  }
  if (is.null(decoder_scale)) {
    rest <- 0
    cnt <- 0L
    if (d > k) for (t in seq_len(d - k)) { rest <- rest + ev[t]; cnt <- cnt + 1L }
    s <- if (cnt > 0L && rest > 0) sqrt(rest / cnt) else 1
  } else {
    s <- as.numeric(decoder_scale)
  }
  if (!(s > 0)) stop("vae_anomaly: decoder_scale must be positive")
  eps <- .vitdraw(L, k, skip, 1)
  cc <- log(2 * pi * s * s)
  lrp <- numeric(n)
  rp <- numeric(n)
  for (i in seq_len(n)) {
    mz <- numeric(k)
    for (t in seq_len(k)) {
      v <- 0
      for (j in seq_len(d)) v <- v + W[j, t] * (A[i, j] - cen[j])
      mz[t] <- v
    }
    ll <- numeric(L)
    best <- NA_real_
    for (l in seq_len(L)) {
      z <- numeric(k)
      for (t in seq_len(k)) z[t] <- mz[t] + esd * eps[l, t]
      q <- 0
      for (j in seq_len(d)) {
        r <- cen[j]
        for (t in seq_len(k)) r <- r + W[j, t] * z[t]
        q <- q + cc + (A[i, j] - r) * (A[i, j] - r) / (s * s)
      }
      ll[l] <- -0.5 * q
      if (is.na(best) || ll[l] > best) best <- ll[l]
    }
    acc <- 0
    for (l in seq_len(L)) acc <- acc + exp(ll[l] - best)
    lrp[i] <- best + log(acc / L)
    rp[i] <- exp(lrp[i])
  }
  cut <- if (is.null(threshold)) .s03quantile7(lrp, alpha) else as.numeric(threshold)
  flag <- as.numeric(lrp < cut)
  .t1_result(estimate = cut, reconstruction_probability = rp, log_rp = lrp,
             anomaly = flag, threshold = cut, n_anomalies = sum(flag),
             W = W, center = cen, decoder_scale = s, eigenvalues = ev,
             n = n, d = d, latent_dim = k, n_samples = L,
             method = paste("Reconstruction probability, An and Cho (2015)",
                            "Algorithm 4, on the closed-form linear-Gaussian",
                            "optimum (Tipping and Bishop 1999 Sec. 3.2)"))
}
