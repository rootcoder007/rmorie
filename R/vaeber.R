# SPDX-License-Identifier: AGPL-3.0-or-later
#' Variational autoencoder evidence lower bound (SGVB estimator)
#'
#' SOURCE. Kingma, D.P. and Welling, M. (2014), "Auto-Encoding
#' Variational Bayes", ICLR 2014; arXiv:1312.6114.
#'
#' The bound is Eq. (3), L = -KL(q_phi(z|x) || p_theta(z)) +
#' E_{q_phi}[log p_theta(x|z)], estimated by the SGVB estimator of
#' Eq. (7) with the reparameterisation z = mu + sigma * eps,
#' eps ~ N(0, I). With a Gaussian encoder and a standard normal prior the
#' KL is closed form (Appendix B / Eq. 10):
#' -KL = (1/2) sum_j (1 + log sigma_j^2 - mu_j^2 - sigma_j^2).
#'
#' DETERMINISM. eps comes from the shared deterministic normal stream
#' (base-2 van der Corput through AS 241), not from a pseudo-random
#' generator, so both language arms hold the SAME draws. That is this
#' implementation's choice; the paper draws eps at random.
#'
#' DECODER. p(x|z) = N(x; W z + b, s^2 I), the Gaussian decoder of
#' Appendix C.2, for which the reconstruction term is also closed form:
#' E_q[log p(x|z)] = -(1/2) sum_k [log(2 pi s^2) + ((x_k - (W mu + b)_k)^2
#' + sum_j sigma_j^2 W[j,k]^2)/s^2], returned as \code{recon_analytic}.
#' The Monte Carlo estimate must approach it as \code{n_samples} grows --
#' an anchor that does not run through the other language arm.
#'
#' @param x n-by-d data matrix.
#' @param encoder list with \code{mu} and \code{logvar}, or NULL.
#' @param decoder list with \code{W} and \code{b}, or NULL.
#' @param latent_dim m, used only when \code{encoder} is NULL.
#' @param n_samples Reparameterised draws per data point.
#' @param decoder_scale s, the decoder standard deviation.
#' @param skip Offset into the shared deterministic stream.
#' @return List with \code{elbo}, \code{kl}, \code{recon},
#'   \code{recon_analytic}, \code{elbo_analytic}, \code{mc_error},
#'   \code{mu}, \code{logvar}, \code{elbo_per_point}, \code{kl_per_point},
#'   \code{recon_per_point}, \code{n}, \code{d}, \code{latent_dim},
#'   \code{n_samples}.
#' @references Kingma, D.P. and Welling, M. (2014). Auto-Encoding
#'   Variational Bayes. ICLR 2014; arXiv:1312.6114.
#' @examples
#' Vaeber(matrix(c(0.5, -0.2, 0.3, 1.1), 2, 2), latent_dim = 1)$kl
#' @export
Vaeber <- function(x, encoder = NULL, decoder = NULL, latent_dim = 2,
                   n_samples = 64, decoder_scale = 1, skip = 0) {
  X <- .s03mat(x)
  n <- nrow(X)
  if (n == 0L) stop("vae_elbo: x is empty")
  d <- ncol(X)
  m <- as.integer(latent_dim)
  L <- as.integer(n_samples)
  s <- as.numeric(decoder_scale)
  if (is.na(m) || m < 1L) stop("vae_elbo: latent_dim must be positive")
  if (is.na(L) || L < 1L) stop("vae_elbo: n_samples must be positive")
  if (!(s > 0)) stop("vae_elbo: decoder_scale must be positive")
  skip <- as.integer(skip)
  if (is.na(skip) || skip < 0L) stop("vae_elbo: skip must be non-negative")
  if (is.null(encoder)) {
    Wm <- .vitdraw(d, m, skip, 1 / sqrt(d))
    Wl <- .vitdraw(d, m, skip + d * m, 0.1 / sqrt(d))
    mu <- .s03matmul(X, Wm)
    lv <- .s03matmul(X, Wl)
  } else {
    mu <- .s03mat(encoder$mu)
    lv <- .s03mat(encoder$logvar)
    if (nrow(mu) != n || nrow(lv) != n) {
      stop("vae_elbo: encoder mu/logvar must have one row per observation")
    }
    m <- ncol(mu)
    if (ncol(lv) != m) stop("vae_elbo: encoder mu/logvar must be n-by-m")
  }
  if (is.null(decoder)) {
    Wd <- .vitdraw(m, d, skip + 2L * d * m, 1 / sqrt(m))
    bd <- numeric(d)
  } else {
    Wd <- .s03mat(decoder$W)
    bd <- .s03vec(decoder$b)
    if (nrow(Wd) != m || ncol(Wd) != d) stop("vae_elbo: decoder W must be m-by-d")
    if (length(bd) != d) stop("vae_elbo: decoder b must have length d")
  }
  eps <- .vitdraw(L, m, skip + 2L * d * m + m * d, 1)
  sig <- matrix(0, n, m)
  for (i in seq_len(n)) for (j in seq_len(m)) sig[i, j] <- exp(0.5 * lv[i, j])
  klp <- numeric(n)
  for (i in seq_len(n)) {
    t <- 0
    for (j in seq_len(m)) {
      v <- sig[i, j] * sig[i, j]
      t <- t + mu[i, j] * mu[i, j] + v - 1 - lv[i, j]
    }
    klp[i] <- 0.5 * t
  }
  cc <- log(2 * pi * s * s)
  recp <- numeric(n)
  anap <- numeric(n)
  for (i in seq_len(n)) {
    acc <- 0
    for (l in seq_len(L)) {
      z <- numeric(m)
      for (j in seq_len(m)) z[j] <- mu[i, j] + sig[i, j] * eps[l, j]
      t <- 0
      for (k in seq_len(d)) {
        r <- bd[k]
        for (j in seq_len(m)) r <- r + z[j] * Wd[j, k]
        t <- t + cc + (X[i, k] - r) * (X[i, k] - r) / (s * s)
      }
      acc <- acc + -0.5 * t
    }
    recp[i] <- acc / L
    t <- 0
    for (k in seq_len(d)) {
      r <- bd[k]
      for (j in seq_len(m)) r <- r + mu[i, j] * Wd[j, k]
      q <- 0
      for (j in seq_len(m)) q <- q + sig[i, j] * sig[i, j] * Wd[j, k] * Wd[j, k]
      t <- t + cc + ((X[i, k] - r) * (X[i, k] - r) + q) / (s * s)
    }
    anap[i] <- -0.5 * t
  }
  kl <- sum(klp) / n
  rec <- sum(recp) / n
  ana <- sum(anap) / n
  .t1_result(estimate = rec - kl, elbo = rec - kl, kl = kl, recon = rec,
             recon_analytic = ana, elbo_analytic = ana - kl,
             mc_error = abs(rec - ana), mu = mu, logvar = lv,
             elbo_per_point = recp - klp, kl_per_point = klp,
             recon_per_point = recp, n = n, d = d, latent_dim = m,
             n_samples = L,
             method = paste("SGVB ELBO with the closed-form Gaussian KL",
                            "(Kingma and Welling 2014 Eqs. 3, 7, 10)"))
}
