# SPDX-License-Identifier: AGPL-3.0-or-later
#' Multinomial variational autoencoder for collaborative filtering
#'
#' SOURCE. Liang, D., Krishnan, R.G., Hoffman, M.D. and Jebara, T.
#' (2018), "Variational Autoencoders for Collaborative Filtering",
#' Proceedings of the 2018 World Wide Web Conference (WWW '18),
#' pp. 689-698, doi:10.1145/3178876.3186150.
#'
#' The contribution is the MULTINOMIAL likelihood in place of the
#' Gaussian or logistic one (Section 2.2): pi(z_u) = softmax(f(z_u)) and
#' log p(x_u|z_u) = sum_i x_ui log pi_i(z_u), which spends a fixed
#' probability budget on the items the model believes will be clicked.
#' The objective is the beta-annealed bound of Eq. (5),
#' L_beta = E_q[log p(x_u|z)] - beta KL(q(z|x_u) || p(z)), with beta < 1
#' the partial regularisation of Section 2.2.2.
#'
#' The encoder input is the L2-normalised log(1 + x_u) transform of
#' Section 2.2, not the raw counts.
#'
#' RANKING. Recall@K and truncated NDCG@K are the Section 4.1
#' definitions: Recall@K = sum_{r<=K} I[w(r) in I_u] / min(K, |I_u|) and
#' DCG@K = sum_{r<=K} (2^{I[w(r) in I_u]} - 1)/log2(r+1). Ties are broken
#' by ascending item index in both language arms -- R scans a matrix
#' column-major and Python row-major, so an unpinned tie rule is a real
#' parity hazard.
#'
#' DETERMINISM. Untrained weights and the reparameterisation noise come
#' from the shared deterministic normal stream. Relevance defaults to the
#' input clicks, making the metrics in-sample; pass \code{relevance} for
#' a held-out set.
#'
#' @param R n_users-by-n_items non-negative click matrix.
#' @param K Ranking cut-off in 1..n_items.
#' @param latent_dim Latent width.
#' @param beta KL annealing weight.
#' @param n_samples Reparameterised draws per user.
#' @param relevance Binary held-out relevance, or NULL for R > 0.
#' @param w_scale Scales the decoder weights; 0 makes pi uniform.
#' @param skip Offset into the shared deterministic stream.
#' @return List with \code{elbo}, \code{loglik}, \code{kl},
#'   \code{elbo_per_user}, \code{loglik_per_user}, \code{kl_per_user},
#'   \code{recall}, \code{ndcg}, \code{recall_per_user},
#'   \code{ndcg_per_user}, \code{ranking}, \code{mu}, \code{logvar},
#'   \code{n_users}, \code{n_items}, \code{k}.
#' @references Liang, D., Krishnan, R.G., Hoffman, M.D. and Jebara, T.
#'   (2018). WWW '18, pp. 689-698. doi:10.1145/3178876.3186150.
#' @examples
#' Vaecf(matrix(c(1, 0, 0, 1, 1, 0), 2, 3), K = 2)$ndcg
#' @export
Vaecf <- function(R, K = 5, latent_dim = 2, beta = 0.2, n_samples = 16,
                  relevance = NULL, w_scale = 1, skip = 0) {
  X <- .s03mat(R)
  nu <- nrow(X)
  if (nu == 0L) stop("vae_cf: R is empty")
  ni <- ncol(X)
  if (any(X < 0)) stop("vae_cf: R must be non-negative")
  K <- as.integer(K)
  if (is.na(K) || K < 1L || K > ni) stop("vae_cf: K must lie in 1 .. n_items")
  m <- as.integer(latent_dim)
  if (is.na(m) || m < 1L) stop("vae_cf: latent_dim must be positive")
  L <- as.integer(n_samples)
  if (is.na(L) || L < 1L) stop("vae_cf: n_samples must be positive")
  beta <- as.numeric(beta)
  if (beta < 0) stop("vae_cf: beta must be non-negative")
  skip <- as.integer(skip)
  if (is.na(skip) || skip < 0L) stop("vae_cf: skip must be non-negative")
  if (is.null(relevance)) {
    R0 <- matrix(as.numeric(X > 0), nu, ni)
  } else {
    R0 <- .s03mat(relevance)
    if (nrow(R0) != nu || ncol(R0) != ni) {
      stop("vae_cf: relevance must have the same shape as R")
    }
    R0 <- matrix(as.numeric(R0 > 0), nu, ni)
  }
  Xn <- matrix(0, nu, ni)
  for (u in seq_len(nu)) {
    row <- log1p(X[u, ])
    nrm <- sqrt(sum(row * row))
    Xn[u, ] <- if (nrm > 0) row / nrm else numeric(ni)
  }
  Wm <- .vitdraw(ni, m, skip, 1 / sqrt(ni))
  Wl <- .vitdraw(ni, m, skip + ni * m, 0.1 / sqrt(ni))
  Wd <- .vitdraw(m, ni, skip + 2L * ni * m, as.numeric(w_scale) / sqrt(m))
  eps <- .vitdraw(L, m, skip + 2L * ni * m + m * ni, 1)
  mu <- .s03matmul(Xn, Wm)
  lv <- .s03matmul(Xn, Wl)
  llu <- numeric(nu); klu <- numeric(nu)
  rec <- numeric(nu); ndc <- numeric(nu)
  rank <- matrix(0, nu, ni)
  for (u in seq_len(nu)) {
    sig <- exp(0.5 * lv[u, ])
    t <- 0
    for (j in seq_len(m)) t <- t + mu[u, j] * mu[u, j] + sig[j] * sig[j] - 1 - lv[u, j]
    klu[u] <- 0.5 * t
    acc <- 0
    for (l in seq_len(L)) {
      z <- numeric(m)
      for (j in seq_len(m)) z[j] <- mu[u, j] + sig[j] * eps[l, j]
      lg <- numeric(ni)
      for (i in seq_len(ni)) {
        s <- 0
        for (j in seq_len(m)) s <- s + z[j] * Wd[j, i]
        lg[i] <- s
      }
      mx <- lg[1]
      for (v in lg) if (v > mx) mx <- v
      se <- 0
      for (v in lg) se <- se + exp(v - mx)
      lse <- mx + log(se)
      s <- 0
      for (i in seq_len(ni)) if (X[u, i] > 0) s <- s + X[u, i] * (lg[i] - lse)
      acc <- acc + s
    }
    llu[u] <- acc / L
    lgm <- numeric(ni)
    for (i in seq_len(ni)) {
      s <- 0
      for (j in seq_len(m)) s <- s + mu[u, j] * Wd[j, i]
      lgm[i] <- s
    }
    idx <- order(-lgm, seq_len(ni))
    rank[u, ] <- idx
    nrel <- sum(R0[u, ] > 0)
    hit <- 0; dcg <- 0
    for (r in seq_len(K)) {
      h <- if (R0[u, idx[r]] > 0) 1 else 0
      hit <- hit + h
      dcg <- dcg + (2^h - 1) / log(r + 1, 2)
    }
    den <- min(K, nrel)
    rec[u] <- if (den > 0) hit / den else 0
    ide <- 0
    if (den > 0) for (r in seq_len(den)) ide <- ide + 1 / log(r + 1, 2)
    ndc[u] <- if (ide > 0) dcg / ide else 0
  }
  ll <- sum(llu) / nu
  kl <- sum(klu) / nu
  .t1_result(estimate = ll - beta * kl, elbo = ll - beta * kl, loglik = ll,
             kl = kl, elbo_per_user = llu - beta * klu, loglik_per_user = llu,
             kl_per_user = klu, recall = sum(rec) / nu, ndcg = sum(ndc) / nu,
             recall_per_user = rec, ndcg_per_user = ndc, ranking = rank,
             mu = mu, logvar = lv, n_users = nu, n_items = ni, k = K,
             method = paste("Mult-VAE: multinomial likelihood with",
                            "beta-annealed KL (Liang et al. 2018",
                            "Secs. 2.2, 2.2.2, 4.1)"))
}
