# SPDX-License-Identifier: AGPL-3.0-or-later
#' HDP topic model, the nonparametric LDA
#'
#' Teh, Jordan, Beal and Blei (2006), JASA 101(476), 1566-1581 (FETCHED),
#' section 6.1: each document is a group, the topics are the shared atoms,
#' and the number of topics is NOT fixed in advance -- the whole
#' difference from latent Dirichlet allocation (Blei, Ng and Jordan 2003),
#' where K is a hyperparameter.  The model is eq. (19) with a multinomial
#' likelihood over the vocabulary.
#'
#' Determinism: topic-word distributions fitted by EM from a symmetric
#' Dirichlet smoothing prior, with the HDP weights as document-side
#' pseudo-counts.  No Gibbs sampling, so no generator.
#'
#' @param docs list of documents, each a vector of zero-based word ids.
#' @param gamma,alpha concentrations.
#' @param truncation number of topics.
#' @param V vocabulary size.
#' @param eta symmetric Dirichlet smoothing.
#' @param max_iter,tol EM controls.
#' @return list: estimate, loglik, phi, theta, beta, n_vocab, method.
#' @keywords internal
#' @examples
#' Hdplda(list(c(0, 1, 0), c(2, 2, 1)), 1, 1, 2, 3)$loglik
#' @export
Hdplda <- function(docs, gamma = 1, alpha = 1, truncation = 3, V = NULL,
                   eta = 0.1, max_iter = 200, tol = 1e-13) {
  D <- lapply(docs, function(d) as.integer(.s03vec(d)))
  Vn <- if (!is.null(V)) as.integer(V) else {
    mx <- -1L
    for (d in D) if (length(d)) mx <- max(mx, max(d))
    mx + 1L
  }
  K <- as.integer(truncation)
  beta <- Stickw(gamma, K)$pi
  tot <- 0
  for (x in beta) tot <- tot + x
  beta <- if (tot > 0) beta / tot else rep(1 / K, K)
  phi <- matrix(0, K, Vn)
  for (t in seq_len(K)) for (w in seq_len(Vn)) {
    phi[t, w] <- 1 + (((t - 1L) * 7L + (w - 1L) * 3L) %% 5L)
  }
  for (t in seq_len(K)) {
    s <- 0
    for (w in seq_len(Vn)) s <- s + phi[t, w]
    phi[t, ] <- phi[t, ] / s
  }
  theta <- matrix(rep(beta, each = length(D)), length(D), K)
  ll <- -Inf
  for (it in seq_len(as.integer(max_iter))) {
    cnt <- matrix(0, K, Vn)
    newll <- 0
    post <- matrix(0, length(D), K)
    for (j in seq_along(D)) {
      for (w in D[[j]]) {
        lp <- numeric(K)
        for (t in seq_len(K)) {
          lp[t] <- log(if (theta[j, t] > 1e-300) theta[j, t] else 1e-300) +
            log(if (phi[t, w + 1L] > 1e-300) phi[t, w + 1L] else 1e-300)
        }
        m <- .s03logsumexp(lp)
        newll <- newll + m
        for (t in seq_len(K)) {
          r <- exp(lp[t] - m)
          post[j, t] <- post[j, t] + r
          cnt[t, w + 1L] <- cnt[t, w + 1L] + r
        }
      }
    }
    for (j in seq_along(D)) {
      nj <- length(D[[j]])
      theta[j, ] <- (as.numeric(alpha) * beta + post[j, ]) / (as.numeric(alpha) + nj)
    }
    for (t in seq_len(K)) {
      s <- 0
      for (w in seq_len(Vn)) s <- s + cnt[t, w] + as.numeric(eta)
      phi[t, ] <- (cnt[t, ] + as.numeric(eta)) / s
    }
    if (abs(newll - ll) < tol) { ll <- newll
    break }
    ll <- newll
  }
  list(estimate = ll, loglik = ll, phi = phi, theta = theta, beta = beta,
       n_vocab = Vn,
       method = "HDP topic model, the nonparametric LDA (Teh et al. 2006, sec. 6.1)")
}
