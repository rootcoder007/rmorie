# SPDX-License-Identifier: AGPL-3.0-or-later
#' BayesR: mixture of normals prior with different variance classes
#'
#' NOT IN THE BOOK.  Montesinos Lopez, Montesinos Lopez and Crossa (2022),
#' Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#' Springer, was searched in full -- all seventeen page-range volumes and the
#' index, \[Pages 683-691\].  Chapter 6, volume \[Pages 171-208\], is the Bayesian
#' chapter and carries BRR, BayesA, BayesB, BayesC and the Bayesian LASSO;
#' BayesR is not among them and the string "BayesR" does not occur anywhere in
#' the book.
#'
#' The method is therefore taken from the originating primary source, Erbe, M.,
#' Hayes, B. J., Matukumalli, L. K., Goswami, S., Bowman, P. J., Reich, C. M.,
#' Mason, B. A. and Goddard, M. E. (2012), Improving accuracy of genomic
#' predictions within and between dairy cattle breeds with imputed high-density
#' single nucleotide polymorphism panels, Journal of Dairy Science 95(7),
#' 4114-4129, doi:10.3168/jds.2011-5019, which first states the four-class
#' mixture.  Moser, G., Lee, S. H., Hayes, B. J., Goddard, M. E., Wray, N. R.
#' and Visscher, P. M. (2015), Simultaneous discovery, estimation and
#' prediction analysis of complex traits using a Bayesian mixture model, PLoS
#' Genetics 11(4), e1004969, doi:10.1371/journal.pgen.1004969, restates it and
#' names it BayesR.
#'
#' CITATION LIMIT, stated rather than papered over.  The Erbe et al. paper is
#' paywalled and its own text was not read.  The four-class specification used
#' here -- beta_j ~ pi_1 N(0, 0) + pi_2 N(0, 1e-4 sigma_g^2)
#' + pi_3 N(0, 1e-3 sigma_g^2) + pi_4 N(0, 1e-2 sigma_g^2), with
#' (pi_1, ..., pi_4) ~ Dirichlet(delta), delta = (1, 1, 1, 1), and the first
#' class a point mass at zero -- is taken from Moser et al.'s verbatim
#' restatement of it, not from Erbe et al. directly.  The multipliers are NOT
#' assumed: they are the 0, 0.0001, 0.001 and 0.01 that Moser et al. print.
#'
#' DETERMINISM.  Nothing is sampled.  The Gibbs sampler is replaced by its EM
#' fixed point, which is exact and identical in both arms.  For marker j with
#' current residual r and column x, the class-conditional marginal likelihood
#' is L_k proportional to pi_k sqrt(s_e2/(x^T x s_k2 + s_e2))
#' exp( (x^T r)^2 s_k2 / (2 s_e2 (x^T x s_k2 + s_e2)) ), which at s_1^2 = 0
#' collapses to L_1 proportional to pi_1, the point mass; gamma_jk is L_k
#' normalised; the coefficient is its posterior mean
#' beta_j = sum_k gamma_jk (x^T r) s_k2 / (x^T x s_k2 + s_e2); and the weights
#' are the Dirichlet posterior mean
#' pi_k = (sum_j gamma_jk + delta_k) / (p + sum delta).
#'
#' @param y length-n phenotypes.
#' @param X n-by-p marker matrix.
#' @param pi optional starting mixture weights; equal weights when absent.
#' @param sigma_classes optional class variance multipliers of sigma_g^2;
#'   default (0, 1e-4, 1e-3, 1e-2).  The first entry must be exactly 0.
#' @param max_iter,tol fixed-point controls.
#' @param delta optional Dirichlet parameter; all ones when absent.
#' @return list: estimate, beta_samples, class_probs, pi, sigma_g2, sigma_e2,
#'   sigma_classes, mu, n_nonzero, n, method.
#' @keywords internal
#' @examples
#' X <- cbind(c(-1, 0, 1, 2, -2), c(1, -1, 0, 2, -1))
#' Baysr(c(-2, 0, 2, 4, -4), X)$n_nonzero
#' @export
Baysr <- function(y, X, pi = NULL, sigma_classes = NULL, max_iter = 500L,
                  tol = 1e-13, delta = NULL) {
  yy <- .s03vec(y)
  n <- length(yy)
  if (n < 2L) stop("bayes_r_prior: need at least two observations")
  XX <- .s03mat(X)
  if (nrow(XX) != n) stop("bayes_r_prior: X has a different number of rows than y")
  p <- ncol(XX)
  if (p < 1L) stop("bayes_r_prior: X has no columns")
  sc <- if (is.null(sigma_classes)) c(0, 1e-4, 1e-3, 1e-2) else .s03vec(sigma_classes)
  K <- length(sc)
  if (K < 2L) stop("bayes_r_prior: need at least two variance classes")
  if (sc[1L] != 0) {
    stop("bayes_r_prior: the first variance class must be exactly 0, the point mass that defines BayesR")
  }
  if (any(sc < 0)) stop("bayes_r_prior: variance class multipliers must be non-negative")
  if (is.null(pi)) {
    pv <- rep(1 / K, K)
  } else {
    pv <- .s03vec(pi)
    if (length(pv) != K) stop("bayes_r_prior: pi must have one entry per variance class")
    if (min(pv) < 0) stop("bayes_r_prior: mixture weights must be non-negative")
    if (sum(pv) <= 0) stop("bayes_r_prior: mixture weights must sum to something positive")
    pv <- pv / sum(pv)
  }
  dl <- if (is.null(delta)) rep(1, K) else .s03vec(delta)
  if (length(dl) != K) stop("bayes_r_prior: delta must have one entry per variance class")
  if (min(dl) <= 0) stop("bayes_r_prior: the Dirichlet parameter must be positive")
  mu <- sum(yy) / n
  beta <- numeric(p)
  xtx <- vapply(seq_len(p), function(j) sum(XX[, j]^2), 0)
  vy <- sum((yy - mu)^2) / (n - 1)
  sg2 <- max(vy / 2, 1e-12); se2 <- max(vy / 2, 1e-12)
  gam <- matrix(0, p, K)
  res <- yy - mu
  for (it in seq_len(as.integer(max_iter))) {
    prev <- beta
    for (j in seq_len(p)) {
      if (xtx[j] <= 0) {
        beta[j] <- 0
        gam[j, ] <- c(1, rep(0, K - 1L))
        next
      }
      bj <- beta[j]
      if (bj != 0) res <- res + XX[, j] * bj
      xr <- sum(XX[, j] * res)
      lg <- numeric(K)
      for (k in seq_len(K)) {
        s2 <- sc[k] * sg2
        den <- xtx[j] * s2 + se2
        lg[k] <- (if (pv[k] > 0) log(pv[k]) else -1e300) +
                 0.5 * log(se2 / den) + xr * xr * s2 / (2 * se2 * den)
      }
      w <- exp(lg - max(lg))
      gam[j, ] <- w / sum(w)
      nb <- 0
      for (k in seq_len(K)) {
        s2 <- sc[k] * sg2
        nb <- nb + gam[j, k] * xr * s2 / (xtx[j] * s2 + se2)
      }
      beta[j] <- nb
      if (nb != 0) res <- res - XX[, j] * nb
    }
    tot <- sum(dl)
    for (k in seq_len(K)) pv[k] <- (sum(gam[, k]) + dl[k]) / (p + tot)
    ss <- 0; wsum <- 0
    for (j in seq_len(p)) {
      for (k in seq(2L, K)) {
        if (sc[k] > 0) {
          ss <- ss + gam[j, k] * beta[j] * beta[j] / sc[k]
          wsum <- wsum + gam[j, k]
        }
      }
    }
    if (wsum > 0) sg2 <- max(ss / wsum, 1e-12)
    se2 <- max(sum(res^2) / n, 1e-12)
    if (max(abs(beta - prev)) < tol) break
  }
  nz <- sum(vapply(seq_len(p), function(j) which.max(gam[j, ]) != 1L, TRUE))
  list(estimate = sg2, beta_samples = beta, class_probs = gam, pi = pv,
       sigma_g2 = sg2, sigma_e2 = se2, sigma_classes = sc, mu = mu,
       n_nonzero = as.integer(nz), n = n,
       method = paste0("BayesR four-class mixture of Erbe et al. (2012), at ",
                       "the EM fixed point of its Gibbs sampler; not in the book"))
}
