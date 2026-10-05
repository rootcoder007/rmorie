# SPDX-License-Identifier: AGPL-3.0-or-later
#
# spatial_voting_bayes_native.R -- native MCMC backends for the five
# Bayesian spatial-voting estimators that previously stopped with
# NotYetPorted when their optional accelerator package was absent.
# Each sampler is deliberately simple (conjugate Gibbs where the
# model admits it, random-walk Metropolis elsewhere) and returns
# posterior means plus acceptance/trace diagnostics.

# --- Aldrich-McKelvey (Hare et al. 2015) ------------------------------------
# Z[i, j] = a_i + b_i * zeta_j + eps_ij, eps ~ N(0, sigma2).
# Gibbs: (a_i, b_i) | zeta conjugate normal per respondent;
# zeta_j | a, b conjugate normal per stimulus; sigma2 inverse gamma.
#' Internal helper: native Bayesian Aldrich-McKelvey sampler
#' @noRd
.morie_sv_bayes_am <- function(Z, n_samples = 1000L, burn_in = 200L,
                               prior_sd = 10.0) {
  Z <- as.matrix(Z)
  n <- nrow(Z)
  m <- ncol(Z)
  obs <- is.finite(Z)
  zeta <- as.numeric(scale(colMeans(Z, na.rm = TRUE)))
  a <- rowMeans(Z, na.rm = TRUE)
  b <- rep(1, n)
  sigma2 <- stats::var(as.numeric(Z[obs]))
  keep <- matrix(0, n_samples, m)
  tau0 <- 1 / prior_sd^2
  for (it in seq_len(burn_in + n_samples)) {
    for (i in seq_len(n)) {
      j <- which(obs[i, ])
      if (length(j) < 2L) next
      Xr <- cbind(1, zeta[j])
      V <- solve(crossprod(Xr) / sigma2 + diag(tau0, 2))
      mu <- V %*% (crossprod(Xr, Z[i, j]) / sigma2)
      ab <- as.numeric(mu + t(chol(V)) %*% stats::rnorm(2))
      a[i] <- ab[1]
      b[i] <- ab[2]
    }
    for (j in seq_len(m)) {
      i <- which(obs[, j])
      if (!length(i)) next
      prec <- sum(b[i]^2) / sigma2 + tau0
      mu <- sum(b[i] * (Z[i, j] - a[i])) / sigma2 / prec
      zeta[j] <- stats::rnorm(1, mu, sqrt(1 / prec))
    }
    # identification: centre and scale zeta, fix polarity
    zeta <- as.numeric(scale(zeta))
    if (zeta[1] > 0) zeta <- -zeta
    resid <- Z - (a + outer(b, zeta))
    sigma2 <- 1 / stats::rgamma(1, sum(obs) / 2 + 2,
                                sum(resid[obs]^2) / 2 + 1)
    if (it > burn_in) keep[it - burn_in, ] <- zeta
  }
  list(zeta_mean = colMeans(keep), zeta_sd = apply(keep, 2, stats::sd),
       sigma2 = sigma2, n_samples = n_samples,
       engine = "native Gibbs (Aldrich-McKelvey)")
}

# --- classical MDS (Torgerson-Gower), native ---------------------------------
#' Internal helper: classical multidimensional scaling
#'
#' Torgerson-Gower classical scaling. Double-centre the squared distance
#' matrix to recover the Gram matrix, then take its leading eigenvectors:
#'
#'   B = -1/2 J D^2 J,  J = I - (1/m) 1 1'
#'   B = V L V',  X = V_k L_k^(1/2)
#'
#' Native replacement for `stats::cmdscale`. Non-positive eigenvalues are
#' clamped to zero, which is what happens whenever D is not Euclidean.
#'
#' @param D Distance matrix (m by m), symmetric with a zero diagonal.
#' @param k Number of dimensions to return.
#' @return An m by k matrix of coordinates, columns ordered by decreasing
#'   eigenvalue.
#' @references Torgerson (1952) Psychometrika 17:401-419; Gower (1966)
#'   Biometrika 53:325-338.
#' @noRd
.morie_sv_cmdscale <- function(D, k = 2L) {
  D <- as.matrix(D)
  m <- nrow(D)
  if (m != ncol(D)) stop("`D` must be square", call. = FALSE)
  k <- as.integer(k)
  if (k < 1L || k >= m) {
    stop("`k` must satisfy 1 <= k < nrow(D)", call. = FALSE)
  }
  D2 <- D^2
  # double-centring: B = -1/2 J D^2 J, done via row/column means so no
  # m by m projection matrix is ever formed
  rm_ <- rowMeans(D2)
  gm <- mean(D2)
  B <- -0.5 * (D2 - outer(rm_, rep(1, m)) - outer(rep(1, m), rm_) + gm)
  B <- (B + t(B)) / 2                    # enforce symmetry against drift
  e <- eigen(B, symmetric = TRUE)
  lam <- pmax(e$values[seq_len(k)], 0)   # clamp: D need not be Euclidean
  X <- e$vectors[, seq_len(k), drop = FALSE] %*% diag(sqrt(lam), nrow = k)
  dimnames(X) <- list(rownames(D), NULL)
  X
}

# --- Bayesian MDS (Oh & Raftery 2001 lognormal distances) -------------------
#' Internal helper: native Bayesian MDS sampler
#' @noRd
.morie_sv_bayes_mds <- function(D, n_dims = 2L, n_samples = 1000L,
                                burn_in = 200L, sigma_init = 1.0) {
  D <- as.matrix(D)
  m <- nrow(D)
  lower <- D[lower.tri(D)]
  if (any(!is.finite(log(lower[lower > 0]))) || all(lower <= 0)) {
    stop("morie_spatial_voting_bayesian_mds: D must contain positive ",
         "distances.", call. = FALSE)
  }
  X <- .morie_sv_cmdscale(D, k = n_dims)
  if (!is.matrix(X)) X <- matrix(X, ncol = n_dims)
  sigma <- sigma_init
  step <- 0.05 * stats::sd(X)
  keep <- array(0, c(n_samples, m, n_dims))
  acc <- 0L
  tot <- 0L
  ll <- function(X, sigma) {
    delta <- as.matrix(stats::dist(X))
    dl <- delta[lower.tri(delta)]
    dl[dl <= 0] <- .Machine$double.eps
    ok <- lower > 0
    sum(stats::dnorm(log(lower[ok]), log(dl[ok]), sigma, log = TRUE))
  }
  cur <- ll(X, sigma)
  for (it in seq_len(burn_in + n_samples)) {
    for (i in seq_len(m)) {
      Xp <- X
      Xp[i, ] <- X[i, ] + stats::rnorm(n_dims, 0, step)
      prop <- ll(Xp, sigma) +
        sum(stats::dnorm(Xp[i, ], 0, 10, log = TRUE)) -
        sum(stats::dnorm(X[i, ], 0, 10, log = TRUE))
      tot <- tot + 1L
      if (log(stats::runif(1)) < prop - cur) {
        X <- Xp
        cur <- prop
        acc <- acc + 1L
      }
    }
    # sigma via random-walk on log scale
    sp <- sigma * exp(stats::rnorm(1, 0, 0.1))
    lp <- ll(X, sp) - log(sp)
    if (log(stats::runif(1)) < lp - (cur - log(sigma))) {
      sigma <- sp
      cur <- ll(X, sigma)
    }
    if (it > burn_in) keep[it - burn_in, , ] <- X
  }
  list(positions = apply(keep, c(2, 3), mean), sigma = sigma,
       acceptance = acc / tot, n_samples = n_samples,
       engine = "native Metropolis (Oh-Raftery MDS)")
}

# --- Bayesian unfolding (ideal points + stimuli from preferences) -----------
#' Internal helper: native Bayesian unfolding sampler
#' @noRd
.morie_sv_bayes_unfold <- function(P, n_dims = 2L, n_samples = 1000L,
                                   burn_in = 200L) {
  P <- as.matrix(P)
  n <- nrow(P)
  m <- ncol(P)
  Pz <- scale(P)
  Pz[!is.finite(Pz)] <- 0
  # init from double-centred SVD
  sv <- svd(Pz, nu = n_dims, nv = n_dims)
  Xi <- sv$u %*% diag(sqrt(sv$d[seq_len(n_dims)]), n_dims)
  Zj <- sv$v %*% diag(sqrt(sv$d[seq_len(n_dims)]), n_dims)
  sigma <- 1
  step <- 0.1
  ll <- function(Xi, Zj, sigma) {
    D2 <- outer(rowSums(Xi^2), rowSums(Zj^2), "+") - 2 * Xi %*% t(Zj)
    mu <- -D2
    mu <- scale(mu) # preferences are interval-scale up to affine
    mu[!is.finite(mu)] <- 0
    sum(stats::dnorm(Pz, mu, sigma, log = TRUE))
  }
  cur <- ll(Xi, Zj, sigma)
  keepZ <- array(0, c(n_samples, m, n_dims))
  acc <- 0L
  tot <- 0L
  for (it in seq_len(burn_in + n_samples)) {
    for (j in seq_len(m)) {
      Zp <- Zj
      Zp[j, ] <- Zj[j, ] + stats::rnorm(n_dims, 0, step)
      prop <- ll(Xi, Zp, sigma)
      tot <- tot + 1L
      if (log(stats::runif(1)) < prop - cur) {
        Zj <- Zp
        cur <- prop
        acc <- acc + 1L
      }
    }
    for (i in seq_len(n)) {
      Xp <- Xi
      Xp[i, ] <- Xi[i, ] + stats::rnorm(n_dims, 0, step)
      prop <- ll(Xp, Zj, sigma)
      tot <- tot + 1L
      if (log(stats::runif(1)) < prop - cur) {
        Xi <- Xp
        cur <- prop
        acc <- acc + 1L
      }
    }
    if (it > burn_in) keepZ[it - burn_in, , ] <- Zj
  }
  list(stimuli = apply(keepZ, c(2, 3), mean), ideal_points = Xi,
       acceptance = acc / tot, n_samples = n_samples,
       engine = "native Metropolis (Bayesian unfolding)")
}

# --- Clinton-Jackman-Rivers binary IRT (Albert-Chib Gibbs) ------------------
# y_ij ~ Bernoulli(Phi(beta_j x_i - alpha_j)), 1-D default.
#' Internal helper: native CJR IRT sampler
#' @noRd
.morie_sv_bayes_cjr <- function(votes, n_samples = 1000L,
                                burn_in = 200L) {
  Y <- as.matrix(votes)
  n <- nrow(Y)
  m <- ncol(Y)
  obs <- is.finite(Y)
  Y01 <- (Y > 0) * 1L
  x <- as.numeric(scale(rowMeans(Y01, na.rm = TRUE)))
  x[!is.finite(x)] <- 0
  alpha <- rep(0, m)
  beta <- rep(1, m)
  keep_x <- matrix(0, n_samples, n)
  rtnorm <- function(n, mu, lower) {
    # truncated N(mu,1) on [lower, Inf) via inverse CDF
    u <- stats::runif(n)
    p0 <- stats::pnorm(lower - mu)
    stats::qnorm(p0 + u * (1 - p0)) + mu
  }
  for (it in seq_len(burn_in + n_samples)) {
    # latent utilities
    Ystar <- matrix(0, n, m)
    mu <- outer(x, beta) - matrix(alpha, n, m, byrow = TRUE)
    pos <- obs & Y01 == 1L
    neg <- obs & Y01 == 0L
    Ystar[pos] <- rtnorm(sum(pos), mu[pos], 0)
    Ystar[neg] <- -rtnorm(sum(neg), -mu[neg], 0)
    # item params (alpha_j, beta_j) | x: Bayesian regression per item
    for (j in seq_len(m)) {
      i <- which(obs[, j])
      Xr <- cbind(-1, x[i])
      V <- solve(crossprod(Xr) + diag(0.04, 2))
      mu_j <- V %*% crossprod(Xr, Ystar[i, j])
      ab <- as.numeric(mu_j + t(chol(V)) %*% stats::rnorm(2))
      alpha[j] <- ab[1]
      beta[j] <- ab[2]
    }
    # ideal points x_i | items
    for (i in seq_len(n)) {
      j <- which(obs[i, ])
      prec <- sum(beta[j]^2) + 1
      mu_i <- sum(beta[j] * (Ystar[i, j] + alpha[j])) / prec
      x[i] <- stats::rnorm(1, mu_i, sqrt(1 / prec))
    }
    x <- as.numeric(scale(x))
    if (it > burn_in) keep_x[it - burn_in, ] <- x
  }
  list(ideal_points = colMeans(keep_x),
       ideal_sd = apply(keep_x, 2, stats::sd),
       discrimination = beta, difficulty = alpha,
       n_samples = n_samples,
       engine = "native Albert-Chib Gibbs (CJR IRT)")
}

# --- Ordinal IRT (graded probit, Albert-Chib + cutpoint MH) -----------------
#' Internal helper: native ordinal IRT sampler
#' @noRd
.morie_sv_bayes_ordinal <- function(votes, n_samples = 1000L,
                                    burn_in = 200L) {
  Y <- as.matrix(votes)
  n <- nrow(Y)
  m <- ncol(Y)
  obs <- is.finite(Y)
  K <- max(Y[obs])
  if (K < 2L) stop("ordinal IRT needs at least two categories.",
                   call. = FALSE)
  # shared cutpoints c_1 < ... < c_{K-1}; c_1 fixed at 0 for scale
  cuts <- stats::qnorm(seq_len(K - 1L) / K)
  cuts <- cuts - cuts[1]
  x <- as.numeric(scale(rowMeans(Y, na.rm = TRUE)))
  x[!is.finite(x)] <- 0
  alpha <- rep(0, m)
  beta <- rep(1, m)
  keep_x <- matrix(0, n_samples, n)
  rtnorm_ab <- function(mu, lo, hi) {
    p_lo <- stats::pnorm(lo - mu)
    p_hi <- stats::pnorm(hi - mu)
    stats::qnorm(p_lo + stats::runif(length(mu)) *
                   pmax(p_hi - p_lo, 1e-12)) + mu
  }
  bnd <- c(-Inf, cuts, Inf)
  for (it in seq_len(burn_in + n_samples)) {
    mu <- outer(x, beta) - matrix(alpha, n, m, byrow = TRUE)
    Ystar <- matrix(0, n, m)
    for (k in seq_len(K)) {
      sel <- obs & Y == k
      if (!any(sel)) next
      Ystar[sel] <- rtnorm_ab(mu[sel], bnd[k], bnd[k + 1L])
    }
    for (j in seq_len(m)) {
      i <- which(obs[, j])
      Xr <- cbind(-1, x[i])
      V <- solve(crossprod(Xr) + diag(0.04, 2))
      mu_j <- V %*% crossprod(Xr, Ystar[i, j])
      ab <- as.numeric(mu_j + t(chol(V)) %*% stats::rnorm(2))
      alpha[j] <- ab[1]
      beta[j] <- ab[2]
    }
    for (i in seq_len(n)) {
      j <- which(obs[i, ])
      prec <- sum(beta[j]^2) + 1
      mu_i <- sum(beta[j] * (Ystar[i, j] + alpha[j])) / prec
      x[i] <- stats::rnorm(1, mu_i, sqrt(1 / prec))
    }
    x <- as.numeric(scale(x))
    # cutpoint MH (skip the fixed first cut)
    if (K > 2L) {
      for (k in 2:(K - 1L)) {
        prop <- cuts
        prop[k] <- stats::rnorm(1, cuts[k], 0.05)
        if (prop[k] <= prop[k - 1L] ||
            (k < K - 1L && prop[k] >= cuts[k + 1L])) next
        mu_o <- outer(x, beta) - matrix(alpha, n, m, byrow = TRUE)
        llk <- function(cts) {
          b <- c(-Inf, cts, Inf)
          s <- 0
          for (kk in seq_len(K)) {
            sel <- obs & Y == kk
            if (!any(sel)) next
            s <- s + sum(log(pmax(
              stats::pnorm(b[kk + 1L] - mu_o[sel]) -
                stats::pnorm(b[kk] - mu_o[sel]), 1e-12)))
          }
          s
        }
        if (log(stats::runif(1)) < llk(prop) - llk(cuts)) cuts <- prop
        bnd <- c(-Inf, cuts, Inf)
      }
    }
    if (it > burn_in) keep_x[it - burn_in, ] <- x
  }
  list(ideal_points = colMeans(keep_x),
       ideal_sd = apply(keep_x, 2, stats::sd),
       discrimination = beta, difficulty = alpha, cutpoints = cuts,
       n_samples = n_samples,
       engine = "native Albert-Chib Gibbs (ordinal probit IRT)")
}

# Martin and Quinn (2002) dynamic one-dimensional IRT by Gibbs sampling, the
# model of MCMCpack::MCMCdynamicIRT1d: z_jk = -alpha_k + beta_k theta_{j,t(k)} + e,
# theta_{j,0} ~ N(e0, E0), theta_{j,t} ~ N(theta_{j,t-1}, tau2_j) for t = 1, ..., T, alpha_k ~
# N(a0, 1/A0), beta_k ~ N(b0, 1/B0), tau2_j ~ IG(c0/2, d0/2) (held at tau2
# when c0 or d0 is not positive). Each sweep: (1) truncated-normal latent
# utilities, (2) a conjugate normal draw of each roll call's (alpha, beta),
# (3) a forward-filter backward-sample draw of every legislator's path,
# (4) the evolution variances. The sign is fixed by reflecting a draw
# (theta, beta) -> (-theta, -beta), which leaves the likelihood unchanged,
# whenever the anchor legislator's mean ideal point comes out negative.
#' @noRd
.morie_sv_dynamic_irt_gibbs <- function(votes, period, n_samples = 500L, burn_in = 100L,
                                        thin = 1L, seed = 42L, tau2 = 1, e0 = 0, E0 = 1,
                                        a0 = 0, A0 = 0.1, b0 = 0, B0 = 0.1, c0 = -1, d0 = -1,
                                        anchor = NULL) {
  Y <- as.matrix(votes)
  Y[!is.na(Y) & Y < 0] <- 0                       # -1/1 coding read as 0/1
  if (any(!is.na(Y) & !Y %in% c(0, 1))) stop("votes must be 0/1 (or -1/1) with NA for missing", call. = FALSE)
  N <- nrow(Y); K <- ncol(Y)
  per <- as.integer(factor(period))
  Tn <- max(per)
  obs <- !is.na(Y)
  .rmorie_local_seed(seed)
  # start: first principal component of the vote matrix, scaled
  Yc <- Y; Yc[!obs] <- 0.5
  pc <- prcomp(Yc, center = TRUE)$x[, 1]
  pc <- if (stats::sd(pc) > 0) as.numeric(scale(pc)) else rep(0, N)
  if (is.null(anchor)) anchor <- which.max(pc)
  theta <- matrix(pc, N, Tn)
  alpha <- rep(0, K); beta <- rep(1, K)
  t2 <- rep(tau2, length.out = N)
  est_tau <- all(c0 > 0) && all(d0 > 0)
  keep <- seq.int(burn_in + thin, burn_in + n_samples * thin, by = thin)
  S_th <- S_th2 <- matrix(0, N, Tn); S_a <- S_b <- numeric(K); S_t2 <- numeric(N); m <- 0L
  lo <- ifelse(obs & Y == 1, 0, -Inf); hi <- ifelse(obs & Y == 0, 0, Inf)
  Z <- matrix(0, N, K)
  for (it in seq_len(burn_in + n_samples * thin)) {
    # (1) latent utilities, truncated by the observed vote
    mu <- sweep(theta[, per, drop = FALSE] * rep(beta, each = N), 2L, alpha)
    pl <- stats::pnorm(lo - mu); ph <- stats::pnorm(hi - mu)
    u <- stats::runif(N * K)
    Z[] <- mu + stats::qnorm(pmin(pmax(pl + u * (ph - pl), 1e-12), 1 - 1e-12))
    # (2) roll-call parameters: regress z_.k on (-1, theta_.t(k))
    for (k in seq_len(K)) {
      x <- theta[, per[k]]
      XtX <- matrix(c(N, -sum(x), -sum(x), sum(x^2)), 2) + diag(c(A0, B0))
      Xtz <- c(-sum(Z[, k]), sum(x * Z[, k])) + c(A0 * a0, B0 * b0)
      V <- solve(XtX)
      draw <- as.numeric(V %*% Xtz + t(chol(V)) %*% stats::rnorm(2))
      alpha[k] <- draw[1]; beta[k] <- draw[2]
    }
    # (3) ideal-point paths by forward filtering, backward sampling
    prec_obs <- matrix(0, N, Tn); lin_obs <- matrix(0, N, Tn)
    for (t in seq_len(Tn)) {
      ks <- which(per == t)
      prec_obs[, t] <- sum(beta[ks]^2)
      lin_obs[, t] <- as.numeric((Z[, ks, drop = FALSE] + rep(alpha[ks], each = N)) %*% beta[ks])
    }
    mf <- Pf <- matrix(0, N, Tn)
    m_prev <- rep(e0, N); P_prev <- rep(E0, N)
    for (t in seq_len(Tn)) {
      Pp <- P_prev + t2   # theta_{j,0} ~ N(e0, E0); period 1 is one step of the walk on
      Pf[, t] <- 1 / (1 / Pp + prec_obs[, t])
      mf[, t] <- Pf[, t] * (m_prev / Pp + lin_obs[, t])
      m_prev <- mf[, t]; P_prev <- Pf[, t]
    }
    theta[, Tn] <- mf[, Tn] + sqrt(Pf[, Tn]) * stats::rnorm(N)
    if (Tn > 1L) for (t in (Tn - 1L):1L) {
      G <- Pf[, t] / (Pf[, t] + t2)
      mb <- mf[, t] + G * (theta[, t + 1L] - mf[, t])
      vb <- Pf[, t] * (1 - G)
      theta[, t] <- mb + sqrt(vb) * stats::rnorm(N)
    }
    # (4) evolution variances
    if (est_tau && Tn > 1L) {
      ss <- rowSums((theta[, -1L, drop = FALSE] - theta[, -Tn, drop = FALSE])^2)
      t2 <- 1 / stats::rgamma(N, shape = (c0 + Tn - 1) / 2, rate = (d0 + ss) / 2)
    }
    if (mean(theta[anchor, ]) < 0) { theta <- -theta; beta <- -beta }
    if (it %in% keep) {
      S_th <- S_th + theta; S_th2 <- S_th2 + theta^2
      S_a <- S_a + alpha; S_b <- S_b + beta; S_t2 <- S_t2 + t2; m <- m + 1L
    }
  }
  th <- S_th / m
  list(theta = th, theta_sd = sqrt(pmax(S_th2 / m - th^2, 0)), alpha = S_a / m,
       beta = S_b / m, tau2 = S_t2 / m, periods = sort(unique(period)),
       n_samples = m, anchor = anchor)
}
