# SPDX-License-Identifier: AGPL-3.0-or-later
#
# spatial_voting_bayes_native.R -- native MCMC samplers for the Bayesian
# spatial-voting estimators: Bayesian Aldrich-McKelvey, Bayesian metric
# MDS and unfolding, Clinton-Jackman-Rivers and ordinal IRT, Martin-Quinn
# dynamic IRT and alpha-NOMINATE.  Conjugate Gibbs steps where the model
# admits them, slice sampling (Neal 2003) elsewhere.

# Draw from N(mu, sd^2) truncated to [lo, hi] by inverting the CDF.
.morie_rtnorm <- function(mu, sd, lo, hi) {
  pl <- stats::pnorm(lo, mu, sd)
  ph <- stats::pnorm(hi, mu, sd)
  u <- pl + stats::runif(length(mu)) * (ph - pl)
  out <- stats::qnorm(u, mu, sd)
  out[!is.finite(out)] <- pmin(pmax(mu, lo), hi)[!is.finite(out)]
  pmin(pmax(out, lo), hi)
}

# --- Bayesian Aldrich-McKelvey (Hare et al. 2015) ----------------------------
# The model of the authors' JAGS template (asmcjr, BAM_JAGScode.bug):
#   z_ij ~ N(a_i + b_i zhat_j, 1 / (taui_i tauj_j)),
#   a_i, b_i ~ U(-100, 100), tauj_j ~ G(0.1, 0.1), taui_i ~ G(ga, gb),
#   ga, gb ~ G(0.1, 0.1), zstar_j ~ N(0, 1) truncated to (-100, 100) and to
#   (-100, 0) for the polarity stimulus, zhat = (zstar - mean) / sd.
# Gibbs: a | b and b | a truncated normal, tauj, taui and gb conjugate
# gamma, ga and each zstar_j by slice sampling (zhat couples every zstar
# through the mean and sd, so they are updated one at a time).
#' Internal helper: native Bayesian Aldrich-McKelvey sampler
#' @noRd
.morie_sv_bayes_am <- function(Z, n_samples = 1000L, burn_in = 200L,
                               polarity = 1L, thin = 1L) {
  Z <- as.matrix(Z)
  Z <- Z[rowSums(is.finite(Z)) > 0L, , drop = FALSE]
  N <- nrow(Z)
  q <- ncol(Z)
  obs <- is.finite(Z)
  Z0 <- Z
  Z0[!obs] <- 0
  lower <- rep(-100, q)
  upper <- rep(100, q)
  upper[polarity] <- 0
  zs <- as.numeric(scale(colMeans(Z, na.rm = TRUE)))
  zs[!is.finite(zs)] <- 0
  if (zs[polarity] > 0) zs <- -zs
  zs <- pmin(pmax(zs, lower + 1e-8), upper - 1e-8)
  std <- function(v) (v - mean(v)) / stats::sd(v)
  a <- rowMeans(Z, na.rm = TRUE)
  b <- rep(1, N)
  taui <- rep(1, N)
  tauj <- rep(1, q)
  ga <- 1
  gb <- 1
  n_i <- rowSums(obs)
  n_j <- colSums(obs)
  n_iter <- burn_in + n_samples * thin
  keep <- seq.int(burn_in + thin, n_iter, by = thin)
  S <- length(keep)
  out_z <- matrix(NA_real_, S, q)
  out_a <- out_b <- matrix(NA_real_, S, N)
  out_tj <- matrix(NA_real_, S, q)
  s <- 0L
  for (it in seq_len(n_iter)) {
    zh <- std(zs)
    W <- outer(taui, tauj) * obs
    ZH <- matrix(zh, N, q, byrow = TRUE)
    sw <- rowSums(W)
    a <- .morie_rtnorm(rowSums(W * (Z0 - b * ZH)) / sw, 1 / sqrt(sw),
                       -100, 100)
    swz <- rowSums(W * ZH^2)
    b <- .morie_rtnorm(rowSums(W * ZH * (Z0 - a)) / swz, 1 / sqrt(swz),
                       -100, 100)
    R2 <- (Z0 - a - b * ZH)^2 * obs
    tauj <- stats::rgamma(q, 0.1 + n_j / 2, 0.1 + colSums(taui * R2) / 2)
    taui <- stats::rgamma(N, ga + n_i / 2,
                          gb + rowSums(R2 * matrix(tauj, N, q,
                                                   byrow = TRUE)) / 2)
    gb <- stats::rgamma(1L, 0.1 + N * ga, 0.1 + sum(taui))
    slt <- sum(log(taui))
    ga <- .morie_slice_vec(function(g) {
      -0.9 * log(g) - 0.1 * g + N * g * log(gb) - N * lgamma(g) +
        (g - 1) * slt
    }, ga, 8, lower = 0)
    lsd <- 0.5 * log(outer(taui, tauj))
    for (j in seq_len(q)) {
      zs[j] <- .morie_slice_vec(function(v) {
        vapply(v, function(vv) {
          zz <- zs
          zz[j] <- vv
          m <- a + outer(b, std(zz))
          stats::dnorm(vv, log = TRUE) +
            sum((lsd - 0.5 * exp(2 * lsd) * (Z0 - m)^2)[obs])
        }, 0)
      }, zs[j], 8, lower = lower[j], upper = upper[j])
    }
    if (it %in% keep) {
      s <- s + 1L
      out_z[s, ] <- std(zs)
      out_a[s, ] <- a
      out_b[s, ] <- b
      out_tj[s, ] <- tauj
    }
  }
  list(zeta_mean = colMeans(out_z),
       zeta_sd = apply(out_z, 2L, stats::sd),
       zeta_interval = apply(out_z, 2L, stats::quantile, c(0.025, 0.975),
                             names = FALSE),
       a = colMeans(out_a), b = colMeans(out_b),
       tau_stimulus = colMeans(out_tj),
       draws = out_z, n_samples = S,
       engine = "native Gibbs (Bayesian Aldrich-McKelvey, Hare et al. 2015)")
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

# Draw the lognormal precision tau from its full conditional under the
# U(0, 10) prior of Bakker and Poole (2013): Gamma(n/2 + 1, SSE/2)
# truncated to (0, 10).
.morie_bp_tau <- function(n, sse) {
  shape <- n / 2 + 1
  rate <- sse / 2
  stats::qgamma(stats::runif(1L) * stats::pgamma(10, shape, rate), shape, rate)
}

# Rigid alignment (translation + orthogonal rotation) of the rows of X onto
# target T; returns the function that applies it.
.morie_rigid_align <- function(X, T) {
  mx <- colMeans(X)
  mt <- colMeans(T)
  Q <- .morie_procrustes_rot(sweep(X, 2L, mx), sweep(T, 2L, mt))
  function(A) sweep(sweep(A, 2L, mx) %*% Q, 2L, mt, "+")
}

# --- Bayesian metric MDS (Bakker and Poole 2013) -----------------------------
# log delta_ij ~ N(log d_ij, 1 / tau) for i < j, with d_ij the Euclidean
# distance between rows i and j of the configuration; coordinates
# N(0, 10^2), tau ~ U(0, 10) -- the model of the authors' JAGS code
# (asmcjr::BMDS).  Non-positive or missing dissimilarities are left out.
# Each coordinate is slice sampled in turn, tau drawn exactly; the draws
# are aligned (translation and rotation) onto the posterior mean.
#' Internal helper: native Bayesian MDS sampler
#' @noRd
.morie_sv_bayes_mds <- function(D, n_dims = 2L, n_samples = 1000L,
                                burn_in = 200L, sigma_init = 1.0) {
  D <- as.matrix(D)
  m <- nrow(D)
  lower <- D[lower.tri(D)]
  if (!any(is.finite(lower) & lower > 0)) {
    stop("morie_spatial_voting_bayesian_mds: D must contain positive ",
         "distances.", call. = FALSE)
  }
  ok <- is.finite(D) & D > 0 & upper.tri(D)
  ok <- ok | t(ok)
  LD <- matrix(0, m, m)
  LD[ok] <- log(D[ok])
  n_obs <- sum(ok) / 2
  X <- .morie_sv_cmdscale(ifelse(is.finite(D), D, 0), k = n_dims)
  X <- X + matrix(stats::rnorm(m * n_dims, 0, 1e-3), m, n_dims)
  tau <- 1 / sigma_init^2
  ld <- function(X) {
    d <- sqrt(.morie_sqdist(X, X))
    log(pmax(d, 1e-12))
  }
  keep <- array(NA_real_, c(n_samples, m, n_dims))
  ktau <- numeric(n_samples)
  for (it in seq_len(burn_in + n_samples)) {
    for (i in seq_len(m)) {
      oi <- ok[i, ]
      for (k in seq_len(n_dims)) {
        rest <- colSums((t(X[oi, -k, drop = FALSE]) - X[i, -k])^2)
        X[i, k] <- .morie_slice_vec(function(v) {
          vapply(v, function(vv) {
            lhat <- 0.5 * log(pmax(rest + (X[oi, k] - vv)^2, 1e-24))
            -0.5 * tau * sum((LD[i, oi] - lhat)^2) - vv^2 / 200
          }, 0)
        }, X[i, k], 1)
      }
    }
    sse <- sum(((LD - ld(X))[ok])^2) / 2
    tau <- .morie_bp_tau(n_obs, sse)
    if (it > burn_in) {
      keep[it - burn_in, , ] <- X
      ktau[it - burn_in] <- tau
    }
  }
  draw <- function(s) matrix(keep[s, , ], ncol = n_dims)
  target <- draw(n_samples)
  for (pass in 1:2) {
    for (s in seq_len(n_samples)) {
      keep[s, , ] <- .morie_rigid_align(draw(s), target)(draw(s))
    }
    target <- apply(keep, c(2L, 3L), mean)
  }
  dmean <- Reduce(`+`, lapply(seq_len(n_samples), function(s) {
    sqrt(.morie_sqdist(draw(s), draw(s)))
  })) / n_samples
  list(positions = target,
       positions_sd = matrix(apply(keep, c(2L, 3L), stats::sd), ncol = n_dims),
       distance_mean = dmean, sigma = mean(1 / sqrt(ktau)),
       tau = mean(ktau), draws = keep, n_samples = n_samples,
       engine = "native slice-within-Gibbs (Bakker-Poole Bayesian MDS)")
}

# --- Bayesian unfolding (Bakker and Poole 2013) ------------------------------
# The same lognormal model for a respondent-by-stimulus matrix:
# log delta_ij ~ N(log ||x_i - z_j||, 1 / tau).  Given the stimuli the
# respondents are independent, and given the respondents the stimuli are,
# so each block is slice sampled as one vector.
#' Internal helper: native Bayesian unfolding sampler
#' @noRd
.morie_sv_bayes_unfold <- function(P, n_dims = 2L, n_samples = 1000L,
                                   burn_in = 200L) {
  P <- as.matrix(P)
  n <- nrow(P)
  m <- ncol(P)
  ok <- is.finite(P) & P > 0
  if (!any(ok)) {
    stop("morie_spatial_voting_bayesian_unfolding: D must contain ",
         "positive dissimilarities.", call. = FALSE)
  }
  LP <- matrix(0, n, m)
  LP[ok] <- log(P[ok])
  Pz <- P
  Pz[!ok] <- mean(P[ok])
  sv <- svd(scale(Pz, scale = FALSE), nu = n_dims, nv = n_dims)
  X <- sv$u %*% diag(sqrt(sv$d[seq_len(n_dims)]), n_dims) / sqrt(n)
  Zs <- sv$v %*% diag(sqrt(sv$d[seq_len(n_dims)]), n_dims) / sqrt(m)
  tau <- 1
  ll <- function(D2) (LP - 0.5 * log(pmax(D2, 1e-24)))^2 * ok
  keepX <- array(NA_real_, c(n_samples, n, n_dims))
  keepZ <- array(NA_real_, c(n_samples, m, n_dims))
  ktau <- numeric(n_samples)
  dsum <- matrix(0, n, m)
  for (it in seq_len(burn_in + n_samples)) {
    for (k in seq_len(n_dims)) {
      base <- .morie_sqdist(X, Zs) - outer(X[, k], Zs[, k], "-")^2
      X[, k] <- .morie_slice_vec(function(v) {
        -0.5 * tau * rowSums(ll(base + outer(v, Zs[, k], "-")^2)) - v^2 / 200
      }, X[, k], 1)
    }
    for (k in seq_len(n_dims)) {
      base <- .morie_sqdist(X, Zs) - outer(X[, k], Zs[, k], "-")^2
      Zs[, k] <- .morie_slice_vec(function(v) {
        -0.5 * tau * colSums(ll(base + outer(X[, k], v, "-")^2)) - v^2 / 200
      }, Zs[, k], 1)
    }
    tau <- .morie_bp_tau(sum(ok), sum(ll(.morie_sqdist(X, Zs))))
    if (it > burn_in) {
      keepX[it - burn_in, , ] <- X
      keepZ[it - burn_in, , ] <- Zs
      ktau[it - burn_in] <- tau
      dsum <- dsum + sqrt(.morie_sqdist(X, Zs))
    }
  }
  dX <- function(s) matrix(keepX[s, , ], ncol = n_dims)
  dZ <- function(s) matrix(keepZ[s, , ], ncol = n_dims)
  target <- dZ(n_samples)
  for (pass in 1:2) {
    for (s in seq_len(n_samples)) {
      f <- .morie_rigid_align(dZ(s), target)
      keepX[s, , ] <- f(dX(s))
      keepZ[s, , ] <- f(dZ(s))
    }
    target <- apply(keepZ, c(2L, 3L), mean)
  }
  list(stimuli = target,
       stimuli_sd = matrix(apply(keepZ, c(2L, 3L), stats::sd), ncol = n_dims),
       ideal_points = apply(keepX, c(2L, 3L), mean),
       distance_mean = dsum / n_samples,
       sigma = mean(1 / sqrt(ktau)), tau = mean(ktau),
       n_samples = n_samples,
       engine = "native slice-within-Gibbs (Bakker-Poole Bayesian unfolding)")
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

# --- Ordinal IRT / factor model (Quinn 2004) ---------------------------------
# y*_ij = lambda_j0 + lambda_j' phi_i + e_ij, e ~ N(0, 1), and
# y_ij = c when gamma_j,c-1 < y*_ij <= gamma_j,c, with gamma_j0 = -Inf,
# gamma_j1 = 0, gamma_jC = Inf; phi_i ~ N(0, I); lambda_j ~ N(0, I / L0)
# (L0 = 0, the default, is the flat prior of MCMCpack::MCMCordfactanal);
# flat prior on the free cutpoints.  Each sweep: the free cutpoints of
# every item by the Cowles (1996) Metropolis-Hastings step with y*
# integrated out, then y* from truncated normals, lambda_j and phi_i from
# their Gaussian full conditionals.
#' Internal helper: native ordinal IRT sampler
#' @noRd
.morie_sv_bayes_ordinal <- function(Y, n_dims = 1L, n_samples = 1000L,
                                    burn_in = 200L, L0 = 0) {
  Y <- as.matrix(Y)
  n <- nrow(Y)
  J <- ncol(Y)
  D <- as.integer(n_dims)
  obs <- !is.na(Y)
  # recode every item to 1..K_j by the order of its observed values
  ncat <- integer(J)
  for (j in seq_len(J)) {
    lv <- sort(unique(Y[obs[, j], j]))
    Y[, j] <- match(Y[, j], lv)
    ncat[j] <- length(lv)
  }
  if (any(ncat < 2L)) {
    stop("Every item needs at least two observed categories.",
         call. = FALSE)
  }
  gam <- lapply(ncat, function(k) {
    c(-Inf, 0, if (k > 2L) seq_len(k - 2L) * 0.5, Inf)
  })
  tune <- 0.05 / ncat
  acc <- numeric(J)
  # start from the leading principal components of the mean-filled data
  M <- Y
  M[!obs] <- colMeans(Y, na.rm = TRUE)[col(M)][!obs]
  M <- M + matrix(stats::rnorm(n * J, 0, 1e-6), n, J)
  phi <- scale(stats::prcomp(M, rank. = D)$x[, seq_len(D), drop = FALSE])
  Lam <- matrix(0, J, D + 1L)
  lo <- hi <- matrix(0, n, J)
  bounds <- function() {
    for (j in seq_len(J)) {
      o <- obs[, j]
      lo[o, j] <<- gam[[j]][Y[o, j]]
      hi[o, j] <<- gam[[j]][Y[o, j] + 1L]
    }
  }
  bounds()
  ystar <- matrix(0, n, J)
  keep_phi <- array(NA_real_, c(n_samples, n, D))
  keep_lam <- array(NA_real_, c(n_samples, J, D + 1L))
  gsum <- lapply(gam, function(g) 0 * g[2:(length(g) - 1L)])
  for (it in seq_len(burn_in + n_samples)) {
    mu <- cbind(1, phi) %*% t(Lam)
    for (j in which(ncat > 2L)) {
      g <- gam[[j]]
      gp <- g
      k <- ncat[j]
      for (cc in 3:k) {
        gp[cc] <- .morie_rtnorm(g[cc], tune[j], gp[cc - 1L], g[cc + 1L])
      }
      o <- obs[, j]
      y <- Y[o, j]
      m <- mu[o, j]
      ll <- function(gg) {
        sum(log(pmax(stats::pnorm(gg[y + 1L] - m) - stats::pnorm(gg[y] - m),
                     1e-300)))
      }
      # proposal-density correction for the truncated proposals
      cc <- 3:k
      corr <- sum(log(stats::pnorm((g[cc + 1L] - g[cc]) / tune[j]) -
                        stats::pnorm((gp[cc - 1L] - g[cc]) / tune[j]))) -
        sum(log(stats::pnorm((gp[cc + 1L] - gp[cc]) / tune[j]) -
                  stats::pnorm((g[cc - 1L] - gp[cc]) / tune[j])))
      if (log(stats::runif(1L)) < ll(gp) - ll(g) + corr) {
        gam[[j]] <- gp
        acc[j] <- acc[j] + 1
      }
    }
    if (it <= burn_in && it %% 50L == 0L) {
      rate <- acc / 50
      tune <- tune * ifelse(rate < 0.2, 0.7, ifelse(rate > 0.5, 1.4, 1))
      acc[] <- 0
    }
    if (it == burn_in) acc[] <- 0
    bounds()
    ystar[obs] <- .morie_rtnorm(mu[obs], 1, lo[obs], hi[obs])
    Xd <- cbind(1, phi)
    for (j in seq_len(J)) {
      o <- obs[, j]
      Xo <- Xd[o, , drop = FALSE]
      V <- solve(crossprod(Xo) + diag(L0, D + 1L))
      Lam[j, ] <- as.numeric(V %*% crossprod(Xo, ystar[o, j]) +
                               t(chol(V)) %*% stats::rnorm(D + 1L))
    }
    B <- Lam[, -1L, drop = FALSE]
    R <- ystar - matrix(Lam[, 1L], n, J, byrow = TRUE)
    full <- rowSums(obs) == J
    if (any(full)) {
      W <- solve(diag(1, D) + crossprod(B))
      phi[full, ] <- R[full, , drop = FALSE] %*% B %*% W +
        matrix(stats::rnorm(sum(full) * D), sum(full), D) %*% chol(W)
    }
    for (i in which(!full)) {
      o <- obs[i, ]
      Bo <- B[o, , drop = FALSE]
      W <- solve(diag(1, D) + crossprod(Bo))
      phi[i, ] <- as.numeric(W %*% crossprod(Bo, R[i, o]) +
                               t(chol(W)) %*% stats::rnorm(D))
    }
    if (it > burn_in) {
      keep_phi[it - burn_in, , ] <- phi
      keep_lam[it - burn_in, , ] <- Lam
      for (j in seq_len(J)) {
        gsum[[j]] <- gsum[[j]] + gam[[j]][2:ncat[j]]
      }
    }
  }
  # Reflection (and, with several factors, rotation) leaves the
  # likelihood and the N(0, I) prior unchanged; fix it after sampling.
  for (s in seq_len(n_samples)) {
    P <- matrix(keep_phi[s, , ], n, D)
    L <- matrix(keep_lam[s, , -1L], J, D)
    if (D == 1L) {
      Q <- matrix(if (L[1, 1] < 0) -1 else 1, 1, 1)
    } else {
      Q <- if (s == 1L) diag(D) else
        .morie_procrustes_rot(P, matrix(keep_phi[1L, , ], n, D))
    }
    keep_phi[s, , ] <- P %*% Q
    keep_lam[s, , -1L] <- L %*% Q
  }
  list(ideal_points = matrix(apply(keep_phi, c(2L, 3L), mean), n, D),
       ideal_sd = matrix(apply(keep_phi, c(2L, 3L), stats::sd), n, D),
       discrimination = matrix(apply(keep_lam[, , -1L, drop = FALSE],
                                     c(2L, 3L), mean), J, D),
       intercept = colMeans(keep_lam[, , 1L, drop = FALSE])[, 1L],
       cutpoints = lapply(gsum, function(g) g / n_samples),
       acceptance = acc / n_samples, n_samples = n_samples,
       engine = "native Gibbs with Cowles cutpoint steps (Quinn 2004)")
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
  N <- nrow(Y)
  K <- ncol(Y)
  per <- as.integer(factor(period))
  Tn <- max(per)
  obs <- !is.na(Y)
  .rmorie_local_seed(seed)
  # start: first principal component of the vote matrix, scaled
  Yc <- Y
  Yc[!obs] <- 0.5
  pc <- prcomp(Yc, center = TRUE)$x[, 1]
  pc <- if (stats::sd(pc) > 0) as.numeric(scale(pc)) else rep(0, N)
  if (is.null(anchor)) anchor <- which.max(pc)
  theta <- matrix(pc, N, Tn)
  alpha <- rep(0, K)
  beta <- rep(1, K)
  t2 <- rep(tau2, length.out = N)
  est_tau <- all(c0 > 0) && all(d0 > 0)
  keep <- seq.int(burn_in + thin, burn_in + n_samples * thin, by = thin)
  S_th <- S_th2 <- matrix(0, N, Tn)
  S_a <- S_b <- numeric(K)
  S_t2 <- numeric(N)
  m <- 0L
  lo <- ifelse(obs & Y == 1, 0, -Inf)
  hi <- ifelse(obs & Y == 0, 0, Inf)
  Z <- matrix(0, N, K)
  for (it in seq_len(burn_in + n_samples * thin)) {
    # (1) latent utilities, truncated by the observed vote
    mu <- sweep(theta[, per, drop = FALSE] * rep(beta, each = N), 2L, alpha)
    pl <- stats::pnorm(lo - mu)
    ph <- stats::pnorm(hi - mu)
    u <- stats::runif(N * K)
    Z[] <- mu + stats::qnorm(pmin(pmax(pl + u * (ph - pl), 1e-12), 1 - 1e-12))
    # (2) roll-call parameters: regress z_.k on (-1, theta_.t(k))
    for (k in seq_len(K)) {
      x <- theta[, per[k]]
      XtX <- matrix(c(N, -sum(x), -sum(x), sum(x^2)), 2) + diag(c(A0, B0))
      Xtz <- c(-sum(Z[, k]), sum(x * Z[, k])) + c(A0 * a0, B0 * b0)
      V <- solve(XtX)
      draw <- as.numeric(V %*% Xtz + t(chol(V)) %*% stats::rnorm(2))
      alpha[k] <- draw[1]
      beta[k] <- draw[2]
    }
    # (3) ideal-point paths by forward filtering, backward sampling
    prec_obs <- matrix(0, N, Tn)
    lin_obs <- matrix(0, N, Tn)
    for (t in seq_len(Tn)) {
      ks <- which(per == t)
      prec_obs[, t] <- sum(beta[ks]^2)
      lin_obs[, t] <- as.numeric((Z[, ks, drop = FALSE] + rep(alpha[ks], each = N)) %*% beta[ks])
    }
    mf <- Pf <- matrix(0, N, Tn)
    m_prev <- rep(e0, N)
    P_prev <- rep(E0, N)
    for (t in seq_len(Tn)) {
      Pp <- P_prev + t2   # theta_{j,0} ~ N(e0, E0); period 1 is one step of the walk on
      Pf[, t] <- 1 / (1 / Pp + prec_obs[, t])
      mf[, t] <- Pf[, t] * (m_prev / Pp + lin_obs[, t])
      m_prev <- mf[, t]
      P_prev <- Pf[, t]
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
    if (mean(theta[anchor, ]) < 0) {
      theta <- -theta
      beta <- -beta
    }
    if (it %in% keep) {
      S_th <- S_th + theta
      S_th2 <- S_th2 + theta^2
      S_a <- S_a + alpha
      S_b <- S_b + beta
      S_t2 <- S_t2 + t2
      m <- m + 1L
    }
  }
  th <- S_th / m
  list(theta = th, theta_sd = sqrt(pmax(S_th2 / m - th^2, 0)), alpha = S_a / m,
       beta = S_b / m, tau2 = S_t2 / m, periods = sort(unique(period)),
       n_samples = m, anchor = anchor)
}

# Alpha-NOMINATE (Carroll et al. 2013) slice-within-Gibbs sampler; see
# morie_spatial_voting_alpha_nominate.

# Vectorised slice sampler, Neal (2003) section 4 stepping-out and
# shrinkage, run for a vector of conditionally independent coordinates at
# once.  f maps a vector of candidates to a vector of log densities.
.morie_slice_vec <- function(f, x0, w, m = 3L, lower = -Inf, upper = Inf) {
  k <- length(x0)
  y <- f(x0) - stats::rexp(k)
  L <- x0 - w * stats::runif(k)
  R <- L + w
  L <- pmax(L, lower)
  R <- pmin(R, upper)
  J <- floor(m * stats::runif(k))
  K <- (m - 1L) - J
  repeat {
    go <- J > 0 & L > lower
    if (!any(go)) break
    go[go] <- y[go] < f(L)[go]
    if (!any(go)) break
    L[go] <- pmax(L[go] - w, lower)
    J[go] <- J[go] - 1
  }
  repeat {
    go <- K > 0 & R < upper
    if (!any(go)) break
    go[go] <- y[go] < f(R)[go]
    if (!any(go)) break
    R[go] <- pmin(R[go] + w, upper)
    K[go] <- K[go] - 1
  }
  x <- x0
  todo <- rep(TRUE, k)
  for (it in seq_len(200L)) {
    cand <- x0
    cand[todo] <- L[todo] + stats::runif(sum(todo)) * (R[todo] - L[todo])
    ok <- todo & f(cand) > y
    x[ok] <- cand[ok]
    todo <- todo & !ok
    if (!any(todo)) break
    lo <- todo & cand < x0
    hi <- todo & cand >= x0
    L[lo] <- cand[lo]
    R[hi] <- cand[hi]
  }
  x
}

# Squared distances between the rows of A (n x d) and B (m x d), n x m.
.morie_sqdist <- function(A, B) {
  out <- 0
  for (k in seq_len(ncol(A))) out <- out + outer(A[, k], B[, k], "-")^2
  out
}

# Log Pr(observed vote) cell by cell for alpha-NOMINATE, with the
# utility weight fixed at 0.5 as in Carroll et al. (2013).  Missing
# votes contribute zero.
.morie_anom_ll <- function(V, dY, dN, beta, alpha) {
  w2 <- 0.25
  quad <- -0.5 * beta * w2 * (dY - dN)
  nom <- beta * (exp(-0.5 * w2 * dY) - exp(-0.5 * w2 * dN))
  u <- quad + alpha * (nom - quad)
  ll <- stats::pnorm(ifelse(V == 1, u, -u), log.p = TRUE)
  ll[is.na(V)] <- 0
  ll
}

.morie_riwish <- function(v, S) {
  solve(stats::rWishart(1L, v, solve(S))[, , 1L])
}

# Orthogonal Procrustes rotation of X onto target T.
.morie_procrustes_rot <- function(X, T) {
  s <- svd(crossprod(X, T))
  s$u %*% t(s$v)
}

.morie_anom_gibbs <- function(V, n_dims, n_iter, burn_in, thin, polarity,
                              constrain) {
  n <- nrow(V)
  m <- ncol(V)
  d <- n_dims
  X <- matrix(stats::runif(n * d, -1, 1), n, d)
  for (k in seq_len(d)) X[polarity[k], k] <- abs(X[polarity[k], k])
  Y <- matrix(stats::runif(m * d, -1, 1), m, d)
  N <- matrix(stats::runif(m * d, -1, 1), m, d)
  beta <- 10
  alpha <- if (constrain) 1 else 0.7
  keep <- seq.int(burn_in + thin, n_iter, by = thin)
  S <- length(keep)
  dX <- array(NA_real_, c(S, n, d))
  dY <- dN <- array(NA_real_, c(S, m, d))
  dbeta <- dalpha <- numeric(S)
  quad <- function(Z, P) rowSums((Z %*% P) * Z)
  s <- 0L
  for (it in seq_len(n_iter)) {
    Sx <- .morie_riwish(n - 1, crossprod(X))
    Sy <- .morie_riwish(m - 1, crossprod(Y))
    Sn <- .morie_riwish(m - 1, crossprod(N))
    DN <- .morie_sqdist(X, N)
    for (k in seq_len(d)) {
      base <- .morie_sqdist(X, Y) - outer(X[, k], Y[, k], "-")^2
      f <- function(v) {
        Z <- Y
        Z[, k] <- v
        DY <- base + outer(X[, k], v, "-")^2
        colSums(.morie_anom_ll(V, DY, DN, beta, alpha)) - quad(Z, Sy) / 2
      }
      Y[, k] <- .morie_slice_vec(f, Y[, k], 8)
    }
    DY <- .morie_sqdist(X, Y)
    for (k in seq_len(d)) {
      base <- .morie_sqdist(X, N) - outer(X[, k], N[, k], "-")^2
      f <- function(v) {
        Z <- N
        Z[, k] <- v
        DNc <- base + outer(X[, k], v, "-")^2
        colSums(.morie_anom_ll(V, DY, DNc, beta, alpha)) - quad(Z, Sn) / 2
      }
      N[, k] <- .morie_slice_vec(f, N[, k], 8)
    }
    for (k in seq_len(d)) {
      bY <- .morie_sqdist(X, Y) - outer(X[, k], Y[, k], "-")^2
      bN <- .morie_sqdist(X, N) - outer(X[, k], N[, k], "-")^2
      f <- function(v) {
        Z <- X
        Z[, k] <- v
        DYc <- bY + outer(v, Y[, k], "-")^2
        DNc <- bN + outer(v, N[, k], "-")^2
        rowSums(.morie_anom_ll(V, DYc, DNc, beta, alpha)) - quad(Z, Sx) / 2
      }
      X[, k] <- .morie_slice_vec(f, X[, k], 8)
    }
    DY <- .morie_sqdist(X, Y)
    DN <- .morie_sqdist(X, N)
    beta <- .morie_slice_vec(function(b) {
      vapply(b, function(bb) sum(.morie_anom_ll(V, DY, DN, bb, alpha)), 0)
    }, beta, 8, lower = 0)
    if (!constrain) {
      alpha <- .morie_slice_vec(function(a) {
        vapply(a, function(aa) sum(.morie_anom_ll(V, DY, DN, beta, aa)), 0)
      }, alpha, 8, lower = 0, upper = 1)
    }
    if (it %in% keep) {
      s <- s + 1L
      dX[s, , ] <- X
      dY[s, , ] <- Y
      dN[s, , ] <- N
      dbeta[s] <- beta
      dalpha[s] <- alpha
    }
  }
  list(X = dX, Y = dY, N = dN, beta = dbeta, alpha = dalpha)
}
