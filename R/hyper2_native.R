# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of hyper2 -- Gaussian-process hyperparameters by MCMC with the
# latent function integrated out. Mirrors src/morie/fn/hyper2.py
# operation for operation, on the shared numerics in
# R/aaa_helpers_w3num.R and the generator in R/aaa_helpers_ghc_rng.R.
#
# Optimising a kernel's hyperparameters and then predicting as if those
# values were known throws away the part of the uncertainty that usually
# matters most. The lengthscale is not well determined by a few dozen
# points, and a predictive interval computed at its maximiser is
# narrower than the data supports. The fix is to SAMPLE the
# hyperparameters and average the predictions over the sample.
#
# That is easy for a Gaussian likelihood and awkward otherwise, and the
# awkwardness is the subject of Murray and Adams (2010): the latent
# function f and the hyperparameters theta are strongly coupled a
# posteriori, because theta controls the prior covariance of f. A
# sampler that moves theta while holding f fixed proposes a function
# wildly improbable under the new kernel, and every such move is
# rejected. Their answer is to reparameterise so the coupling breaks.
# Three routes, targeting the SAME posterior -- which is what the parity
# harness anchors on.
#
#   "marginal"   For a Gaussian likelihood f integrates out in closed
#                form: y ~ N(0, K_theta + s2n I). No latent variable to
#                couple to, and theta comes from its exact marginal
#                posterior. Correct, cheapest, and the reference the
#                other two are checked against.
#
#   "whitened"   Murray and Adams' ancillary augmentation. Write
#                f = L_theta nu with L_theta the Cholesky factor and
#                nu ~ N(0, I). nu carries no dependence on theta, so
#                moving theta with nu fixed drags f along coherently
#                instead of stranding it. nu is updated by elliptical
#                slice sampling, which needs no step size because its
#                proposal is an exact ellipse through the prior.
#
#   "surrogate"  Their surrogate data method. Draw g ~ N(f, S), treat g
#                as fixed, and reparameterise f around the conditional
#                mean and covariance it induces. The surrogate is a
#                noisy summary of f that theta must stay consistent
#                with -- a weaker tie than f itself, so theta can move
#                further. S is the likelihood's own noise, the paper's
#                default choice.
#
# Hyperparameters are slice sampled one coordinate at a time on the log
# scale (Neal 2003: stepping out, then shrinkage). Slice sampling rather
# than Metropolis because it has no step size to tune -- the stepping
# out finds the scale itself, which matters when the same code must work
# for a lengthscale and a noise variance without being retuned.
#
# Kernels, all selectable: squared exponential, Matern 3/2, Matern 5/2.
# The Materns are there because the squared exponential assumes an
# infinitely differentiable function, which is a very strong statement
# about a real process and is almost never checked.
#
# References
#   Murray, I. and Adams, R.P. (2010) "Slice sampling covariance
#     hyperparameters of latent Gaussian models." NIPS 23, 1732-1740.
#   Murray, I., Adams, R.P. and MacKay, D.J.C. (2010) "Elliptical slice
#     sampling." AISTATS 9, 541-548.
#   Neal, R.M. (2003) "Slice sampling." Annals of Statistics 31(3),
#     705-767.
#   Rasmussen, C.E. and Williams, C.K.I. (2006) "Gaussian Processes for
#     Machine Learning." MIT Press, chapters 2 and 4.

.HYPER2_KERNELS <- c("squared_exponential", "matern32", "matern52")
.HYPER2_ROUTES <- c("marginal", "whitened", "surrogate")

#' .hyper2_dist
#'
#' A step of the hyper2_native implementation. Called by \code{morie_hyper2_kernel}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a Numeric; combined arithmetically in the body.
#' @param b Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.hyper2_dist <- function(a, b) sqrt(.w3_csum((a - b) * (a - b)))

#' Covariance between two sets of inputs
#'
#' @param X First input matrix.
#' @param Z Second input matrix.
#' @param log_ls log lengthscale.
#' @param log_sf log signal standard deviation.
#' @param kind A member of the kernel list.
#' @return The covariance matrix.
#' @export
morie_hyper2_kernel <- function(X, Z, log_ls, log_sf,
                                kind = "squared_exponential") {
  if (!(kind %in% .HYPER2_KERNELS))
    stop("kind must be one of ", paste(.HYPER2_KERNELS, collapse = ", "))
  ls <- exp(log_ls)
  sf2 <- exp(2 * log_sf)
  nx <- nrow(X); nz <- nrow(Z)
  out <- matrix(0, nx, nz)
  for (i in seq_len(nx)) for (j in seq_len(nz)) {
    r <- .hyper2_dist(X[i, ], Z[j, ]) / ls
    v <- if (kind == "squared_exponential") exp(-0.5 * r * r)
    else if (kind == "matern32") {
      s <- sqrt(3) * r
      (1 + s) * exp(-s)
    } else {
      s <- sqrt(5) * r
      (1 + s + s * s / 3) * exp(-s)
    }
    out[i, j] <- sf2 * v
  }
  out
}

#' .hyper2_jit
#'
#' A step of the hyper2_native implementation. Called by \code{morie_hyper2}, \code{morie_hyper2_logml}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param K A matrix; indexed by row and column.
#' @param v Numeric; combined arithmetically in the body.
#' @return The value of \code{K}, as built in the body.
#' @export
.hyper2_jit <- function(K, v) {
  n <- nrow(K)
  for (i in seq_len(n)) K[i, i] <- K[i, i] + v
  K
}

#' log N(y; 0, K + s2n I), by Cholesky
#'
#' The form that never builds an inverse: -0.5 y' A^-1 y minus the sum
#' of the log diagonal of the factor minus n/2 log(2 pi).
#'
#' @param y Targets.
#' @param X Inputs.
#' @param log_ls log lengthscale.
#' @param log_sf log signal standard deviation.
#' @param log_sn log noise standard deviation.
#' @param kind Kernel name.
#' @return The log marginal likelihood.
#' @export
morie_hyper2_logml <- function(y, X, log_ls, log_sf, log_sn, kind) {
  n <- length(y)
  K <- .hyper2_jit(morie_hyper2_kernel(X, X, log_ls, log_sf, kind),
                   exp(2 * log_sn) + 1e-10)
  L <- .w3_chol(K)
  a <- .w3_solve_chol(L, y)
  quad <- .w3_dot(y, a)
  logdet <- 2 * .w3_csum(vapply(seq_len(n), function(i) log(L[i, i]),
                                numeric(1)))
  -0.5 * quad - 0.5 * logdet - 0.5 * n * log(2 * pi)
}

# Standard normal on each log-hyperparameter. A lognormal prior, which
# is proper -- an improper flat prior on a log lengthscale gives an
# improper posterior whenever the data cannot rule out an arbitrarily
# long one, and a sampler will wander off into that region without ever
# saying so.
#' Standard normal on each log-hyperparameter. A lognormal prior, which
#'
#' is proper -- an improper flat prior on a log lengthscale gives an
#' improper posterior whenever the data cannot rule out an arbitrarily
#' long one, and a sampler will wander off into that region without ever
#' saying so.
#'
#' @param theta Numeric; combined arithmetically in the body.
#' @return The value of \code{.w3_csum}.
#' @export
.hyper2_logprior <- function(theta)
  .w3_csum(-0.5 * theta * theta - 0.5 * log(2 * pi))

#' Neal's univariate slice sampler: stepping out, then shrinkage
#'
#' No step size to tune: w only sets the unit the stepping-out uses, and
#' the procedure is correct for any positive value of it.
#'
#' @param logf The log target.
#' @param x0 Current value.
#' @param e A generator environment from .ghc_rng.
#' @param w Stepping-out width.
#' @param m Maximum stepping-out steps.
#' @return The next value.
#' @export
morie_hyper2_slice <- function(logf, x0, e, w = 1, m = 10L) {
  ly <- logf(x0) + log(.ghc_unif(e, 1L))
  u <- .ghc_unif(e, 1L)
  lo <- x0 - w * u
  hi <- lo + w
  j <- floor(.ghc_unif(e, 1L) * m)
  k <- m - 1 - j
  while (j > 0 && ly < logf(lo)) { lo <- lo - w; j <- j - 1 }
  while (k > 0 && ly < logf(hi)) { hi <- hi + w; k <- k - 1 }
  for (t in seq_len(200L)) {
    x1 <- lo + .ghc_unif(e, 1L) * (hi - lo)
    if (ly < logf(x1)) return(x1)
    if (x1 < x0) lo <- x1 else hi <- x1
  }
  x0
}

# Elliptical slice sampling for a latent with prior N(0, L L'). The
# proposal is an exact ellipse through the current point and a fresh
# prior draw, so the prior term cancels and only the likelihood enters
# the acceptance test. No rejection and no step size.
#' Elliptical slice sampling for a latent with prior N(0, L L\'). The
#'
#' proposal is an exact ellipse through the current point and a fresh
#' prior draw, so the prior term cancels and only the likelihood enters
#' the acceptance test. No rejection and no step size.
#'
#' @param logl Accepted by the signature and not used anywhere in the body.
#' @param f A vector; its length is taken.
#' @param L A matrix; indexed by row and column.
#' @param e Passed to \code{.ghc_norm}.
#' @return The value of \code{f}, as built in the body.
#' @export
.hyper2_elliptical <- function(logl, f, L, e) {
  n <- length(f)
  nu <- vapply(seq_len(n), function(i) .ghc_norm(e, 1L), numeric(1))
  v <- vapply(seq_len(n), function(i)
    .w3_dot(L[i, seq_len(i)], nu[seq_len(i)]), numeric(1))
  ly <- logl(f) + log(.ghc_unif(e, 1L))
  a <- 2 * pi * .ghc_unif(e, 1L)
  amin <- a - 2 * pi
  amax <- a
  for (t in seq_len(200L)) {
    ca <- cos(a); sa <- sin(a)
    fp <- f * ca + v * sa
    if (logl(fp) > ly) return(fp)
    if (a > 0) amax <- a else amin <- a
    a <- amin + .ghc_unif(e, 1L) * (amax - amin)
  }
  f
}

#' Posterior over GP hyperparameters, predictions averaged over it
#'
#' @param X Training inputs.
#' @param y Training targets.
#' @param prior Starting values for the three log-hyperparameters. The
#'   prior itself is standard normal on each; this argument only sets
#'   where the chain begins, and is named prior because the ledger's
#'   signature calls it that.
#' @param kind Kernel name.
#' @param route "marginal", "whitened" or "surrogate".
#' @param n_iter Chain length.
#' @param burn Burn-in; half of n_iter by default.
#' @param thin Thinning.
#' @param seed Seed for the generator shared with the Python arm.
#' @param Xstar Test inputs; the training inputs by default.
#' @param w Slice-sampling stepping-out width.
#' @param m Slice-sampling step limit.
#' @param jitter Added to the diagonal for positive-definiteness.
#' @return A list with the hyperparameter draws, their posterior means
#'   and standard deviations, and the predictive mean and standard
#'   deviation at Xstar averaged over the draws.
#' @export
morie_hyper2 <- function(X, y, prior = NULL, kind = "squared_exponential",
                         route = "marginal", n_iter = 200L, burn = NULL,
                         thin = 1L, seed = 1, Xstar = NULL, w = 1,
                         m = 10L, jitter = 1e-8) {
  if (!(kind %in% .HYPER2_KERNELS))
    stop("kind must be one of ", paste(.HYPER2_KERNELS, collapse = ", "))
  if (!(route %in% .HYPER2_ROUTES))
    stop("route must be one of ", paste(.HYPER2_ROUTES, collapse = ", "))
  yv <- as.numeric(y)
  n <- length(yv)
  Xv <- matrix(as.numeric(as.matrix(X)), nrow = n)
  if (nrow(Xv) != n) stop("X and y must have the same length")
  if (n < 3L) stop("need at least three points")
  n_iter <- as.integer(n_iter)
  if (is.null(burn)) burn <- n_iter %/% 2L
  burn <- as.integer(burn); thin <- as.integer(thin)
  theta <- if (is.null(prior)) c(0, 0, -1) else as.numeric(prior)
  if (length(theta) != 3L)
    stop("prior must hold three starting log values")
  Xs <- if (is.null(Xstar)) Xv else
    matrix(as.numeric(as.matrix(Xstar)), ncol = ncol(Xv))

  e <- .ghc_rng(seed)
  f <- numeric(n)
  nu <- numeric(n)

  chol_of <- function(th)
    .w3_chol(.hyper2_jit(morie_hyper2_kernel(Xv, Xv, th[1], th[2], kind),
                         jitter))

  loglik <- function(fv, sn) {
    s2 <- exp(2 * sn)
    .w3_csum(-0.5 * (yv - fv) * (yv - fv) / s2 - 0.5 * log(2 * pi * s2))
  }

  Lmul <- function(L, v)
    vapply(seq_len(n), function(i) .w3_dot(L[i, seq_len(i)], v[seq_len(i)]),
           numeric(1))

  if (route != "marginal") {
    L <- chol_of(theta)
    nu <- vapply(seq_len(n), function(i) .ghc_norm(e, 1L), numeric(1))
    f <- Lmul(L, nu)
  }

  draws <- list()
  lml <- numeric(0)

  post <- function(th, g) {
    Kt <- .hyper2_jit(morie_hyper2_kernel(Xv, Xv, th[1], th[2], kind), jitter)
    A <- .hyper2_jit(Kt, exp(2 * th[3]))
    La <- .w3_chol(A)
    sol <- .w3_solve_chol(La, g)
    mvec <- vapply(seq_len(n), function(i) .w3_dot(Kt[i, ], sol), numeric(1))
    cols <- lapply(seq_len(n), function(j) .w3_solve_chol(La, Kt[, j]))
    cov <- matrix(0, n, n)
    for (i in seq_len(n)) for (j in seq_len(n))
      cov[i, j] <- Kt[i, j] - .w3_dot(Kt[i, ], cols[[j]])
    cov <- .hyper2_jit(cov, jitter)
    list(m = mvec, R = .w3_chol(cov), La = La)
  }

  for (it in seq_len(n_iter)) {
    if (route == "marginal") {
      for (cix in 1:3) {
        target <- local({
          cc <- cix
          function(v) {
            th <- theta; th[cc] <- v
            morie_hyper2_logml(yv, Xv, th[1], th[2], th[3], kind) +
              .hyper2_logprior(th)
          }
        })
        theta[cix] <- morie_hyper2_slice(target, theta[cix], e, w, m)
      }
      cur <- morie_hyper2_logml(yv, Xv, theta[1], theta[2], theta[3], kind)
    } else if (route == "whitened") {
      # theta moves with nu held fixed, so f = L(theta) nu follows the
      # kernel instead of being stranded by it.
      for (cix in 1:3) {
        target <- local({
          cc <- cix
          function(v) {
            th <- theta; th[cc] <- v
            Lt <- chol_of(th)
            ft <- Lmul(Lt, nu)
            loglik(ft, th[3]) + .hyper2_logprior(th)
          }
        })
        theta[cix] <- morie_hyper2_slice(target, theta[cix], e, w, m)
      }
      L <- chol_of(theta)
      f <- Lmul(L, nu)
      f <- .hyper2_elliptical(function(fv) loglik(fv, theta[3]), f, L, e)
      # Recover nu from the new f so the next theta move is consistent:
      # nu = L^-1 f, by forward substitution.
      nu <- numeric(n)
      for (i in seq_len(n)) {
        acc <- if (i > 1L) .w3_dot(L[i, seq_len(i - 1L)], nu[seq_len(i - 1L)]) else 0
        nu[i] <- (f[i] - acc) / L[i, i]
      }
      cur <- loglik(f, theta[3]) + .hyper2_logprior(theta)
    } else {
      # Surrogate data: g is a noisy view of f that theta must stay
      # consistent with. S is the likelihood's own noise, the paper's
      # default choice.
      s2 <- exp(2 * theta[3])
      g <- f + sqrt(s2) * vapply(seq_len(n), function(i) .ghc_norm(e, 1L),
                                 numeric(1))
      pp <- post(theta, g)
      eta <- numeric(n)
      for (i in seq_len(n)) {
        acc <- if (i > 1L) .w3_dot(pp$R[i, seq_len(i - 1L)], eta[seq_len(i - 1L)]) else 0
        eta[i] <- (f[i] - pp$m[i] - acc) / pp$R[i, i]
      }
      for (cix in 1:3) {
        target <- local({
          cc <- cix
          function(v) {
            th <- theta; th[cc] <- v
            q <- post(th, g)
            ft <- vapply(seq_len(n), function(i)
              q$m[i] + .w3_dot(q$R[i, seq_len(i)], eta[seq_len(i)]), numeric(1))
            sol <- .w3_solve_chol(q$La, g)
            logdet <- 2 * .w3_csum(vapply(seq_len(n), function(i)
              log(q$La[i, i]), numeric(1)))
            lg <- -0.5 * .w3_dot(g, sol) - 0.5 * logdet - 0.5 * n * log(2 * pi)
            loglik(ft, th[3]) + lg + .hyper2_logprior(th)
          }
        })
        theta[cix] <- morie_hyper2_slice(target, theta[cix], e, w, m)
      }
      pp <- post(theta, g)
      f <- vapply(seq_len(n), function(i)
        pp$m[i] + .w3_dot(pp$R[i, seq_len(i)], eta[seq_len(i)]), numeric(1))
      L <- chol_of(theta)
      f <- .hyper2_elliptical(function(fv) loglik(fv, theta[3]), f, L, e)
      cur <- loglik(f, theta[3]) + .hyper2_logprior(theta)
    }

    if (it - 1L >= burn && (it - 1L - burn) %% thin == 0L) {
      draws[[length(draws) + 1L]] <- theta
      lml <- c(lml, cur)
    }
  }

  if (!length(draws)) stop("burn-in consumed every sweep")

  # Predictions averaged over the hyperparameter draws. This is the
  # "integrate out" the module is for: the predictive variance picks up
  # the spread of the means across draws as well as each draw's own
  # variance, which a plug-in estimate at the maximiser cannot.
  ns <- nrow(Xs)
  pm <- numeric(ns); pv <- numeric(ns); pm2 <- numeric(ns)
  for (th in draws) {
    Kxx <- .hyper2_jit(morie_hyper2_kernel(Xv, Xv, th[1], th[2], kind),
                       exp(2 * th[3]) + jitter)
    Lx <- .w3_chol(Kxx)
    alpha <- .w3_solve_chol(Lx, yv)
    Ksx <- morie_hyper2_kernel(Xs, Xv, th[1], th[2], kind)
    Kss <- morie_hyper2_kernel(Xs, Xs, th[1], th[2], kind)
    for (j in seq_len(ns)) {
      mj <- .w3_dot(Ksx[j, ], alpha)
      vj <- .w3_solve_chol(Lx, Ksx[j, ])
      sj <- Kss[j, j] - .w3_dot(Ksx[j, ], vj)
      if (sj < 0) sj <- 0
      pm[j] <- pm[j] + mj
      pm2[j] <- pm2[j] + mj * mj
      pv[j] <- pv[j] + sj
    }
  }
  M <- length(draws)
  mean_ <- pm / M
  var_ <- pv / M + pm2 / M - mean_ * mean_
  sd_ <- sqrt(ifelse(var_ > 0, var_, 0))

  means <- vapply(1:3, function(cix)
    .w3_csum(vapply(draws, function(d) d[cix], numeric(1))) / M, numeric(1))
  sds <- vapply(1:3, function(cix) {
    if (M > 1L) {
      v <- vapply(draws, function(d) d[cix], numeric(1))
      sqrt(.w3_csum((v - means[cix]) * (v - means[cix])) / (M - 1))
    } else 0
  }, numeric(1))

  list(draws = draws, log_lengthscale = means[1], log_signal_sd = means[2],
       log_noise_sd = means[3], lengthscale = exp(means[1]),
       signal_sd = exp(means[2]), noise_sd = exp(means[3]),
       posterior_sd = sds, log_target = lml,
       mean_log_target = .w3_csum(lml) / M,
       predict_mean = mean_, predict_sd = sd_, n = n, n_test = ns,
       kept = M, kind = kind, route = route, seed = as.integer(seed),
       estimate = means[1],
       method = "GP hyperparameter MCMC with the latent integrated out")
}

#' One-line summary of the hyper2 module
#'
#' @return A character scalar.
#' @export
morie_hyper2_cheatsheet <- function()
  paste0("hyper2: GP hyperparameter MCMC. kernels ",
         paste(.HYPER2_KERNELS, collapse = ", "), "; routes ",
         paste(.HYPER2_ROUTES, collapse = ", "))
