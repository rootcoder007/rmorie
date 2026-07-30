# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Differential-privacy shelf, part 3: the private learners. R mirrors of
# dpsgd, dpadam, dpkmn, dplog, dpsyn, dpchpr, dpfed and dpgan.
#
# Every one of these clips first and adds noise second, and the order is
# the whole guarantee. Clipping an already-averaged gradient bounds
# nothing about any single record's influence, which is why dp_sgd
# refuses an averaged gradient outright rather than accepting it and
# reporting an epsilon it cannot deliver.

#' One DP-SGD step
#'
#' Per-example gradients are clipped to L2 norm C, summed, Gaussian
#' noise of scale \eqn{\sigma C} added to the SUM, and the result divided
#' by the batch size.
#'
#' The per-example clipping is not a detail. Clipping an averaged
#' gradient provides NO privacy: the average has already mixed every
#' record together, and bounding its norm says nothing about how much
#' any one record moved it. This function requires a (B, p) matrix and
#' errors on a vector, because accepting one would mean returning an
#' epsilon the mechanism does not satisfy.
#'
#' @param grads per-example gradient matrix, shape (B, p).
#' @param C L2 clipping norm.
#' @param sigma noise multiplier; the noise sd on the sum is
#'   \code{sigma * C}.
#' @param lr learning rate.
#' @param theta optional current parameters; the updated vector is
#'   returned when supplied.
#' @param seed optional integer seed.
#' @return list with \code{update}, \code{private_gradient},
#'   \code{clipped_fraction}, \code{noise_sd}, and \code{theta} when
#'   given.
#' @references Abadi, M. et al. (2016). Deep learning with differential
#'   privacy. \emph{CCS}, 308-318.
#' @examples
#' set.seed(1)
#' morie_dp_sgd(matrix(rnorm(80), ncol = 4), C = 1, sigma = 1)$noise_sd
#' @export
morie_dp_sgd <- function(grads, C = 1, sigma = 1, lr = 0.1, theta = NULL,
                         seed = NULL) {
  C <- as.numeric(C)[1L]
  if (C <= 0) stop("C must be positive", call. = FALSE)
  if (sigma < 0) stop("sigma must be non-negative", call. = FALSE)
  if (is.null(dim(grads)) || length(dim(grads)) != 2L) {
    stop(paste("grads must be per-example, shape (B, p); clipping an",
               "averaged gradient provides no privacy"), call. = FALSE)
  }
  G <- as.matrix(grads)
  storage.mode(G) <- "double"
  B <- nrow(G)
  p <- ncol(G)
  norms <- sqrt(rowSums(G^2))
  Gc <- G * pmin(1, C / pmax(norms, 1e-12))
  old <- .morie_dp_seed(seed)
  on.exit(.morie_dp_unseed(old), add = TRUE)
  noise <- if (sigma > 0) stats::rnorm(p, 0, sigma * C) else numeric(p)
  gbar <- (colSums(Gc) + noise) / B
  update <- -lr * gbar
  out <- list(update = update, private_gradient = gbar,
              clipped_fraction = mean(norms > C), noise_sd = sigma * C,
              C = C, sigma = as.numeric(sigma), batch_size = B,
              warnings = paste("this accounts for ONE step; compose across",
                               "steps with morie_renyi_dp_composition, and",
                               "apply subsampling amplification"),
              method = "dp_sgd")
  if (!is.null(theta)) {
    th <- as.numeric(theta)
    if (length(th) != p) {
      stop(sprintf("theta has %d entries but gradients have %d",
                   length(th), p), call. = FALSE)
    }
    out$theta <- th + update
  }
  out
}


#' One DP-Adam step
#'
#' Adam's moment updates applied to the privatised gradient. The moments
#' are post-processing and cost nothing extra.
#'
#' They can still hurt. Adam's second moment estimates the gradient
#' scale, and under heavy DP noise what it actually tracks is the NOISE
#' scale -- the adaptive step then shrinks in the directions where the
#' noise happens to be large rather than where the signal is small.
#' \code{signal_to_noise} below 1 is the regime where plain DP-SGD is
#' often the better optimiser, and the function says so.
#'
#' @inheritParams morie_dp_sgd
#' @param betas moment decay rates.
#' @param eps denominator stabiliser.
#' @param state optional state list from a previous call.
#' @return list with \code{update}, \code{state}, \code{m}, \code{v},
#'   \code{signal_to_noise}.
#' @references Kingma, D. P. and Ba, J. (2015). Adam: a method for
#'   stochastic optimization. \emph{ICLR}. Abadi, M. et al. (2016).
#'   \emph{CCS}, 308-318.
#' @examples
#' set.seed(1)
#' morie_dp_adam(matrix(rnorm(80), ncol = 4), C = 1, sigma = 0.1)$t
#' @export
morie_dp_adam <- function(grads, C = 1, sigma = 1, lr = 1e-3,
                          betas = c(0.9, 0.999), eps = 1e-8, state = NULL,
                          seed = NULL) {
  step <- morie_dp_sgd(grads, C = C, sigma = sigma, lr = 1, seed = seed)
  g <- step$private_gradient
  b1 <- as.numeric(betas[1L])
  b2 <- as.numeric(betas[2L])
  st <- .morie_opt_state(state, length(g), keys = c("m", "v"))
  t <- st$t
  st$m <- b1 * st$m + (1 - b1) * g
  st$v <- b2 * st$v + (1 - b2) * g^2
  m_hat <- st$m / (1 - b1^t)
  v_hat <- st$v / (1 - b2^t)
  update <- -lr * m_hat / (sqrt(v_hat) + eps)
  B <- step$batch_size
  noise_sd <- (sigma * C) / max(B, 1)
  snr <- sqrt(sum(g^2)) / max(noise_sd * sqrt(length(g)), 1e-300)
  list(update = update, state = st, m = st$m, v = st$v,
       private_gradient = g, signal_to_noise = snr,
       clipped_fraction = step$clipped_fraction, noise_sd = noise_sd,
       t = as.integer(t),
       warnings = if (snr < 1) {
         paste("signal-to-noise is below 1, so Adam's second moment is",
               "tracking DP noise rather than gradient scale; plain DP-SGD",
               "is often better in this regime")
       } else {
         character(0)
       },
       method = "dp_adam")
}


#' Differentially private k-means
#'
#' Lloyd iterations where each centre update uses a noisy count and a
#' noisy sum.
#'
#' The budget splits across ITERATIONS as well as across the count and
#' sum queries, so each pass is cheaper and noisier than the last one
#' would like. More iterations therefore make the result WORSE, which
#' inverts the usual intuition: keep \code{n_iter} small. A noisy count
#' below 1 means the cluster cannot be located at all and the centre is
#' reinitialised, which is reported.
#'
#' @param X data matrix.
#' @param k number of clusters.
#' @param epsilon total budget across all iterations.
#' @param n_iter Lloyd iterations.
#' @param bounds \code{c(lo, hi)} for clipping, from outside the data.
#' @param seed optional integer seed.
#' @return list with \code{centers}, \code{labels},
#'   \code{epsilon_per_iteration}, \code{n_reinitialised},
#'   \code{inertia}.
#' @references Su, D. et al. (2016). Differentially private k-means
#'   clustering. \emph{CODASPY}, 26-37.
#' @examples
#' set.seed(1)
#' r <- morie_dp_kmeans(matrix(rnorm(200), ncol = 2), k = 2, epsilon = 5,
#'                      bounds = c(-4, 4))
#' dim(r$centers)
#' @export
morie_dp_kmeans <- function(X, k = 3, epsilon = 1, n_iter = 5, bounds = NULL,
                            seed = NULL) {
  eps <- .morie_dp_check_budget(epsilon)$epsilon
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  n <- nrow(X)
  p <- ncol(X)
  k <- as.integer(k)
  if (k < 1L) stop("k must be at least 1", call. = FALSE)
  n_iter <- as.integer(n_iter)
  if (n_iter < 1L) stop("n_iter must be at least 1", call. = FALSE)
  warn <- character(0)
  if (is.null(bounds)) {
    lo <- min(X)
    hi <- max(X)
    warn <- paste("bounds were taken from the data, which is a non-private",
                  "query; supply `bounds` from outside the data")
  } else {
    lo <- as.numeric(bounds[1L])
    hi <- as.numeric(bounds[2L])
  }
  cl <- .morie_dp_clip(X, lo, hi)
  Xc <- matrix(cl$x, n, p)
  lo <- cl$a
  hi <- cl$b
  eps_iter <- eps / (2 * n_iter)
  old <- .morie_dp_seed(seed)
  on.exit(.morie_dp_unseed(old), add = TRUE)
  centers <- Xc[sample.int(n, k, replace = FALSE), , drop = FALSE]
  reinit <- 0L
  labels <- integer(n)
  assign_labels <- function(cen) {
    d2 <- matrix(0, n, nrow(cen))
    for (j in seq_len(nrow(cen))) {
      d2[, j] <- rowSums(sweep(Xc, 2L, cen[j, ], "-")^2)
    }
    list(d2 = d2, labels = max.col(-d2, ties.method = "first"))
  }
  for (it in seq_len(n_iter)) {
    a <- assign_labels(centers)
    labels <- a$labels
    for (j in seq_len(k)) {
      m <- labels == j
      noisy_count <- sum(m) + .morie_dp_rlaplace(1L, 1 / eps_iter)
      base_sum <- if (any(m)) {
        colSums(Xc[m, , drop = FALSE])
      } else {
        numeric(p)
      }
      noisy_sum <- base_sum + .morie_dp_rlaplace(p, (hi - lo) / eps_iter)
      if (noisy_count < 1) {
        centers[j, ] <- Xc[sample.int(n, 1L), ]
        reinit <- reinit + 1L
      } else {
        centers[j, ] <- noisy_sum / noisy_count
      }
    }
  }
  a <- assign_labels(centers)
  labels <- a$labels
  list(centers = centers, labels = labels - 1L,
       epsilon_per_iteration = eps_iter, n_reinitialised = reinit,
       inertia = sum(a$d2[cbind(seq_len(n), labels)]), epsilon = eps, k = k,
       n_iter = n_iter, bounds = c(lo, hi),
       warnings = c(warn, if (reinit > 0L) {
         paste("clusters were reinitialised from noisy counts below 1; the",
               "budget is thin for this k")
       }),
       method = "dp_kmeans")
}


#' Differentially private logistic regression
#'
#' Objective perturbation (a random linear term added to the penalised
#' objective) or output perturbation (noise on the fitted coefficients).
#'
#' Objective perturbation is the better mechanism at equal epsilon: the
#' noise enters before the optimisation, so the fit adapts to it instead
#' of carrying it. Note what the regulariser buys -- strong convexity is
#' what BOUNDS the sensitivity in the first place, so \code{lam} is not
#' a tuning knob here but part of the guarantee, and lam = 0 has no
#' guarantee at all.
#'
#' @param X design matrix.
#' @param y 0/1 response.
#' @param epsilon privacy budget.
#' @param method \code{"objective"} or \code{"output"}.
#' @param lam L2 regularisation; must be positive for objective
#'   perturbation.
#' @param C row-norm clipping bound.
#' @param n_iter,lr gradient-descent controls.
#' @param seed optional integer seed.
#' @return list with \code{beta}, \code{prob}, \code{accuracy},
#'   \code{clipped_fraction}.
#' @references Chaudhuri, K., Monteleoni, C. and Sarwate, A. D. (2011).
#'   Differentially private empirical risk minimization. \emph{JMLR},
#'   12, 1069-1109.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(400), ncol = 2)
#' y <- as.numeric(runif(200) < 1 / (1 + exp(-(X %*% c(1, -1)))))
#' morie_dp_logistic(X, y, epsilon = 4)$accuracy > 0.4
#' @export
morie_dp_logistic <- function(X, y, epsilon = 1, method = c("objective",
                                                            "output"),
                              lam = 0.01, C = 1, n_iter = 100, lr = 0.1,
                              seed = NULL) {
  method <- match.arg(method)
  eps <- .morie_dp_check_budget(epsilon)$epsilon
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  y <- as.numeric(y)
  if (nrow(X) != length(y)) {
    stop(sprintf("X has %d rows but y has %d", nrow(X), length(y)),
         call. = FALSE)
  }
  if (!all(y == 0 | y == 1)) stop("y must be 0/1", call. = FALSE)
  if (identical(method, "objective") && lam <= 0) {
    stop("objective perturbation requires lam > 0 for strong convexity",
         call. = FALSE)
  }
  n <- length(y)
  norms <- sqrt(rowSums(X^2))
  Xc <- X * pmin(1, C / pmax(norms, 1e-12))
  p <- ncol(Xc)
  old <- .morie_dp_seed(seed)
  on.exit(.morie_dp_unseed(old), add = TRUE)
  b <- numeric(p)
  if (identical(method, "objective")) {
    scale <- 2 * C / (n * eps)
    direction <- stats::rnorm(p)
    direction <- direction / max(sqrt(sum(direction^2)), 1e-12)
    b <- direction * stats::rgamma(1L, shape = p, scale = scale)
  }
  beta <- numeric(p)
  for (it in seq_len(as.integer(n_iter))) {
    mu <- 1 / (1 + exp(-pmax(pmin(as.vector(Xc %*% beta), 500), -500)))
    grad <- as.vector(crossprod(Xc, mu - y)) / n + lam * beta + b / n
    beta <- beta - lr * grad
  }
  if (identical(method, "output")) {
    beta <- beta + .morie_dp_rlaplace(p, 2 * C / (n * lam * eps))
  }
  prob <- 1 / (1 + exp(-pmax(pmin(as.vector(Xc %*% beta), 500), -500)))
  list(beta = beta, prob = prob, accuracy = mean((prob >= 0.5) == y),
       clipped_fraction = mean(norms > C), method_used = method,
       epsilon = eps, lam = as.numeric(lam), C = as.numeric(C), n = n,
       method = "dp_logistic")
}


#' Differentially private synthetic data
#'
#' Independent noisy histograms per feature, resampled.
#'
#' This preserves MARGINALS ONLY. Every inter-feature correlation is
#' destroyed by construction, because each column is drawn independently
#' -- so a regression, an interaction, or any multivariate model fitted
#' to the output will be wrong while looking entirely plausible. The
#' returned \code{correlation_real} and \code{correlation_synthetic} are
#' there to make that visible rather than discoverable later.
#'
#' @param X data matrix.
#' @param epsilon privacy budget (parallel across bins, split across
#'   features by composition).
#' @param n_synth rows to generate; defaults to \code{nrow(X)}.
#' @param bins histogram bins per feature.
#' @param bounds \code{c(lo, hi)} from outside the data.
#' @param seed optional integer seed.
#' @return list with \code{synthetic}, \code{marginal_error},
#'   \code{correlation_real}, \code{correlation_synthetic},
#'   \code{preserved}, \code{destroyed}.
#' @references Zhang, J. et al. (2017). PrivBayes: private data release
#'   via Bayesian networks. \emph{ACM TODS}, 42(4), 1-41.
#' @examples
#' set.seed(1)
#' r <- morie_dp_synthetic_data(matrix(rnorm(200), ncol = 2), epsilon = 4,
#'                              bounds = c(-4, 4))
#' dim(r$synthetic)
#' @export
morie_dp_synthetic_data <- function(X, epsilon = 1, n_synth = NULL, bins = 10,
                                    bounds = NULL, seed = NULL) {
  eps <- .morie_dp_check_budget(epsilon)$epsilon
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  n <- nrow(X)
  p <- ncol(X)
  n_synth <- as.integer(if (is.null(n_synth)) n else n_synth)
  warn <- character(0)
  if (is.null(bounds)) {
    lo <- min(X)
    hi <- max(X)
    warn <- "bounds were taken from the data, which is a non-private query"
  } else {
    lo <- as.numeric(bounds[1L])
    hi <- as.numeric(bounds[2L])
  }
  cl <- .morie_dp_clip(X, lo, hi)
  Xc <- matrix(cl$x, n, p)
  lo <- cl$a
  hi <- cl$b
  old <- .morie_dp_seed(seed)
  on.exit(.morie_dp_unseed(old), add = TRUE)
  synth <- matrix(0, n_synth, p)
  marg_err <- numeric(p)
  nb <- as.integer(bins)
  edges <- seq(lo, hi, length.out = nb + 1L)
  for (j in seq_len(p)) {
    idx <- pmin(pmax(findInterval(Xc[, j], edges), 1L), nb)
    counts <- tabulate(idx, nbins = nb)
    noisy <- pmax(counts + .morie_dp_rlaplace(nb, 2 / eps), 0)
    if (sum(noisy) <= 0) noisy <- rep(1, nb)
    prob <- noisy / sum(noisy)
    pick <- sample.int(nb, n_synth, replace = TRUE, prob = prob)
    synth[, j] <- stats::runif(n_synth, edges[pick], edges[pick + 1L])
    marg_err[j] <- sum(abs(prob - counts / max(sum(counts), 1)))
  }
  cr <- if (p >= 2L) stats::cor(Xc[, 1L], Xc[, 2L]) else NA_real_
  cs <- if (p >= 2L) stats::cor(synth[, 1L], synth[, 2L]) else NA_real_
  list(synthetic = synth,
       preserved = "per-feature marginal distributions",
       destroyed = c("all inter-feature correlation", "joint structure",
                     "interactions"),
       marginal_error = marg_err, correlation_real = cr,
       correlation_synthetic = cs, epsilon = eps, n_synth = n_synth,
       bins = nb,
       warnings = c(warn,
                    paste("this preserves MARGINALS ONLY; every inter-feature",
                          "correlation is destroyed, so regressions and",
                          "interactions on it will be wrong while looking",
                          "plausible")),
       method = "dp_synthetic_data")
}


#' Differentially private changepoint location
#'
#' The exponential mechanism over split points, with utility the
#' between-segment sum of squares.
#'
#' The mechanism ALWAYS returns a location, including on a series with
#' no changepoint at all -- it is a selection procedure, not a test.
#' Compare \code{best_utility} against what pure noise would produce
#' before believing the location means anything.
#'
#' @param y series.
#' @param epsilon privacy budget.
#' @param bounds \code{c(lo, hi)} from outside the data.
#' @param min_segment shortest admissible segment.
#' @param seed optional integer seed.
#' @return list with \code{changepoint} (the number of observations in
#'   the FIRST segment, as in the Python module -- the second segment
#'   starts at index changepoint + 1),
#'   \code{utility}, \code{best_utility}, \code{utility_ratio},
#'   \code{probabilities}.
#' @references Cummings, R. et al. (2018). Differentially private
#'   change-point detection. \emph{NeurIPS}, 10825-10834.
#' @examples
#' set.seed(1)
#' y <- c(rnorm(50), rnorm(50, 4))
#' morie_dp_changepoint(y, epsilon = 20, bounds = c(-6, 10))$changepoint
#' @export
morie_dp_changepoint <- function(y, epsilon = 1, bounds = NULL,
                                 min_segment = 5, seed = NULL) {
  eps <- .morie_dp_check_budget(epsilon)$epsilon
  y <- as.numeric(y)
  n <- length(y)
  min_segment <- as.integer(min_segment)
  if (n < 2L * min_segment + 1L) {
    stop(sprintf("series too short for min_segment=%d", min_segment),
         call. = FALSE)
  }
  warn <- character(0)
  if (is.null(bounds)) {
    lo <- min(y)
    hi <- max(y)
    warn <- "bounds were taken from the data, which is a non-private query"
  } else {
    lo <- as.numeric(bounds[1L])
    hi <- as.numeric(bounds[2L])
  }
  cl <- .morie_dp_clip(y, lo, hi)
  yc <- cl$x
  lo <- cl$a
  hi <- cl$b
  # Candidate tau counts observations in the FIRST segment, matching the
  # Python's 0-based slice boundary exactly.
  cand <- seq.int(min_segment, n - min_segment - 1L)
  ss_tot <- sum((yc - mean(yc))^2)
  util <- vapply(cand, function(tau) {
    a <- yc[seq_len(tau)]
    b <- yc[(tau + 1L):n]
    ss_tot - sum((a - mean(a))^2) - sum((b - mean(b))^2)
  }, numeric(1))
  sens <- (hi - lo)^2
  logp <- eps * util / (2 * sens)
  logp <- logp - max(logp)
  p <- exp(logp)
  p <- p / sum(p)
  old <- .morie_dp_seed(seed)
  on.exit(.morie_dp_unseed(old), add = TRUE)
  pick <- sample.int(length(cand), 1L, prob = p)
  list(changepoint = cand[pick], utility = util[pick],
       best_utility = max(util),
       utility_ratio = if (max(util) > 0) util[pick] / max(util) else NA_real_,
       candidates = cand, probabilities = p, epsilon = eps, n = n,
       warnings = c(warn,
                    paste("the mechanism always returns a location; compare",
                          "best_utility against what noise alone would give",
                          "before believing there is a changepoint")),
       method = "dp_changepoint")
}


#' Differentially private federated averaging
#'
#' Client updates clipped to L2 norm C, summed, Gaussian noise added
#' once to the sum, divided by the number of clients.
#'
#' The privacy unit here is the CLIENT, not the example. Clipping per
#' example inside a client would leave a client with many examples
#' contributing far more than C in total and therefore exposed by
#' exactly the amount the accounting assumes away. Because the noise is
#' added once to the sum, its effect per client falls as 1/m -- more
#' clients is strictly better, which is the one place in DP where
#' scaling up helps for free.
#'
#' @param client_updates matrix, one row per client.
#' @param C L2 clipping norm per client.
#' @param sigma noise multiplier.
#' @param seed optional integer seed.
#' @return list with \code{aggregate}, \code{clipped_fraction},
#'   \code{noise_sd_per_client}, \code{noise_sd_aggregate}.
#' @references McMahan, H. B. et al. (2018). Learning differentially
#'   private recurrent language models. \emph{ICLR}.
#' @examples
#' set.seed(1)
#' morie_dp_fedavg(matrix(rnorm(50), ncol = 5), C = 1,
#'                 sigma = 1)$noise_sd_per_client
#' @export
morie_dp_fedavg <- function(client_updates, C = 1, sigma = 1, seed = NULL) {
  if (is.null(dim(client_updates)) || length(dim(client_updates)) != 2L) {
    stop("client_updates must be (m, p): one row per client", call. = FALSE)
  }
  U <- as.matrix(client_updates)
  storage.mode(U) <- "double"
  C <- as.numeric(C)[1L]
  if (C <= 0) stop("C must be positive", call. = FALSE)
  m <- nrow(U)
  p <- ncol(U)
  norms <- sqrt(rowSums(U^2))
  Uc <- U * pmin(1, C / pmax(norms, 1e-12))
  old <- .morie_dp_seed(seed)
  on.exit(.morie_dp_unseed(old), add = TRUE)
  noise <- if (sigma > 0) stats::rnorm(p, 0, sigma * C) else numeric(p)
  list(aggregate = (colSums(Uc) + noise) / m,
       clipped_fraction = mean(norms > C),
       noise_sd_per_client = sigma * C / m, noise_sd_aggregate = sigma * C,
       n_clients = m, C = C, sigma = as.numeric(sigma),
       warnings = paste("the privacy unit here is the CLIENT; clipping per",
                        "example inside a client would leave a heavy",
                        "contributor exposed"),
       method = "dp_fedavg")
}


#' Differentially private GAN discriminator step
#'
#' Only the DISCRIMINATOR touches real data, so only the discriminator
#' needs privatising. The generator sees the discriminator's output and
#' nothing else, which makes it post-processing and therefore free --
#' adding noise to it spends budget for nothing.
#'
#' Account over discriminator steps, which typically outnumber generator
#' steps several to one; counting generator steps instead understates
#' the spend.
#'
#' @param disc_grads per-example discriminator gradients, (B, p).
#' @param C,sigma,lr as for \code{\link{morie_dp_sgd}}.
#' @param n_disc_steps discriminator steps per generator step, recorded
#'   for accounting.
#' @param seed optional integer seed.
#' @return list with \code{disc_update}, \code{private_gradient},
#'   \code{generator_is_free}, \code{steps_to_account}.
#' @references Xie, L. et al. (2018). Differentially private generative
#'   adversarial network. \emph{arXiv:1802.06739}.
#' @examples
#' set.seed(1)
#' morie_dp_gan(matrix(rnorm(80), ncol = 4))$generator_is_free
#' @export
morie_dp_gan <- function(disc_grads, C = 1, sigma = 1, lr = 0.1,
                         n_disc_steps = 1, seed = NULL) {
  step <- morie_dp_sgd(disc_grads, C = C, sigma = sigma, lr = 1, seed = seed)
  g <- step$private_gradient
  list(disc_update = -lr * g, private_gradient = g, generator_is_free = TRUE,
       steps_to_account = as.integer(n_disc_steps),
       clipped_fraction = step$clipped_fraction, noise_sd = step$noise_sd,
       C = as.numeric(C), sigma = as.numeric(sigma),
       warnings = c(paste("only the discriminator needs privatising; noising",
                          "the generator as well spends budget for nothing"),
                    paste("account over discriminator steps, which outnumber",
                          "generator steps")),
       method = "dp_gan")
}
