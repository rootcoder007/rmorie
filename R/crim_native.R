# SPDX-License-Identifier: AGPL-3.0-or-later
#
# crim_native.R -- module 17: the four criminology methods missing
# after the Phase 29.3 variant audit (reaction-diffusion, Levy
# scaling, urban scaling, and the hotspot stack already exist in
# tps_statphysics / mrm). Builds on the native Hawkes C++ kernels.

#' Temporal ETAS model (Epidemic-Type Aftershock Sequence)
#'
#' Ogata (1988) ETAS with the modified-Omori (Lomax) triggering kernel
#' and exponential magnitude productivity: conditional intensity
#' \deqn{\lambda(t) = \mu + K \sum_{t_i < t} e^{\alpha (m_i - m_0)}
#'   (t - t_i + c)^{-p}.}
#' MLE by L-BFGS-B on the exact log-likelihood (direct sum plus the
#' closed-form kernel integral).
#'
#' @srrstats {G2.1} Inputs validated below.
#' @param times Numeric event times (sorted or sortable).
#' @param magnitudes Numeric marks (same length); constant marks give
#'   a plain Omori-Hawkes process.
#' @param m0 Reference (cutoff) magnitude. Default \code{min(magnitudes)}.
#' @param t_max Observation horizon. Default \code{max(times)}.
#' @return List of class \code{"morie_etas"}: par (mu, K, alpha, c, p),
#'   loglik, branching_ratio, n, converged, call.
#' @references Ogata (1988) JASA 83(401).
#' @examples
#' set.seed(1)
#' tt <- sort(runif(120, 0, 100))
#' mm <- rexp(120, 1.5) + 2
#' morie_crim_etas(tt, mm)
#' @export
morie_crim_etas <- function(times, magnitudes = NULL, m0 = NULL,
                            t_max = NULL) {
  t <- sort(as.numeric(times))
  n <- length(t)
  if (n < 10L) stop("Need >= 10 events for ETAS.", call. = FALSE)
  m <- if (is.null(magnitudes)) rep(0, n) else as.numeric(magnitudes)[order(as.numeric(times))]
  if (length(m) != n) stop("magnitudes length mismatch.", call. = FALSE)
  if (is.null(m0)) m0 <- min(m)
  if (is.null(t_max)) t_max <- max(t)
  dm <- m - m0

  # Constant marks leave alpha unidentified (a flat likelihood
  # direction no optimizer can converge along): fix alpha = 0 then.
  alpha_free <- diff(range(dm)) > 1e-12
  negll <- function(par) {
    # Bounded transforms keep every intermediate finite (optim
    # aborts outright on a non-finite objective).
    if (any(!is.finite(par))) return(1e10)
    mu <- exp(min(par[1], 20)); K <- exp(min(par[2], 20))
    alpha <- if (alpha_free) max(min(par[3], 10), -10) else 0
    cc <- exp(min(par[4], 20)); p <- 1 + exp(min(par[5], 5)) # p > 1
    prod_m <- exp(pmin(alpha * dm, 30))
    lam <- numeric(n)
    for (i in seq_len(n)) {
      if (i == 1L) { lam[i] <- mu; next }
      dt <- t[i] - t[seq_len(i - 1L)]
      lam[i] <- mu + K * sum(prod_m[seq_len(i - 1L)] * (dt + cc)^(-p))
    }
    if (any(lam <= 0) || any(!is.finite(lam))) return(1e10)
    # Integral of the triggering kernel over [t_i, t_max]:
    # K e^{a dm_i} [c^{1-p} - (t_max - t_i + c)^{1-p}] / (p - 1)
    integ <- mu * t_max + K * sum(prod_m *
      (cc^(1 - p) - (t_max - t + cc)^(1 - p)) / (p - 1))
    v <- -(sum(log(lam)) - integ)
    if (!is.finite(v)) 1e10 else v
  }
  init <- c(log(n / (2 * t_max)), log(0.2), 0.5, log(0.01), log(0.1))
  # Nelder-Mead with one restart from the first optimum -- the
  # standard remedy for premature simplex collapse in 5 parameters.
  opt <- stats::optim(init, negll, method = "Nelder-Mead",
                      control = list(maxit = 2000L))
  opt <- stats::optim(opt$par, negll, method = "Nelder-Mead",
                      control = list(maxit = 2000L))
  mu <- exp(opt$par[1]); K <- exp(opt$par[2])
  alpha <- if (alpha_free) opt$par[3] else 0
  cc <- exp(opt$par[4]); p <- 1 + exp(opt$par[5])
  # Branching ratio: E[offspring] = K E[e^{a dm}] c^{1-p} / (p-1).
  br <- K * mean(exp(alpha * dm)) * cc^(1 - p) / (p - 1)
  out <- list(par = c(mu = mu, K = K, alpha = alpha, c = cc, p = p),
              loglik = -opt$value, branching_ratio = br,
              n = n, converged = opt$convergence == 0L,
              call = match.call())
  class(out) <- "morie_etas"
  out
}

#' @examples
#' \donttest{
#' set.seed(1)
#' tt <- sort(runif(120, 0, 100))
#' mm <- rexp(120, 1.5) + 2
#' obj <- morie_crim_etas(tt, mm)
#' \references{
#' Ogata (1988) JASA 83(401).
#' print(obj)
#' }
#' @export
print.morie_etas <- function(x, ...) {
  cat("ETAS (Ogata 1988), n =", x$n, "\n")
  print(round(x$par, 4))
  cat(sprintf("  loglik = %.2f  branching ratio = %.3f%s\n",
              x$loglik, x$branching_ratio,
              if (x$branching_ratio >= 1) "  ** supercritical **" else ""))
  invisible(x)
}

#' Multivariate Hawkes process (exponential kernels)
#'
#' K-dimensional mutually exciting process with kernel matrix
#' \eqn{\phi_{jk}(u) = a_{jk} \beta e^{-\beta u}}: MLE via the exact
#' log-likelihood with the O(n) exponential recursion per (j,k) pair.
#'
#' @srrstats {G2.1} Inputs validated below.
#' @param times Numeric event times.
#' @param marks Integer/factor component labels (length of times).
#' @param t_max Observation horizon. Default \code{max(times)}.
#' @param beta Fixed decay rate (shared); estimated when NULL via an
#'   outer profile grid.
#' @return List of class \code{"morie_mv_hawkes"}: mu (K-vector),
#'   A (KxK excitation matrix), beta, loglik, spectral_radius, n,
#'   converged, call.
#' @references Hawkes (1971) Biometrika 58(1).
#' @examples
#' set.seed(2)
#' tt <- sort(runif(150, 0, 100)); mk <- sample(1:2, 150, TRUE)
#' morie_crim_hawkes_multivariate(tt, mk, beta = 1)
#' @export
morie_crim_hawkes_multivariate <- function(times, marks, t_max = NULL,
                                           beta = NULL) {
  o <- order(as.numeric(times))
  t <- as.numeric(times)[o]
  k_lab <- as.integer(as.factor(marks))[o]
  K <- max(k_lab)
  n <- length(t)
  if (n < 10L * K) stop("Need >= 10 events per component.", call. = FALSE)
  if (is.null(t_max)) t_max <- max(t)

  ll_at <- function(mu, A, b) {
    # R[j,k] recursion: R_i = e^{-b dt}(R_{i-1} + 1[mark_{i-1} = k]).
    R <- matrix(0, n, K)
    for (i in 2:n) {
      dec <- exp(-b * (t[i] - t[i - 1]))
      R[i, ] <- dec * R[i - 1, ]
      R[i, k_lab[i - 1]] <- R[i, k_lab[i - 1]] + dec
    }
    lam <- mu[k_lab] + rowSums(A[k_lab, , drop = FALSE] * R) * b
    if (any(lam <= 0)) return(-Inf)
    comp <- sum(mu) * t_max
    surv <- 1 - exp(-b * (t_max - t))
    for (j in seq_len(K)) {
      comp <- comp + sum(A[j, k_lab] * surv)
    }
    sum(log(lam)) - comp
  }
  fit_b <- function(b) {
    par0 <- c(log(rep(n / (K * t_max), K)), rep(log(0.1), K * K))
    negll <- function(par) {
      mu <- exp(par[seq_len(K)])
      A <- matrix(exp(par[-seq_len(K)]), K, K)
      v <- ll_at(mu, A, b)
      if (!is.finite(v)) 1e10 else -v
    }
    stats::optim(par0, negll, method = "L-BFGS-B",
                 control = list(maxit = 400L))
  }
  if (is.null(beta)) {
    grid <- c(0.25, 0.5, 1, 2, 4)
    fits <- lapply(grid, fit_b)
    best <- which.min(vapply(fits, `[[`, numeric(1), "value"))
    beta <- grid[best]; opt <- fits[[best]]
  } else {
    opt <- fit_b(beta)
  }
  mu <- exp(opt$par[seq_len(K)])
  A <- matrix(exp(opt$par[-seq_len(K)]), K, K)
  sr <- max(Mod(eigen(A, only.values = TRUE)$values))
  out <- list(mu = mu, A = A, beta = beta, loglik = -opt$value,
              spectral_radius = sr, n = n, K = K,
              converged = opt$convergence == 0L, call = match.call())
  class(out) <- "morie_mv_hawkes"
  out
}

#' @examples
#' \donttest{
#' set.seed(2)
#' tt <- sort(runif(150, 0, 100)); mk <- sample(1:2, 150, TRUE)
#' obj <- morie_crim_hawkes_multivariate(tt, mk, beta = 1)
#' \references{
#' Hawkes (1971) Biometrika 58(1).
#' print(obj)
#' }
#' @export
print.morie_mv_hawkes <- function(x, ...) {
  cat(sprintf("Multivariate Hawkes (K = %d, beta = %.3g), n = %d\n",
              x$K, x$beta, x$n))
  cat("  mu:", round(x$mu, 4), "\n  A:\n")
  print(round(x$A, 4))
  cat(sprintf("  spectral radius = %.3f%s\n", x$spectral_radius,
              if (x$spectral_radius >= 1) "  ** unstable **" else ""))
  invisible(x)
}

#' Knox near-repeat test (space-time interaction)
#'
#' Knox (1964) contingency statistic with Monte Carlo permutation
#' inference (Townsley et al. 2003 near-repeat formulation): counts
#' event pairs close in BOTH space and time and compares against the
#' permutation distribution obtained by shuffling event times.
#'
#' @srrstats {G2.1} Inputs validated below.
#' @param x,y Event coordinates.
#' @param times Event times.
#' @param s_threshold Spatial closeness threshold.
#' @param t_threshold Temporal closeness threshold.
#' @param n_perm Permutations. Default 499.
#' @param seed RNG seed.
#' @return List of class \code{"morie_knox"}: statistic (observed
#'   close-pair count), expected, ratio, p.value, n, call.
#' @references Knox (1964); Townsley, Homel & Chaseling (2003).
#' @examples
#' set.seed(3)
#' morie_crim_near_repeat(runif(60), runif(60), runif(60, 0, 30),
#'                        s_threshold = 0.1, t_threshold = 3)
#' @export
morie_crim_near_repeat <- function(x, y, times, s_threshold,
                                   t_threshold, n_perm = 499L,
                                   seed = 42L) {
  x <- as.numeric(x); y <- as.numeric(y); tt <- as.numeric(times)
  n <- length(x)
  stopifnot(length(y) == n, length(tt) == n, n >= 10L)
  ds <- as.matrix(stats::dist(cbind(x, y)))
  close_s <- ds <= s_threshold
  diag(close_s) <- FALSE
  knox_stat <- function(tvec) {
    dt <- abs(outer(tvec, tvec, "-"))
    sum(close_s & dt <= t_threshold) / 2
  }
  obs <- knox_stat(tt)
  set.seed(seed)
  perm <- vapply(seq_len(n_perm), function(b) knox_stat(sample(tt)),
                 numeric(1))
  expected <- mean(perm)
  p <- (1 + sum(perm >= obs)) / (n_perm + 1)
  out <- list(statistic = obs, expected = expected,
              ratio = obs / max(expected, 1e-12), p.value = p,
              n = n, n_perm = n_perm, call = match.call())
  class(out) <- "morie_knox"
  out
}

#' @examples
#' \donttest{
#' set.seed(3)
#' obj <- morie_crim_near_repeat(runif(60), runif(60), runif(60, 0, 30),
#'                        s_threshold = 0.1, t_threshold = 3)
#' \references{
#' Knox (1964); Townsley, Homel & Chaseling (2003).
#' print(obj)
#' }
#' @export
print.morie_knox <- function(x, ...) {
  cat("Knox near-repeat test\n")
  cat(sprintf("  close pairs: %d observed vs %.1f expected (ratio %.2f)\n",
              x$statistic, x$expected, x$ratio))
  cat(sprintf("  permutation p = %.4f (%d permutations)\n",
              x$p.value, x$n_perm))
  invisible(x)
}

#' Risk terrain model
#'
#' Caplan-Kennedy-Miller style risk terrain: kernel-density surfaces
#' for each risk-factor point layer on a common grid, then a Poisson
#' regression of gridded incident counts on the standardized layer
#' densities. Relative risk scores per cell come from the fitted
#' surface.
#'
#' @srrstats {G2.1} Inputs validated below.
#' @param incidents Two-column matrix/data.frame of incident x,y.
#' @param layers Named list of two-column matrices (risk-factor point
#'   layers, e.g. bars, transit stops).
#' @param n_grid Grid cells per axis. Default 25.
#' @param bandwidth Kernel sd. Default Silverman per layer.
#' @return List of class \code{"morie_rtm"}: coefficients (one per
#'   layer, log relative risk), risk_surface (matrix), grid_x, grid_y,
#'   deviance_ratio, n, call.
#' @references Caplan, Kennedy & Miller (2011) Justice Quarterly 28(2).
#' @examples
#' set.seed(4)
#' inc <- cbind(runif(80), runif(80))
#' lay <- list(bars = cbind(runif(15), runif(15)))
#' morie_crim_risk_terrain(inc, lay, n_grid = 10L)
#' @export
morie_crim_risk_terrain <- function(incidents, layers, n_grid = 25L,
                                    bandwidth = NULL) {
  inc <- as.matrix(incidents)
  stopifnot(ncol(inc) == 2L, is.list(layers), length(layers) >= 1L)
  if (is.null(names(layers)) || any(names(layers) == "")) {
    names(layers) <- paste0("layer", seq_along(layers))
  }
  gx <- seq(min(inc[, 1]), max(inc[, 1]), length.out = n_grid)
  gy <- seq(min(inc[, 2]), max(inc[, 2]), length.out = n_grid)
  cell_x <- findInterval(inc[, 1], gx, all.inside = TRUE)
  cell_y <- findInterval(inc[, 2], gy, all.inside = TRUE)
  counts <- table(factor(cell_x, levels = seq_len(n_grid)),
                  factor(cell_y, levels = seq_len(n_grid)))
  y <- as.numeric(counts)
  grid_pts <- as.matrix(expand.grid(x = gx, y = gy))
  dens <- vapply(layers, function(L) {
    L <- as.matrix(L)
    h <- if (is.null(bandwidth)) {
      1.06 * mean(apply(L, 2, stats::sd)) * nrow(L)^(-1 / 5)
    } else bandwidth
    h <- max(h, 1e-6)
    v <- vapply(seq_len(nrow(grid_pts)), function(i) {
      du <- (grid_pts[i, 1] - L[, 1]) / h
      dv <- (grid_pts[i, 2] - L[, 2]) / h
      sum(exp(-0.5 * (du^2 + dv^2)))
    }, numeric(1))
    as.numeric(scale(v))
  }, numeric(nrow(grid_pts)))
  df <- data.frame(y = y, dens)
  fit <- stats::glm(y ~ ., data = df, family = stats::poisson())
  risk <- matrix(stats::fitted(fit), n_grid, n_grid)
  dev_ratio <- 1 - fit$deviance / fit$null.deviance
  out <- list(coefficients = stats::coef(fit)[-1L],
              risk_surface = risk, grid_x = gx, grid_y = gy,
              deviance_ratio = dev_ratio, n = nrow(inc),
              call = match.call())
  class(out) <- "morie_rtm"
  out
}

#' @examples
#' \donttest{
#' set.seed(4)
#' inc <- cbind(runif(80), runif(80))
#' lay <- list(bars = cbind(runif(15), runif(15)))
#' obj <- morie_crim_risk_terrain(inc, lay, n_grid = 10L)
#' \references{
#' Caplan, Kennedy & Miller (2011) Justice Quarterly 28(2).
#' print(obj)
#' }
#' @export
print.morie_rtm <- function(x, ...) {
  cat("Risk terrain model,", x$n, "incidents\n")
  cat("  log relative risk per layer (standardized densities):\n")
  print(round(x$coefficients, 4))
  cat(sprintf("  deviance explained: %.1f%%\n", 100 * x$deviance_ratio))
  invisible(x)
}
