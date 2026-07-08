# SPDX-License-Identifier: AGPL-3.0-or-later
#' Spatiotemporal (self-exciting) Hawkes point process
#'
#' Models the conditional intensity of clustered space-time events -- near-miss
#' airspace violations, crime, seismic aftershocks -- where each event
#' transiently raises the risk of further events nearby and soon after. The
#' existing \code{\link{morie_hawkes_fit}} is purely temporal; this adds the
#' spatial dimension:
#' \deqn{\lambda(t,x,y) = \mu + \sum_{t_i < t} \alpha\,\beta e^{-\beta(t-t_i)}
#'       \, \frac{1}{2\pi\sigma^2} e^{-((x-x_i)^2+(y-y_i)^2)/(2\sigma^2)}}
#' with background rate \eqn{\mu} (per unit area per unit time), branching ratio
#' \eqn{\alpha} (expected offspring per event; \eqn{\alpha<1} for stability),
#' temporal decay \eqn{\beta}, and Gaussian spatial spread \eqn{\sigma}. The
#' triggering kernel integrates to \eqn{\alpha} in time and 1 in space, so the
#' spatial factor is a proper density (Reinhart 2018).
#'
#' @name hawkes_spatial
#' @references
#' Reinhart A (2018). A review of self-exciting spatio-temporal point processes.
#'   \emph{Statistical Science} 33(3), 299--318. \doi{10.1214/17-STS629}
#'
#' Ogata Y (1988). Statistical models for earthquake occurrences. \emph{JASA}
#'   83(401), 9--27. \doi{10.1080/01621459.1988.10478560}
NULL

.hst_check_params <- function(p) {
  need <- c("mu", "alpha", "beta", "sigma")
  if (!all(need %in% names(p))) {
    stop("`params` must be a list with mu, alpha, beta, sigma", call. = FALSE)
  }
  if (p$mu < 0 || p$alpha < 0 || p$beta <= 0 || p$sigma <= 0) {
    stop("require mu>=0, alpha>=0, beta>0, sigma>0", call. = FALSE)
  }
  invisible(p)
}

# Gaussian spatial density evaluated at squared distances.
.hst_spatial <- function(d2, sigma) {
  exp(-d2 / (2 * sigma^2)) / (2 * pi * sigma^2)
}

#' Conditional intensity of a spatiotemporal Hawkes process at a query point
#'
#' @param events A data frame / matrix with columns \code{t}, \code{x},
#'   \code{y} of past events (only those with \code{t < t_q} contribute).
#' @param t_q,x_q,y_q Query time and location.
#' @param params List \code{list(mu, alpha, beta, sigma)}.
#' @return The intensity \eqn{\lambda(t_q, x_q, y_q)} (a non-negative scalar).
#' @examples
#' ev <- data.frame(t = c(0.1, 0.5), x = c(0, 1), y = c(0, 1))
#' morie_hawkes_st_intensity(ev, 0.6, 0.1, 0.1,
#'                           list(mu = 0.2, alpha = 0.5, beta = 1, sigma = 0.5))
#' @export
morie_hawkes_st_intensity <- function(events, t_q, x_q, y_q, params) {
  .hst_check_params(params)
  t <- as.numeric(events$t)
  x <- as.numeric(events$x)
  y <- as.numeric(events$y)
  past <- which(t < t_q)
  lam <- params$mu
  if (length(past)) {
    dt <- t_q - t[past]
    d2 <- (x_q - x[past])^2 + (y_q - y[past])^2
    trig <- params$alpha * params$beta * exp(-params$beta * dt) *
      .hst_spatial(d2, params$sigma)
    lam <- lam + sum(trig)
  }
  lam
}

#' Log-likelihood of a spatiotemporal Hawkes process
#'
#' Evaluates \eqn{\sum_j \log\lambda(t_j,x_j,y_j) - \int \lambda}, with the
#' compensator \eqn{\mu\,T\,A + \sum_i \alpha(1 - e^{-\beta(T - t_i)})} (the
#' spatial kernel integrates to 1 over the plane; interior-region boundary
#' effects are neglected, standard for events away from the edge).
#'
#' @param events Data frame with \code{t}, \code{x}, \code{y} (sorted or not).
#' @param params List \code{list(mu, alpha, beta, sigma)}.
#' @param end_time Observation horizon \eqn{T} (default \code{max(t)}).
#' @param area Spatial region area \eqn{A} used for the background term.
#' @return Scalar log-likelihood.
#' @examples
#' ev <- morie_hawkes_st_simulate(
#'   list(mu = 0.2, alpha = 0.5, beta = 1, sigma = 0.3),
#'   end_time = 20, region = c(0, 10, 0, 10), seed = 1)
#' morie_hawkes_st_loglik(ev, list(mu = 0.2, alpha = 0.5, beta = 1, sigma = 0.3),
#'                        end_time = 20, area = 100)
#' @export
morie_hawkes_st_loglik <- function(events, params, end_time = NULL, area = 1) {
  .hst_check_params(params)
  t <- as.numeric(events$t)
  x <- as.numeric(events$x)
  y <- as.numeric(events$y)
  o <- order(t)
  t <- t[o]
  x <- x[o]
  y <- y[o]
  n <- length(t)
  T_h <- end_time %||% max(t)
  if (n == 0L) return(-params$mu * T_h * area)

  # Sum of log-intensities at each event (O(n^2) pairwise).
  loglam <- 0
  for (j in seq_len(n)) {
    lam <- params$mu
    if (j > 1L) {
      dt <- t[j] - t[seq_len(j - 1L)]
      d2 <- (x[j] - x[seq_len(j - 1L)])^2 + (y[j] - y[seq_len(j - 1L)])^2
      lam <- lam + sum(params$alpha * params$beta * exp(-params$beta * dt) *
                         .hst_spatial(d2, params$sigma))
    }
    loglam <- loglam + log(lam)
  }
  compensator <- params$mu * T_h * area +
    sum(params$alpha * (1 - exp(-params$beta * (T_h - t))))
  loglam - compensator
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Simulate a spatiotemporal Hawkes process (immigrant-offspring branching)
#'
#' Exact simulation via the branching construction (Hawkes & Oakes 1974):
#' immigrants are a homogeneous Poisson process of rate \eqn{\mu} over the
#' region and horizon; each event spawns \eqn{\mathrm{Poisson}(\alpha)}
#' offspring at exponential-\eqn{\beta} time lags and Gaussian-\eqn{\sigma}
#' spatial offsets, recursively. Requires \eqn{\alpha < 1}.
#'
#' @param params List \code{list(mu, alpha, beta, sigma)} with \code{alpha < 1}.
#' @param end_time Horizon \eqn{T}.
#' @param region Numeric \code{c(xmin, xmax, ymin, ymax)}.
#' @param seed Optional integer seed.
#' @param max_events Safety cap on total events (default 1e5).
#' @return A data frame with columns \code{t}, \code{x}, \code{y}, \code{gen}
#'   (generation: 0 = immigrant), sorted by \code{t}.
#' @examples
#' morie_hawkes_st_simulate(list(mu = 0.1, alpha = 0.4, beta = 1, sigma = 0.5),
#'                          end_time = 10, region = c(0, 5, 0, 5), seed = 42)
#' @export
morie_hawkes_st_simulate <- function(params, end_time, region, seed = NULL,
                                     max_events = 1e5L) {
  .hst_check_params(params)
  if (params$alpha >= 1) {
    stop("simulation requires alpha < 1 (subcritical)", call. = FALSE)
  }
  if (length(region) != 4L) {
    stop("`region` must be c(xmin, xmax, ymin, ymax)", call. = FALSE)
  }
  if (!is.null(seed)) set.seed(seed)
  xmin <- region[1]
  xmax <- region[2]
  ymin <- region[3]
  ymax <- region[4]
  area <- (xmax - xmin) * (ymax - ymin)

  n_imm <- stats::rpois(1, params$mu * area * end_time)
  t <- stats::runif(n_imm, 0, end_time)
  x <- stats::runif(n_imm, xmin, xmax)
  y <- stats::runif(n_imm, ymin, ymax)
  gen <- rep(0L, n_imm)

  # Process each parent generation, appending offspring.
  queue_from <- 1L
  all_t <- t
  all_x <- x
  all_y <- y
  all_g <- gen
  cur_t <- t
  cur_x <- x
  cur_y <- y
  cur_g <- gen
  while (length(cur_t) > 0L && length(all_t) < max_events) {
    n_off <- stats::rpois(length(cur_t), params$alpha)
    keep <- n_off > 0L
    if (!any(keep)) break
    pt <- rep(cur_t[keep], n_off[keep])
    px <- rep(cur_x[keep], n_off[keep])
    py <- rep(cur_y[keep], n_off[keep])
    pg <- rep(cur_g[keep], n_off[keep])
    m <- length(pt)
    ot <- pt + stats::rexp(m, rate = params$beta)
    ox <- px + stats::rnorm(m, 0, params$sigma)
    oy <- py + stats::rnorm(m, 0, params$sigma)
    within <- ot < end_time
    ot <- ot[within]
    ox <- ox[within]
    oy <- oy[within]
    og <- pg[within] + 1L
    if (length(ot) == 0L) break
    all_t <- c(all_t, ot)
    all_x <- c(all_x, ox)
    all_y <- c(all_y, oy)
    all_g <- c(all_g, og)
    cur_t <- ot
    cur_x <- ox
    cur_y <- oy
    cur_g <- og
  }
  o <- order(all_t)
  data.frame(t = all_t[o], x = all_x[o], y = all_y[o], gen = all_g[o])
}

#' Fit a spatiotemporal Hawkes process by maximum likelihood
#'
#' Maximises \code{\link{morie_hawkes_st_loglik}} over
#' \eqn{(\mu, \alpha, \beta, \sigma)} on the log scale (positivity) via
#' \code{stats::optim} (L-BFGS-B). \eqn{\alpha} is unconstrained above here;
#' check \code{fit$params$alpha < 1} for a stable fit.
#'
#' \strong{Identifiability.} The background/branching split (\eqn{\mu} vs
#' \eqn{\alpha}) is weakly identified at small samples, and the likelihood
#' neglects spatial edge effects (offspring that drift outside the region are
#' attributed to background). Interpret \eqn{\mu} vs \eqn{\alpha} cautiously and
#' prefer large regions relative to \eqn{\sigma} and long records.
#'
#' @param events Data frame with \code{t}, \code{x}, \code{y}.
#' @param end_time Horizon \eqn{T} (default \code{max(t)}).
#' @param area Spatial region area \eqn{A}.
#' @param start Optional named starting list \code{list(mu, alpha, beta, sigma)}.
#' @return A list of class \code{morie_hawkes_st_fit}: \code{params} (named
#'   list), \code{loglik}, \code{n}, \code{convergence} (0 = success).
#' @examples
#' ev <- morie_hawkes_st_simulate(
#'   list(mu = 0.2, alpha = 0.5, beta = 1, sigma = 0.4),
#'   end_time = 40, region = c(0, 10, 0, 10), seed = 7)
#' morie_hawkes_st_fit(ev, end_time = 40, area = 100)$params
#' @export
morie_hawkes_st_fit <- function(events, end_time = NULL, area = 1,
                                start = NULL) {
  t <- as.numeric(events$t)
  T_h <- end_time %||% max(t)
  s <- start %||% list(mu = max(length(t) / (T_h * area), 1e-3),
                       alpha = 0.3, beta = 1, sigma = 1)
  par0 <- log(c(s$mu, s$alpha, s$beta, s$sigma))
  nll <- function(lp) {
    p <- list(mu = exp(lp[1]), alpha = exp(lp[2]),
              beta = exp(lp[3]), sigma = exp(lp[4]))
    val <- tryCatch(
      morie_hawkes_st_loglik(events, p, end_time = T_h, area = area),
      error = function(e) NA_real_)
    if (!is.finite(val)) return(1e10)
    -val
  }
  opt <- stats::optim(par0, nll, method = "L-BFGS-B")
  p <- exp(opt$par)
  out <- list(
    params = list(mu = p[1], alpha = p[2], beta = p[3], sigma = p[4]),
    loglik = -opt$value, n = length(t), convergence = opt$convergence
  )
  class(out) <- "morie_hawkes_st_fit"
  out
}

#' @return \code{x}, invisibly.
#' @export
print.morie_hawkes_st_fit <- function(x, ...) {
  p <- x$params
  cat(sprintf("Spatiotemporal Hawkes fit (n = %d, logLik = %.2f)\n",
              x$n, x$loglik))
  cat(sprintf("  mu = %.4g  alpha = %.4g%s  beta = %.4g  sigma = %.4g\n",
              p$mu, p$alpha, if (p$alpha >= 1) " (UNSTABLE, >=1)" else "",
              p$beta, p$sigma))
  invisible(x)
}
