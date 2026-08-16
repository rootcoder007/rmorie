# Particle filtering for partially observed Markov processes.
# Reference: King, Nguyen & Ionides (2016) "Statistical Inference for
# Partially Observed Markov Processes: The R Package pomp", JSS 69(12).

#' logmeanexp
#'
#' A step of the pftrep_native implementation. Called by \code{replicated_pfilter}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param values Coerced to numeric by the body, with \code{as.numeric}.
#' @return A numeric value.
#' @export
logmeanexp <- function(values) {
  v <- as.numeric(values)
  if (length(v) == 0L) stop("pftrep: nothing to average")
  mx <- max(v)
  if (is.infinite(mx) && mx < 0) return(-Inf)
  mx + log(sum(exp(v - mx)) / length(v))
}

#' particle_filter_simple
#'
#' A step of the pftrep_native implementation. Called by \code{replicated_pfilter}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y See Usage.
#' @param n_particles A count; the body uses it as \code{rep(...)}.
#' @param init See Usage.
#' @param step See Usage.
#' @param loglik See Usage.
#' @param seed Coerced to integer by the body, with \code{as.integer}. Defaults to \code{0L}.
#' @return A list with \code{loglik}, \code{min_ess}.
#' @export
particle_filter_simple <- function(y, n_particles, init, step, loglik,
                                   seed = 0L) {
  set.seed(as.integer(seed))
  parts <- init(n_particles)
  w <- rep(1.0 / n_particles, n_particles)
  ll <- 0
  min_ess <- n_particles
  for (obs in y) {
    parts <- step(parts, 1L)
    lw <- vapply(parts, function(p) loglik(p, obs), numeric(1))
    mx <- max(lw)
    w <- exp(lw - mx) * w
    s <- sum(w)
    ll <- ll + log(s) + mx
    w <- w / s
    ess <- 1 / sum(w * w)
    if (ess < min_ess) min_ess <- ess
    if (ess < n_particles / 2) {
      idx <- sample.int(n_particles, n_particles, replace = TRUE, prob = w)
      parts <- parts[idx]
      w <- rep(1.0 / n_particles, n_particles)
    }
  }
  list(loglik = ll, min_ess = min_ess)
}

#' replicated_pfilter
#'
#' A step of the pftrep_native implementation. Called by \code{loglik_profile}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y See Usage.
#' @param n_particles Coerced to integer by the body, with \code{as.integer}.
#' @param init See Usage.
#' @param step See Usage.
#' @param loglik See Usage.
#' @param n_reps Coerced to integer by the body, with \code{as.integer}. Defaults to \code{10L}.
#' @param seed Numeric; combined arithmetically in the body. Defaults to \code{0L}.
#' @return A list with \code{estimate}, \code{loglik}, \code{logmeanexp}, \code{mean_loglik}, \code{jensen_gap}, \code{se}, \code{replicates}, \code{n_reps}, \code{n_particles}, \code{min_ess}, \code{mean_min_ess}, \code{method}.
#' @export
replicated_pfilter <- function(y, n_particles, init, step, loglik,
                               n_reps = 10L, seed = 0L) {
  R <- as.integer(n_reps)
  if (R < 1L) stop(sprintf("pftrep: need at least 1 replicate, got %d", R))
  lls <- numeric(R); minless <- numeric(R)
  for (r in seq_len(R)) {
    res <- particle_filter_simple(y, n_particles, init, step, loglik,
                                  seed = seed * 1013L + r)
    lls[r] <- res$loglik; minless[r] <- res$min_ess
  }
  lme <- logmeanexp(lls)
  mean_ll <- mean(lls)
  se <- if (R > 1L) stats::sd(lls) / sqrt(R) else NaN
  list(estimate = lme, loglik = lme, logmeanexp = lme,
       mean_loglik = mean_ll, jensen_gap = lme - mean_ll,
       se = se, replicates = lls, n_reps = R,
       n_particles = as.integer(n_particles),
       min_ess = min(minless), mean_min_ess = mean(minless),
       method = "replicated particle filtering, King, Nguyen & Ionides (2016)")
}

#' loglik_profile
#'
#' A step of the pftrep_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y See Usage.
#' @param grid Coerced to numeric by the body, with \code{as.numeric}.
#' @param make_model See Usage.
#' @param n_particles Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200L}.
#' @param n_reps Coerced to integer by the body, with \code{as.integer}. Defaults to \code{5L}.
#' @param seed Numeric; combined arithmetically in the body. Defaults to \code{0L}.
#' @return A list with \code{estimate}, \code{mle}, \code{grid}, \code{loglik}, \code{se}, \code{max_loglik}, \code{n_particles}, \code{n_reps}, \code{method}.
#' @export
loglik_profile <- function(y, grid, make_model, n_particles = 200L,
                           n_reps = 5L, seed = 0L) {
  g <- as.numeric(grid)
  if (length(g) < 2L)
    stop(sprintf("pftrep: need at least 2 grid points, got %d", length(g)))
  vals <- numeric(length(g)); ses <- numeric(length(g))
  for (t in seq_along(g)) {
    ms <- make_model(g[t])
    init <- ms[[1]]; step <- ms[[2]]; loglik <- ms[[3]]
    r <- replicated_pfilter(y, n_particles, init, step, loglik,
                            n_reps = n_reps, seed = seed + 97L * t)
    vals[t] <- r$loglik; ses[t] <- r$se
  }
  best <- which.max(vals)
  list(estimate = g[best], mle = g[best], grid = g,
       loglik = vals, se = ses, max_loglik = vals[best],
       n_particles = as.integer(n_particles),
       n_reps = as.integer(n_reps),
       method = "particle-filter likelihood profile, King, Nguyen & Ionides (2016)")
}

replicatedpfilter <- replicated_pfilter
particle_filter_epi <- replicated_pfilter

# house entry point: the package exports one morie_<module>
morie_pftrep <- replicated_pfilter
