# Bayesian credible sets vs frequentist confidence sets under
# partial identification. Moon & Schorfheide (2012) Econometrica 80(2),
# 755-782. The Kadane insight: P_{phi,theta} of theta given phi is
# never updated by data. The HPD credible set excludes parts of the
# estimated identified set; the confidence set extends beyond it.

.GHC_EPS <- 1e-12

morie_identified_set_interval <- function(phi_hat, half_width) {
  h <- as.numeric(half_width)
  if (h < 0) stop("bndbye: the half-width must be non-negative")
  list(lower = as.numeric(phi_hat) - h, upper = as.numeric(phi_hat) + h,
       width = 2 * h, phi_hat = as.numeric(phi_hat))
}

morie_conditional_prior_uniform <- function(theta_set, n_grid = 401L) {
  lo <- as.numeric(theta_set$lower); hi <- as.numeric(theta_set$upper)
  if (hi < lo) stop("bndbye: the identified set is empty")
  if (hi - lo <= .GHC_EPS) return(list(grid = lo, density = 1))
  g <- lo + (hi - lo) * seq_len(as.integer(n_grid) - 1L) /
                (as.integer(n_grid) - 1L)
  list(grid = g, density = rep(1 / (hi - lo), length(g)))
}

morie_posterior_hpd <- function(theta_set, level = 0.95,
                               conditional_prior = NULL, n_grid = 401L) {
  if (level <= 0 || level >= 1) stop("bndbye: level must lie in (0, 1)")
  cp <- if (is.null(conditional_prior))
    morie_conditional_prior_uniform(theta_set, n_grid) else conditional_prior
  g <- as.numeric(cp$grid); d <- as.numeric(cp$density)
  if (length(g) != length(d))
    stop("bndbye: the prior grid and density differ in length")
  if (length(g) == 1L)
    return(list(lower = g[1], upper = g[1], width = 0,
                level = as.numeric(level), covered = 1))
  step <- (g[length(g)] - g[1]) / (length(g) - 1L)
  mass <- d * step
  tot <- sum(mass)
  if (tot <= .GHC_EPS) stop("bndbye: the conditional prior has no mass")
  mass <- mass / tot
  ord <- order(-d)
  acc <- 0; chosen <- integer(0)
  for (i in ord) {
    chosen <- c(chosen, i); acc <- acc + mass[i]
    if (acc >= as.numeric(level)) break
  }
  lo <- min(g[chosen]); hi <- max(g[chosen])
  list(lower = lo, upper = hi, width = hi - lo, level = as.numeric(level),
       covered = acc, n_grid_points = length(chosen),
       method = "HPD of the conditional prior at phi_hat -- the large-sample limit of the posterior (Moon & Schorfheide 2012)")
}

morie_frequentist_confidence_set <- function(theta_set, se_phi, level = 0.95,
                                            target = "parameter") {
  if (!(target %in% c("parameter", "set")))
    stop("bndbye: target must be parameter or set")
  s <- as.numeric(se_phi)
  if (s < 0) stop("bndbye: the standard error must be non-negative")
  if (level <= 0 || level >= 1) stop("bndbye: level must lie in (0, 1)")
  c <- if (target == "parameter") qnorm(as.numeric(level))
       else qnorm(0.5 + as.numeric(level) / 2)
  list(lower = theta_set$lower - c * s, upper = theta_set$upper + c * s,
       width = theta_set$width + 2 * c * s, critical_value = c,
       target = target, level = as.numeric(level),
       note = "extends beyond Theta(phi_hat) by c * se on each side, because phi_hat is estimated")
}

morie_compare_sets <- function(phi_hat, half_width, se_phi, level = 0.95,
                               conditional_prior = NULL, n_grid = 401L) {
  ts <- morie_identified_set_interval(phi_hat, half_width)
  hpd <- morie_posterior_hpd(ts, level = level,
                             conditional_prior = conditional_prior,
                             n_grid = n_grid)
  cs <- morie_frequentist_confidence_set(ts, se_phi, level = level)
  list(estimate = hpd$width / max(cs$width, .GHC_EPS),
       identified_set = ts, credible_hpd = hpd, confidence_set = cs,
       hpd_inside_identified_set =
         hpd$lower >= ts$lower - 1e-9 && hpd$upper <= ts$upper + 1e-9,
       cs_contains_identified_set =
         cs$lower <= ts$lower + 1e-9 && cs$upper >= ts$upper - 1e-9,
       width_ratio_hpd_over_cs = hpd$width / max(cs$width, .GHC_EPS),
       conditional_prior_reported = !is.null(conditional_prior),
       method = "Moon & Schorfheide (2012): HPD excludes parts of Theta(phi_hat); the confidence set extends beyond it",
       recommendation = "report Theta(phi_hat) and the conditional prior alongside any credible set -- the credible set alone cannot be interpreted")
}

morie_bayescrediblebound <- morie_compare_sets
morie_bound_bayes_credible <- morie_compare_sets

# house entry point: the package exports one morie_<module>
morie_bndbye <- morie_compare_sets
