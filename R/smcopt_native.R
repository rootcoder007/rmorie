# morie.fn -- function file (rootcoder007/morie)
# R arm of smcopt (smcopt, sequential_mc, smc_optimise,
# annealing_ladder).
# Sources:
#   Del Moral, P., Doucet, A. & Jasra, A. (2006) "Sequential Monte Carlo
#   samplers", JRSS-B 68(3), 411-436, section 2.3.1(c). The
#   optimisation route is one of the three sequences the paper lists
#   for {pi_n}: pi_n = pi^{phi_n} with phi_n increasing. The
#   particles anneal together and are resampled; the sampler,
#   incremental weights of equation 31, the ESS criterion and the
#   resampling schemes all live in smcsam.

annealing_ladder <- function(n_steps, phi_max = 50.0, phi_min = 0.1,
                             kind = "geometric") {
  n_steps <- as.integer(n_steps)
  if (n_steps < 2L)
    stop("smcopt: need at least two steps")
  if (phi_min <= 0 || phi_max <= phi_min)
    stop("smcopt: need 0 < phi_min < phi_max")
  if (kind == "geometric") {
    r <- (phi_max / phi_min) ^ (1.0 / (n_steps - 1L))
    return(vapply(seq_len(n_steps) - 1L,
                  function(t) phi_min * r ^ t, numeric(1)))
  }
  if (kind == "linear") {
    return(vapply(seq_len(n_steps) - 1L,
                  function(t) phi_min + (phi_max - phi_min) *
                    t / (n_steps - 1L),
                  numeric(1)))
  }
  stop("smcopt: kind must be 'geometric' or 'linear'")
}

smcopt <- function(objective, initial, n_particles = 200, n_steps = 30,
                   phi_max = 50.0, phi_min = 0.1, kind = "geometric",
                   kernel = NULL, ess_threshold = 0.5,
                   scheme = "systematic", seed = 0, maximise = TRUE) {
  sign <- if (isTRUE(maximise)) 1.0 else -1.0
  ladder <- annealing_ladder(n_steps, phi_max, phi_min, kind)
  best_v <- -Inf
  best_x <- NULL

  log_gamma <- function(x, phi) {
    v <- sign * as.numeric(objective(x))
    if (v > best_v) {
      best_v <<- v
      best_x <<- as.numeric(x)
    }
    phi * v
  }

  fit <- morie_smcsam$smcsam(log_gamma, initial,
                             n_particles = n_particles,
                             ladder = ladder, kernel = kernel,
                             ess_threshold = ess_threshold,
                             scheme = scheme, seed = seed)
  if (is.null(best_x))
    stop("smcopt: the objective was never evaluated")
  list(estimate = best_x,
       best_x = best_x,
       best_value = sign * best_v,
       particles = fit$particles,
       weights = fit$weights,
       particle_mean = fit$mean,
       ladder = ladder,
       ess_trace = fit$ess_trace,
       resampled = fit$resampled,
       accept_trace = fit$accept_trace,
       n_particles = as.integer(n_particles),
       maximise = isTRUE(maximise),
       note = paste0("annealing concentrates on the modes but cannot ",
                     "find one no particle visits; widen `initial` ",
                     "before raising phi_max"),
       method = paste0("annealed SMC optimisation (Del Moral, Doucet ",
                       "& Jasra 2006, section 2.3.1c)"))
}

.smcopt_cheatsheet <- function() {
  paste0("smcopt: SMC as a global optimiser (Del Moral, Doucet & Jasra ",
         "2006, sec 2.3.1c). Anneal pi_n = pi^phi_n with phi rising, so ",
         "the target concentrates on the modes. Unlike single-chain ",
         "simulated annealing the particles INTERACT: resampling kills ",
         "the ones in poor modes and copies the ones in good modes. ",
         "Shares the sampler, weights and resampling with smcsam.")
}

# carried-over names / compact aliases
smc_optimise <- smcopt
sequential_mc <- smcopt
sequentialmc <- smcopt

morie_smcopt <- list(smcopt = smcopt,
                     sequential_mc = sequential_mc,
                     smc_optimise = smc_optimise,
                     annealing_ladder = annealing_ladder,
                     cheatsheet = .smcopt_cheatsheet,
                     sequentialmc = sequentialmc)
