# Contact tracing and isolation: when is it enough on its own?
# Sources: Hellewell, J., Abbott, S., Gimma, A., Bosse, N. I.,
# Jarvis, C. I., Russell, T. W., Munday, J. D., Kucharski, A. J.,
# Edmunds, W. J., Centre for the Mathematical Modelling of Infectious
# Diseases COVID-19 Working Group, Funk, S. & Eggo, R. M. (2020)
# "Feasibility of controlling COVID-19 outbreaks by isolation of
# cases and contacts", The Lancet Global Health 8, e488-e496.
# Section "Methods -- Model structure": the negative binomial
# offspring distribution, serial-interval assignment, the rule that
# secondary cases arise only before the infector's isolation, initial
# outbreak sizes of 5/20/40, isolation assumed 100% effective, and
# the 100%/90% symptomatic split. Lloyd-Smith, J. O., Schreiber, S.
# J., Kopp, P. E. & Getz, W. M. (2005) "Superspreading and the
# effect of individual variation on disease emergence", Nature
# 438(7066), 355-359, doi:10.1038/nature04153. The negative binomial
# offspring parameterisation with dispersion k that this model uses.

.EPS <- 1e-12

.gamma_draw <- function(shape, scale, e) {
  a <- as.numeric(shape)
  if (a < 1.0) {
    u <- max(.ghc_unif(e, 1L), 1e-300)
    return(.gamma_draw(a + 1.0, scale, e) * (u ^ (1.0 / a)))
  }
  d <- a - 1.0 / 3.0
  c1 <- 1.0 / sqrt(9.0 * d)
  repeat {
    x <- .ghc_norm(e, 1L)
    v <- (1.0 + c1 * x) ^ 3
    if (v <= 0.0) next
    u <- max(.ghc_unif(e, 1L), 1e-300)
    if (log(u) < 0.5 * x * x + d - d * v + d * log(v))
      return(d * v * as.numeric(scale))
  }
}

.poisson_draw <- function(lam, e) {
  lm <- as.numeric(lam)
  if (lm <= 0.0) return(0L)
  if (lm > 500.0) {
    z <- .ghc_norm(e, 1L)
    return(max(0L, as.integer(round(lm + sqrt(lm) * z))))
  }
  L <- exp(-lm)
  n <- 0L
  p <- 1.0
  repeat {
    p <- p * max(.ghc_unif(e, 1L), 1e-300)
    if (p <= L) return(n)
    n <- n + 1L
    if (n > 100000L) return(n)
  }
}

negbinom_offspring <- function(R0, dispersion, e) {
  r0 <- as.numeric(R0)
  kk <- as.numeric(dispersion)
  if (r0 < 0.0)
    stop("ttrace: R0 must be non-negative, got ", format(R0))
  if (kk <= 0.0)
    stop("ttrace: the dispersion k must be positive, got ",
         format(dispersion))
  if (r0 <= .EPS) return(0L)
  if (kk > 1e6) {
    lam <- r0
  } else {
    lam <- .gamma_draw(kk, r0 / kk, e)
  }
  .poisson_draw(lam, e)
}

serial_interval_draw <- function(mean, sd, e, allow_presymptomatic = TRUE) {
  m <- as.numeric(mean)
  s <- as.numeric(sd)
  if (s <= 0.0)
    stop("ttrace: the serial-interval sd must be positive")
  v <- m + s * .ghc_norm(e, 1L)
  if (!allow_presymptomatic)
    return(max(v, 0.0))
  v
}

simulate_outbreak <- function(R0 = 2.5, dispersion = 0.16,
                              n_initial = 20, trace_prob = 0.8,
                              delay_mean = 3.83, delay_sd = 2.4,
                              si_mean = 4.7, si_sd = 2.9,
                              subclinical = 0.0, max_cases = 5000,
                              max_weeks = 12, seed = 0,
                              allow_presymptomatic = TRUE) {
  e <- .ghc_rng(as.numeric(seed))
  if (! (0.0 <= as.numeric(trace_prob) && as.numeric(trace_prob) <= 1.0))
    stop("ttrace: trace_prob must lie in [0, 1], got ",
         format(trace_prob))
  if (! (0.0 <= as.numeric(subclinical) && as.numeric(subclinical) <= 1.0))
    stop("ttrace: subclinical must lie in [0, 1], got ",
         format(subclinical))
  if (as.integer(n_initial) < 1L)
    stop("ttrace: need at least one initial case")
  horizon <- as.numeric(max_weeks) * 7.0

  active <- vector("list", as.integer(n_initial))
  for (i in seq_len(as.integer(n_initial))) {
    sub <- .ghc_unif(e, 1L) < as.numeric(subclinical)
    iso <- if (sub) Inf else
      max(0.0, as.numeric(delay_mean)
          + as.numeric(delay_sd) * .ghc_norm(e, 1L))
    active[[i]] <- list(t_inf = 0.0, t_iso = iso, sub = sub)
  }
  total <- as.integer(n_initial)
  weekly <- rep(0L, as.integer(max_weeks) + 1L)
  weekly[1L] <- as.integer(n_initial)
  hit_cap <- FALSE

  while (length(active) > 0L) {
    nxt <- list()
    for (case in active) {
      t_inf <- case$t_inf
      t_iso <- case$t_iso
      n_off <- negbinom_offspring(R0, dispersion, e)
      for (j in seq_len(n_off)) {
        si <- serial_interval_draw(si_mean, si_sd, e,
                                   allow_presymptomatic = allow_presymptomatic)
        t_new <- t_inf + si
        if (t_new < t_inf) next
        if (t_new >= t_iso) next
        if (t_new > horizon) next
        sub <- .ghc_unif(e, 1L) < as.numeric(subclinical)
        traced <- (!sub) && (.ghc_unif(e, 1L) < as.numeric(trace_prob))
        if (sub) {
          iso_new <- Inf
        } else if (traced) {
          iso_new <- max(t_new, t_iso)
        } else {
          iso_new <- t_new + max(0.0, as.numeric(delay_mean)
                                 + as.numeric(delay_sd)
                                 * .ghc_norm(e, 1L))
        }
        nxt[[length(nxt) + 1L]] <- list(t_inf = t_new, t_iso = iso_new,
                                        sub = sub)
        total <- total + 1L
        wk <- as.integer(floor(t_new / 7.0))
        if (wk >= 0L && wk <= as.integer(max_weeks))
          weekly[wk + 1L] <- weekly[wk + 1L] + 1L
        if (total > as.integer(max_cases)) {
          hit_cap <- TRUE
          break
        }
      }
      if (hit_cap) break
    }
    if (hit_cap) break
    active <- nxt
  }

  controlled <- (!hit_cap) && (length(active) == 0L)
  list(controlled = controlled, total_cases = total,
       weekly = as.integer(weekly), hit_cap = hit_cap,
       extinct = length(active) == 0L)
}

probability_of_control <- function(reps = 200, seed = 0, ...) {
  ok <- 0L
  sizes <- integer(as.integer(reps))
  args <- list(...)
  for (r in seq_len(as.integer(reps))) {
    args$seed <- as.integer(seed) * 7919L + r - 1L
    out <- do.call(simulate_outbreak, args)
    ok <- ok + (1L * out$controlled)
    sizes[r] <- out$total_cases
  }
  p <- ok / as.numeric(reps)
  se <- sqrt(max(p * (1.0 - p), 0.0) / as.integer(reps))
  sizes_sorted <- sort(sizes)
  list(estimate = p, probability_of_control = p, se = se,
       reps = as.integer(reps),
       median_size = sizes_sorted[floor(length(sizes_sorted) / 2) + 1L],
       max_size = sizes_sorted[length(sizes_sorted)],
       max_cases = if (!is.null(args$max_cases)) as.integer(args$max_cases)
         else 5000L,
       max_weeks = if (!is.null(args$max_weeks)) as.integer(args$max_weeks)
         else 12L,
       definition = paste0("extinct within max_weeks without ",
                           "exceeding max_cases; both change the answer"),
       method = paste0("branching-process simulation, Hellewell ",
                       "et al. (2020) Methods"))
}

effective_reproduction_number <- function(R0, si_mean, si_sd, delay_mean,
                                          delay_sd, trace_prob,
                                          subclinical = 0.0, draws = 20000,
                                          seed = 0) {
  e <- .ghc_rng(as.numeric(seed))
  hit <- 0L
  for (i in seq_len(as.integer(draws))) {
    if (.ghc_unif(e, 1L) < as.numeric(subclinical)) {
      hit <- hit + 1L
      next
    }
    traced <- .ghc_unif(e, 1L) < as.numeric(trace_prob)
    t_iso <- if (traced) 0.0
      else max(0.0, as.numeric(delay_mean)
               + as.numeric(delay_sd) * .ghc_norm(e, 1L))
    si <- as.numeric(si_mean) + as.numeric(si_sd) * .ghc_norm(e, 1L)
    if (si < t_iso) hit <- hit + 1L
  }
  frac <- hit / as.numeric(draws)
  list(R_eff = as.numeric(R0) * frac, R0 = as.numeric(R0),
       fraction_before_isolation = frac,
       controlled_in_expectation = as.numeric(R0) * frac < 1.0,
       note = paste0("a traced contact is quarantined when its ",
                     "infector is isolated, so its own transmission ",
                     "window is measured from that point"))
}

.ttrace_cheatsheet <- function() {
  paste0("ttrace: branching process. Offspring ~ NegBinom(mean R0, ",
         "dispersion k), variance R0(1 + R0/k) -- overdispersion ",
         "matters because small k means most chains die alone. A ",
         "secondary case exists ONLY if the infector was not yet ",
         "isolated. So the lever is the fraction of the serial ",
         "interval falling before isolation, which is why ",
         "PRESYMPTOMATIC transmission decides feasibility. ",
         "Subclinical cases are never isolated at all -- a hard ",
         "ceiling no amount of tracing clears.")
}

contacttracingyield <- probability_of_control
contact_tracing_yield <- probability_of_control

morie_ttrace <- function(reps = 200, seed = 0, ...) {
  probability_of_control(reps = reps, seed = seed, ...)
}
