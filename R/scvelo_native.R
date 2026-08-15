# morie.fn -- function file (rootcoder007/morie)
# RNA velocity from the full splicing kinetics, not the steady state.
#
# **The kinetics.** For one gene, unspliced precursor u(t) and
# spliced mature mRNA s(t) obey
#   du/dt = alpha(t) - beta * u(t), ds/dt = beta * u(t) - gamma * s(t),
# with transcription rate alpha switching between an induction ("on")
# and a repression ("off") phase, splicing rate beta and degradation
# rate gamma. RNA velocity is ds/dt = beta*u - gamma*s; everything
# else is inference of the parameters.
#
# **Why the steady-state model is not enough.** The original approach
# reads velocity off the residual from a fitted steady-state ratio
# gamma/beta, which needs two assumptions: that the full dynamics are
# observed for each gene, so the steady states actually appear in the
# data, and that all genes share one splicing rate. Both fail on
# transient populations and on mixtures of subpopulations with
# different kinetics. steady_state_velocity implements that model,
# because it is the baseline the paper improves on, and dynamical_fit
# solves the kinetics instead.
#
# **The closed form is the point.** With tau the time since the last
# phase switch and (u0, s0) the state at the switch,
#   u(tau) = u0*exp(-beta*tau) + (alpha/beta)*(1 - exp(-beta*tau))
#   s(tau) = s0*exp(-gamma*tau) + (alpha/gamma)*(1 - exp(-gamma*tau))
#            + (alpha - beta*u0)/(gamma - beta)*(exp(-gamma*tau) - exp(-beta*tau))
# Solving explicitly is what lets an unobserved steady state still be
# inferred. The anchor holds this against a Runge-Kutta integration of
# the ODEs themselves, so a slip in the algebra fails rather than
# propagating.
#
# The gamma = beta case is a removable singularity in that expression,
# not a real one; solve_kinetics takes the limit rather than dividing
# by zero.
#
# **Inference.** Expectation-maximisation, as in the paper: in the E
# step each observation xi = (ui, si) is assigned the latent time ti
# minimising its distance to the phase trajectory, and a
# transcriptional state ki in {on, off, steady-on, steady-off} by
# likelihood on the corresponding segment; in the M step the rates
# are updated. dynamical_fit records the likelihood at every
# iteration so the monotone increase is visible.
#
# References
# ----------
# Bergen, V., Lange, M., Peidli, S., Wolf, F. A. & Theis, F. J. (2019)
# "Generalizing RNA velocity to transient cell states through dynamical
# modeling", bioRxiv 820936, doi:10.1101/820936; published as Bergen
# et al. (2020) Nature Biotechnology 38(12), 1408-1414,
# doi:10.1038/s41587-020-0591-3. The splicing ODEs reproduced above,
# the two assumptions the steady-state model needs (full dynamics
# observed per gene, one shared splicing rate) and why transient or
# heterogeneous populations violate them, the explicit solution of
# the kinetics, the latent variables (a discrete state ki and
# continuous time ti per cell), the EM scheme assigning ti by minimum
# distance to the phase trajectory and ki by segment likelihood, and
# velocity as the derivative of spliced abundance.
#
# La Manno, G., Soldatov, R., Zeisel, A., Braun, E., Hochgerner, H.,
# Petukhov, V., Lidschreiber, K., Kastriti, M. E., Lönnerberg, P.,
# Furlan, A., Fan, J., Borm, L. E., Liu, Z., van Bruggen, D., Guo, J.,
# He, X., Barker, R., Sundström, E., Castelo-Branco, G., Cramer, P.,
# Adameyko, I., Linnarsson, S. & Kharchenko, P. V. (2018) "RNA
# velocity of single cells", Nature 560(7719), 494-498,
# doi:10.1038/s41586-018-0414-6, for the steady-state model this
# generalises.

# STATES tuple (the four transcriptional phases)
.scvelo_STATES <- c("on", "off", "steady_on", "steady_off")

# The explicit solution of the splicing ODEs at time tau after a
# phase switch. The gamma = beta branch is the limit of the
# (e^-gt - e^-bt)/(g - b) term, which is t*e^-bt at g = b, not a
# genuine singularity.
morie_solve_kinetics <- function(tau, alpha, beta, gamma,
                                u0 = 0.0, s0 = 0.0) {
  if (beta <= 0) stop("scvelo: beta must be positive", call. = FALSE)
  if (gamma <= 0) stop("scvelo: gamma must be positive", call. = FALSE)
  if (alpha < 0) stop("scvelo: the transcription rate cannot be negative",
                     call. = FALSE)
  t <- as.numeric(tau)
  if (t < 0) stop("scvelo: tau cannot be negative", call. = FALSE)
  eb <- exp(-beta * t)
  eg <- exp(-gamma * t)
  u <- u0 * eb + (alpha / beta) * (1 - eb)
  if (abs(gamma - beta) < 1e-10) {
    s <- s0 * eb + (alpha / beta) * (1 - eb) -
         (alpha - beta * u0) * t * eb
  } else {
    s <- s0 * eg + (alpha / gamma) * (1 - eg) +
         (alpha - beta * u0) / (gamma - beta) * (eg - eb)
  }
  list(u = u, s = s, tau = t)
}

# nu = beta*u - gamma*s, the derivative of spliced abundance.
morie_velocity <- function(u, s, beta, gamma) {
  beta * as.numeric(u) - gamma * as.numeric(s)
}

# A gene through induction (on) then repression (off) on the closed
# form. At the switch, (u, s) lands on the steady-on value scaled by
# the (1 - exp) factor; after the switch the gene decays freely
# toward zero.
morie_simulate_gene <- function(alpha, beta, gamma, t_switch, times) {
  if (t_switch < 0) stop("scvelo: t_switch cannot be negative", call. = FALSE)
  sw <- morie_solve_kinetics(t_switch, alpha, beta, gamma)
  n <- length(times)
  out <- vector("list", n)
  for (i in seq_len(n)) {
    t <- as.numeric(times[[i]])
    if (t <= t_switch) {
      st <- morie_solve_kinetics(t, alpha, beta, gamma)
      k <- "on"
    } else {
      st <- morie_solve_kinetics(t - t_switch, 0.0, beta, gamma,
                                 sw$u, sw$s)
      k <- "off"
    }
    out[[i]] <- list(t = t, u = st$u, s = st$s, state = k,
                     velocity = morie_velocity(st$u, st$s, beta, gamma))
  }
  list(observations = out, switch = sw,
       steady_on = list(u = alpha / beta, s = alpha / gamma))
}

# The baseline model: regress gamma/beta through the origin on the
# cells assumed to be at steady state, and call the residual the
# velocity. It needs the steady states to be present in the data,
# which is the assumption the dynamical model removes.
morie_steady_state_velocity <- function(u, s, quantile = 0.95) {
  n <- length(u)
  if (n != length(s)) stop("scvelo: u and s must have the same length",
                           call. = FALSE)
  if (n < 3) stop("scvelo: need at least three cells", call. = FALSE)
  order_idx <- order(s)
  k <- max(1, as.integer(n * (1 - quantile)))
  keep <- unique(c(order_idx[1:k], order_idx[(n - k + 1):n]))
  num <- sum(u[keep] * s[keep])
  den <- sum(s[keep] * s[keep])
  if (den <= 0) stop("scvelo: the spliced counts are all zero at the fitted extremes",
                     call. = FALSE)
  ratio <- num / den
  list(
    gamma_over_beta = ratio,
    velocity = as.numeric(u) - ratio * as.numeric(s),
    n_fitted = length(keep),
    assumptions = paste("steady states observed, and one splicing",
                        "rate shared across genes"),
    method = "steady-state model; La Manno et al. (2018)"
  )
}

# E step: each observation is assigned the time on the trajectory
# closest to it, in Euclidean (u, s) distance, and the transcriptional
# state of the segment it lands on.
morie_assign_latent_time <- function(u, s, alpha, beta, gamma, t_switch,
                                     grid = 200, t_max = NULL) {
  if (is.null(t_max)) {
    t_max <- 2 * t_switch + 5 / min(beta, gamma)
  }
  ts <- t_max * (0:grid) / grid
  traj <- morie_simulate_gene(alpha, beta, gamma, t_switch, ts)$observations
  traj_t <- vapply(traj, function(p) p$t, numeric(1))
  traj_u <- vapply(traj, function(p) p$u, numeric(1))
  traj_s <- vapply(traj, function(p) p$s, numeric(1))
  traj_state <- vapply(traj, function(p) p$state, character(1))

  n <- length(u)
  out <- vector("list", n)
  for (i in seq_len(n)) {
    d2 <- (traj_u - u[i])^2 + (traj_s - s[i])^2
    j <- which.min(d2)
    out[[i]] <- list(t = traj_t[j], state = traj_state[j],
                     distance = sqrt(d2[j]))
  }
  out
}

# Private helper: residual sum of squares for the EM loop, with the
# latent assignment as a side product.
.scvelo_residual <- function(u, s, alpha, beta, gamma, t_switch,
                             grid = 200) {
  a <- morie_assign_latent_time(u, s, alpha, beta, gamma, t_switch, grid)
  rss <- sum(vapply(a, function(x) x$distance^2, numeric(1)))
  list(rss = rss, assign = a)
}

# EM over the rates and the latent variables. E step is the closest
# point on the trajectory; M step is a one-at-a-time multiplicative
# grid search on alpha, beta, gamma, t_switch. The loop stops as soon
# as no candidate improves the residual, so the rss_history is the
# visible monotone sequence.
morie_dynamical_fit <- function(u, s, alpha0 = NULL, beta0 = 1.0,
                                gamma0 = 0.5, t_switch0 = NULL,
                                n_iter = 25, grid = 120) {
  n <- length(u)
  if (n != length(s)) stop("scvelo: u and s must have the same length",
                           call. = FALSE)
  if (n < 4) stop("scvelo: need at least four cells", call. = FALSE)

  alpha <- if (is.null(alpha0)) max(u) * beta0 else as.numeric(alpha0)
  beta <- as.numeric(beta0)
  gamma <- as.numeric(gamma0)
  t_switch <- if (is.null(t_switch0)) 1.0 / beta else as.numeric(t_switch0)

  history <- numeric(0)
  for (iter in seq_len(as.integer(n_iter))) {
    res <- .scvelo_residual(u, s, alpha, beta, gamma, t_switch, grid)
    rss <- res$rss
    history <- c(history, rss)

    best_rss <- rss
    best_alpha <- alpha
    best_beta <- beta
    best_gamma <- gamma
    best_t_switch <- t_switch

    for (scale in c(0.8, 0.9, 1.1, 1.25)) {
      for (which in 1:4) {
        cand_alpha <- alpha
        cand_beta <- beta
        cand_gamma <- gamma
        cand_t_switch <- t_switch
        if (which == 1) cand_alpha <- cand_alpha * scale
        if (which == 2) cand_beta <- cand_beta * scale
        if (which == 3) cand_gamma <- cand_gamma * scale
        if (which == 4) cand_t_switch <- cand_t_switch * scale
        if (min(cand_beta, cand_gamma) <= 0 || cand_alpha < 0) next
        r2 <- .scvelo_residual(u, s, cand_alpha, cand_beta, cand_gamma,
                               cand_t_switch, grid)$rss
        if (r2 < best_rss) {
          best_rss <- r2
          best_alpha <- cand_alpha
          best_beta <- cand_beta
          best_gamma <- cand_gamma
          best_t_switch <- cand_t_switch
        }
      }
    }

    if (best_rss >= rss - 1e-12) break
    alpha <- best_alpha
    beta <- best_beta
    gamma <- best_gamma
    t_switch <- best_t_switch
  }

  final <- .scvelo_residual(u, s, alpha, beta, gamma, t_switch, grid)

  velocity_vec <- vapply(seq_len(n), function(i) {
    morie_velocity(u[i], s[i], beta, gamma)
  }, numeric(1))

  list(
    estimate = final$rss,
    alpha = alpha,
    beta = beta,
    gamma = gamma,
    t_switch = t_switch,
    rss = final$rss,
    rss_history = history,
    latent = final$assign,
    velocity = velocity_vec,
    steady_on = list(u = alpha / beta, s = alpha / gamma),
    method = paste("dynamical model by EM on the explicit kinetics;",
                   "Bergen et al. (2019)")
  )
}

# A gene-shared clock: the per-cell median of the gene times. Each
# gene has its own fit and its own rate, so without this aggregation
# the per-cell times are not comparable across genes.
morie_latent_time <- function(fits) {
  if (length(fits) == 0) stop("scvelo: no gene fits supplied", call. = FALSE)
  n <- length(fits[[1]]$latent)
  for (f in fits) {
    if (length(f$latent) != n) {
      stop("scvelo: every gene must cover the same cells", call. = FALSE)
    }
  }
  out <- numeric(n)
  for (i in seq_len(n)) {
    ts <- sort(vapply(fits, function(f) f$latent[[i]]$t, numeric(1)))
    m <- length(ts)
    out[i] <- if (m %% 2 == 1) ts[(m + 1) %/% 2] else
                0.5 * (ts[m %/% 2] + ts[m %/% 2 + 1])
  }
  list(latent_time = out, n_genes = length(fits), n_cells = n,
       note = paste("gene times coupled into one clock so rates are",
                    "comparable across genes"))
}

# One-paragraph reminder of the whole model and why each piece is
# there. The dynamical model is the answer to the two assumptions the
# steady-state model makes and that transient or heterogeneous
# populations break.
morie_cheatsheet <- function() {
  paste("scvelo: du/dt = alpha - beta u, ds/dt = beta u - gamma s,",
        "and velocity IS ds/dt. The steady-state model reads velocity",
        "off a fitted gamma/beta ratio and needs the steady states to",
        "be observed and one splicing rate shared; the dynamical model",
        "solves the kinetics in closed form and infers rates plus a",
        "per-cell latent time and state by EM, so unobserved steady",
        "states are still recovered. gamma = beta is a removable",
        "singularity, taken as a limit rather than a division by zero.")
}

# Compact alias per ledger/NAMING.md: rna_velocity is the dynamic fit.
morie_rna_velocity <- morie_dynamical_fit

# Main entry point for the module: the dynamical fit, which is the
# algorithm the paper introduces and which subsumes the steady-state
# baseline as a degenerate case.
morie_scvelo <- morie_dynamical_fit

#' @rdname morie_solve_kinetics
#' @export
morie_scvelo <- morie_solve_kinetics

#' @rdname morie_solve_kinetics
#' @export
morie_scvelo <- morie_solve_kinetics

#' @rdname morie_solve_kinetics
#' @export
morie_scvelo <- morie_solve_kinetics

#' @rdname morie_solve_kinetics
#' @export
morie_scvelo <- morie_solve_kinetics

#' @rdname morie_solve_kinetics
#' @export
morie_scvelo <- morie_solve_kinetics

#' @rdname morie_solve_kinetics
#' @export
morie_scvelo <- morie_solve_kinetics

#' @rdname morie_solve_kinetics
#' @export
morie_scvelo <- morie_solve_kinetics

#' @rdname morie_solve_kinetics
#' @export
morie_scvelo <- morie_solve_kinetics

#' @rdname morie_solve_kinetics
#' @export
morie_scvelo <- morie_solve_kinetics

#' @rdname morie_solve_kinetics
#' @export
morie_scvelo <- morie_solve_kinetics

#' @rdname morie_solve_kinetics
#' @export
morie_scvelo <- morie_solve_kinetics
