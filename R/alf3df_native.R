# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of alf3df -- AlphaFold-3 style diffusion sampling step on atom
# coordinates. Mirrors src/morie/fn/alf3df.py operation for operation,
# on the shared numerics in R/aaa_helpers_w3num.R and the matched random
# stream in R/aaa_helpers_ghc_rng.R.
#
# A structure-prediction diffusion model does not predict coordinates in
# one shot. It starts from noise at a large scale and walks down a
# schedule of decreasing noise levels, and at each level it asks a
# denoiser "given this rattled structure, what was the clean one?" and
# steps part of the way there.
#
# The update is the EDM sampler, written in the form where the denoiser
# D(x, sigma) returns an estimate of the CLEAN coordinates rather than
# of the noise. Then d = (x - D(x, sigma)) / sigma is the direction of
# increasing noise, and stepping along it by (sigma_next - sigma) walks
# down the schedule. That parameterisation has a property worth stating
# because it is the module's strongest check: if the denoiser is an
# oracle that returns the true structure, then a point at noise level
# sigma lands at EXACTLY noise level sigma_next, and at the end of the
# schedule, where sigma is zero, it lands exactly on the answer. Not
# approximately -- exactly, in floating point.
#
# Churn is the stochastic part. With the churn parameter at zero the
# sampler is fully deterministic and consumes no randomness at all,
# which is worth being able to check.
#
# The AlphaFold-3 addition is CENTRE RANDOM AUGMENTATION: before each
# step the coordinates are recentred on their own centroid and given a
# random rotation. Structure prediction should not care where in space
# the molecule sits or how it is oriented, and augmenting during
# sampling is how that invariance is enforced at inference rather than
# merely hoped for. Centring makes the centroid exactly zero and
# rotation preserves every interatomic distance exactly, and both are
# checked.
#
# On constants. The AlphaFold-3 paper describes the sampler in the main
# text but puts its hyperparameters in Supplementary Methods 3.7, which
# is not in hand. So the schedule shape follows Karras et al., whose
# formula is published in full, and every AlphaFold-3-specific constant
# is a PARAMETER with no default pretending to be theirs. A module that
# invented gamma0 and called it AlphaFold's would be worse than one that
# asks.
#
# References
#   Abramson, J., Adler, J., Dunger, J., Evans, R., Green, T., Pritzel,
#     A., Ronneberger, O., Willmore, L., Ballard, A.J., Bambrick, J. et
#     al. (2024) "Accurate structure prediction of biomolecular
#     interactions with AlphaFold 3." Nature 630(8016), 493-500.
#     doi:10.1038/s41586-024-07487-w.
#   Karras, T., Aittala, M., Aila, T. and Laine, S. (2022) "Elucidating
#     the design space of diffusion-based generative models." Advances
#     in Neural Information Processing Systems 35, 26565-26577.
#     arXiv:2206.00364. Equation 5 and Algorithm 2.
#   Song, Y. et al. (2021) "Score-based generative modeling through
#     stochastic differential equations." ICLR.
#   Shoemake, K. (1992) "Uniform random rotations." Graphics Gems III,
#     124-132.

.ALF3DF_ORDERS <- c("heun", "euler")

#' The EDM noise schedule, Karras et al. equation 5
#'
#' rho controls how much of the schedule is spent at low noise: larger
#' puts more steps near the end, where the structure is actually being
#' decided.
#'
#' @param n_steps The number of noise levels before the final zero.
#' @param sigma_min The smallest positive level.
#' @param sigma_max The starting level.
#' @param rho The schedule exponent.
#' @return The levels, with a zero appended.
#' @export
morie_alf3df_schedule <- function(n_steps, sigma_min = 0.002,
                                  sigma_max = 80, rho = 7) {
  n <- as.integer(n_steps)
  if (n < 2L) stop("need at least two noise levels")
  if (sigma_min <= 0 || sigma_max <= sigma_min)
    stop("need 0 < sigma_min < sigma_max")
  a <- sigma_max^(1 / rho)
  b <- sigma_min^(1 / rho)
  c(vapply(0:(n - 1L), function(i) (a + i * (b - a) / (n - 1))^rho,
           numeric(1)), 0)
}

#' A uniform random rotation matrix by Shoemake's quaternion method
#'
#' Three uniforms in, one rotation out, with no rejection step -- so it
#' consumes exactly three draws whatever it returns and two
#' implementations stay in step.
#'
#' @param e A random stream from the shared generator.
#' @return A three by three rotation matrix.
#' @export
morie_alf3df_rotation <- function(e) {
  u1 <- .ghc_unif(e, 1L); u2 <- .ghc_unif(e, 1L); u3 <- .ghc_unif(e, 1L)
  s1 <- sqrt(1 - u1); s2 <- sqrt(u1)
  t1 <- 2 * pi * u2; t2 <- 2 * pi * u3
  x <- s1 * sin(t1); y <- s1 * cos(t1)
  z <- s2 * sin(t2); w <- s2 * cos(t2)
  matrix(c(1 - 2 * (y * y + z * z), 2 * (x * y - z * w), 2 * (x * z + y * w),
           2 * (x * y + z * w), 1 - 2 * (x * x + z * z), 2 * (y * z - x * w),
           2 * (x * z - y * w), 2 * (y * z + x * w), 1 - 2 * (x * x + y * y)),
         3L, 3L, byrow = TRUE)
}

#' Recentre on the centroid, then rotate at random
#'
#' The centring is exact: the returned centroid is zero to rounding, not
#' approximately zero. The rotation preserves every interatomic
#' distance, which is the property that makes the augmentation harmless
#' to the structure and useful to the model.
#'
#' @param x A coordinate matrix, one row per atom.
#' @param e A random stream.
#' @return A list with the augmented coordinates and the old centroid.
#' @export
morie_alf3df_augment <- function(x, e) {
  n <- nrow(x)
  if (!n) return(list(x = x, centroid = c(0, 0, 0)))
  cen <- vapply(1:3, function(t) .w3_csum(x[, t]) / n, numeric(1))
  R <- morie_alf3df_rotation(e)
  out <- matrix(0, n, 3L)
  for (i in seq_len(n)) {
    cc <- x[i, ] - cen
    for (t in 1:3) out[i, t] <- .w3_dot(R[t, ], cc)
  }
  list(x = out, centroid = cen)
}

#' One step of the sampler, from noise level t down to sigma_next
#'
#' @param x Current coordinates, one row per atom, three columns.
#' @param t The current noise level.
#' @param score_fn The denoiser returning an estimate of the CLEAN
#'   coordinates. Note the parameterisation: not a score, not a noise
#'   prediction.
#' @param sigma_next The level to step to; zero when NULL.
#' @param gamma Churn. Zero is deterministic.
#' @param noise_scale Scale on the injected noise. The AlphaFold-3
#'   values live in Supplementary Methods 3.7 and are not reproduced
#'   here, so this defaults to one -- the plain EDM sampler.
#' @param step_scale Scale on the step itself, likewise.
#' @param order "heun" applies the second-order correction.
#' @param e A random stream, needed for churn or augmentation.
#' @param augment Whether to centre and rotate first.
#' @return A list with the stepped coordinates, the churned level and
#'   the direction.
#' @export
morie_alf3df_step <- function(x, t, score_fn, sigma_next = NULL,
                              gamma = 0, noise_scale = 1,
                              step_scale = 1, order = "heun", e = NULL,
                              augment = FALSE) {
  if (!(order %in% .ALF3DF_ORDERS))
    stop("order must be one of ", paste(.ALF3DF_ORDERS, collapse = ", "))
  t <- as.numeric(t)
  if (t <= 0) stop("the current noise level must be positive")
  sn <- if (is.null(sigma_next)) 0 else as.numeric(sigma_next)
  if (sn < 0 || sn > t) stop("the next level must lie in [0, t]")
  cur <- as.matrix(x); storage.mode(cur) <- "double"
  if (augment) {
    if (is.null(e)) stop("the augmentation needs a random stream")
    cur <- morie_alf3df_augment(cur, e)$x
  }

  that <- t * (1 + as.numeric(gamma))
  if (gamma > 0) {
    if (is.null(e)) stop("churn needs a random stream")
    amt <- sqrt(that * that - t * t) * as.numeric(noise_scale)
    for (i in seq_len(nrow(cur))) for (c0 in 1:3)
      cur[i, c0] <- cur[i, c0] + amt * .ghc_norm(e, 1L)
  }

  den <- as.matrix(score_fn(cur, that))
  d <- (cur - den) / that
  dt <- (sn - that) * as.numeric(step_scale)
  nxt <- cur + dt * d
  if (order == "heun" && sn > 0) {
    # The second-order correction: average the direction at the start
    # and the end of the step. It costs one more denoiser call and is
    # what makes the sampler second order rather than first.
    den2 <- as.matrix(score_fn(nxt, sn))
    d2 <- (nxt - den2) / sn
    nxt <- cur + dt * 0.5 * (d + d2)
  }
  list(x = nxt, sigma_hat = that, direction = d)
}

#' Run the whole schedule from noise to a structure
#'
#' @param shape_n The number of atoms.
#' @param score_fn The denoiser.
#' @param n_steps Schedule length.
#' @param sigma_min The smallest positive level.
#' @param sigma_max The starting level.
#' @param rho The schedule exponent.
#' @param gamma Churn.
#' @param noise_scale Scale on the injected noise.
#' @param step_scale Scale on the step.
#' @param order A member of the order list.
#' @param seed The random stream.
#' @param augment Whether to centre and rotate at each step.
#' @return A list with the final coordinates, the schedule and the
#'   churned levels visited.
#' @export
morie_alf3df_sample <- function(shape_n, score_fn, n_steps = 8L,
                                sigma_min = 0.002, sigma_max = 80,
                                rho = 7, gamma = 0, noise_scale = 1,
                                step_scale = 1, order = "heun", seed = 0,
                                augment = FALSE) {
  sig <- morie_alf3df_schedule(n_steps, sigma_min, sigma_max, rho)
  e <- .ghc_rng(seed)
  n <- as.integer(shape_n)
  x <- matrix(0, n, 3L)
  for (i in seq_len(n)) for (c0 in 1:3)
    x[i, c0] <- sig[1] * .ghc_norm(e, 1L)
  traj <- numeric(0)
  for (i in seq_len(length(sig) - 1L)) {
    r <- morie_alf3df_step(x, sig[i], score_fn, sig[i + 1L], gamma,
                           noise_scale, step_scale, order, e, augment)
    x <- r$x
    traj <- c(traj, r$sigma_hat)
  }
  list(x = x, schedule = sig, trajectory = traj)
}

#' The ledger entry point: one step, reported richly
#'
#' @param x Current coordinates.
#' @param t The current noise level.
#' @param score_fn The denoiser.
#' @param ... Passed to the step function.
#' @return A list with the stepped coordinates and summary geometry.
#' @export
morie_alf3df <- function(x, t, score_fn, ...) {
  r <- morie_alf3df_step(x, t, score_fn, ...)
  nx <- r$x
  n <- nrow(nx)
  cen <- vapply(1:3, function(c0) .w3_csum(nx[, c0]) / n, numeric(1))
  # One flat compensated sum in atom-major, coordinate-minor order --
  # the same order the Python arm accumulates in. Summing per atom and
  # then summing those totals groups the arithmetic differently and the
  # two arms would part company in the last bit.
  flat <- numeric(0)
  if (n) for (i in seq_len(n)) for (c0 in 1:3)
    flat <- c(flat, (nx[i, c0] - cen[c0]) * (nx[i, c0] - cen[c0]))
  rad <- if (n) sqrt(.w3_csum(flat) / n) else NaN
  list(x = nx, direction = r$direction, sigma_hat = r$sigma_hat,
       sigma = as.numeric(t), centroid = cen, radius_of_gyration = rad,
       estimate = rad, se = NaN, n_atoms = n,
       method = "AlphaFold-3 style diffusion sampling step")
}

#' One-line summary of the alf3df module
#'
#' @return A character scalar.
#' @export
morie_alf3df_cheatsheet <- function()
  paste0("alf3df: AlphaFold-3 style diffusion step. orders ",
         paste(.ALF3DF_ORDERS, collapse = ", "),
         "; Karras schedule and update, centre-random augmentation, ",
         "AF3 constants supplied by the caller")
