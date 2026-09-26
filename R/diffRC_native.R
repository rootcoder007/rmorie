# DiffRec: diffusion over interactions, with the noise turned down.
# Sources: Wang, W., Xu, Y., Feng, F., Lin, X., He, X. & Chua, T.-S.
# (2023) "Diffusion Recommender Model", SIGIR '23, 832-841,
# doi:10.1145/3539618.3591663, arXiv:2304.04971 (the criticism of
# GAN-based recommenders, the diffusion formulation over user
# interaction histories, the reduced noise scale in the forward
# process, and importance sampling over diffusion steps for training);
# Ho, J., Jain, A. & Abbeel, P. (2020) "Denoising Diffusion
# Probabilistic Models", NeurIPS 2020, arXiv:2006.11239 (the forward
# process and its closed form); Liang, D. et al. (2018) "Variational
# Autoencoders for Collaborative Filtering", WWW 2018, arXiv:1802.05814
# (the VAE recommender being displaced).
#
# Native implementation mirroring Python morie.fn.diffRC exactly: the
# same linear beta schedule scaled DOWN, the same closed-form forward
# corruption, the same DDPM posterior mean, the same importance
# weights, and the same reverse chain that returns its input
# unchanged at scale 0.

#' A linear noise schedule, scaled DOWN
#'
#' A linear \code{beta} schedule as in Ho et al. (2020), scaled by
#' \code{scale}. At image-diffusion scales the personal history is
#' destroyed, and with it the thing being predicted, so the scale is
#' what the module is about.
#'
#' @param T Integer number of diffusion steps.
#' @param scale Numeric scale factor on the betas.
#' @param beta_min Numeric lower bound on beta.
#' @param beta_max Numeric upper bound on beta.
#' @return A list with \code{beta}, \code{alpha_bar}, \code{T},
#'   \code{scale}, \code{signal_retained} and a note.
#' @export
#' @examples
#' morie_diffRC_noise_schedule(T = 5L)
#' @keywords internal
morie_diffRC_noise_schedule <- function(T, scale = 0.001,
                                         beta_min = 0.0001,
                                         beta_max = 0.02) {
  n <- as.integer(T)
  s <- as.numeric(scale)
  if (n < 1L) stop("diffRC: T must be at least 1")
  if (s < 0) stop("diffRC: the noise scale cannot be negative")
  betas <- numeric(n)
  abar <- numeric(n)
  acc <- 1
  denom <- max(n - 1L, 1L)
  for (t in seq_len(n)) {
    b <- s * (beta_min + (beta_max - beta_min) * (t - 1L) / denom)
    betas[t] <- b
    acc <- acc * (1 - b)
    abar[t] <- acc
  }
  list(beta = betas, alpha_bar = abar, T = n, scale = s,
       signal_retained = abar[n],
       note = paste("scale = 0 leaves alpha_bar at 1, so the forward",
                    "process is the identity"))
}

#' Forward corruption
#'
#' Closed form, so no simulation is needed to check it.
#'
#' @param x0 Numeric vector.
#' @param alpha_bar_t Numeric.
#' @param rng Optional generator environment.
#' @return A list with \code{x_t}, \code{mean}, \code{std} and
#'   \code{sampled}.
#' @export
#' @examples
#' rng <- rmorie:::.ghc_rng(1)
#' r <- morie_diffRC_forward_corrupt(x0 = c(1, -1, 0.5),
#'                                   alpha_bar_t = 0.7, rng = rng)
#' str(r, max.level = 1)
#' @keywords internal
morie_diffRC_forward_corrupt <- function(x0, alpha_bar_t, rng = NULL) {
  x <- as.numeric(x0)
  ab <- as.numeric(alpha_bar_t)
  if (ab < 0 || ab > 1)
    stop(sprintf("diffRC: alpha_bar must lie in [0,1], got %s",
                 format(ab)))
  sm <- sqrt(ab)
  sv <- sqrt(max(1 - ab, 0))
  mn <- sm * x
  if (is.null(rng) || sv <= 1e-12)
    return(list(x_t = mn, mean = mn, std = sv, sampled = FALSE))
  u <- .ghc_unif(rng, length(x))
  xt <- mn + sv * (2 * u - 1) * sqrt(3)
  list(x_t = xt, mean = mn, std = sv, sampled = TRUE)
}

#' DDPM posterior mean
#'
#' @param x_t Numeric vector.
#' @param x0_hat Numeric vector, the model's estimate of x0.
#' @param alpha_bar_t Numeric.
#' @param alpha_bar_prev Numeric.
#' @param beta_t Numeric.
#' @return A list with the mean and the two coefficients.
#' @export
#' @examples
#' r <- morie_diffRC_posterior_mean(x_t = c(0.8, -0.9), x0_hat = c(1, -1),
#'                                  alpha_bar_t = 0.7, alpha_bar_prev = 0.8,
#'                                  beta_t = 0.1)
#' str(r, max.level = 1)
#' @keywords internal
morie_diffRC_posterior_mean <- function(x_t, x0_hat, alpha_bar_t,
                                        alpha_bar_prev, beta_t) {
  xt <- as.numeric(x_t)
  x0 <- as.numeric(x0_hat)
  if (length(xt) != length(x0))
    stop("diffRC: x_t and the estimate of x_0 differ in length")
  ab <- as.numeric(alpha_bar_t)
  abp <- as.numeric(alpha_bar_prev)
  b <- as.numeric(beta_t)
  denom <- 1 - ab
  if (denom <= 1e-12)
    return(list(mean = x0, degenerate = TRUE,
                note = "alpha_bar = 1: nothing was added, so nothing is removed"))
  c0 <- sqrt(max(abp, 0)) * b / denom
  ct <- sqrt(max(1 - b, 0)) * (1 - abp) / denom
  list(mean = c0 * x0 + ct * xt, coef_x0 = c0, coef_xt = ct,
       degenerate = FALSE)
}

#' Importance-sample the timesteps that actually carry loss
#'
#' Uniform sampling spends most of its budget where the model is
#' already right.
#'
#' @param step_losses Numeric vector of per-step losses.
#' @param uniform Logical, return uniform weights.
#' @param smoothing Numeric additive smoothing.
#' @return A list with \code{weights} and \code{effective_steps}.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_diffRC_importance_weights(V)
#' @keywords internal
morie_diffRC_importance_weights <- function(step_losses, uniform = FALSE,
                                             smoothing = 0.1) {
  L <- as.numeric(step_losses)
  if (length(L) == 0L) stop("diffRC: no per-step losses given")
  if (any(L < 0)) stop("diffRC: a loss cannot be negative")
  n <- length(L)
  if (isTRUE(uniform))
    return(list(weights = rep(1 / n, n), uniform = TRUE,
                effective_steps = as.numeric(n)))
  s <- as.numeric(smoothing)
  w <- sqrt(L) + s
  z <- sum(w)
  p <- w / z
  eff <- 1 / sum(p ^ 2)
  list(weights = p, uniform = FALSE, effective_steps = eff,
       note = paste("effective sample size falls as the loss",
                    "concentrates, which is the intended behaviour"))
}

#' Run the reverse chain from t back to 0
#'
#' With \code{scale = 0} the schedule is the identity and this
#' returns its input unchanged.
#'
#' @param x_t Numeric vector.
#' @param model A function \code{(x, t)} returning a numeric vector.
#' @param schedule A list as returned by \code{noise_schedule}.
#' @param t_start Optional integer start time.
#' @return A list with the denoised estimate, the path and the
#'   schedule summary.
#' @export
#' @examples
#' sched <- morie_diffRC_noise_schedule(10L)
#' model <- function(x, t) x * 0.9
#' r <- morie_diffRC_denoise(c(0.5, -0.3, 0.2), model, sched)
#' str(r, max.level = 1)
#' @keywords internal
morie_diffRC_denoise <- function(x_t, model, schedule, t_start = NULL) {
  x <- as.numeric(x_t)
  ab <- schedule$alpha_bar
  beta <- schedule$beta
  T <- schedule$T
  t <- if (is.null(t_start)) T - 1L else as.integer(t_start)
  if (t < 0L || t >= T)
    stop("diffRC: t is outside the schedule")
  path <- list()
  while (t >= 0L) {
    x0h <- as.numeric(model(x, t))
    prev <- if (t > 0L) ab[t] else 1
    pm <- morie_diffRC_posterior_mean(x, x0h, ab[t + 1L], prev, beta[t + 1L])
    x <- pm$mean
    path[[length(path) + 1L]] <- x
    t <- t - 1L
  }
  list(estimate = x, x0 = x, path = path, steps = length(path),
       signal_retained = schedule$signal_retained,
       method = "diffusion over interaction histories; Wang et al. (2023)",
       note = paste("the noise scale is REDUCED so the personalised",
                    "history survives the forward process"))
}

# house entry point: the package exports one morie_<module>
morie_diffRC <- morie_diffRC_noise_schedule

# -- restored: morie-only definition kept through the rmorie sync --
#' Reverse chain
#'
#' Walks the schedule from \code{t} back to 0. With \code{scale = 0}
#' the schedule is the identity and this returns its input unchanged.
#'
#' @param x_t Noisy state to start from.
#' @param model Function \code{(x, t)} returning the model's
#'   \code{x_0} estimate.
#' @param schedule Schedule list from \code{noise_schedule}.
#' @param t_start Starting timestep; defaults to the last step.
#' @return A list mirroring the Python \code{RichResult} payload.
#' @export
#' @aliases diffusion_rec diffusionrec diffusionrecommender
denoise <- function(x_t, model, schedule, t_start = NULL) {
  x <- as.numeric(x_t)
  ab <- schedule$alpha_bar
  beta <- schedule$beta
  T <- as.integer(schedule$T)
  t <- if (is.null(t_start)) T - 1L else as.integer(t_start)
  if (t < 0L || t >= T) stop("diffRC: t is outside the schedule")
  path <- list()
  while (t >= 0L) {
    x0h <- as.numeric(model(x, t))
    prev <- if (t > 0L) ab[t] else 1
    pm <- posterior_mean(x, x0h, ab[t + 1L], prev, beta[t + 1L])
    x <- pm$mean
    path[[length(path) + 1L]] <- x
    t <- t - 1L
  }
  list(estimate = x, x0 = x, path = path, steps = length(path),
       signal_retained = schedule$signal_retained,
       method = "diffusion over interaction histories; Wang et al. (2023)",
       note = paste("the noise scale is REDUCED so the personalised",
                    "history survives the forward process"))
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Forward corruption (closed form)
#'
#' Mean \code{sqrt(alpha_bar_t) * x_0}, variance \code{1 - alpha_bar_t}.
#' No simulation is needed to check it.
#'
#' @param x0 Original history.
#' @param alpha_bar_t \code{alpha_bar} at time \code{t}.
#' @param e Shared generator environment. If \code{NULL} or
#'   \code{std == 0}, no sample is drawn.
#' @return A list with \code{x_t}, \code{mean}, \code{std},
#'   \code{sampled}.
#' @export
forward_corrupt <- function(x0, alpha_bar_t, e = NULL) {
  x <- as.numeric(x0)
  ab <- as.numeric(alpha_bar_t)
  if (!(0 <= ab && ab <= 1))
    stop(sprintf("diffRC: alpha_bar must lie in [0,1], got %g", ab))
  sm <- sqrt(ab)
  sv <- sqrt(max(1 - ab, 0))
  mean_v <- sm * x
  if (is.null(e) || sv <= .ghc_DIFFRC_EPS)
    return(list(x_t = mean_v, mean = mean_v, std = sv, sampled = FALSE))
  u <- .ghc_unif(e, length(x))
  xt <- mean_v + sv * (2 * u - 1) * sqrt(3)
  list(x_t = xt, mean = mean_v, std = sv, sampled = TRUE)
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Importance weights over diffusion steps
#'
#' Sample the steps that actually carry loss instead of drawing
#' uniformly.
#'
#' @param step_losses Numeric per-step losses.
#' @param uniform Use uniform weights.
#' @param smoothing Additive constant in the square-root.
#' @return A list with \code{weights}, \code{uniform},
#'   \code{effective_steps} (and \code{note} for the importance path).
#' @export
importance_weights <- function(step_losses, uniform = FALSE,
                                smoothing = 0.1) {
  L <- as.numeric(step_losses)
  if (length(L) == 0L) stop("diffRC: no per-step losses given")
  if (any(L < 0)) stop("diffRC: a loss cannot be negative")
  n <- length(L)
  if (isTRUE(uniform))
    return(list(weights = rep(1 / n, n), uniform = TRUE,
                effective_steps = as.numeric(n)))
  s <- as.numeric(smoothing)
  w <- sqrt(L) + s
  z <- sum(w)
  p <- w / z
  eff <- 1 / sum(p^2)
  list(weights = as.numeric(p), uniform = FALSE,
       effective_steps = eff,
       note = paste("effective sample size falls as the loss",
                    "concentrates, which is the intended behaviour"))
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Linear noise schedule, scaled down
#'
#' At image-diffusion scales the personal history is destroyed. The
#' \code{scale} knob keeps it recoverable.
#'
#' @param T Number of diffusion steps.
#' @param scale Noise scale multiplier.
#' @param beta_min Lower endpoint of the unscaled linear schedule.
#' @param beta_max Upper endpoint of the unscaled linear schedule.
#' @return A list with \code{beta}, \code{alpha_bar}, \code{T},
#'   \code{scale}, \code{signal_retained}, \code{note}.
#' @export
noise_schedule <- function(T, scale = 0.001, beta_min = 0.0001,
                           beta_max = 0.02) {
  n <- as.integer(T)
  s <- as.numeric(scale)
  if (n < 1L) stop("diffRC: T must be at least 1")
  if (s < 0) stop("diffRC: the noise scale cannot be negative")
  denom <- max(n - 1L, 1L)
  betas <- s * (beta_min + (beta_max - beta_min) * ((seq_len(n) - 1L) / denom))
  abar <- cumprod(1 - betas)
  list(beta = as.numeric(betas), alpha_bar = as.numeric(abar),
       T = n, scale = s, signal_retained = abar[n],
       note = paste("scale = 0 leaves alpha_bar at 1, so the forward",
                    "process is the identity"))
}

# -- restored: morie-only definition kept through the rmorie sync --
#' DDPM posterior mean
#'
#' \code{mu(x_t, x_0_hat)} with the standard coefficients.
#'
#' @param x_t Current noisy state.
#' @param x0_hat Model estimate of \code{x_0}.
#' @param alpha_bar_t \code{alpha_bar} at \code{t}.
#' @param alpha_bar_prev \code{alpha_bar} at \code{t-1}.
#' @param beta_t \code{beta} at \code{t}.
#' @return A list with \code{mean}, \code{coef_x0}, \code{coef_xt},
#'   \code{degenerate}.
#' @export
posterior_mean <- function(x_t, x0_hat, alpha_bar_t, alpha_bar_prev,
                            beta_t) {
  xt <- as.numeric(x_t)
  x0 <- as.numeric(x0_hat)
  if (length(xt) != length(x0))
    stop("diffRC: x_t and the estimate of x_0 differ in length")
  ab <- as.numeric(alpha_bar_t)
  abp <- as.numeric(alpha_bar_prev)
  b <- as.numeric(beta_t)
  denom <- 1 - ab
  if (denom <= .ghc_DIFFRC_EPS)
    return(list(mean = x0, degenerate = TRUE,
                note = "alpha_bar = 1: nothing was added, so nothing is removed"))
  c0 <- sqrt(max(abp, 0)) * b / denom
  ct <- sqrt(max(1 - b, 0)) * (1 - abp) / denom
  list(mean = c0 * x0 + ct * xt, coef_x0 = c0, coef_xt = ct,
       degenerate = FALSE)
}

# -- restored: morie-only objects kept through the rmorie sync --
.ghc_DIFFRC_EPS <- 1e-12

diffusion_rec <- denoise

diffusionrec <- denoise

diffusionrecommender <- denoise
