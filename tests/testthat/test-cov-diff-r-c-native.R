# Coverage for DiffRec (Wang et al. 2023) on the DDPM machinery of Ho et
# al. (2020): the scaled linear beta schedule and its cumulative product,
# the closed-form forward corruption (mean sqrt(abar) x0, variance
# 1 - abar, uniform noise of matching variance), the posterior-mean
# coefficients, the importance weights and the reverse chain. The
# morie_diffRC_* functions and the restored short-name copies must agree.

test_that("noise schedule: beta_t = s (bmin + (bmax - bmin)(t-1)/(T-1)), abar = cumprod(1 - beta)", {
  s <- morie_diffRC_noise_schedule(5, scale = 0.5)
  b <- 0.5 * (1e-4 + (0.02 - 1e-4) * (0:4) / 4)
  expect_equal(s$beta, b, tolerance = 1e-12)
  expect_equal(s$alpha_bar, cumprod(1 - b), tolerance = 1e-12)
  expect_equal(s$signal_retained, prod(1 - b), tolerance = 1e-12)
  expect_equal(noise_schedule(5, scale = 0.5)[1:5], s[1:5], tolerance = 1e-12)
  expect_identical(morie_diffRC, morie_diffRC_noise_schedule)
  expect_equal(morie_diffRC_noise_schedule(1)$beta, 0.001 * 1e-4)
  expect_identical(morie_diffRC_noise_schedule(3, scale = 0)$alpha_bar, c(1, 1, 1))
  expect_error(morie_diffRC_noise_schedule(0), "at least 1")
  expect_error(noise_schedule(3, scale = -1), "cannot be negative")
})

test_that("forward corruption has mean sqrt(abar) x0 and uniform noise of sd sqrt(1 - abar)", {
  x0 <- c(1, 0, 0, 1, 1)
  d <- morie_diffRC_forward_corrupt(x0, 0.64)
  expect_equal(d$x_t, 0.8 * x0, tolerance = 1e-12)
  expect_equal(d$std, 0.6, tolerance = 1e-12)
  expect_false(d$sampled)
  r <- morie_diffRC_forward_corrupt(x0, 0.64, rng = .ghc_rng(4))
  u <- .ghc_unif(.ghc_rng(4), 5)
  expect_equal(r$x_t, 0.8 * x0 + 0.6 * (2 * u - 1) * sqrt(3), tolerance = 1e-12)
  expect_true(r$sampled)
  expect_equal(forward_corrupt(x0, 0.64, e = .ghc_rng(4))$x_t, r$x_t, tolerance = 1e-12)
  expect_false(morie_diffRC_forward_corrupt(x0, 1, rng = .ghc_rng(4))$sampled)
  expect_error(morie_diffRC_forward_corrupt(x0, 1.2), "must lie in \\[0,1\\]")
  expect_error(forward_corrupt(x0, -0.1), "must lie in \\[0,1\\]")
})

test_that("posterior mean uses the DDPM coefficients", {
  xt <- c(0.3, -0.2, 0.9)
  x0 <- c(1, 0, 1)
  ab <- 0.9
  abp <- 0.95
  b <- 1 - ab / abp
  p <- morie_diffRC_posterior_mean(xt, x0, ab, abp, b)
  c0 <- sqrt(abp) * b / (1 - ab)
  ct <- sqrt(1 - b) * (1 - abp) / (1 - ab)
  expect_equal(c(p$coef_x0, p$coef_xt), c(c0, ct), tolerance = 1e-12)
  expect_equal(p$mean, c0 * x0 + ct * xt, tolerance = 1e-12)
  # the two coefficients reproduce x0 when x_t sits at its forward mean
  expect_equal(morie_diffRC_posterior_mean(sqrt(ab) * x0, x0, ab, abp, b)$mean, sqrt(abp) * x0, tolerance = 1e-12)
  expect_equal(posterior_mean(xt, x0, ab, abp, b)$mean, p$mean, tolerance = 1e-12)
  dg <- morie_diffRC_posterior_mean(xt, x0, 1, 1, 0)
  expect_true(dg$degenerate)
  expect_identical(dg$mean, x0)
  expect_error(morie_diffRC_posterior_mean(xt, x0[-1], ab, abp, b), "differ in length")
})

test_that("importance weights are (sqrt(L) + s) normalised, ESS = 1 / sum p^2", {
  L <- c(4, 1, 0, 9)
  w <- morie_diffRC_importance_weights(L, smoothing = 0.5)
  p <- (sqrt(L) + 0.5) / sum(sqrt(L) + 0.5)
  expect_equal(w$weights, p, tolerance = 1e-12)
  expect_equal(w$effective_steps, 1 / sum(p^2), tolerance = 1e-12)
  expect_equal(importance_weights(L, smoothing = 0.5)$weights, p, tolerance = 1e-12)
  u <- morie_diffRC_importance_weights(L, uniform = TRUE)
  expect_equal(u$weights, rep(0.25, 4))
  expect_identical(u$effective_steps, 4)
  expect_error(morie_diffRC_importance_weights(numeric(0)), "no per-step losses")
  expect_error(importance_weights(c(1, -1)), "cannot be negative")
})

test_that("the reverse chain applies the posterior mean from t_start down to 0", {
  s <- morie_diffRC_noise_schedule(4, scale = 5)
  x0 <- c(1, 0, 1, 0)
  model <- function(x, t) pmin(pmax(x, 0), 1)
  d <- morie_diffRC_denoise(c(0.7, 0.2, 0.9, -0.1), model, s)
  x <- c(0.7, 0.2, 0.9, -0.1)
  path <- list()
  for (t in 3:0) {
    abp <- if (t > 0) s$alpha_bar[t] else 1
    ab <- s$alpha_bar[t + 1]
    b <- s$beta[t + 1]
    x <- sqrt(abp) * b / (1 - ab) * model(x, t) + sqrt(1 - b) * (1 - abp) / (1 - ab) * x
    path[[length(path) + 1]] <- x
  }
  expect_equal(d$estimate, x, tolerance = 1e-12)
  expect_equal(d$path, path, tolerance = 1e-12)
  expect_identical(d$steps, 4L)
  expect_equal(denoise(c(0.7, 0.2, 0.9, -0.1), model, s)$estimate, x, tolerance = 1e-12)
  expect_identical(morie_diffRC_denoise(x0, model, s, t_start = 1)$steps, 2L)
  # scale 0: every step is degenerate and returns the model's x0 estimate
  z <- morie_diffRC_denoise(c(0.5, 2), model, morie_diffRC_noise_schedule(3, scale = 0))
  expect_identical(z$estimate, c(0.5, 1))
  expect_identical(diffusion_rec, denoise)
  expect_identical(diffusionrec, denoise)
  expect_identical(diffusionrecommender, denoise)
  expect_error(morie_diffRC_denoise(x0, model, s, t_start = 4), "outside the schedule")
  expect_error(denoise(x0, model, s, t_start = -1), "outside the schedule")
})
