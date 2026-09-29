# RNA velocity (Bergen et al. 2020; La Manno et al. 2018): the closed-form
# splicing kinetics are checked against the ODEs by central differences,
# the steady-state baseline by its through-origin regression, and the
# dynamical EM by its monotone residual.

test_that("the closed-form kinetics solve du/dt = a - b u, ds/dt = b u - g s", {
  a <- 2
  b <- 0.8
  for (g in c(0.3, 0.8, 1.7)) {
    for (tt in c(0.4, 2.5)) {
      h <- 1e-5
      p <- morie_solve_kinetics(tt + h, a, b, g, u0 = 0.5, s0 = 1.2)
      m <- morie_solve_kinetics(tt - h, a, b, g, u0 = 0.5, s0 = 1.2)
      k <- morie_solve_kinetics(tt, a, b, g, u0 = 0.5, s0 = 1.2)
      # central differences are O(h^2) accurate, well inside 1e-8 here
      expect_equal((p$u - m$u) / (2 * h), a - b * k$u, tolerance = 1e-8)
      expect_equal((p$s - m$s) / (2 * h), b * k$u - g * k$s, tolerance = 1e-8)
    }
    z <- morie_solve_kinetics(0, a, b, g, u0 = 0.5, s0 = 1.2)
    expect_equal(c(z$u, z$s), c(0.5, 1.2), tolerance = 1e-15)
  }
  # the gamma = beta branch is the limit of the general formula
  lim <- morie_solve_kinetics(1.3, a, b, b, u0 = 0.2, s0 = 0.1)$s
  near <- morie_solve_kinetics(1.3, a, b, b + 1e-6, u0 = 0.2, s0 = 0.1)$s
  expect_equal(lim, near, tolerance = 1e-5)
  expect_error(morie_solve_kinetics(1, 1, 0, 1), "beta")
  expect_error(morie_solve_kinetics(1, 1, 1, 0), "gamma")
  expect_error(morie_solve_kinetics(1, -1, 1, 1), "negative")
  expect_error(morie_solve_kinetics(-1, 1, 1, 1), "tau")
})

test_that("velocity and the simulated on/off gene", {
  expect_equal(morie_velocity(c(1, 2), c(3, 1), 0.5, 0.2), c(0.5 - 0.6, 1 - 0.2))
  g <- morie_simulate_gene(2, 1, 0.5, 3, c(1, 3, 4.5))
  sw <- morie_solve_kinetics(3, 2, 1, 0.5)
  off <- morie_solve_kinetics(1.5, 0, 1, 0.5, sw$u, sw$s)
  expect_identical(vapply(g$observations, `[[`, "", "state"), c("on", "on", "off"))
  expect_equal(g$observations[[3]]$u, off$u, tolerance = 1e-15)
  expect_equal(g$observations[[3]]$velocity, off$u - 0.5 * off$s, tolerance = 1e-15)
  expect_equal(unlist(g$steady_on), c(u = 2, s = 4))
  expect_error(morie_simulate_gene(1, 1, 1, -1, 1), "t_switch")
})

test_that("steady-state velocity regresses u on s through the origin at the extremes", {
  set.seed(6)
  s <- stats::runif(40, 0, 5)
  u <- 0.4 * s + stats::rnorm(40, sd = 0.1)
  r <- morie_steady_state_velocity(u, s, quantile = 0.9)
  k <- as.integer(40 * (1 - 0.9))
  o <- order(s)
  keep <- unique(c(o[1:k], o[(40 - k + 1):40]))
  ratio <- stats::coef(stats::lm(u[keep] ~ 0 + s[keep]))[[1]]
  expect_equal(r$gamma_over_beta, ratio, tolerance = 1e-12)
  expect_equal(r$velocity, u - ratio * s, tolerance = 1e-12)
  expect_identical(r$n_fitted, length(keep))
  expect_error(morie_steady_state_velocity(1:3, 1:2), "same length")
  expect_error(morie_steady_state_velocity(1:2, 1:2), "three cells")
})

test_that("latent-time assignment finds the nearest trajectory point", {
  tmax <- 10
  ts <- tmax * (0:50) / 50
  tr <- morie_simulate_gene(2, 1, 0.5, 3, ts)$observations
  pick <- c(4, 20, 37)
  u <- vapply(tr[pick], `[[`, 1, "u")
  s <- vapply(tr[pick], `[[`, 1, "s")
  a <- morie_assign_latent_time(u, s, 2, 1, 0.5, 3, grid = 50, t_max = tmax)
  expect_equal(vapply(a, `[[`, 1, "t"), ts[pick], tolerance = 1e-12)
  expect_equal(vapply(a, `[[`, 1, "distance"), rep(0, 3), tolerance = 1e-12)
  expect_identical(vapply(a, `[[`, "", "state"), c("on", "off", "off"))
})

test_that("the dynamical fit lowers the residual monotonically", {
  ts <- seq(0.2, 9, length.out = 25)
  tr <- morie_simulate_gene(2, 1, 0.5, 3, ts)$observations
  u <- vapply(tr, `[[`, 1, "u") + 0.01 * sin(1:25)
  s <- vapply(tr, `[[`, 1, "s") + 0.01 * cos(1:25)
  f <- morie_dynamical_fit(u, s, alpha0 = 1.5, beta0 = 1.2, gamma0 = 0.6, t_switch0 = 2.5,
                           n_iter = 8, grid = 60)
  expect_true(all(diff(f$rss_history) < 0))
  expect_lte(f$rss, f$rss_history[1])
  expect_equal(f$velocity, f$beta * u - f$gamma * s, tolerance = 1e-14)
  rs <- sum(vapply(morie_assign_latent_time(u, s, f$alpha, f$beta, f$gamma, f$t_switch, 60),
                   function(x) x$distance^2, 1))
  expect_equal(f$rss, rs, tolerance = 1e-12)
  expect_identical(morie_scvelo, morie_dynamical_fit)
  expect_error(morie_dynamical_fit(1:3, 1:3), "four cells")
  lt <- morie_latent_time(list(f, f, list(latent = lapply(1:25, function(i) list(t = 0)))))
  expect_equal(lt$latent_time, vapply(f$latent, `[[`, 1, "t"))
  expect_error(morie_latent_time(list()), "no gene fits")
  expect_error(morie_latent_time(list(f, list(latent = list()))), "same cells")
})
