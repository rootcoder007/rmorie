# Coverage for HMC / NUTS (Neal 2011; Hoffman & Gelman 2014, Alg. 1-6):
# the leapfrog map, the step-size heuristic's stopping rule, the dual
# averaging recursion, the U-turn criterion, the base case and one
# doubling of build_tree, the HMC chain regenerated from the counter
# generator, and NUTS on invariant functionals of a Gaussian target.

.lp <- function(t) -0.5 * sum(t^2 / c(1, 9))
.gr <- function(t) -t / c(1, 9)

test_that("leapfrog is half kick, drift, half kick", {
  lf <- leapfrog(c(1, 2), c(0.5, -1), 0.2, .gr)
  r <- c(0.5, -1) + 0.1 * .gr(c(1, 2))
  t <- c(1, 2) + 0.2 * r
  expect_equal(lf$theta, t, tolerance = 1e-12)
  expect_equal(lf$r, r + 0.1 * .gr(t), tolerance = 1e-12)
})

test_that("the step-size heuristic stops where the acceptance ratio crosses 1/2", {
  one <- function() 1
  eps <- find_reasonable_epsilon(c(3, 1), .lp, .gr, one)
  lr <- function(e) {
    lf <- leapfrog(c(3, 1), c(1, 1), e, .gr)
    (.lp(lf$theta) - 0.5 * sum(lf$r^2)) - (.lp(c(3, 1)) - 1)
  }
  a <- if (lr(1) > log(0.5)) 1 else -1
  expect_lte(a * lr(eps), a * log(0.5))
  if (eps != 1) expect_gt(a * lr(eps / 2^a), a * log(0.5))
  expect_equal(log2(eps), round(log2(eps)))
})

test_that("dual averaging follows Nesterov's recursion", {
  d <- dual_averaging_update(3, 0.1, -0.5, 0.2, log(10), gamma = 0.05, t0 = 10, kappa = 0.75)
  hb <- (1 - 1 / 13) * 0.1 + 0.2 / 13
  le <- log(10) - sqrt(3) / 0.05 * hb
  w <- 3^-0.75
  expect_equal(d$h_bar, hb, tolerance = 1e-12)
  expect_equal(d$eps, exp(le), tolerance = 1e-12)
  expect_equal(d$log_eps_bar, w * le + (1 - w) * -0.5, tolerance = 1e-12)
  expect_error(dual_averaging_update(0, 0, 0, 0, 0), "must start at 1")
  expect_error(dual_averaging_update(1, 0, 0, 0, 0, kappa = 0.5), "kappa in \\(0.5, 1\\]")
})

test_that("U-turn test and the build_tree base case and doubling", {
  expect_true(no_u_turn(c(0, 0), c(1, 1), c(1, 0), c(0, 1)))
  expect_false(no_u_turn(c(0, 0), c(1, 1), c(-1, -1), c(0, 1)))
  th <- c(1, -1)
  r <- c(0.3, 0.8)
  j0 <- .lp(th) - 0.5 * sum(r^2)
  lu <- j0 - 0.4
  b0 <- build_tree(th, r, lu, 1L, 0L, 0.3, .lp, .gr, function() 0.5, j0)
  lf <- leapfrog(th, r, 0.3, .gr)
  jj <- .lp(lf$theta) - 0.5 * sum(lf$r^2)
  expect_equal(b0$t_p, lf$theta, tolerance = 1e-12)
  expect_identical(b0$n, as.integer(lu <= jj))
  expect_equal(b0$alpha, min(1, exp(jj - j0)), tolerance = 1e-12)
  b1 <- build_tree(th, r, lu, -1L, 1L, 0.3, .lp, .gr, function() 0, j0)
  l1 <- leapfrog(th, r, -0.3, .gr)
  l2 <- leapfrog(l1$theta, l1$r, -0.3, .gr)
  expect_equal(b1$tm, l2$theta, tolerance = 1e-12)
  expect_equal(b1$tp, l1$theta, tolerance = 1e-12)
  expect_identical(b1$na, 2L)
  # rnd() = 0 < n2 / (n' + n2) whenever the second half has a slice point
  j2 <- .lp(l2$theta) - 0.5 * sum(l2$r^2)
  if (lu <= j2) expect_equal(b1$t_p, l2$theta, tolerance = 1e-12)
})

test_that("the HMC chain is regenerated step for step", {
  r <- morie_bayhmc(.lp, c(1, 1), n_iter = 12, warmup = 0, grad = .gr, sampler = "hmc", eps = 0.4, n_steps = 5, seed = 3)
  e <- .ghc_rng(3)
  th <- c(1, 1)
  ref <- list()
  for (m in 1:12) {
    p <- vapply(1:2, function(i) .ghc_norm(e, 1), 1)
    j0 <- .lp(th) - 0.5 * sum(p^2)
    t2 <- th
    p2 <- p
    for (k in 1:5) {
      lf <- leapfrog(t2, p2, 0.4, .gr)
      t2 <- lf$theta
      p2 <- lf$r
    }
    if (.ghc_unif(e, 1) < min(1, exp(.lp(t2) - 0.5 * sum(p2^2) - j0))) th <- t2
    ref[[m]] <- th
  }
  expect_equal(r$samples, ref, tolerance = 1e-12)
  expect_identical(r$depths, rep(5L, 12))
  expect_identical(r$eps, 0.4)
})

test_that("NUTS with dual averaging targets N(0, diag(1, 9))", {
  r <- morie_bayhmc(.lp, c(0, 0), n_iter = 1600, grad = .gr, seed = 5)
  M <- do.call(rbind, r$samples)
  expect_identical(r$n_samples, 800L)
  # 800 NUTS draws are close to independent here; |z| < 4 with ESS >= 200
  se <- c(1, 3) / sqrt(200)
  expect_lt(max(abs(colMeans(M) / se)), 4)
  expect_lt(max(abs((colMeans(M^2) - c(1, 9)) / (sqrt(2) * c(1, 9) / sqrt(200)))), 4)
  expect_equal(r$eps, r$eps_trace[1600])
  # tuned toward delta = 0.65
  expect_lt(abs(r$acceptance - 0.65), 0.15)
  ng <- morie_bayhmc(.lp, c(0, 0), n_iter = 4, seed = 1, eps = 0.5, warmup = 0)
  expect_identical(ng$n_samples, 4L)
  expect_error(morie_bayhmc(.lp, 0, sampler = "mala"), "must be one of")
  expect_error(morie_bayhmc(.lp, numeric(0)), "theta0 is empty")
  expect_error(morie_bayhmc(.lp, 0, delta = 1), "delta must be in")
  expect_error(morie_bayhmc(.lp, 0, eps = -1), "eps must be positive")
  expect_error(morie_bayhmc(.lp, 0, n_iter = 5, warmup = 6), "warmup must be between")
})
