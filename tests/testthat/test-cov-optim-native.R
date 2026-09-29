# Coverage tests for R/optim_native.R: first-order optimiser updates
# (Kingma and Ba 2015; Duchi et al. 2011; You et al. 2017/2020) and Boyd
# and Vandenberghe (2004) Newton, backtracking and proximal steps.

test_that("Adam and AdamW: two bias-corrected steps with carried state", {
  g1 <- c(0.5, -1.2, 0.03)
  g2 <- c(0.1, -0.8, -0.4)
  a1 <- morie_adam(g1, lr = 0.01)
  a2 <- morie_adam(g2, lr = 0.01, state = a1$state)
  m <- 0.1 * g1
  v <- 0.001 * g1^2
  m <- 0.9 * m + 0.1 * g2
  v <- 0.999 * v + 0.001 * g2^2
  mh <- m / (1 - 0.9^2)
  vh <- v / (1 - 0.999^2)
  expect_equal(a2$t, 2L)
  expect_equal(a2$update, -0.01 * mh / (sqrt(vh) + 1e-8), tolerance = 1e-12)
  expect_equal(a1$update, -0.01 * g1 / (abs(g1) + 1e-8), tolerance = 1e-12)
  expect_error(morie_adam(g1, beta1 = 1), "beta1")
  expect_error(morie_adam(c(1, NA)), "non-finite")
  th <- c(1, -2, 0.5)
  w1 <- morie_adamw_step(g1, lr = 0.01, wd = 0.1, theta = th)
  expect_equal(w1$update, a1$update - 0.01 * 0.1 * th, tolerance = 1e-12)
  expect_equal(morie_adamw_step(g1, lr = 0.01)$update, a1$update, tolerance = 1e-12)
  expect_error(morie_adamw_step(g1, theta = 1:2), "theta has 2")
})

test_that("AdaGrad, RMSProp, momentum and Nesterov recursions", {
  g1 <- c(0.4, -0.3)
  g2 <- c(-0.2, 0.6)
  ag <- morie_adagrad(g2, lr = 0.1, state = morie_adagrad(g1, lr = 0.1)$state)
  expect_equal(ag$update, -0.1 * g2 / (sqrt(g1^2 + g2^2) + 1e-8), tolerance = 1e-12)
  rp <- morie_rmsprop(g2, rho = 0.8, lr = 0.1, state = morie_rmsprop(g1, rho = 0.8, lr = 0.1)$state)
  v <- 0.8 * (0.2 * g1^2) + 0.2 * g2^2
  expect_equal(rp$update, -0.1 * g2 / (sqrt(v) + 1e-8), tolerance = 1e-12)
  expect_error(morie_rmsprop(g1, rho = -0.1), "rho")
  sm <- morie_sgd_momentum(g2, mu = 0.5, lr = 0.1, state = morie_sgd_momentum(g1, mu = 0.5, lr = 0.1)$state)
  expect_equal(sm$update, 0.5 * (-0.1 * g1) - 0.1 * g2, tolerance = 1e-12)
  ns <- morie_nesterov(g2, mu = 0.5, lr = 0.1, state = morie_nesterov(g1, mu = 0.5, lr = 0.1)$state)
  vn <- 0.5 * (-0.1 * g1) - 0.1 * g2
  expect_equal(ns$update, 0.5 * vn - 0.1 * g2, tolerance = 1e-12)
  expect_error(morie_sgd_momentum(g1, mu = 1), "mu")
  expect_error(morie_nesterov(g1, mu = 2), "mu")
})

test_that("plain and minibatch gradient steps", {
  gd <- morie_gradient_descent_update(c(1, 2), c(0.5, -1), alpha = 0.2)
  expect_equal(gd$beta, c(0.9, 2.2))
  expect_equal(gd$step_norm, 0.2 * sqrt(1.25), tolerance = 1e-12)
  expect_error(morie_gradient_descent_update(1, 1, alpha = 0), "positive")
  G <- rbind(c(1, 2), c(3, -2), c(2, 0))
  sg <- morie_sgd_update(c(0, 0), G, eta = 0.5)
  expect_equal(sg$beta, -0.5 * colMeans(G))
  expect_equal(sg$grad_se, apply(G, 2, sd) / sqrt(3), tolerance = 1e-12)
  expect_true(all(is.na(morie_sgd_update(c(0, 0), c(1, 1))$grad_se)))
  expect_error(morie_sgd_update(c(0, 0, 0), G), "3 entries")
})

test_that("LARS and LAMB layer-wise trust ratios", {
  g <- c(0.3, -0.4)
  w <- c(3, 4)
  l <- morie_lars(g, w, lr = 0.1, mu = 0.9, wd = 0.01, eta = 0.002)
  tr <- 0.002 * 5 / (0.5 + 0.01 * 5 + 1e-8)
  expect_equal(l$trust_ratio, tr, tolerance = 1e-12)
  expect_equal(l$update, -0.1 * tr * (g + 0.01 * w), tolerance = 1e-12)
  expect_equal(morie_lars(g, c(0, 0))$trust_ratio, 1)
  expect_error(morie_lars(g, 1), "w has 1")
  lb <- morie_lamb(g, w, lr = 0.01, wd = 0.1)
  r <- g / (abs(g) + 1e-6) + 0.1 * w
  expect_equal(lb$direction, r, tolerance = 1e-12)
  expect_equal(lb$trust_ratio, 5 / sqrt(sum(r^2)), tolerance = 1e-12)
  expect_equal(lb$update, -0.01 * 5 / sqrt(sum(r^2)) * r, tolerance = 1e-12)
  expect_error(morie_lamb(g, w, beta2 = 1), "beta2")
})

test_that("Newton step, decrement and backtracking line search", {
  H <- rbind(c(4, 1), c(1, 3))
  g <- c(1, -2)
  nt <- morie_boyd_newton(g, H)
  expect_equal(nt$step, -as.numeric(solve(H, g)), tolerance = 1e-12)
  expect_equal(nt$decrement, sqrt(sum(g * solve(H, g))), tolerance = 1e-12)
  expect_true(nt$pd && nt$is_descent)
  expect_equal(morie_boyd_newton(g, H, ridge = 1)$step, -as.numeric(solve(H + diag(2), g)), tolerance = 1e-12)
  expect_false(morie_boyd_newton(g, -H)$pd)
  expect_error(morie_boyd_newton(g, rbind(c(1, 2), c(0, 1))), "symmetric")
  nd <- morie_boyd_newton_decrement(g, H)
  expect_equal(nd$suboptimality, nd$decrement^2 / 2, tolerance = 1e-12)
  expect_error(morie_boyd_newton_decrement(g, -H), "not positive definite")
  expect_error(morie_boyd_newton_decrement(g, matrix(0, 2, 2)), "singular")
  f <- function(x) exp(x[1] + 3 * x[2] - 0.1) + exp(x[1] - 3 * x[2] - 0.1) + exp(-x[1] - 0.1)
  x0 <- c(-1, 0.7)
  gr <- c(exp(x0[1] + 3 * x0[2] - 0.1) + exp(x0[1] - 3 * x0[2] - 0.1) - exp(-x0[1] - 0.1),
    3 * exp(x0[1] + 3 * x0[2] - 0.1) - 3 * exp(x0[1] - 3 * x0[2] - 0.1))
  bt <- morie_boyd_backtracking(f, gr, x0, -gr, alpha = 0.1, beta = 0.7)
  tt <- 1
  k <- 0
  while (f(x0 - tt * gr) > f(x0) - 0.1 * tt * sum(gr^2)) {
    tt <- 0.7 * tt
    k <- k + 1
  }
  expect_equal(bt$t, tt, tolerance = 1e-12)
  expect_equal(bt$n_backtracks, k)
  expect_equal(bt$f_new, f(x0 - tt * gr), tolerance = 1e-12)
  expect_false(morie_boyd_backtracking(f, gr, x0, -gr, max_iter = 1L)$converged)
  expect_error(morie_boyd_backtracking(f, gr, x0, gr), "not a descent direction")
  expect_error(morie_boyd_backtracking(f, gr, x0, -gr, alpha = 0.6), "alpha")
})

test_that("proximal operators are the minimisers of h(x) + |x - v|^2/(2t)", {
  v <- c(1.5, -0.3, 0.8, -2)
  t <- 0.5
  expect_equal(morie_boyd_proximal("l1", v, t)$prox, sign(v) * pmax(abs(v) - t, 0))
  pl2 <- morie_boyd_proximal("l2", v, t)$prox
  expect_equal(pl2, (1 - t / sqrt(sum(v^2))) * v, tolerance = 1e-12)
  expect_equal(morie_boyd_proximal("l2", v / 10, t)$prox, numeric(4))
  expect_equal(morie_boyd_proximal("l2sq", v, t)$prox, v / 2)
  expect_equal(morie_boyd_proximal("nonneg", v)$prox, pmax(v, 0))
  expect_equal(morie_boyd_proximal("box", v, lo = -1, hi = 1)$prox, pmin(pmax(v, -1), 1))
  expect_equal(morie_boyd_proximal("zero", v)$prox, v)
  # optimality: no nearby point has a smaller Moreau objective
  obj <- function(x) sum(abs(x)) + sum((x - v)^2) / (2 * t)
  p1 <- morie_boyd_proximal("l1", v, t)
  expect_equal(p1$moreau, obj(p1$prox), tolerance = 1e-12)
  expect_lt(p1$moreau, obj(p1$prox + c(0.01, 0, -0.01, 0.01)))
  expect_error(morie_boyd_proximal("box", v), "requires both")
  expect_error(morie_boyd_proximal("l3", v), "unknown h")
  expect_error(morie_boyd_proximal("l1", v, t = 0), "positive")
})
