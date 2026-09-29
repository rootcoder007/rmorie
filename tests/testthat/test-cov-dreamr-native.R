# Coverage tests for R/dreamr_native.R (Hafner et al. 2020): latent
# imagination, the V_R, V_N^k and V_lambda returns (V_lambda against its
# backward recursion) and the critic loss.

dr_models <- list(
  act = function(s) -0.5 * s,
  trans = function(s, a) 0.9 * s + a + 1,
  rew = function(s) s^2 - 1,
  val = function(s) 2 * s
)

test_that("imagination rolls the transition model forward", {
  tr <- morie_dreamr_imagine(1, dr_models$act, dr_models$trans, dr_models$rew, 3, dr_models$val)
  s <- 1
  st <- s
  for (i in 1:3) {
    s <- 0.9 * s - 0.5 * s + 1
    st <- c(st, s)
  }
  expect_equal(unlist(tr$states), st, tolerance = 1e-12)
  expect_equal(unlist(tr$actions), -0.5 * st[1:3], tolerance = 1e-12)
  expect_equal(tr$rewards, st[1:3]^2 - 1, tolerance = 1e-12)
  expect_equal(tr$values, 2 * st, tolerance = 1e-12)
  expect_null(morie_dreamr_imagine(1, dr_models$act, dr_models$trans, dr_models$rew, 2)$values)
  expect_error(morie_dreamr_imagine(1, dr_models$act, dr_models$trans, dr_models$rew, 0), "horizon must be >= 1")
  expect_error(morie_dreamr_imagine(1, 5, dr_models$trans, dr_models$rew, 2), "action_model must be callable")
})

test_that("reward sum, k-step and lambda returns", {
  r <- c(1, -0.5, 2, 0.3)
  v <- c(0.2, 1.1, -0.4, 0.8, 1.5)
  g <- 0.9
  l <- 0.7
  expect_equal(morie_dreamr_lambda_return(r, v, estimator = "reward")$returns, rev(cumsum(rev(r))))
  k2 <- morie_dreamr_lambda_return(r, v, g, estimator = "k-step", k = 2)$returns
  ref <- vapply(0:3, function(t) {
    h <- min(t + 2, 4)
    sum(g^(0:(h - t - 1)) * r[(t + 1):h]) + g^(h - t) * v[h + 1]
  }, 0)
  expect_equal(k2, ref, tolerance = 1e-12)
  vl <- morie_dreamr_lambda_return(r, v, g, l)$returns
  rec <- numeric(4)
  nxt <- v[5]
  for (t in 4:1) {
    rec[t] <- r[t] + g * ((1 - l) * v[t + 1] + l * nxt)
    nxt <- rec[t]
  }
  expect_equal(vl, rec, tolerance = 1e-12)
  expect_equal(morie_dreamr_lambda_return(r, v, g, 0)$returns, morie_dreamr_lambda_return(r, v, g, estimator = "k-step", k = 1)$returns, tolerance = 1e-12)
  expect_equal(morie_dreamr_lambda_return(r, v, g, 1)$returns, morie_dreamr_lambda_return(r, v, g, estimator = "k-step", k = 4)$returns, tolerance = 1e-12)
  expect_equal(morie_dreamr_lambda_return(2, c(1, 3), 0.5)$returns, 2 + 1.5)
  expect_error(morie_dreamr_lambda_return(r, v, estimator = "td"), "estimator must be")
  expect_error(morie_dreamr_lambda_return(r, v[-1]), "one more entry")
  expect_error(morie_dreamr_lambda_return(r, v, lam = 1.2), "lam must lie")
  expect_error(morie_dreamr_lambda_return(r, v, estimator = "k-step", k = 0), "k must be >= 1")
  expect_error(morie_dreamr_lambda_return(numeric(0), 1), "rewards must be non-empty")
})

test_that("critic loss and the full behaviour step", {
  u <- morie_dreamr_value_update(c(1, 2, 3), c(1.5, 1, 3))
  expect_equal(u$loss, 0.5 * (0.25 + 1))
  expect_equal(u$grad, c(-0.5, 1, 0))
  expect_error(morie_dreamr_value_update(1:2, 1), "same length")
  d <- morie_dreamr(1, dr_models$act, dr_models$trans, dr_models$rew, dr_models$val, horizon = 4, gamma = 0.8, lam = 0.6)
  tr <- morie_dreamr_imagine(1, dr_models$act, dr_models$trans, dr_models$rew, 4, dr_models$val)
  ret <- morie_dreamr_lambda_return(tr$rewards, tr$values, 0.8, 0.6)$returns
  expect_equal(d$returns, ret)
  expect_equal(d$objective, sum(ret))
  expect_equal(d$value_loss, 0.5 * sum((tr$values[1:4] - ret)^2), tolerance = 1e-12)
  expect_identical(morie_dreamer, morie_dreamr)
  expect_match(morie_dreamr_cheatsheet(), "V_lambda")
})
