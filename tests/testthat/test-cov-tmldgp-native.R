# Penalised TMLE: lasso KKT, post-lasso OLS refit (Belloni & Chernozhukov
# 2013) and the unpenalised logistic fluctuation (van der Laan & Gruber 2016).

td_data <- function() {
  set.seed(42)
  n <- 80
  W <- matrix(stats::rnorm(n * 4), n, 4)
  a <- stats::rbinom(n, 1, stats::plogis(0.8 * W[, 1]))
  y <- stats::rbinom(n, 1, stats::plogis(-0.3 + a + 1.2 * W[, 2]))
  list(W = W, a = a, y = y, n = n)
}
td_expit <- function(x) 1 / (1 + exp(-x))
td_logit <- function(p) {
  q <- pmin(pmax(p, 1e-9), 1 - 1e-9)
  log(q / (1 - q))
}

test_that("lasso_path satisfies the lasso KKT conditions", {
  d <- td_data()
  lam <- 0.05
  for (f in list(lasso_path, morie_lasso_path)) {
    fit <- f(d$W, d$y, lam, iters = 2000, tol = 1e-13)
    r <- d$y - fit$intercept - as.numeric(d$W %*% fit$beta)
    g <- as.numeric(crossprod(d$W, r)) / d$n
    act <- abs(fit$beta) > 1e-10
    expect_equal(g[act], lam * sign(fit$beta[act]), tolerance = 1e-9)
    expect_true(all(abs(g[!act]) <= lam + 1e-9))
    expect_equal(mean(r), 0, tolerance = 1e-12)
  }
  expect_identical(lasso_path(d$W, d$y, lam)$support,
                   morie_lasso_path(d$W, d$y, lam)$support + 1L)
  # lambda above max |X'(y - ybar)| / n selects nothing
  lmax <- max(abs(crossprod(d$W, d$y - mean(d$y)))) / d$n
  expect_length(lasso_path(d$W, d$y, lmax * 1.01)$support, 0L)
  expect_error(lasso_path(d$W, d$y[-1], lam), "differ")
  expect_error(lasso_path(d$W, d$y, -1), "negative")
  expect_error(morie_lasso_path(d$W, d$y[-1], lam), "mismatch")
  expect_error(morie_lasso_path(d$W, d$y, -1), "negative")
})

test_that("post-lasso refits OLS on the lasso support", {
  d <- td_data()
  lam <- 0.05
  pl <- post_lasso(d$W, d$y, lam)
  S <- lasso_path(d$W, d$y, lam)$support
  expect_identical(pl$support, S)
  ols <- stats::lm.fit(cbind(1, d$W[, S, drop = FALSE]), d$y)$coefficients
  expect_equal(pl$coef, unname(ols), tolerance = 1e-10)
  expect_equal(pl$predict(d$W[3, ]), sum(ols * c(1, d$W[3, S])), tolerance = 1e-10)
  mp <- morie_post_lasso(d$W, d$y, lam)
  expect_identical(mp$support + 1L, S)
  expect_equal(as.numeric(mp$coef), unname(ols), tolerance = 1e-10)
  expect_equal(as.numeric(mp$predict(d$W[3, ])), pl$predict(d$W[3, ]), tolerance = 1e-10)
  none <- post_lasso(d$W, d$y, 10)
  expect_length(none$support, 0L)
  expect_equal(none$predict(d$W[1, ]), mean(d$y), tolerance = 1e-12)
  expect_equal(morie_post_lasso(d$W, d$y, 10)$intercept, mean(d$y), tolerance = 1e-12)
})

test_that("the ridge-penalised fluctuation solves its own shrunk score", {
  d <- td_data()
  Q <- td_expit(0.4 * d$W[, 1])
  H <- d$a - 0.5
  for (f in list(shrunk_targeting_unsafe, morie_shrunk_targeting_unsafe)) {
    r <- f(Q, H, d$y, ridge = 2)
    p <- td_expit(td_logit(Q) + r$epsilon * H)
    expect_equal(sum(H * (d$y - p)) - 2 * r$epsilon, 0, tolerance = 1e-9)
    expect_equal(r$Q_star, p, tolerance = 1e-12)
    expect_equal(r$score, sum(H * (d$y - p)) / d$n, tolerance = 1e-12)
    # with a ridge the plain score is not solved
    expect_gt(abs(r$score), 1e-6)
  }
  expect_error(shrunk_targeting_unsafe(Q, H[-1], d$y), "same length")
  expect_error(morie_shrunk_targeting_unsafe(Q, H, d$y[-1]), "same length")
})

test_that("penalised TMLE solves the efficient influence curve equation", {
  d <- td_data()
  pen <- 0.03
  r <- morie_tmldgp(d$y, d$a, d$W, penalty = pen)
  # recompute the nuisances and the targeting step from post_lasso
  gf <- post_lasso(d$W, d$a, pen)
  g <- pmin(pmax(apply(d$W, 1, gf$predict), 0.02), 0.98)
  qf <- post_lasso(cbind(d$a, d$W), d$y, pen)
  q1 <- pmin(pmax(apply(cbind(1, d$W), 1, qf$predict), 1e-6), 1 - 1e-6)
  q0 <- pmin(pmax(apply(cbind(0, d$W), 1, qf$predict), 1e-6), 1 - 1e-6)
  H <- d$a / g - (1 - d$a) / (1 - g)
  off <- td_logit(ifelse(d$a == 1, q1, q0))
  expect_equal(sum(H * (d$y - td_expit(off + r$epsilon * H))), 0, tolerance = 1e-8)
  q1s <- td_expit(td_logit(q1) + r$epsilon / g)
  q0s <- td_expit(td_logit(q0) - r$epsilon / (1 - g))
  psi <- mean(q1s - q0s)
  expect_equal(r$psi, psi, tolerance = 1e-10)
  D <- H * (d$y - ifelse(d$a == 1, q1s, q0s)) + q1s - q0s - psi
  expect_equal(r$se, sqrt(sum((D - mean(D))^2) / d$n^2), tolerance = 1e-10)
  expect_equal(r$ci, psi + c(-1.96, 1.96) * r$se, tolerance = 1e-10)
  expect_true(r$solves_eic)
  expect_identical(r$g_support, gf$support)
  r2 <- morie_penalised_tmle(d$y, d$a, d$W, penalty = pen)
  expect_equal(r2$psi, r$psi, tolerance = 1e-10)
  expect_equal(morie_penalisedtmle(d$y, d$a, d$W, pen)$psi, r$psi)
  expect_equal(morie_tmle_doubly_robust_pen(d$y, d$a, d$W, pen)$psi, r$psi)
  expect_error(morie_tmldgp(d$y + 1, d$a, d$W), "\\[0,1\\]")
  expect_error(morie_tmldgp(d$y, d$a[-1], d$W), "differ in length")
  expect_error(morie_penalised_tmle(d$y * 2, d$a, d$W), "rescale")
})

test_that("the cheatsheet says to leave the targeting unpenalised", {
  expect_match(morie_tmldgp_cheatsheet(), "POST-LASSO", fixed = TRUE)
})
