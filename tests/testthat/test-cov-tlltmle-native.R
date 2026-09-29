# TMLE / LTMLE pieces (van der Laan & Rose 2018, Chap. 4): the clever
# covariate, the logistic fluctuation (checked against glm with an
# offset), the point TMLE, backward sequential LTMLE and the IC standard
# error.

tl_data <- function() {
  set.seed(15)
  n <- 80
  w <- stats::rnorm(n)
  g <- stats::plogis(0.3 * w)
  a <- stats::rbinom(n, 1, g)
  y <- stats::rbinom(n, 1, stats::plogis(-0.2 + 0.8 * a + 0.5 * w))
  list(w = w, g = g, a = a, y = y, n = n)
}

test_that("the clever covariate is I(A = d) / g", {
  h <- tlltmle_clever_covariate(c(1, 0, 1), c(0.5, 0.25, 0.8))
  expect_equal(h$H, c(2, 0, 1.25))
  expect_equal(h$max, 2)
  expect_equal(tlltmle_clever_covariate(c(1, 0), c(0.5, 0.25), rule = 0)$H, c(0, 4))
  expect_error(tlltmle_clever_covariate(1:2, 0.5), "2 treatments but 1")
  expect_error(tlltmle_clever_covariate(1, 1), "strictly inside")
})

test_that("the fluctuation is the logistic MLE of epsilon with the fit as offset", {
  d <- tl_data()
  q <- stats::plogis(0.1 + 0.4 * d$w)
  H <- d$a / d$g
  fl <- tlltmle_fluctuate(q, H, d$y)
  ref <- stats::glm(d$y ~ -1 + H, offset = stats::qlogis(q), family = stats::binomial())
  expect_equal(fl$epsilon, unname(stats::coef(ref)), tolerance = 1e-8)
  expect_equal(fl$Q_star, unname(stats::fitted(ref)), tolerance = 1e-8)
  expect_equal(fl$score, 0, tolerance = 1e-10)
  expect_error(tlltmle_fluctuate(q, H[-1], d$y), "differ in length")
})

test_that("the point TMLE updates both counterfactual fits and solves the EIC", {
  d <- tl_data()
  q1 <- stats::plogis(0.5 + 0.4 * d$w)
  q0 <- stats::plogis(-0.2 + 0.4 * d$w)
  r <- tlltmle_tmle_point(d$a, d$y, q1, q0, d$g)
  H <- d$a / d$g - (1 - d$a) / (1 - d$g)
  e <- tlltmle_fluctuate(ifelse(d$a == 1, q1, q0), H, d$y)$epsilon
  q1s <- stats::plogis(stats::qlogis(q1) + e / d$g)
  q0s <- stats::plogis(stats::qlogis(q0) - e / (1 - d$g))
  psi <- mean(q1s - q0s)
  D <- H * (d$y - ifelse(d$a == 1, q1s, q0s)) + q1s - q0s - psi
  expect_equal(r$psi, psi, tolerance = 1e-12)
  expect_equal(r$se, sqrt(sum((D - mean(D))^2)) / d$n, tolerance = 1e-12)
  expect_true(r$solves_eic)
  expect_equal(r$initial_plugin, mean(q1 - q0), tolerance = 1e-15)
  expect_equal(r$max_clever_covariate, max(abs(H)))
})

test_that("LTMLE fluctuates backwards, each step's update the next step's outcome", {
  d <- tl_data()
  Q2 <- stats::plogis(0.2 + 0.3 * d$w)
  Q1 <- stats::plogis(0.1 * d$w)
  H2 <- d$a / d$g
  H1 <- rep(1.5, d$n)
  r <- tlltmle_ltmle(list(Q1, Q2), list(H1, H2), list(NULL, d$y))
  s2 <- tlltmle_fluctuate(Q2, H2, d$y)
  s1 <- tlltmle_fluctuate(Q1, H1, s2$Q_star)
  expect_equal(r$epsilons, c(s1$epsilon, s2$epsilon), tolerance = 1e-15)
  expect_equal(r$psi, mean(s1$Q_star), tolerance = 1e-15)
  expect_identical(r$T, 2L)
  one <- tlltmle_longitudinaltmle(list(Q2), list(H2), list(d$y))
  expect_equal(one$psi, mean(s2$Q_star), tolerance = 1e-15)
  expect_error(tlltmle_ltmle(list(), list(), list()), "empty")
  expect_error(tlltmle_ltmle(list(Q1, Q2), list(H1), list(d$y)), "2 fits but 1")
})

test_that("the IC standard error is sd / sqrt(n); the bundle exposes every step", {
  v <- c(0.3, -1.2, 0.8, 0.1, 2)
  expect_equal(tlltmle_influence_curve_se(v), stats::sd(v) / sqrt(5), tolerance = 1e-15)
  expect_error(tlltmle_influence_curve_se(1), "at least 2")
  expect_same_function(morie_tlltmle$ltmle, tlltmle_ltmle)
  expect_match(tlltmle_cheatsheet(), "DOUBLE ROBUST", fixed = TRUE)
})
