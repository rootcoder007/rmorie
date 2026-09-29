# Collaborative TMLE (van der Laan & Rose 2018, Chap. 10): candidate
# propensity models against glm, the instrument penalty, the targeted
# log loss, and the selection by cross-validated targeted risk.

ct_data <- function() {
  set.seed(33)
  n <- 120
  W <- cbind(stats::rnorm(n), stats::rnorm(n), stats::rnorm(n))
  a <- stats::rbinom(n, 1, stats::plogis(0.4 * W[, 1] + 1.5 * W[, 3]))
  y <- stats::rbinom(n, 1, stats::plogis(-0.3 + 0.7 * a + 0.8 * W[, 1]))
  q <- function(av) stats::plogis(-0.3 + 0.6 * av + 0.7 * W[, 1])
  list(W = W, a = a, y = y, q1 = q(1), q0 = q(0), n = n)
}

test_that("candidate g models are logistic fits on the named covariates", {
  d <- ct_data()
  cs <- candidate_sequence(d$a, d$W, list(1L, c(1L, 3L)))
  for (k in 1:2) {
    cols <- list(1L, c(1L, 3L))[[k]]
    g <- stats::fitted(stats::glm(d$a ~ d$W[, cols], family = stats::binomial()))
    expect_equal(cs[[k]]$g, pmin(pmax(unname(g), 0.01), 0.99), tolerance = 1e-8)
    expect_equal(cs[[k]]$max_clever, max(1 / cs[[k]]$g, 1 / (1 - cs[[k]]$g)))
  }
  ip <- instrument_penalty(cs[[1]]$g, cs[[2]]$g)
  expect_equal(ip$ratio, cs[[2]]$max_clever / cs[[1]]$max_clever, tolerance = 1e-15)
})

test_that("the targeted loss is the mean Bernoulli log loss", {
  q <- c(0.2, 0.9, 0.5)
  y <- c(0, 1, 1)
  expect_equal(targeted_loss(q, y), -mean(y * log(q) + (1 - y) * log(1 - q)), tolerance = 1e-15)
})

test_that("C-TMLE picks the g with the smallest cross-validated targeted risk", {
  d <- ct_data()
  gm <- list(1L, c(1L, 3L), 1:3)
  r <- ctmle(d$a, d$y, d$q1, d$q0, d$W, gm, V = 4, seed = 2)
  expect_identical(r$selected, which.min(r$cv_risks) - 1L)
  expect_equal(r$cv_risks, r$cv_losses + r$variance_penalties, tolerance = 1e-15)
  g <- candidate_sequence(d$a, d$W, gm)[[r$selected + 1]]$g
  H <- d$a / g - (1 - d$a) / (1 - g)
  off <- stats::qlogis(ifelse(d$a == 1, d$q1, d$q0))
  expect_equal(sum(H * (d$y - stats::plogis(off + r$epsilon * H))), 0, tolerance = 1e-8)
  q1s <- stats::plogis(stats::qlogis(d$q1) + r$epsilon / g)
  q0s <- stats::plogis(stats::qlogis(d$q0) - r$epsilon / (1 - g))
  expect_equal(r$psi, mean(q1s - q0s), tolerance = 1e-12)
  expect_true(r$solves_eic)
  np <- ctmle(d$a, d$y, d$q1, d$q0, d$W, gm, V = 4, seed = 2, penalty = FALSE)
  expect_equal(np$cv_risks, np$cv_losses, tolerance = 1e-15)
  expect_equal(morie_tlctmle(d$a, d$y, d$q1, d$q0, d$W, gm, V = 4, seed = 2)$psi, r$psi)
  expect_error(ctmle(d$a, d$y, d$q1, d$q0, d$W, list()), "no candidate")
})
