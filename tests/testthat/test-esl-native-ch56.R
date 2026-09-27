esl56_xy <- function() {
  x <- sort(((7 * (1:25)) %% 23) / 23 + (1:25) / 100)
  list(x = x, y = sin(12 * (x + 0.2)) / (x + 0.2) + 0.3 * cos(5 * (1:25)))
}

test_that("local linear, Nadaraya-Watson and kernel ridge equal their definitions", {
  d <- esl56_xy()
  ep <- function(u) ifelse(abs(u) <= 1, 0.75 * (1 - u^2), 0)
  ll <- morie_esl_local_linear(c(0.3, 0.6), d$x, d$y, 0.2)
  nw <- morie_esl_nadaraya_watson(c(0.3, 0.6), d$x, d$y, 0.2)
  for (k in 1:2) {
    x0 <- c(0.3, 0.6)[k]
    w <- ep((d$x - x0) / 0.2)
    expect_equal(c(ll$values[k], ll$slopes[k]), unname(coef(lm(d$y ~ I(d$x - x0), weights = w))), tolerance = 1e-12)
    expect_equal(nw$values[k], sum(w * d$y) / sum(w), tolerance = 1e-12)
  }
  k <- kernel_ridge_regression(d$x, d$y, lam = 0.1, bandwidth = 0.2, x_eval = c(0.3, 0.6))
  expect_equal(k$y_hat, c(-0.565622851739059795, -0.026796905212836009), tolerance = 1e-12)
})

test_that("natural spline basis spans ns() and Cp and naive Bayes follow their formulas", {
  x <- seq(0.05, 0.95, length.out = 12)
  B <- morie_esl_natural_spline(x, c(0.2, 0.5, 0.8))$basis
  R <- cbind(1, splines::ns(x, knots = 0.5, Boundary.knots = c(0.2, 0.8)))
  expect_lt(max(abs(qr.resid(qr(R), B))), 1e-12)
  expect_equal(morie_esl_mallows_cp(10, 3, 50, 0.5)$estimate, 0.26)
  i <- 1:30
  nb <- morie_esl_naive_bayes(cbind(sin(i), cos(3 * i) + (i %% 3)), i %% 3, rbind(c(0.2, 1)))
  expect_equal(as.numeric(nb$log_posterior), c(-3.4270370202619498, -1.9641204550416957, -3.6230859821947980),
               tolerance = 1e-8)
})

test_that("penalised logistic, varying coefficients, local logistic, RBF network and FWER", {
  i <- 1:40
  x <- sin(i) + 0.1 * i / 10
  yb <- as.integer(sin(3 * i) + x > 0.3)
  Om <- matrix(0, 4, 4)
  Om[3:4, 3:4] <- c(4, 6, 6, 12)
  r <- morie_esl_penalized_logistic(cbind(1, x, x^2, x^3), yb, Om, 0.5)
  expect_equal(r$theta, c(-1.295262125853519075, 4.219150558200408518, -0.602041164046019261, 0.095061923090123832),
               tolerance = 1e-7)
  expect_equal(r$penalized_loglik, -11.872385602248254344, tolerance = 1e-10)
  X <- cbind(cos(2 * i), sin(5 * i))
  z <- ((7 * i) %% 40) / 40
  y <- 1 + z * X[, 1] - (1 - z) * X[, 2] + 0.1 * cos(9 * i)
  v <- morie_esl_varying_coef(X, z, y, c(0.3, 0.7), 0.35)
  expect_equal(v$coefficients[1, ], unname(coef(lm(y ~ X, weights = pmax(0, 0.75 * (1 - (abs(z - 0.3) / 0.35)^2))))),
               tolerance = 1e-12)
  XX <- cbind(sin(i), cos(3 * i))
  lg <- morie_esl_local_logistic(XX, (i * 7) %% 3, rbind(c(0.2, 0.3), c(-0.5, 0.8)), 1)
  expect_equal(lg$prob[1, ], c(0.30580338856933753, 0.38233137071712425, 0.31186524071353816), tolerance = 1e-7)
  cen <- rbind(c(0, 0), c(1, 1), c(-1, 0.5))
  b <- morie_esl_rbf_network(XX, sin(2 * i), cen, c(0.8, 1, 1.2), normalized = TRUE, query = rbind(c(0.2, 0.3)))
  expect_equal(b$coefficients, c(0.453143574359092305, -0.655609262139223659, -0.059510247707946336), tolerance = 1e-12)
  expect_equal(morie_esl_fwer(0.05, 10)$fwer_independent, 1 - 0.95^10)
})
