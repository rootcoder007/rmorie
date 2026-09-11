# Anchors for the SAIGE score test.
#
# The module's numerical core is checkable against base R: the IRLS fit
# against glm(binomial), the normal tail against pnorm, and the cumulant
# generating function against its own derivative identities at the null,
# K(0) = 0, K'(0) = 0 and K''(0) = sum(G^2 mu (1 - mu)). None of it was
# checked; the file sat at 24.1% with no test naming any of its functions.

test_that("the sigmoid matches plogis and survives both tails", {
  for (x in c(-3, -1, 0, 0.5, 2)) {
    expect_equal(.saigeg_sigmoid(x), plogis(x), tolerance = 1e-14)
  }
  expect_equal(.saigeg_sigmoid(800), 1)
  expect_equal(.saigeg_sigmoid(-800), 0)
  expect_false(is.nan(.saigeg_sigmoid(-800)))
  expect_equal(.saigeg_sigmoid(0), 0.5)
})

test_that("the normal tail matches pnorm", {
  for (x in c(-6, -4, -1.96, -1, 0, 1, 1.96, 4, 6)) {
    expect_equal(.saigeg_pnorm(x), pnorm(x), tolerance = 1e-12)
  }
  # it is a distribution function
  expect_equal(.saigeg_pnorm(0), 0.5, tolerance = 1e-12)
  g <- vapply(seq(-5, 5, by = 0.5), .saigeg_pnorm, numeric(1))
  expect_true(all(diff(g) > 0))
  expect_true(all(g >= 0 & g <= 1))
})

test_that("the variance helper is the unbiased sample variance", {
  expect_equal(.saigeg_variance(c(1, 2, 3, 4)), var(c(1, 2, 3, 4)),
               tolerance = 1e-12)
  expect_equal(.saigeg_variance(c(2, 2, 2)), 0, tolerance = 1e-12)
  # fewer than two observations has no sample variance
  expect_true(is.na(.saigeg_variance(5)))
  expect_true(is.na(.saigeg_variance(numeric(0))))
})

test_that("the design matrix carries an intercept", {
  X <- cbind(rnorm(10), rnorm(10))
  D <- .saigeg_design(X, 10L)
  expect_identical(dim(D), c(10L, 3L))
  expect_true(all(D[, 1] == 1))
  expect_equal(D[, 2:3], X, ignore_attr = TRUE)
  # with no covariates it is the intercept alone
  expect_identical(dim(.saigeg_design(NULL, 7L)), c(7L, 1L))
  expect_true(all(.saigeg_design(NULL, 7L) == 1))
  expect_identical(dim(.saigeg_design(list(), 7L)), c(7L, 1L))
})

fixture <- function(seed = 1, n = 300) {
  set.seed(seed)
  X <- cbind(rnorm(n), rnorm(n))
  y <- rbinom(n, 1, plogis(-0.5 + 0.8 * X[, 1] - 0.4 * X[, 2]))
  list(n = n, X = X, y = y, G = rbinom(n, 2, 0.3))
}

test_that("the IRLS fit agrees with glm(binomial)", {
  f <- fixture()
  D <- .saigeg_design(f$X, f$n)
  beta <- .saigeg_logit_irls(D, f$y)
  g <- glm(f$y ~ f$X, family = binomial())
  expect_equal(as.numeric(beta), as.numeric(coef(g)), tolerance = 1e-6)
  # and so does the null fit's fitted mean
  nul <- .saigeg_fit_null(f$y, f$X)
  expect_equal(as.numeric(nul$mu), as.numeric(fitted(g)), tolerance = 1e-6)
  expect_equal(as.numeric(nul$beta), as.numeric(coef(g)), tolerance = 1e-6)
  # every fitted probability is a probability
  expect_true(all(nul$mu > 0 & nul$mu < 1))
})

test_that("the score and its variance are the closed forms", {
  f <- fixture()
  mu <- as.numeric(.saigeg_fit_null(f$y, f$X)$mu)
  s <- .saigeg_score_statistic(f$y, f$G, mu)
  expect_equal(as.numeric(s$score), sum(f$G * (f$y - mu)), tolerance = 1e-10)
  expect_equal(as.numeric(s$variance), sum(f$G^2 * mu * (1 - mu)),
               tolerance = 1e-10)
  expect_equal(as.numeric(s$n), f$n)
  # a variant carried by nobody is refused rather than scored: a zero
  # variance would divide through to an infinite statistic
  expect_error(.saigeg_score_statistic(f$y, rep(0, f$n), mu),
               "zero variance")
})

test_that("the cumulant generating function satisfies its null identities", {
  f <- fixture()
  mu <- as.numeric(.saigeg_fit_null(f$y, f$X)$mu)
  # the score is centred at the null, so K(0) = K'(0) = 0 and K''(0) is
  # its variance
  expect_equal(.saigeg_cgf(0, f$G, mu, 0), 0, tolerance = 1e-10)
  expect_equal(.saigeg_cgf(0, f$G, mu, 1), 0, tolerance = 1e-10)
  expect_equal(.saigeg_cgf(0, f$G, mu, 2), sum(f$G^2 * mu * (1 - mu)),
               tolerance = 1e-10)
  # K'' is a variance, so it is positive wherever it is evaluated
  for (t in c(-0.5, -0.1, 0, 0.1, 0.5)) {
    expect_gt(.saigeg_cgf(t, f$G, mu, 2), 0)
  }
})

test_that("the saddlepoint solve returns the root of K'(t) = s", {
  f <- fixture()
  mu <- as.numeric(.saigeg_fit_null(f$y, f$X)$mu)
  s <- as.numeric(.saigeg_score_statistic(f$y, f$G, mu)$score)
  that <- .saigeg_solve_saddle(s, f$G, mu)
  expect_equal(.saigeg_cgf(that, f$G, mu, 1), s, tolerance = 1e-6)
  # a score of zero puts the saddlepoint at the origin
  expect_equal(.saigeg_solve_saddle(0, f$G, mu), 0, tolerance = 1e-6)
})

test_that("the normal p-value is the closed form on both tails", {
  for (sc in c(-3, 0, 1, 2.5)) {
    v <- 4
    z <- sc / sqrt(v)
    expect_equal(as.numeric(.saigeg_normal_pvalue(sc, v)$p_value),
                 2 * pnorm(-abs(z)), tolerance = 1e-12)
    expect_equal(as.numeric(.saigeg_normal_pvalue(sc, v, two_sided = FALSE)$p_value),
                 1 - pnorm(z), tolerance = 1e-12)
    expect_equal(as.numeric(.saigeg_normal_pvalue(sc, v)$z), z, tolerance = 1e-12)
  }
  expect_error(.saigeg_normal_pvalue(1, 0), "variance must be positive")
  expect_error(.saigeg_normal_pvalue(1, -1), "variance must be positive")
})

test_that("with a balanced design the saddlepoint agrees with the normal", {
  set.seed(2); n <- 2000
  mu <- rep(0.5, n); y <- rbinom(n, 1, 0.5); G <- rbinom(n, 2, 0.05)
  s <- as.numeric(.saigeg_score_statistic(y, G, mu)$score)
  var0 <- .saigeg_cgf(0, G, mu, 2)
  pn <- as.numeric(.saigeg_normal_pvalue(s, var0)$p_value)
  ps <- as.numeric(.saigeg_saddlepoint_pvalue(s, G, mu)$p_value)
  # this is the regime where the normal approximation is fine, so the
  # correction should be small
  expect_equal(ps, pn, tolerance = 0.01)
})

test_that("a score at the edge of K' does not collapse to a p-value of zero", {
  # The regression. With a prevalence of 0.002 every case can carry no
  # copies of the variant, which pins the score at -sum(G mu), the
  # infimum of K'. There is then no interior saddlepoint: the solve runs
  # out to its bracket, K'' there is 5e-12, and the 1/v term of
  # Lugannani-Rice diverged to -4685, which clamped to zero. A p-value of
  # zero is a genome-wide significant hit, and the normal approximation
  # called the same score unremarkable at 0.54.
  set.seed(2); n <- 2000
  invisible(rbinom(n, 1, 0.5)); invisible(rbinom(n, 2, 0.05))
  invisible(rbinom(n, 1, 0.05)); invisible(rbinom(n, 2, 0.05))
  mu <- rep(0.002, n); y <- rbinom(n, 1, 0.002); G <- rbinom(n, 2, 0.05)
  s <- as.numeric(.saigeg_score_statistic(y, G, mu)$score)
  var0 <- .saigeg_cgf(0, G, mu, 2)
  r <- .saigeg_saddlepoint_pvalue(s, G, mu)
  expect_gt(as.numeric(r$p_value), 0.1)
  expect_equal(as.numeric(r$p_value),
               as.numeric(.saigeg_normal_pvalue(s, var0)$p_value),
               tolerance = 1e-12)
  # and it says which method it fell back to
  expect_match(r$method, "no interior saddlepoint")
})

test_that("no configuration yields a p-value outside [0, 1] or a false zero", {
  set.seed(7); n <- 1200
  for (rep in 1:25) {
    prev <- 10^runif(1, log10(0.001), log10(0.5))
    mu <- rep(prev, n)
    y <- rbinom(n, 1, prev)
    G <- rbinom(n, 2, 10^runif(1, log10(0.01), log10(0.3)))
    var0 <- .saigeg_cgf(0, G, mu, 2)
    if (var0 <= 1e-12) next
    s <- as.numeric(.saigeg_score_statistic(y, G, mu)$score)
    ps <- as.numeric(.saigeg_saddlepoint_pvalue(s, G, mu)$p_value)
    pn <- as.numeric(.saigeg_normal_pvalue(s, var0)$p_value)
    expect_true(is.finite(ps) && ps >= 0 && ps <= 1)
    # it must not declare significance where the normal sees nothing
    if (pn > 0.01) expect_gt(ps, 1e-8)
  }
})

test_that("the variance ratio is the ratio of the two sample variances", {
  full <- c(1, 2, 3, 4, 5)
  naive <- c(2, 4, 6, 8, 10)
  r <- .saigeg_variance_ratio(full, naive)
  expect_equal(as.numeric(r[[1]]), var(full) / var(naive), tolerance = 1e-12)
  expect_equal(as.numeric(r[[1]]), 0.25, tolerance = 1e-12)
})

test_that("the entry point reports the test it ran", {
  set.seed(3); n <- 1000
  y <- rbinom(n, 1, 0.01)
  G <- rbinom(n, 2, 0.1)
  r <- morie_saigeg(y, G)
  expect_true(r$p_value >= 0 && r$p_value <= 1)
  expect_true(r$p_normal >= 0 && r$p_normal <= 1)
  expect_identical(r$n_cases, sum(y))
  expect_identical(r$n_controls, as.integer(n - sum(y)))
  expect_equal(r$case_control_ratio, sum(y) / (n - sum(y)), tolerance = 1e-12)
  expect_identical(r$estimate, r$p_value)
  expect_match(r$method, "score test")
  # the reported score and variance are the closed forms on the fitted null
  expect_equal(r$z, r$score / sqrt(r$variance), tolerance = 1e-10)
})
