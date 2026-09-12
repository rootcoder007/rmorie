# Marginal structural model for a continuous-dose treatment (Robins,
# Hernan & Brumback 2000; Hirano & Imbens generalized propensity score).
#
# Anchors outside the module: base R's glm for the binary treatment model,
# lm for the continuous one, and the densities written out longhand. The
# inverse-probability weights are checked against the Bernoulli and
# Gaussian forms computed by hand, which is what distinguishes a weight
# with a probabilistic meaning from one without.

mk <- function(n = 60, seed = 1) {
  set.seed(seed)
  H <- cbind(rnorm(n), rnorm(n))
  Ac <- 0.8 * H[, 1] + rnorm(n, 0, 1)        # continuous dose
  Ab <- rbinom(n, 1, plogis(0.5 * H[, 1]))   # binary dose
  y <- 1 + 2 * Ac + 0.5 * H[, 2] + rnorm(n, 0, 0.4)
  list(H = H, Ac = Ac, Ab = Ab, y = y, n = n)
}

test_that("the iteratively reweighted fit is base R's logistic regression", {
  d <- mk(80, 3)
  Z <- cbind(1, d$H)
  got <- .gentmt_logit_irls(d$Ab, Z)
  ref <- glm(d$Ab ~ d$H, family = binomial())
  expect_equal(got$coef, unname(coef(ref)), tolerance = 1e-6)
  expect_equal(got$fitted, unname(fitted(ref)), tolerance = 1e-7)
  # fitted values are probabilities
  expect_true(all(got$fitted > 0 & got$fitted < 1))
  # an intercept-only design returns the sample proportion
  only1 <- .gentmt_logit_irls(d$Ab, matrix(1, d$n, 1))
  expect_equal(unique(round(only1$fitted, 10)), round(mean(d$Ab), 10))
})

test_that("the treatment density is the density of the dose received", {
  d <- mk(60, 5)
  # a binary dose is scored with its Bernoulli probability, not a Gaussian
  tb <- .gentmt_treatment_density(d$Ab, d$H, "binary")
  ref <- glm(d$Ab ~ d$H, family = binomial())
  expect_equal(tb$info$prob, unname(fitted(ref)), tolerance = 1e-7)
  expect_equal(tb$dens, ifelse(d$Ab > 0.5, tb$info$prob, 1 - tb$info$prob))
  # so every value is a probability, which a Gaussian density need not be
  expect_true(all(tb$dens > 0 & tb$dens <= 1))
  expect_named(tb$info, c("coef", "prob"))

  # a continuous dose is scored with the Gaussian density of its model
  tc <- .gentmt_treatment_density(d$Ac, d$H, "normal")
  expect_equal(tc$info$mu, unname(fitted(lm(d$Ac ~ d$H))), tolerance = 1e-8)
  expect_true(tc$info$sigma2 > 0)
  # the reported density is that Gaussian evaluated at the dose received
  expect_equal(tc$dens,
               dnorm(d$Ac, mean = tc$info$mu, sd = sqrt(tc$info$sigma2)))
  expect_true(all(tc$dens > 0))
  expect_named(tc$info, c("mu", "sigma2"))

  expect_error(.gentmt_treatment_density(d$Ac, d$H, "poisson"),
               "kind must be 'binary' or 'normal'")
  # a dose the covariates predict exactly leaves no density to weight by,
  # and the routine says so rather than dividing by zero
  exact <- as.numeric(d$H[, 1])
  expect_error(.gentmt_treatment_density(exact, d$H, "normal"),
               "degenerate and no IP weight exists")
})

test_that("the weights use the density of the dose actually received", {
  d <- mk(60, 7)
  # A regression test. The weights used to re-derive a Gaussian density
  # here regardless of the treatment type, which both scored a binary dose
  # with a Gaussian and read mu and sigma2 that the binary branch does not
  # return, so the call failed outright.
  tb <- .gentmt_treatment_density(d$Ab, d$H, "binary")
  rb <- .gentmt_ip_weights(d$Ab, d$H, kind = "binary")
  marg <- mean(d$Ab)
  want <- ifelse(d$Ab > 0.5, marg / tb$info$prob,
                 (1 - marg) / (1 - tb$info$prob))
  expect_equal(rb$w, want)
  expect_true(all(is.finite(rb$w)))
  # a stabilised binary weight averages about one
  expect_equal(rb$info$mean_weight, mean(rb$w))
  expect_equal(rb$info$mean_weight, 1, tolerance = 0.1)
  # the denominator reported is the Bernoulli density
  expect_equal(rb$info$denominator, tb$dens)
  # unstabilised weights invert that density
  ru <- .gentmt_ip_weights(d$Ab, d$H, kind = "binary", stabilize = FALSE)
  expect_equal(ru$w, 1 / tb$dens)
  # the variance diagnostic compares dose variances, which means nothing
  # for a binary dose, so it is not invented
  expect_true(is.na(rb$info$finite_variance))
  expect_true(is.na(rb$info$variance_ratio))

  # the continuous route is the Gaussian ratio
  tc <- .gentmt_treatment_density(d$Ac, d$H, "normal")
  rc <- .gentmt_ip_weights(d$Ac, d$H, kind = "normal")
  expect_equal(rc$info$denominator, tc$dens)
  expect_equal(rc$w, dnorm(d$Ac, mean(d$Ac), stats::sd(d$Ac)) / tc$dens)
  expect_equal(rc$info$variance_ratio, tc$info$sigma2 / stats::var(d$Ac))
  expect_equal(rc$info$finite_variance,
               tc$info$sigma2 > 0.5 * stats::var(d$Ac))
  # Kish's effective sample size
  expect_equal(rc$info$effective_sample_size, sum(rc$w)^2 / sum(rc$w^2))
  expect_equal(rc$info$max_weight, max(rc$w))
})

test_that("trimming clamps the weights at their own quantiles", {
  d <- mk(80, 11)
  raw <- .gentmt_ip_weights(d$Ac, d$H, kind = "normal")
  tr <- .gentmt_ip_weights(d$Ac, d$H, kind = "normal", trim = 0.1)
  lo <- as.numeric(quantile(raw$w, 0.1))
  hi <- as.numeric(quantile(raw$w, 0.9))
  expect_equal(tr$w, pmin(pmax(raw$w, lo), hi))
  expect_true(max(tr$w) <= max(raw$w) + 1e-12)
  # trimming raises the effective sample size by pulling in the tails
  expect_true(tr$info$effective_sample_size >=
              raw$info$effective_sample_size - 1e-9)
  # a trim outside the open interval is ignored rather than applied
  expect_equal(.gentmt_ip_weights(d$Ac, d$H, trim = 0)$w, raw$w)
  expect_equal(.gentmt_ip_weights(d$Ac, d$H, trim = 0.5)$w, raw$w)
})

test_that("subclassification splits on the fitted dose", {
  d <- mk(80, 13)
  g <- .gentmt_gps_subclassify(d$y, d$Ac, d$H, n_strata = 4L)
  expect_true(is.list(g))
  # the strata partition the sample
  if (!is.null(g$sizes)) {
    expect_equal(sum(g$sizes), d$n)
    expect_true(all(g$sizes > 0))
  }
  expect_error(.gentmt_gps_subclassify(d$y, d$Ac, d$H, n_strata = 1L),
               "at least 2 strata")
  # too few observations to support the requested strata is reported
  expect_error(.gentmt_gps_subclassify(d$y[1:8], d$Ac[1:8],
                                       d$H[1:8, , drop = FALSE],
                                       n_strata = 5L),
               "cannot support")
})

test_that("the dose-response curve spans the observed doses", {
  d <- mk(60, 17)
  r <- .gentmt_dose_response_curve(d$y, d$Ac, d$H)
  expect_true(is.list(r))
  # the default grid is twenty-one points across the observed dose range
  expect_length(r$doses, 21L)
  expect_equal(min(r$doses), min(d$Ac))
  expect_equal(max(r$doses), max(d$Ac))
  expect_length(r$curve, 21L)
  expect_true(all(is.finite(r$curve)))
  # the outcome rises with the dose by construction, so the curve does too
  expect_true(r$curve[21] > r$curve[1])
  # an explicit dose grid is honoured
  g <- c(-1, 0, 1)
  e <- .gentmt_dose_response_curve(d$y, d$Ac, d$H, doses = g)
  expect_equal(e$doses, g)
  expect_length(e$curve, 3L)
})
