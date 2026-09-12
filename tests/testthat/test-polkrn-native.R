# Marginal structural model with a polynomial or kernel exposure basis
# (Hernan, Brumback & Robins 2002; weights by Robins, Hernan & Brumback
# 2000).
#
# Anchors outside the module: at the first time point both weight models
# are intercept-only, so a stabilised weight must be exactly one; when
# treatment is assigned independently of the covariates the two models
# agree and the weights stay at one; the unstabilised weights are the
# reciprocal of the probability of the treatment actually received; and a
# known linear dose-response must come back out of the fitted curve.

sim_hist <- function(n = 400, seed = 1, confounded = FALSE) {
  set.seed(seed)
  L1 <- rbinom(n, 1, 0.5)
  A1 <- if (confounded) rbinom(n, 1, 0.3 + 0.4 * L1) else rbinom(n, 1, 0.5)
  L2 <- rbinom(n, 1, 0.3 + 0.4 * A1)
  A2 <- if (confounded) rbinom(n, 1, 0.3 + 0.4 * L2) else rbinom(n, 1, 0.5)
  cum <- A1 + A2
  y <- 1 + 2 * cum + 0.5 * L1 + rnorm(n, 0, 0.5)
  list(y = y, A = list(A1, A2), L = list(L1, L2), cum = cum)
}

test_that("the first stabilised weight is exactly one", {
  d <- sim_hist(300, 2)
  ipw <- .polkrn_ip_weights_history(d$A, d$L, kind = "binary",
                                    stabilize = TRUE, trim = NULL)
  # with no past to condition on, the numerator and denominator models are
  # the same intercept-only fit, so their ratio is one for everybody
  expect_equal(ipw$per_time[[1]]$weight, rep(1, 300))
  expect_length(ipw$per_time, 2L)
  expect_length(ipw$w, 300L)
  # the cumulative weight is the product over time
  expect_equal(ipw$w, ipw$per_time[[1]]$weight * ipw$per_time[[2]]$weight)
  expect_true(all(ipw$w > 0))
})

test_that("unstabilised weights invert the probability of what happened", {
  d <- sim_hist(300, 3)
  un <- .polkrn_ip_weights_history(d$A, d$L, kind = "binary",
                                    stabilize = FALSE, trim = NULL)
  # at the first time the denominator is an intercept-only fit, so the
  # probability is the sample proportion of the treatment received
  p1 <- mean(d$A[[1]])
  want <- ifelse(d$A[[1]] == 1, 1 / p1, 1 / (1 - p1))
  expect_equal(un$per_time[[1]]$weight, want, tolerance = 1e-6)
  # every unstabilised weight is at least one, since it inverts a
  # probability
  expect_true(all(un$per_time[[1]]$weight >= 1 - 1e-9))
  # and their mean is about twice the number of arms for a fair coin
  expect_equal(mean(un$per_time[[1]]$weight), 2, tolerance = 0.05)
})

test_that("weighting only bites when treatment depends on the covariates", {
  # assignment independent of L: the numerator and denominator models carry
  # the same information, so the stabilised weights stay at one
  ind <- sim_hist(1500, 5, confounded = FALSE)
  wi <- .polkrn_ip_weights_history(ind$A, ind$L, kind = "binary",
                                    stabilize = TRUE, trim = NULL)
  expect_equal(mean(wi$w), 1, tolerance = 0.03)
  expect_true(stats::sd(wi$w) < 0.15)

  # assignment driven by L: the weights genuinely vary, but a stabilised
  # set still averages about one
  dep <- sim_hist(1500, 5, confounded = TRUE)
  wd <- .polkrn_ip_weights_history(dep$A, dep$L, kind = "binary",
                                    stabilize = TRUE, trim = NULL)
  expect_equal(mean(wd$w), 1, tolerance = 0.05)
  expect_true(stats::sd(wd$w) > stats::sd(wi$w))

  # trimming caps the weights from above
  tr <- .polkrn_ip_weights_history(dep$A, dep$L, kind = "binary",
                                   stabilize = FALSE, trim = 3)
  expect_true(all(tr$per_time[[1]]$weight <= 3 + 1e-12))
  expect_true(all(tr$per_time[[2]]$weight <= 3 + 1e-12))
  untr <- .polkrn_ip_weights_history(dep$A, dep$L, kind = "binary",
                                     stabilize = FALSE, trim = NULL)
  expect_true(max(untr$w) >= max(tr$w) - 1e-12)
})

test_that("a covariate block may be absent at a time point", {
  d <- sim_hist(200, 7)
  # no covariates at all: both models condition on past treatment only
  none <- .polkrn_ip_weights_history(d$A, list(NULL, NULL), kind = "binary",
                                     stabilize = TRUE, trim = NULL)
  # with nothing to adjust for, the numerator and denominator coincide
  expect_equal(none$w, rep(1, 200), tolerance = 1e-8)
})

test_that("only a binary exposure is accepted", {
  d <- sim_hist(50, 11)
  # the argument used to be taken and ignored, so a caller asking for a
  # continuous exposure silently received binary weights
  expect_error(.polkrn_ip_weights_history(d$A, d$L, kind = "continuous",
                                          stabilize = TRUE, trim = NULL),
               "kind must be 'binary'")
  expect_error(morie_polkrn(d$y, d$A, d$L, kind = "gaussian"),
               "kind must be 'binary'")
  # the default is accepted
  expect_true(is.list(.polkrn_ip_weights_history(d$A, d$L, kind = "binary",
                                                 stabilize = TRUE,
                                                 trim = NULL)))
})

test_that("the exposure summary and the radial basis are their definitions", {
  A <- list(c(1, 0, 1), c(1, 1, 0))
  expect_equal(as.numeric(exposure_summary(A, "cumulative")), c(2, 1, 1))
  # a quantile matches base R's default type
  x <- c(3, 1, 4, 1, 5, 9, 2, 6)
  for (q in c(0, 0.25, 0.5, 0.75, 1)) {
    expect_equal(.polkrn_quantile7(x, q),
                 unname(quantile(x, q, type = 7)))
  }
  # the radial basis is a Gaussian bump at each centre
  rb <- rbf_basis(c(0, 1, 2), n_centres = 3)
  expect_length(rb$centres, 3L)
  expect_true(rb$width > 0)
  expect_equal(nrow(rb$X), 3L)
  # each column is a Gaussian bump centred on its own centre, so the
  # largest value in a column sits at the nearest exposure
  for (k in seq_along(rb$centres)) {
    expect_equal(rb$X[which.min(abs(c(0, 1, 2) - rb$centres[k])), k],
                 max(rb$X[, k]))
  }
  expect_true(all(rb$X > 0 & rb$X <= 1))
})

test_that("the weighted fit and the logistic fit agree with base R", {
  set.seed(13)
  n <- 60
  x <- rnorm(n)
  y <- 2 - 0.5 * x + rnorm(n, 0, 0.3)
  w <- runif(n, 0.5, 2)
  # the helper adds its own intercept, so the design passed in must not
  # already carry one or the cross-product is singular
  got <- .polkrn_wls(matrix(x, ncol = 1), y, w)
  ref <- lm(y ~ x, weights = w)
  expect_equal(as.numeric(got$coef), unname(coef(ref)), tolerance = 1e-8)
  expect_equal(as.numeric(got$se), unname(sqrt(diag(vcov(ref)))),
               tolerance = 1e-7)
  expect_equal(dim(got$vcov), c(2L, 2L))
  # unit weights reduce to ordinary least squares
  gu <- .polkrn_wls(matrix(x, ncol = 1), y, rep(1, n))
  expect_equal(as.numeric(gu$coef), unname(coef(lm(y ~ x))),
               tolerance = 1e-8)

  # the logistic fit is base R's, and it takes the design with an intercept
  # because it is called that way internally
  z <- rbinom(n, 1, plogis(0.3 * x))
  gl <- .polkrn_logistic(cbind(1, x), z)
  rg <- glm(z ~ x, family = binomial())
  expect_equal(as.numeric(gl$mu), unname(fitted(rg)), tolerance = 1e-6)
  expect_true(all(gl$mu > 0 & gl$mu < 1))
})

test_that("the model recovers a known linear dose-response", {
  # the outcome is 1 + 2 * cumulative treatment, and treatment is assigned
  # independently of everything, so the fitted curve must have slope two
  d <- sim_hist(1200, 17, confounded = FALSE)
  r <- morie_polkrn(d$y, d$A, d$L, degree = 1, basis = "polynomial")
  expect_true(is.list(r))
  expect_match(r$method, "Hernan, Brumback & Robins")
  # the default grid spans the observed exposure in twenty-one steps
  expect_length(r$grid, 21L)
  expect_equal(min(r$grid), min(d$cum))
  expect_equal(max(r$grid), max(d$cum))
  # the polynomial curve is evaluated on that grid
  expect_length(r$curve_polynomial, 21L)
  expect_true(all(is.finite(r$curve_polynomial)))
  # a degree-one fit is a straight line of slope two
  sl <- diff(r$curve_polynomial) / diff(r$grid)
  expect_equal(sl, rep(2, 20), tolerance = 0.25)
  # and the reported estimate is that slope
  expect_equal(r$estimate, 2, tolerance = 0.25)
})

test_that("the three bases each produce their own curve", {
  d <- sim_hist(400, 19)
  # the cumulative exposure of a two-period history takes three values, so
  # the radial basis has to stay inside that rank
  both <- morie_polkrn(d$y, d$A, d$L, degree = 2, basis = "both",
                       n_centres = 2)
  expect_false(is.null(both$curve_polynomial))
  expect_false(is.null(both$curve_kernel))
  poly <- morie_polkrn(d$y, d$A, d$L, degree = 2, basis = "polynomial")
  expect_false(is.null(poly$curve_polynomial))
  kern <- morie_polkrn(d$y, d$A, d$L, basis = "kernel", n_centres = 2)
  expect_false(is.null(kern$curve_kernel))
  # the kernel route reports an averaged slope rather than a coefficient
  expect_true(is.finite(kern$estimate) || is.nan(kern$estimate))
  # an explicit grid is honoured
  g <- c(0, 0.5, 1, 1.5, 2)
  eg <- morie_polkrn(d$y, d$A, d$L, degree = 1, basis = "polynomial",
                     grid = g)
  expect_equal(eg$grid, g)
  expect_length(eg$curve_polynomial, 5L)
})

test_that("polkrn validates its arguments", {
  d <- sim_hist(60, 23)
  expect_error(morie_polkrn(d$y, d$A, d$L, basis = "spline"),
               "basis must be one of")
  expect_error(morie_polkrn(d$y, d$A, d$L, degree = 0),
               "degree must be at least 1")
  expect_error(morie_polkrn(d$y, d$A, list(d$L[[1]])),
               "treatment times but .* covariate blocks")
  # a missing covariate history is filled in as absent, not an error
  expect_true(is.list(morie_polkrn(d$y, d$A, NULL, degree = 1,
                                   basis = "polynomial")))
  expect_type(.polkrn_cheatsheet(), "character")
})
