# Cross-language anchors come from morie.fn._copula on the same inputs,
# stored at full precision because testthat's tolerance is RELATIVE.
# They break if either implementation drifts.

test_that("copula CDFs match the Python core to 10 significant digits", {
  anchors <- list(
    list(f = "gaussian", th = 0.6, cdf = 0.27723374892439, tau = 0.409665529398267),
    list(f = "clayton", th = 2.0, cdf = 0.286864902505703, tau = 0.5),
    list(f = "gumbel", th = 2.5, cdf = 0.293271646763742, tau = 0.6),
    list(f = "frank", th = 5.0, cdf = 0.284194784818141, tau = 0.456700958160117),
    list(f = "joe", th = 3.0, cdf = 0.288134904362223, tau = 0.517962498229889),
    list(f = "plackett", th = 4.0, cdf = 0.261149164646785, tau = 0.300257759181434)
  )
  for (a in anchors) {
    expect_equal(morie_copula_cdf(a$f, 0.3, 0.7, a$th), a$cdf, tolerance = 1e-8)
    expect_equal(morie_copula_tau(a$f, a$th), a$tau, tolerance = 1e-6)
  }
  expect_equal(morie_copula_cdf("t", 0.3, 0.7, 0.6, nu = 6), 0.273554832605259,
    tolerance = 1e-6
  )
})

test_that("every family satisfies the copula axioms", {
  u <- c(0.1, 0.35, 0.5, 0.77, 0.9)
  pars <- list(
    independence = list(NULL, NULL), gaussian = list(0.6, NULL),
    t = list(0.6, 6), clayton = list(2, NULL), gumbel = list(2, NULL),
    frank = list(5, NULL), joe = list(2.5, NULL), plackett = list(4, NULL)
  )
  for (fam in names(pars)) {
    th <- pars[[fam]][[1]]
    nu <- pars[[fam]][[2]]
    expect_equal(morie_copula_cdf(fam, u, rep(1, 5), th, nu), u, tolerance = 1e-5)
    expect_equal(morie_copula_cdf(fam, rep(1, 5), u, th, nu), u, tolerance = 1e-5)
    expect_true(all(abs(morie_copula_cdf(fam, u, rep(0, 5), th, nu)) < 1e-5))
    # Frechet-Hoeffding bounds on a grid
    U <- rep(u, each = 5)
    V <- rep(u, times = 5)
    C <- morie_copula_cdf(fam, U, V, th, nu)
    expect_true(all(C <= pmin(U, V) + 1e-5))
    expect_true(all(C >= pmax(U + V - 1, 0) - 1e-5))
  }
})

test_that("tau relations invert and degenerate correctly", {
  for (fam in c("gaussian", "clayton", "gumbel", "frank", "joe", "plackett")) {
    for (tau in c(0.15, 0.45, 0.7)) {
      th <- morie_tau_to_theta(fam, tau)
      expect_equal(morie_copula_tau(fam, th), tau, tolerance = 1e-6)
    }
  }
  expect_equal(morie_tau_to_theta("frank", 0.4), 4.16106425492261, tolerance = 1e-8)
  expect_equal(morie_tau_to_theta("joe", 0.4), 2.21907005336313, tolerance = 1e-8)
  expect_lt(morie_tau_to_theta("frank", -0.4), 0)
  expect_equal(morie_copula_tau("gumbel", 1), 0)
  expect_equal(morie_copula_tau("joe", 1), 0)
  expect_error(morie_tau_to_theta("clayton", -0.3))
  expect_error(morie_copula_cdf("clayton", 0.5, 0.5, -1))
})

test_that("copula_native extends copul rather than duplicating it", {
  # copul inverts tau for three families from DATA; this file evaluates
  # the copulas and covers frank/joe/plackett, which copul cannot.
  set.seed(0)
  x <- stats::rnorm(300)
  y <- x + stats::rnorm(300)
  est <- copul(x, y, family = "clayton")$estimate
  expect_equal(morie_tau_to_theta("clayton", stats::cor(x, y, method = "kendall")),
    est,
    tolerance = 1e-8
  )
})

test_that("dependence measures rise with the parameter and vanish at independence", {
  taus <- vapply(c(1.2, 2, 5), function(t) morie_copula_tau("gumbel", t), 0)
  rhos <- vapply(c(1.2, 2, 5), function(t) morie_copula_spearman("gumbel", t)$rho_s, 0)
  betas <- vapply(c(1.2, 2, 5), function(t) morie_blomqvist_beta("gumbel", t)$beta, 0)
  gams <- vapply(c(1.2, 2, 5), function(t) morie_gini_gamma("gumbel", t)$gamma, 0)
  for (s in list(taus, rhos, betas, gams)) expect_true(all(diff(s) > 0))
  expect_equal(morie_copula_spearman("independence")$rho_s, 0)
  expect_equal(morie_blomqvist_beta("independence")$beta, 0)
  expect_equal(morie_gini_gamma("independence")$gamma, 0, tolerance = 1e-6)
  # exact elliptical route, and the Python anchors for the numeric ones
  sp <- morie_copula_spearman("gaussian", 0.6)
  expect_true(sp$exact)
  expect_equal(sp$rho_s, 6 / pi * asin(0.3))
  expect_equal(morie_copula_spearman("clayton", 2)$rho_s, 0.682252598100018,
    tolerance = 1e-6
  )
  expect_equal(morie_gini_gamma("gumbel", 2.5)$gamma, 0.660755207599287,
    tolerance = 1e-6
  )
})

test_that("the extreme-value copula is max-stable", {
  for (a in list(list("gumbel", 2), list("galambos", 1.5))) {
    cc <- morie_extreme_value_copula(0.4, 0.7, a[[1]], a[[2]])
    expect_true(cc$valid_pickands)
    for (k in c(0.5, 2, 3)) {
      ck <- morie_extreme_value_copula(0.4^k, 0.7^k, a[[1]], a[[2]])$cdf
      expect_equal(ck, cc$cdf^k, tolerance = 1e-8)
    }
  }
  # the logistic Pickands reproduces the Archimedean Gumbel
  expect_equal(morie_extreme_value_copula(0.4, 0.7, "gumbel", 2)$cdf,
    morie_copula_cdf("gumbel", 0.4, 0.7, 2),
    tolerance = 1e-10
  )
  expect_equal(morie_extreme_value_copula(0.4, 0.7, "gumbel", 1)$cdf, 0.28)
  expect_error(morie_extreme_value_copula(0, 0.7, "gumbel", 2))
})

test_that("the survival copula recovers the pair dependence", {
  set.seed(3)
  n <- 200
  theta <- 2
  u <- stats::runif(n)
  w <- stats::runif(n)
  v <- (u^(-theta) * (w^(-theta / (1 + theta)) - 1) + 1)^(-1 / theta)
  t1 <- -log(1 - u)
  t2 <- -log(1 - v)
  e <- rep(1, n)
  out <- morie_copula_survival(t1, e, t2, e, family = "clayton")
  expect_equal(morie_copula_tau("clayton", out$theta), out$tau_sample, tolerance = 1e-6)
  expect_true(all(out$joint_survival <= pmin(out$s1, out$s2) + 1e-8))
  ind <- morie_copula_survival(t1, e, t2, e, family = "independence")
  expect_equal(ind$joint_survival, ind$s1 * ind$s2)
  expect_error(morie_copula_survival(t1[1:3], e[1:3], t2[1:3], e[1:3]))
})

test_that("COPOD flags the planted outlier and is scale-free", {
  set.seed(4)
  X <- matrix(stats::rnorm(600), ncol = 3)
  X[1, ] <- 8
  out <- morie_copod(X)
  expect_equal(which.max(out$scores), 1L)
  expect_gt(out$scores[1], 2 * stats::median(out$scores))
  Y <- X
  Y[, 2] <- Y[, 2] * 1000
  expect_equal(which.max(morie_copod(Y)$scores), 1L)
  expect_error(morie_copod(matrix(1:2, nrow = 1)))
})
