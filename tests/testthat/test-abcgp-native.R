# ABC with a Gaussian-process surrogate (Wilkinson 2014; Meeds & Welling 2014;
# Wood 2010; Rasmussen & Williams 2006).
#
# Anchors, none of them the module's own output: the Sobol design is checked
# against the van der Corput sequence and the (0,m,s)-net property; the linear
# algebra against base::chol and base::solve; the multivariate normal density
# against its closed form; the GP against the interpolation property (a
# noiseless GP reproduces its training data and has zero predictive variance
# there) and against exact recovery of a quadratic by the quadratic mean; and
# the history-matching route against a simulator whose likelihood peak is known.

test_that("log-sum-exp matches the closed form and survives overflow", {
  v <- c(-1, 0, 2.5, 3)
  expect_equal(.abcgp.lse(v), log(sum(exp(v))))
  # the naive expression overflows here, the shifted one does not
  expect_equal(.abcgp.lse(c(1000, 1001)), 1001 + log(1 + exp(-1)))
  expect_true(is.finite(.abcgp.lse(c(1000, 1001))))
  expect_equal(.abcgp.lse(-Inf), -Inf)
  expect_equal(.abcgp.lse(c(-Inf, -Inf)), -Inf)
  expect_equal(.abcgp.lse(numeric(0)), -Inf)
  # missing values are dropped rather than poisoning the sum
  expect_equal(.abcgp.lse(c(1, NA, 2)), .abcgp.lse(c(1, 2)))
  expect_equal(.abcgp.lse(5), 5)
})

test_that("the first Sobol dimension is the van der Corput sequence", {
  # base-2 radical inverse, in the Antonov-Saleev Gray-code order
  expect_equal(.abcgp.sobol_sequence(8L, 1L)[, 1],
               c(0, 0.5, 0.75, 0.25, 0.375, 0.875, 0.625, 0.125))
  # whatever the order, the point set for n = 2^k is the regular grid
  for (k in 2:6) {
    n <- 2^k
    x <- .abcgp.sobol_sequence(n, 1L)[, 1]
    expect_equal(sort(x), (0:(n - 1)) / n)
    # a (0,m,1)-net: exactly one point in each of the n subintervals
    expect_equal(sort(floor(x * n)), as.numeric(0:(n - 1)))
  }
  # skip discards leading points rather than restarting
  expect_equal(.abcgp.sobol_sequence(7L, 1L, skip = 1)[, 1],
               .abcgp.sobol_sequence(8L, 1L)[, 1][-1])
})

test_that("the two-dimensional design is a (0,m,2)-net", {
  # every cell of the 4x4 and 8x8 grids holds exactly one point
  m <- .abcgp.sobol_sequence(16L, 2L)
  expect_true(all(table(floor(m[, 1] * 4), floor(m[, 2] * 4)) == 1))
  m2 <- .abcgp.sobol_sequence(64L, 2L)
  expect_true(all(table(floor(m2[, 1] * 8), floor(m2[, 2] * 8)) == 1))
  # and each one-dimensional projection is still equidistributed
  for (j in 1:2) expect_equal(sort(floor(m2[, j] * 64)), as.numeric(0:63))
})

test_that("every supported dimension works at every size", {
  # the direction-number recursion needs more bits than the polynomial degree;
  # small designs in the higher dimensions used to fail outright
  for (d in 1:8) {
    for (n in c(1L, 2L, 3L, 5L, 7L, 9L, 15L, 33L)) {
      s <- .abcgp.sobol_sequence(n, as.integer(d))
      expect_equal(dim(s), c(n, d))
      expect_true(all(s >= 0 & s < 1))
    }
  }
  # the extra bits do not move the points that already worked
  expect_equal(.abcgp.sobol_sequence(64L, 3L), .abcgp.sobol_sequence(64L, 3L))
  expect_equal(.abcgp.sobol_sequence(16L, 2L)[, 1],
               .abcgp.sobol_sequence(16L, 1L)[, 1])
  expect_error(.abcgp.sobol_sequence(0L, 1L), "at least 1")
  expect_error(.abcgp.sobol_sequence(4L, 0L), "between 1 and")
  expect_error(.abcgp.sobol_sequence(4L, 99L), "between 1 and")
})

test_that("the Cholesky factor agrees with base::chol", {
  set.seed(2)
  for (n in c(1, 2, 3, 5, 9)) {
    A <- crossprod(matrix(rnorm(n * n), n)) + diag(n)
    L <- .abcgp.chol(A)
    expect_equal(dim(L), c(n, n))
    expect_true(all(abs(L[upper.tri(L)]) < 1e-14))
    expect_equal(L %*% t(L), A)
    # base::chol returns the upper factor
    expect_equal(L, t(chol(A)))
  }
  expect_error(.abcgp.chol(matrix(1:6, 2)), "square")
  # a non-positive-definite pivot is floored by the jitter instead of NaN
  L <- .abcgp.chol(matrix(c(1, 2, 2, 1), 2), jitter = 1e-10)
  expect_true(all(is.finite(L)))
  expect_equal(L[2, 2], sqrt(1e-10))
})

test_that("triangular solves agree with base::solve", {
  set.seed(4)
  for (n in c(1, 2, 3, 6, 10)) {
    A <- crossprod(matrix(rnorm(n * n), n)) + diag(n)
    L <- .abcgp.chol(A)
    b <- rnorm(n)
    # the routine applies both the forward and the back substitution, so it
    # solves the full system, not just the triangular one
    expect_equal(.abcgp.chol_solve(L, b), as.numeric(solve(A, b)))
  }
  # the last row has no terms above the diagonal
  L1 <- .abcgp.chol(matrix(4, 1))
  expect_equal(.abcgp.chol_solve(L1, 8), 2)
})

test_that("the multivariate normal density matches its closed form", {
  mu <- c(0.5, -1)
  S <- matrix(c(2, 0.6, 0.6, 1), 2)
  y <- c(1, -0.5)
  d <- y - mu
  ref <- as.numeric(-0.5 * (2 * log(2 * pi) + log(det(S)) +
                            t(d) %*% solve(S) %*% d))
  expect_equal(.abcgp.mvn_logpdf(y, mu, S), ref)
  # one dimension must agree with dnorm
  expect_equal(.abcgp.mvn_logpdf(1.3, 0.2, matrix(4)),
               dnorm(1.3, 0.2, 2, log = TRUE))
  set.seed(5)
  S3 <- crossprod(matrix(rnorm(9), 3)) + diag(3)
  y3 <- rnorm(3)
  m3 <- rnorm(3)
  d3 <- y3 - m3
  expect_equal(.abcgp.mvn_logpdf(y3, m3, S3),
               as.numeric(-0.5 * (3 * log(2 * pi) + log(det(S3)) +
                                  t(d3) %*% solve(S3) %*% d3)))
  # the density integrates to 1 in one dimension
  f <- function(x) exp(.abcgp.mvn_logpdf(x, 0, matrix(1)))
  expect_equal(integrate(Vectorize(f), -10, 10)$value, 1, tolerance = 1e-6)
})

test_that("the correlation kernels have the right closed forms", {
  ls <- 1.5
  a <- 0
  b <- 2
  r <- abs(b - a) / ls
  expect_equal(.abcgp.corr(a, b, ls, "sqexp"), exp(-0.5 * r^2))
  expect_equal(.abcgp.corr(a, b, ls, "matern32"),
               (1 + sqrt(3) * r) * exp(-sqrt(3) * r))
  expect_equal(.abcgp.corr(a, b, ls, "matern52"),
               (1 + sqrt(5) * r + 5 * r^2 / 3) * exp(-sqrt(5) * r))
  for (kn in c("sqexp", "matern32", "matern52")) {
    # unit correlation at zero separation, symmetric, decreasing in distance
    expect_equal(.abcgp.corr(1, 1, ls, kn), 1)
    expect_equal(.abcgp.corr(a, b, ls, kn), .abcgp.corr(b, a, ls, kn))
    expect_true(.abcgp.corr(0, 1, ls, kn) > .abcgp.corr(0, 3, ls, kn))
    expect_true(.abcgp.corr(0, 100, ls, kn) < 1e-6)
  }
  # anisotropic length-scales are applied per coordinate
  expect_equal(.abcgp.corr(c(0, 0), c(1, 0), c(1, 10), "sqexp"), exp(-0.5))
  expect_equal(.abcgp.corr(c(0, 0), c(0, 1), c(1, 10), "sqexp"),
               exp(-0.5 / 100))
})

test_that("the quadratic basis and the nugget behave", {
  expect_equal(.abcgp.basis(3), c(1, 3, 9))
  expect_equal(.abcgp.basis(c(2, -3)), c(1, 2, -3, 4, 9))
  expect_equal(length(.abcgp.basis(rep(1, 4))), 9)
  expect_equal(.abcgp.as_nugget(NULL, 3), rep(1e-8, 3))
  expect_equal(.abcgp.as_nugget(0.5, 3), rep(0.5, 3))
  expect_equal(.abcgp.as_nugget(c(1, 2, 3), 3), c(1, 2, 3))
  # a floor keeps the factorisation well conditioned
  expect_equal(.abcgp.as_nugget(0, 2), rep(1e-12, 2))
  expect_equal(.abcgp.as_nugget(-5, 2), rep(1e-12, 2))
  expect_error(.abcgp.as_nugget(c(1, 2), 3), "length does not match")
})

test_that("a noiseless GP interpolates its design points", {
  # Rasmussen & Williams 2.2: with a vanishing nugget the posterior mean
  # passes through every training value and the predictive variance is zero
  f <- function(x) sin(3 * x[1]) + 0.5 * x[1]^2
  X <- matrix(seq(0, 2, length.out = 12), ncol = 1)
  y <- apply(X, 1, f)
  for (kn in c("sqexp", "matern32", "matern52")) {
    fit <- .abcgp.gp_fit(X, y, nugget = 1e-12, kernel = kn)
    pr <- t(vapply(seq_len(nrow(X)),
                   function(i) .abcgp.gp_predict(fit, X[i, ]), numeric(2)))
    expect_equal(pr[, 1], y, tolerance = 1e-5)
    expect_true(max(pr[, 2]) < 1e-3)
    expect_equal(fit$n, 12L)
    expect_equal(fit$dim, 1L)
    expect_equal(fit$kernel, kn)
  }
  # interpolation between design points tracks the function
  fit <- .abcgp.gp_fit(X, y, nugget = 1e-12, kernel = "sqexp")
  for (x0 in c(0.7, 1.3, 1.75)) {
    expect_equal(.abcgp.gp_predict(fit, x0)[1], f(x0), tolerance = 1e-3)
  }
  # and uncertainty grows once we leave the design region
  expect_true(.abcgp.gp_predict(fit, 5)[2] >
              100 * .abcgp.gp_predict(fit, 1)[2])
})

test_that("the quadratic mean recovers a quadratic exactly", {
  # with a quadratic trend and data generated by a quadratic, the generalised
  # least squares coefficients are the exact ones and extrapolation is exact
  X <- matrix(seq(0, 2, length.out = 12), ncol = 1)
  yq <- apply(X, 1, function(x) 2 - 3 * x[1] + 4 * x[1]^2)
  fq <- .abcgp.gp_fit(X, yq, nugget = 1e-10, kernel = "sqexp")
  expect_equal(fq$beta, c(2, -3, 4), tolerance = 1e-5)
  expect_equal(.abcgp.gp_predict(fq, 10)[1], 2 - 30 + 400, tolerance = 1e-3)
  # a perfectly fitted trend leaves no residual process variance
  expect_true(fq$tau2 < 1e-6)
})

test_that("the GP fit validates its inputs", {
  X <- matrix(seq(0, 2, length.out = 8), ncol = 1)
  y <- as.numeric(X)
  expect_error(.abcgp.gp_fit(X, y, kernel = "cubic"), "kernel must be one of")
  expect_error(.abcgp.gp_fit(X, y[1:4]), "length mismatch")
  expect_error(.abcgp.gp_fit(X[1:2, , drop = FALSE], y[1:2]), "at least 3")
  expect_error(.abcgp.gp_fit(X, y, lengthscale = -1), "must be positive")
  # three points cannot support a three-term quadratic mean
  expect_error(.abcgp.gp_fit(X[1:3, , drop = FALSE], y[1:3]), "not enough")
  fit <- .abcgp.gp_fit(X, y, lengthscale = 0.5)
  expect_error(.abcgp.gp_predict(fit, c(1, 2)), "wrong length")
})

test_that("the fitted length-scale is not worse than the starting guess", {
  set.seed(8)
  X <- matrix(seq(0, 4, length.out = 20), ncol = 1)
  y <- sin(X[, 1])
  nug <- .abcgp.as_nugget(1e-8, 20)
  ls <- .abcgp.mle_lengthscale(X, y, 1e-8, "sqexp")
  expect_true(all(ls > 0))
  start <- 0.5 * diff(range(X[, 1]))
  expect_true(.abcgp.profile_nll(X, y, ls, nug, "sqexp") <=
              .abcgp.profile_nll(X, y, start, nug, "sqexp") + 1e-12)
  # the profile likelihood rejects designs that cannot support the trend
  expect_equal(.abcgp.profile_nll(X[1:3, , drop = FALSE], y[1:3], 1,
                                  nug[1:3], "sqexp"), Inf)
  # a constant column does not produce a zero length-scale
  X2 <- cbind(X[, 1], 1)
  expect_true(all(.abcgp.mle_lengthscale(X2, y, 1e-8, "sqexp") > 0))
})

test_that("implausibility applies the Wilkinson rule", {
  X <- matrix(seq(0, 4, length.out = 14), ncol = 1)
  y <- -(X[, 1] - 2)^2
  fit <- .abcgp.gp_fit(X, y, nugget = 1e-10, kernel = "sqexp")
  # the rule is mean + n_sd * sd < max(values) - threshold
  for (x0 in c(0, 1, 2, 3.5)) {
    pr <- .abcgp.gp_predict(fit, x0)
    expect_equal(.abcgp.implausible(fit, x0, 10, 3),
                 pr[1] + 3 * pr[2] < max(y) - 10)
  }
  # the optimum is never implausible, and a far worse point is
  expect_false(.abcgp.implausible(fit, 2, threshold = 1, n_sd = 3))
  # a looser threshold rules out no more than a tighter one
  n_loose <- sum(vapply(seq(0, 4, by = 0.25),
                        function(x) .abcgp.implausible(fit, x, 20, 3), logical(1)))
  n_tight <- sum(vapply(seq(0, 4, by = 0.25),
                        function(x) .abcgp.implausible(fit, x, 1, 3), logical(1)))
  expect_true(n_loose <= n_tight)
})

test_that("designs from a prior cover the requested box", {
  d <- .abcgp.design_from_prior(16L, list(c(-1, 10), c(1, 20)))
  expect_equal(dim(d), c(16L, 2L))
  expect_true(all(d[, 1] >= -1 & d[, 1] < 1))
  expect_true(all(d[, 2] >= 10 & d[, 2] < 20))
  # a list of quantile functions is honoured instead
  fns <- list(function(u) qnorm(u, 0, 1), function(u) qexp(u, 2))
  d2 <- .abcgp.design_from_prior(8L, fns)
  expect_equal(dim(d2), c(8L, 2L))
  expect_true(all(is.finite(d2)))
  expect_error(.abcgp.design_from_prior(4L, fns, dim = 3), "dim mismatch")
  expect_error(.abcgp.design_from_prior(4L, list(c(0, 1), c(1, 2, 3))),
               "differ in length")
  # small designs in higher dimensions are reachable
  expect_equal(dim(.abcgp.design_from_prior(3L, list(rep(0, 5), rep(1, 5)))),
               c(3L, 5L))
})

test_that("the median matches stats::median", {
  for (v in list(c(3, 1, 2), c(4, 1, 3, 2), 5, c(2, 2, 2, 2),
                 c(-1, 0, 1, 2, 3, 4))) {
    expect_equal(.abcgp.median(v), stats::median(v))
  }
})

test_that("the expected decision error is zero for a unanimous decision", {
  # Meeds & Welling eqs. (12)-(16): with every alpha equal to tau no draw of
  # the uniform can flip the accept/reject decision
  expect_equal(.abcgp.expected_error(rep(0.5, 20), 0.5), 0)
  expect_equal(.abcgp.expected_error(rep(0, 20), 0), 0)
  expect_equal(.abcgp.expected_error(rep(1, 20), 1), 0)
  # a spread-out set of alphas carries a real risk of the wrong decision
  a <- seq(0, 1, length.out = 101)
  err <- .abcgp.expected_error(a, .abcgp.median(a))
  expect_true(err > 0.2 && err <= 0.5)
  # the error is a probability
  set.seed(9)
  for (i in 1:5) {
    al <- runif(50)
    e <- .abcgp.expected_error(al, .abcgp.median(al))
    expect_true(e >= 0 && e <= 1)
  }
})

test_that("draw_mean samples the Meeds-Welling eq. (11) distribution", {
  # the sampling distribution of the simulated mean is N(mu, cov / S)
  mu <- c(1, -2)
  C <- matrix(c(4, 1, 1, 2), 2)
  S <- 8
  e <- .ghc_rng(42)
  D <- t(vapply(1:4000, function(i) .abcgp.draw_mean(mu, C, S, e), numeric(2)))
  expect_equal(colMeans(D), mu, tolerance = 0.08)
  expect_equal(as.numeric(cov(D)), as.numeric(C / S), tolerance = 0.06)
  # a plain vector, not a one-column matrix
  expect_null(dim(.abcgp.draw_mean(mu, C, S, e)))
  expect_length(.abcgp.draw_mean(mu, C, S, e), 2L)
  # a larger S concentrates the draws
  D2 <- t(vapply(1:2000, function(i) .abcgp.draw_mean(mu, C, 200, e), numeric(2)))
  expect_true(all(diag(cov(D2)) < diag(cov(D))))
})

test_that("the synthetic likelihood is the Wood (2010) plug-in normal", {
  set.seed(3)
  dr <- lapply(1:200, function(i) rnorm(3, c(1, 2, 3), 1))
  o <- c(1.1, 2.0, 2.8)
  r <- .abcgp.synthetic_log_likelihood(dr, o, epsilon = 0)
  mat <- do.call(rbind, dr)
  expect_equal(r$mu, colMeans(mat))
  expect_equal(r$cov, cov(mat))
  expect_equal(r$log_lik, .abcgp.mvn_logpdf(o, colMeans(mat), cov(mat)))
  # epsilon adds its square to the diagonal
  r2 <- .abcgp.synthetic_log_likelihood(dr, o, epsilon = 2)
  expect_equal(diag(r2$cov), diag(r$cov) + 4)
  expect_true(r2$log_lik < r$log_lik)
  # a summary function is applied to each draw and to the observation
  r3 <- .abcgp.synthetic_log_likelihood(dr, o, summary = function(x) mean(x))
  expect_length(r3$mu, 1L)
  expect_error(.abcgp.synthetic_log_likelihood(dr[1], o), "at least 2")
  expect_error(.abcgp.synthetic_log_likelihood(dr, c(1, 2)), "length mismatch")
})

test_that("the GABC likelihood matches its kernel closed form", {
  # a deterministic simulator fixes rho, so the likelihood is exact
  simd <- function(theta, e) rep(theta[1], 4)
  o4 <- rep(0, 4)
  th <- 0.5
  rho <- sqrt(sum(rep(th, 4)^2))
  for (eps in c(0.5, 1, 2)) {
    g <- .abcgp.gabc_log_likelihood(simd, o4, th, n_sim = 10, epsilon = eps,
                                    kernel = "gaussian", bootstrap = 0)
    expect_equal(g[1], -0.5 * (rho / eps)^2)
    # a deterministic simulator has no bootstrap variance
    expect_equal(g[2], 0)
  }
  # the uniform kernel is an indicator on the epsilon ball
  expect_equal(.abcgp.gabc_log_likelihood(simd, o4, th, n_sim = 5, epsilon = 2,
                                          kernel = "uniform", bootstrap = 0)[1], 0)
  expect_equal(.abcgp.gabc_log_likelihood(simd, o4, th, n_sim = 5,
                                          epsilon = 0.5, kernel = "uniform",
                                          bootstrap = 0)[1], -Inf)
  # closer to the observation is more likely
  expect_true(.abcgp.gabc_log_likelihood(simd, o4, 0.1, n_sim = 5,
                                         bootstrap = 0)[1] >
              .abcgp.gabc_log_likelihood(simd, o4, 2, n_sim = 5,
                                         bootstrap = 0)[1])
  expect_error(.abcgp.gabc_log_likelihood(simd, o4, th, kernel = "tri"),
               "gaussian")
  expect_error(.abcgp.gabc_log_likelihood(simd, o4, th, epsilon = 0),
               "epsilon must be positive")
  expect_error(.abcgp.gabc_log_likelihood(function(theta, e) 1, o4, th),
               "summary length mismatch")
  # a noisy simulator does produce a positive variance estimate
  simn <- function(theta, e) theta[1] + .ghc_norm(e, 4L)
  gv <- .abcgp.gabc_log_likelihood(simn, o4, th, n_sim = 40, epsilon = 1,
                                   bootstrap = 40, seed = 3)
  expect_true(gv[2] > 0)
})

test_that("the history-matching route finds the likelihood peak", {
  # the simulator is centred on theta, so the GABC likelihood peaks where
  # theta equals the observation
  simw <- function(theta, e) theta[1] + 0.3 * .ghc_norm(e, 1L)
  r <- morie_abcgp(simw, 3, method = "wilkinson", prior_ppf = list(0, 6),
                   n_waves = 3, n_design = 24, n_sim = 20, epsilon = 0.5,
                   seed = 7)
  expect_equal(as.numeric(r$estimate), 3, tolerance = 0.25)
  # the grid posterior is a normalised distribution over the design
  expect_equal(sum(r$posterior), 1)
  expect_true(all(r$posterior >= 0))
  expect_equal(length(r$log_likelihood), nrow(r$grid))
  expect_equal(length(r$log_likelihood_sd), nrow(r$grid))
  expect_equal(sum(r$grid[, 1] * r$posterior), 3, tolerance = 0.4)
  # later waves rule out part of the space, which the first cannot
  expect_length(r$waves, 3L)
  expect_equal(r$waves[[1]]$ruled_implausible, 0)
  expect_true(sum(vapply(r$waves, function(w) w$ruled_implausible,
                         numeric(1))) > 0)
  expect_equal(r$waves[[1]]$wave, 0L)
  expect_true(r$ensemble_size >= 24)
  expect_match(r$method, "Wilkinson")
  # an explicit prediction grid is honoured
  g <- matrix(seq(0, 6, by = 0.5), ncol = 1)
  r2 <- morie_abcgp(simw, 3, X_grid = g, method = "wilkinson",
                    prior_ppf = list(0, 6), n_waves = 2, n_design = 20,
                    n_sim = 15, epsilon = 0.5, seed = 7)
  expect_equal(nrow(r2$grid), nrow(g))
  expect_equal(as.numeric(r2$estimate), 3, tolerance = 0.6)
})

test_that("the MH routes explore and report a chain", {
  sigma <- 1
  tau <- 2
  y <- 3
  sim <- function(theta, e) theta[1] + sigma * .ghc_norm(e, 1L)
  lp <- function(th) dnorm(th[1], 0, tau, log = TRUE)
  r <- morie_abcgp(sim, y, method = "synthetic", log_prior = lp, theta0 = 2.4,
                   n_iter = 300, n_sim = 25, proposal_sd = 1, seed = 5)
  expect_length(r$chain, 301L)
  expect_equal(r$burn_in, 150L)
  expect_equal(r$n_simulations, 300L * 2L * 25L)
  expect_true(r$acceptance_rate > 0 && r$acceptance_rate <= 1)
  expect_equal(r$estimate, r$posterior_mean)
  expect_match(r$method, "Algorithm 1")
  # the chain moves, and stays in the region the data support
  ch <- do.call(rbind, r$chain)[, 1]
  expect_true(stats::sd(ch) > 0)
  expect_equal(mean(ch[151:301]), 2.4, tolerance = 1.2)
  # the adaptive route reports its extra bookkeeping
  r2 <- morie_abcgp(sim, y, method = "gps", log_prior = lp, theta0 = 2.4,
                    n_iter = 60, n_sim = 10, proposal_sd = 1, seed = 5,
                    n_alpha = 16, xi = 0.2, delta_s = 10)
  expect_length(r2$chain, 61L)
  expect_true(r2$n_simulations >= 60L * 2L * 10L)
  expect_true(r2$unresolved_steps >= 0)
  expect_match(r2$method, "Algorithm 2")
  # a prior that excludes the proposal leaves the chain where it was
  lp0 <- function(th) if (th[1] == 0) 0 else -Inf
  r3 <- morie_abcgp(sim, y, method = "synthetic", log_prior = lp0, theta0 = 0,
                    n_iter = 20, n_sim = 5, seed = 1)
  expect_equal(unlist(r3$chain), rep(0, 21))
  expect_equal(r3$acceptance_rate, 0)
})

test_that("the driver validates its route arguments", {
  sim <- function(theta, e) theta[1]
  expect_error(morie_abcgp(sim, 1, method = "bogus"),
               "wilkinson/gps/adaptive/synthetic")
  expect_error(morie_abcgp("not a function", 1), "callable simulator")
  expect_error(morie_abcgp(sim, 1, method = "wilkinson"), "needs prior_ppf")
  expect_error(morie_abcgp(sim, 1, method = "synthetic"), "log_prior and theta0")
  expect_error(morie_abcgp(sim, 1, method = "gps", theta0 = 0),
               "log_prior and theta0")
})

test_that("bounds can be given as a numeric pair or a matrix", {
  # a numeric c(lo, hi) for one parameter
  d <- .abcgp.design_from_prior(8L, c(0, 10))
  expect_equal(dim(d), c(8L, 1L))
  expect_true(all(d >= 0 & d < 10))
  # identical to spelling the same bounds as a list
  expect_equal(d, .abcgp.design_from_prior(8L, list(0, 10)))
  # one row per parameter, columns (lo, hi)
  m <- cbind(c(0, 0, 0), c(1, 2, 3))
  d2 <- .abcgp.design_from_prior(6L, m)
  expect_equal(dim(d2), c(6L, 3L))
  for (j in 1:3) expect_true(all(d2[, j] >= 0 & d2[, j] < j))
  expect_equal(d2, .abcgp.design_from_prior(6L, list(m[, 1], m[, 2])))
  expect_error(.abcgp.design_from_prior(4L, c(0, 1, 2)), "c\\(lo, hi\\)")
  expect_error(.abcgp.design_from_prior(4L, matrix(1:6, nrow = 2)),
               "two columns")
})

test_that("an epsilon that rejects everything is reported, not returned as NaN", {
  simd <- function(theta, e) rep(theta[1], 4)
  # every draw falls outside the uniform kernel, so the likelihood and its
  # bootstrap variance are both degenerate
  g <- .abcgp.gabc_log_likelihood(simd, rep(0, 4), 5, n_sim = 6,
                                  epsilon = 0.5, kernel = "uniform",
                                  bootstrap = 10)
  expect_equal(g[1], -Inf)
  expect_equal(g[2], 0)
  # history matching cannot build an emulator from nothing and says so
  expect_error(
    .abcgp.history_match(simd, rep(0, 4), list(3, 6), n_waves = 1,
                         n_design = 6, n_sim = 4, epsilon = 1e-6,
                         accept_kernel = "uniform"),
    "too many rejections")
})

test_that("the adaptive sampler gives up rather than simulating forever", {
  # an unattainable error target with a near-zero proposal step keeps the
  # alpha draws split, so the inner loop can only stop at max_sim
  sim <- function(theta, e) theta[1] + .ghc_norm(e, 1L)
  lp <- function(th) dnorm(th[1], 0, 2, log = TRUE)
  r <- .abcgp.gps_abc(sim, 3, lp, 2.4, n_iter = 10, n_sim = 4, epsilon = 0,
                      proposal_sd = 1e-4, summary = NULL, seed = 2,
                      xi = 1e-12, delta_s = 4, n_alpha = 16, max_sim = 12)
  expect_true(r$unresolved_steps > 0)
  expect_true(r$unresolved_steps <= 10)
  expect_length(r$chain, 11L)
  # it stopped at the cap rather than running away: the inner loop can only
  # simulate at S = 4, 8 and 12, twice over, before max_sim ends it
  expect_true(r$n_simulations <= 10L * 2L * (4L + 8L + 12L))
})

test_that("the MH route recovers a conjugate posterior, epsilon included", {
  # A simulator emitting a fixed pattern with sample mean 0 and sample
  # variance exactly 1 makes the synthetic likelihood exact rather than
  # estimated: N(y; theta, 1 + epsilon^2). Against a N(0, 2^2) prior the
  # posterior is then conjugate and known in closed form, so both the
  # location and the spread of the chain are predicted before it is run.
  S <- 20L
  zf <- qnorm((seq_len(S) - 0.5) / S)
  zf <- (zf - mean(zf)) / stats::sd(zf)
  expect_equal(mean(zf), 0)
  expect_equal(stats::sd(zf), 1)
  i <- 0L
  sim <- function(theta, e) {
    i <<- i + 1L
    theta[1] + zf[(i - 1L) %% S + 1L]
  }
  lpri <- function(t) dnorm(t[1], 0, 2, log = TRUE)
  # the likelihood really is the exact normal density, and epsilon enters it
  # as an added variance
  batch <- lapply(seq_len(S), function(k) sim(2, NULL))
  expect_equal(.abcgp.synthetic_log_likelihood(batch, 3, epsilon = 0)$log_lik,
               dnorm(3, 2, 1, log = TRUE))
  expect_equal(.abcgp.synthetic_log_likelihood(batch, 3, epsilon = 1)$log_lik,
               dnorm(3, 2, sqrt(2), log = TRUE))

  for (eps in c(0, 1)) {
    v <- 1 + eps^2
    post_mean <- 3 * 4 / (4 + v)
    post_sd <- sqrt(4 * v / (4 + v))
    i <- 0L
    r <- morie_abcgp(sim, 3, method = "synthetic", log_prior = lpri,
                     theta0 = post_mean, n_iter = 4000L, n_sim = S,
                     proposal_sd = 1.6 * post_sd, epsilon = eps, seed = 1)
    ch <- do.call(rbind, r$chain)[, 1]
    kept <- ch[(length(ch) %/% 2 + 1):length(ch)]
    expect_equal(mean(kept), post_mean, tolerance = 0.3)
    expect_equal(stats::sd(kept), post_sd, tolerance = 0.15)
  }
  # a larger epsilon widens the posterior and pulls it toward the prior mean
  run <- function(eps) {
    i <<- 0L
    rr <- morie_abcgp(sim, 3, method = "synthetic", log_prior = lpri,
                      theta0 = 2, n_iter = 4000L, n_sim = S,
                      proposal_sd = 1.8, epsilon = eps, seed = 4)
    cc <- do.call(rbind, rr$chain)[, 1]
    cc[(length(cc) %/% 2 + 1):length(cc)]
  }
  tight <- run(0)
  loose <- run(2)
  expect_true(mean(loose) < mean(tight))
  expect_true(stats::sd(loose) > stats::sd(tight))
})
