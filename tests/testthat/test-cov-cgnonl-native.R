# Coverage for nonlinear conjugate gradients (Fletcher & Reeves 1964;
# Polak & Ribiere 1969). The beta rules and the cubic interpolant are
# checked against their formulas (the cubic step is exact on a cubic), the
# line search against the exact quadratic step, and the full method
# against solve() on a quadratic -- which conjugate gradients must finish
# in n exact steps -- and against the Rosenbrock minimum.

.A <- matrix(c(4, 1, 0, 1, 3, 0.5, 0, 0.5, 2), 3)
.b <- c(1, -2, 0.5)
.fq <- function(x) 0.5 * sum(x * (.A %*% x)) - sum(.b * x)
.gq <- function(x) as.numeric(.A %*% x) - .b

test_that("Fletcher-Reeves and Polak-Ribiere(+) betas", {
  g1 <- c(1, -2, 0.5)
  g0 <- c(2, 1, -1)
  expect_equal(beta_fletcher_reeves(g1, g0), sum(g1^2) / sum(g0^2), tolerance = 1e-12)
  expect_equal(beta_polak_ribiere(g1, g0), sum(g1 * (g1 - g0)) / sum(g0^2), tolerance = 1e-12)
  expect_equal(beta_polak_ribiere(g1, g0, plus = TRUE), max(0, sum(g1 * (g1 - g0)) / sum(g0^2)), tolerance = 1e-12)
  expect_identical(beta_fletcher_reeves(g1, c(0, 0, 0)), 0)
  expect_equal(cgnonl_beta_fletcher_reeves(g1, g0), beta_fletcher_reeves(g1, g0))
  expect_equal(cgnonl_beta_polak_ribiere(g1, g0, TRUE), beta_polak_ribiere(g1, g0, TRUE))
})

test_that("cubic interpolation is exact on a cubic", {
  f <- function(t) t^3 - 3 * t
  d <- function(t) 3 * t^2 - 3
  expect_equal(cubic_interpolate(0, f(0), d(0), 2, f(2), d(2)), 1, tolerance = 1e-12)
  expect_equal(cubic_interpolate(-0.5, f(-0.5), d(-0.5), 3, f(3), d(3)), 1, tolerance = 1e-12)
  expect_equal(cgnonl_cubic_interpolate(0, f(0), d(0), 2, f(2), d(2)), 1, tolerance = 1e-12)
  expect_identical(cubic_interpolate(1, 0, 1, 1, 0, 1), 1)
})

test_that("the Fletcher-Reeves line search finds the exact quadratic step", {
  x <- c(0.5, 0.2, -1)
  g <- .gq(x)
  p <- -g
  r <- line_search_fr(.fq, .gq, x, p, .fq(x), g)
  texact <- sum(g * g) / sum(p * (.A %*% p))
  expect_equal(r$t, texact, tolerance = 1e-9)
  expect_equal(r$x_new, x + texact * p, tolerance = 1e-9)
  expect_lt(abs(sum(p * r$g_new)), 1e-9)
  r2 <- cgnonl_line_search_fr(.fq, .gq, x, p, .fq(x), g, est = .fq(x) - 1)
  expect_equal(r2$t, texact, tolerance = 1e-9)
  expect_error(line_search_fr(.fq, .gq, x, -p, .fq(x), g), "not a descent direction")
})

test_that("CG solves an n-dimensional quadratic in n exact steps", {
  xs <- solve(.A, .b)
  for (rule in c("fletcher-reeves", "polak-ribiere", "polak-ribiere-plus")) {
    r <- nonlinear_cg(.fq, .gq, c(0, 0, 0), beta = rule, line_search = "exact-quadratic",
                      hess_vec = function(p) as.numeric(.A %*% p), keep_path = TRUE)
    expect_true(r$converged)
    expect_lte(r$n_iter, 3L)
    expect_equal(r$x, xs, tolerance = 1e-9)
    expect_equal(r$fun, .fq(xs), tolerance = 1e-12)
    expect_length(r$path, r$n_iter + 1L)
  }
  rf <- morie_cgnonl(.fq, .gq, c(1, 1, 1))
  expect_true(rf$converged)
  expect_equal(rf$x, xs, tolerance = 1e-9)
  expect_lt(rf$gnorm, 1e-10)
  expect_identical(cgnonl, nonlinear_cg)
  expect_error(nonlinear_cg(.fq, .gq, c(0, 0, 0), beta = "hestenes"), "beta must be one of")
  expect_error(nonlinear_cg(.fq, .gq, c(0, 0, 0), line_search = "exact-quadratic"), "needs hess_vec")
  expect_error(nonlinear_cg(.fq, .gq, numeric(0)), "x0 is empty")
})

test_that("restarts and the Rosenbrock valley", {
  fr <- function(x) 100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2
  gr <- function(x) c(-400 * x[1] * (x[2] - x[1]^2) - 2 * (1 - x[1]), 200 * (x[2] - x[1]^2))
  r <- nonlinear_cg(fr, gr, c(-1.2, 1), beta = "polak-ribiere-plus", max_iter = 5000, tol = 1e-8)
  expect_true(r$converged)
  expect_equal(r$x, c(1, 1), tolerance = 1e-6)
  rr <- nonlinear_cg(.fq, .gq, c(3, -1, 2), restart = 1L, max_iter = 500)
  expect_true(all(rr$betas == 0))
  expect_identical(rr$n_restart, length(rr$betas))
  expect_equal(rr$x, solve(.A, .b), tolerance = 1e-9)
  cr <- cgnonl_nonlinear_cg(.fq, .gq, c(1, 1, 1))
  expect_equal(cr$x, solve(.A, .b), tolerance = 1e-9)
})
