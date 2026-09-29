# Projected gradient (Goldstein 1964; Levitin & Polyak 1966) with fixed,
# backtracking and FISTA steps: box, orthant and ball projections, and
# convergence to the constrained least-squares solution checked through
# the projection fixed point and against nnls-style KKT conditions.

pg_A <- matrix(c(2, 1, 0.5, 1, 3, -1, 0, 1, 2, 1, 0, 1), 4)
pg_b <- c(1, -2, 3, 0.5)
pg_f <- function(x) 0.5 * sum((pg_A %*% x - pg_b)^2)
pg_g <- function(x) as.numeric(crossprod(pg_A, pg_A %*% x - pg_b))

test_that("projections onto a box, the orthant and a ball", {
  expect_equal(project_box(c(-2, 0.5, 3), c(-1, 0, 0), c(1, 1, 2)), c(-1, 0.5, 2))
  expect_equal(project_box(c(-2, 5), list(NULL, 0), list(0, NULL)), c(-2, 5))
  expect_equal(project_box(c(-2, 5)), c(-2, 5))
  expect_error(project_box(1:2, 0, 1), "one entry per coordinate")
  expect_error(project_box(1, 2, 1), "box is empty")
  expect_equal(project_nonneg(c(-1, 0, 2)), c(0, 0, 2))
  expect_equal(project_ball(c(3, 4)), c(0.6, 0.8), tolerance = 1e-15)
  expect_equal(project_ball(c(3, 4), radius = 2, centre = c(1, 1)), c(1, 1) + c(2, 3) * 2 / sqrt(13),
               tolerance = 1e-15)
  expect_equal(project_ball(c(0.1, 0.2)), c(0.1, 0.2))
  expect_error(project_ball(1, 0), "positive")
  expect_error(project_ball(1:2, 1, 1), "centre has 1")
})

test_that("all three step rules reach the nonnegative least-squares KKT point", {
  L <- max(eigen(crossprod(pg_A))$values)
  for (rule in c("fixed", "backtracking", "fista")) {
    r <- projected_gradient(pg_f, pg_g, rep(0, 3), project_nonneg, step = 1 / L,
                            rule = rule, max_iter = 20000, tol = 1e-14)
    g <- pg_g(r$x)
    act <- r$x > 1e-9
    expect_lt(max(abs(g[act])), 1e-7)
    expect_true(all(g[!act] >= -1e-7))
    expect_lt(r$fixed_point_residual, 1e-7)
    if (rule != "fista") expect_true(r$monotone)
  }
  # an unconstrained projection gives ordinary least squares
  ols <- qr.solve(pg_A, pg_b)
  u <- projected_gradient(pg_f, pg_g, rep(0, 3), identity, step = 1 / L, rule = "fixed",
                          max_iter = 20000, tol = 1e-15)
  expect_equal(u$x, as.numeric(ols), tolerance = 1e-8)
  expect_equal(morie_pgdsdg(pg_f, pg_g, rep(0, 3), identity, step = 1 / L, rule = "fixed",
                            max_iter = 20000, tol = 1e-15)$x, u$x)
})

test_that("inputs are validated", {
  expect_error(projected_gradient(pg_f, pg_g, rep(0, 3), identity, rule = "newton"), "rule must be one of")
  expect_error(projected_gradient(pg_f, pg_g, rep(0, 3), identity, rule = "fixed"), "explicit step")
  expect_error(projected_gradient(pg_f, pg_g, rep(0, 3), identity, step = -1), "positive")
  expect_error(projected_gradient(pg_f, function(x) 1, rep(0, 3), identity), "gradient has 1 components")
})
