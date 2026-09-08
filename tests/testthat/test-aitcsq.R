# Checked against stats::chisq.test, which computes the same Pearson
# statistic by the ordinary (o - e)^2 / e route, and against the
# identities the correspondence-analysis construction guarantees.

.aitcsq_table <- function(seed = 0, I = 5, J = 4, lam = 40) {
  set.seed(seed)
  matrix(rpois(I * J, lam), I, J)
}

test_that("morie_table_inertia matches stats::chisq.test", {
  for (s in 0:4) {
    X <- .aitcsq_table(seed = s)
    got <- morie_table_inertia(X)
    ref <- suppressWarnings(stats::chisq.test(X, correct = FALSE))
    expect_equal(got$statistic, unname(ref$statistic), tolerance = 1e-12)
    expect_equal(got$df, unname(ref$parameter))
    expect_equal(got$p_value, ref$p.value, tolerance = 1e-10)
  }
})

test_that("chi-square is n times total inertia", {
  r <- morie_table_inertia(.aitcsq_table(seed = 1))
  expect_equal(r$statistic, r$n * r$inertia, tolerance = 1e-12)
})

test_that("total inertia is the sum of the principal inertias", {
  r <- morie_table_inertia(.aitcsq_table(seed = 2))
  expect_equal(sum(r$principal_inertias), r$inertia, tolerance = 1e-12)
})

test_that("a rank-one table is exactly independent", {
  # N = r c' has S identically zero.
  X <- outer(c(10, 20, 30), c(0.2, 0.3, 0.5))
  expect_equal(morie_table_inertia(X)$inertia, 0, tolerance = 1e-20)
})

test_that("inertia ignores an overall rescaling of a closed table", {
  X <- .aitcsq_table(seed = 4)
  closed <- X / rowSums(X)
  expect_equal(morie_table_inertia(closed)$inertia,
               morie_table_inertia(closed * 7)$inertia, tolerance = 1e-12)
})

test_that("n overrides the grand total", {
  r <- morie_table_inertia(.aitcsq_table(seed = 5), n = 1000)
  expect_equal(r$n, 1000)
  expect_equal(r$statistic, 1000 * r$inertia, tolerance = 1e-12)
})

test_that("masses sum to one", {
  r <- morie_table_inertia(.aitcsq_table(seed = 6))
  expect_equal(sum(r$row_masses), 1, tolerance = 1e-12)
  expect_equal(sum(r$col_masses), 1, tolerance = 1e-12)
})

test_that("morie_table_inertia validates its inputs", {
  set.seed(9)
  expect_error(morie_table_inertia(matrix(rnorm(40), 10, 4)), "non-negative")
  expect_error(morie_table_inertia(matrix(1, 1, 4)), "at least a 2x2")
  expect_error(morie_table_inertia(matrix(c(1, NA, 2, 3), 2, 2)), "must be finite")
  expect_error(morie_table_inertia(matrix(0, 3, 3)), "sums to zero")
  expect_error(morie_table_inertia(matrix(c(1, 0, 2, 0), 2, 2, byrow = TRUE)),
               "positive mass")
  expect_error(morie_table_inertia(.aitcsq_table(seed = 8), n = 0), "n must be positive")
})
