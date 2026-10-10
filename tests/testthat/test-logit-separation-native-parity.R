# SPDX-License-Identifier: AGPL-3.0-or-later
#
# The separation linear programme (Konis 2007) is solved by rmorie's own
# simplex; lpSolve and detectseparation are cross-validation references.

sep_data <- function(seed, n, kind = c("none", "complete", "quasi")) {
  kind <- match.arg(kind)
  set.seed(seed)
  x <- cbind(a = stats::rnorm(n), b = stats::rnorm(n), c = stats::rbinom(n, 1, 0.3))
  y <- stats::rbinom(n, 1, stats::plogis(x[, 1]))
  if (kind == "complete") y <- as.numeric(x[, 1] + 0.5 * x[, 2] > 0)
  if (kind == "quasi") y[x[, 3] == 1] <- 1
  list(x = x, y = y)
}

lp_objective_ref <- function(y, x) {
  X <- cbind(1, x)
  sx <- (2 * y - 1) * X
  n <- nrow(X)
  lpSolve::lp("max", c(colSums(sx), -colSums(sx)),
              rbind(cbind(sx, -sx), cbind(sx, -sx)),
              c(rep(">=", n), rep("<=", n)), c(rep(0, n), rep(1, n)))$objval
}

lp_objective_native <- function(y, x) {
  X <- cbind(1, x)
  sx <- (2 * y - 1) * X
  n <- nrow(X)
  rmorie:::.lsep_simplex_max(c(colSums(sx), -colSums(sx)),
                             rbind(cbind(-sx, sx), cbind(sx, -sx)),
                             c(rep(0, n), rep(1, n)))$objval
}

test_that("method = 'auto' / 'lp' is native (no lpSolve call)", {
  expect_false(any(grepl("lpSolve|requireNamespace",
                         deparse(morie_logit_separation))))
  d <- sep_data(2, 200, "complete")
  r <- morie_logit_separation(d$y, d$x)
  expect_identical(r$method, "lp")
  expect_true(r$separation %in% c("complete", "quasi-complete"))
  expect_true(all(r$margins > -1e-8))
})

test_that("native LP optimum equals lpSolve's on separated / quasi / overlapping data", {
  skip_if_not_installed("lpSolve")
  cases <- list(
    list(1, 30, "none"), list(4, 500, "none"),
    list(2, 200, "complete"), list(5, 200, "complete"),
    list(3, 500, "quasi"), list(6, 500, "quasi")
  )
  for (cs in cases) {
    d <- sep_data(cs[[1]], cs[[2]], cs[[3]])
    # the optimum value is unique even when the optimal vertex is not;
    # observed max |diff| 3e-11 (pivoting round-off on objectives ~ 1e2)
    expect_equal(lp_objective_native(d$y, d$x), lp_objective_ref(d$y, d$x),
                 tolerance = 1e-8)
    r <- morie_logit_separation(d$y, d$x)
    expect_identical(r$separation == "none", cs[[3]] == "none")
  }
})

test_that("verdicts agree with detectseparation", {
  skip_if_not_installed("detectseparation")
  for (cs in list(list(1, 30, "none"), list(2, 200, "complete"),
                  list(3, 500, "quasi"), list(4, 500, "none"))) {
    d <- sep_data(cs[[1]], cs[[2]], cs[[3]])
    ref <- detectseparation::detect_separation(y = d$y, x = cbind(1, d$x),
                                               family = stats::binomial())
    r <- morie_logit_separation(d$y, d$x)
    expect_identical(r$separation != "none", isTRUE(ref$outcome))
  }
})

test_that("the simplex reports an unbounded programme", {
  # max z1 s.t. -z1 + z2 <= 1: z1 can grow without bound
  r <- rmorie:::.lsep_simplex_max(c(1, 0), matrix(c(-1, 1), 1), 1)
  expect_identical(r$status, 3L)
})
