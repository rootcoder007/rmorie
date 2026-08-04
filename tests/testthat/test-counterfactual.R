# Counterfactual() against Pearl (2000) Section 1.4, the copy in the
# corpus.  Model 2 of eq. (1.48): x = u1, y = 1 iff x == u2, both
# exogenous variables binary.  The book: evidence {y=1, x=1} is
# "compatible with only one realization of U1 and U2 -- namely u1 = 1,
# u2 = 1", and under do(x=0) "the probability of recovery (y = 0) is
# unity".

.pearl_eqs <- list(
  x = list(parents = "u1", fn = function(u1) u1),
  y = list(parents = c("x", "u2"),
           fn = function(x, u2) if (abs(x - u2) < 0.5) 1 else 0)
)

test_that("Pearl 1.4 worked example reproduces exactly", {
  r <- Counterfactual(evidence = list(x = 1, y = 1), equations = .pearl_eqs,
                      exogenous = c("u1", "u2"), do = list(x = 0),
                      query = "y")
  expect_equal(unname(r$abducted), c(1, 1))
  expect_equal(r$factual, 1)
  expect_equal(r$counterfactual, 0)
  expect_true(r$counterfactual_unique)
  expect_equal(r$n_compatible_u, 1)
  expect_equal(r$residual, 0)
})

test_that("the treated survivor cell gives the opposite counterfactual", {
  # x=1, y=0 forces u2=0: dies iff NOT treated, so do(x=0) gives y=1
  r <- Counterfactual(evidence = list(x = 1, y = 0), equations = .pearl_eqs,
                      exogenous = c("u1", "u2"), do = list(x = 0),
                      query = "y")
  expect_equal(unname(r$abducted), c(1, 0))
  expect_equal(r$counterfactual, 1)
})

test_that("ambiguous evidence is reported, not hidden", {
  r <- Counterfactual(evidence = list(x = 1), equations = .pearl_eqs,
                      exogenous = c("u1", "u2"), do = list(x = 0),
                      query = "y")
  expect_equal(r$n_compatible_u, 2)
  expect_false(r$counterfactual_unique)
})

test_that("continuous linear model solves through the gradient path", {
  eqs <- list(
    x = list(parents = "u1", fn = function(u1) u1),
    y = list(parents = c("x", "u2"), fn = function(x, u2) 2 * x + u2)
  )
  r <- Counterfactual(evidence = list(x = 1, y = 3.5), equations = eqs,
                      exogenous = c("u1", "u2"), do = list(x = 2),
                      query = "y")
  expect_equal(unname(r$abducted), c(1, 1.5), tolerance = 1e-5)
  expect_equal(r$counterfactual, 5.5, tolerance = 1e-5)
  expect_match(r$method, "gradient")
})

test_that("an oversized declared support is refused immediately", {
  mk <- function(name) {
    force(name)
    function(...) if (list(...)[[1]] > 0.5) 1 else 0
  }
  eqs <- setNames(lapply(sprintf("u%d", 1:30), function(u)
    list(parents = u, fn = mk(u))), sprintf("y%d", 1:30))
  ev <- setNames(as.list(rep(7.7, 30)), sprintf("y%d", 1:30))
  expect_error(
    Counterfactual(evidence = ev, equations = eqs,
                   exogenous = sprintf("u%d", 1:30),
                   do = list(y1 = 0), query = "y2",
                   u_support = c(0, 1)),
    "too large")
})

test_that("pre-policy spelling still works", {
  r <- morie_counterfactual(evidence = list(x = 1, y = 1),
                            equations = .pearl_eqs,
                            exogenous = c("u1", "u2"),
                            do = list(x = 0), query = "y")
  expect_equal(r$counterfactual, 0)
})
