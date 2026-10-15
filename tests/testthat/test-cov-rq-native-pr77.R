# R/rq_native.R, the lines PR #77 added that no other test reached: the solver's refusals and
# iteration-limit warning, a singular basis, the vertex search that widens its candidate set, that
# finds no design rank or stops at the pivot limit, the Bofinger bandwidth, the "nid" crossing
# warning, singular bootstrap draws, case weights with dropped rows and the small methods, against
# quantreg.

.rq <- function(name) get(name, envir = environment(morie_rq))

.rq_data <- function(n = 60, seed = 1) {
  set.seed(seed)
  x <- stats::runif(n)
  data.frame(x = x, y = 1 + 2 * x + stats::rnorm(n) * (1 + x))
}

test_that("the solver refuses a boundary tau and warns at the iteration limit", {
  skip_if_not_installed("quantreg")
  d <- .rq_data()
  X <- cbind(1, d$x)
  fnb <- .rq(".rqn_fnb")
  expect_error(fnb(X, d$y, 1e-8), "the Frisch-Newton method needs tau in (0, 1).", fixed = TRUE)
  expect_warning(f1 <- fnb(X, d$y, 0.5, maxit = 1L), "Frisch-Newton stopped at the iteration limit")
  # one step is not the optimum: its check loss is above quantreg's minimum
  ref <- quantreg::rq.fit(X, d$y, tau = 0.5, method = "br")
  rho <- function(u) sum(u * (0.5 - (u < 0)))
  expect_gt(rho(f1$residuals), rho(ref$residuals))
  expect_gt(f1$gap, 1e-6)
})

test_that("a basis of two equal rows is singular for the certificate and the pivots", {
  X <- cbind(1, c(1, 1, 2, 3, 4))
  y <- c(1, 2, 2, 4, 5)
  expect_equal(det(X[1:2, ]), 0)
  expect_null(.rq(".rqn_cert")(X, y, 0.5, c(1L, 2L), 1e-12))
  pv <- .rq(".rqn_pivot")(X, y, 0.5, c(1L, 2L), 1e-12, 10L)
  expect_false(pv$optimal)
  expect_identical(pv$why, "singular basis")
  expect_identical(pv$pivots, 0L)
})

test_that("the vertex search widens its candidates, refuses a rank-deficient design, and stops at the pivot limit", {
  skip_if_not_installed("quantreg")
  vtx <- .rq(".rqn_vertex")
  # the twelve smallest residuals all sit at x = 0, so the first candidate block has rank 1
  X <- cbind(1, c(rep(0, 20), rep(1, 10)))
  set.seed(1)
  y <- c(rep(1, 20), 3 + stats::rnorm(10))
  v <- vtx(X, y, 0.5, c(1, 2))
  expect_true(v$optimal)
  ref <- suppressWarnings(quantreg::rq.fit(X, y, tau = 0.5, method = "br"))
  expect_equal(v$coefficients, unname(ref$coefficients), tolerance = 1e-10)
  # a repeated column: no p rows are independent
  expect_error(vtx(cbind(1, 1, seq_len(8)), as.numeric(1:8), 0.5, c(0, 0, 1)),
               "singular design matrix.", fixed = TRUE)
  # no pivots allowed from a basis that is not optimal: every route fails and b0 comes back
  v0 <- vtx(X, y, 0.3, c(0, 0), maxpiv = 0L)
  expect_false(v0$optimal)
  expect_identical(v0$why, "pivot limit")
  expect_equal(v0$coefficients, c(0, 0))
  expect_equal(v0$residuals, y)
  expect_null(v0$basis)
})

test_that("an uncertified vertex warns and keeps the interior point solution", {
  skip_if_not_installed("quantreg")
  d <- .rq_data()
  local_mocked_bindings(.rqn_vertex = function(X, y, tau, b0, maxpiv = NULL) list(coefficients = b0, optimal = FALSE),
                        .package = "rmorie")
  w <- character()
  fit <- withCallingHandlers(morie_rq(y ~ x, d, method = "br", se = "ker"),
                             warning = function(e) {
                               w <<- c(w, conditionMessage(e))
                               invokeRestart("muffleWarning")
                             })
  expect_true(any(grepl("the simplex finish did not certify a vertex", w, fixed = TRUE)))
  ref <- quantreg::rq(y ~ x, data = d, tau = 0.5, method = "fn")
  expect_equal(unname(coef(fit)), unname(coef(ref)), tolerance = 1e-5)
})

test_that("the Bofinger bandwidth is quantreg's", {
  skip_if_not_installed("quantreg")
  bw <- .rq(".rqn_bandwidth")
  for (tau in c(0.1, 0.5, 0.8)) for (n in c(50, 1000)) {
    expect_equal(bw(tau, n, hs = FALSE), quantreg::bandwidth.rq(tau, n, hs = FALSE), tolerance = 1e-14)
  }
  x0 <- stats::qnorm(0.3)
  f0 <- stats::dnorm(x0)
  expect_equal(bw(0.3, 200, hs = FALSE), 200^-0.2 * ((4.5 * f0^4) / (2 * x0^2 + 1)^2)^0.2)
})

test_that("crossing quantiles warn under nid and the covariance is still quantreg's", {
  skip_if_not_installed("quantreg")
  set.seed(3)
  d <- data.frame(x = stats::rexp(15))
  d$y <- stats::rnorm(15)
  expect_warning(fit <- morie_rq(y ~ x, d, method = "br", se = "nid"), "non-positive fis")
  ref <- suppressWarnings(summary(quantreg::rq(y ~ x, data = d, tau = 0.5), se = "nid", covariance = TRUE))
  expect_equal(unname(fit$vcov[[1L]]), unname(ref$cov), tolerance = 1e-6)
  expect_equal(unname(coef(fit)), unname(coef(quantreg::rq(y ~ x, data = d, tau = 0.5))), tolerance = 1e-6)
})

test_that("bootstrap draws that miss a singleton dummy are dropped and counted", {
  skip_if_not_installed("quantreg")
  set.seed(8)
  n <- 20L
  X <- cbind(1, c(1, rep(0, n - 1L)), stats::runif(n))
  y <- drop(X %*% c(1, 2, 3)) + stats::rnorm(n)
  R <- 12L
  set.seed(5)
  U <- matrix(sample(n, n * R, replace = TRUE), n, R)
  sing <- colSums(U == 1L) == 0L
  expect_gt(sum(sing), 0L)
  expect_lt(sum(sing), R)
  b0 <- quantreg::rq.fit(X, y, tau = 0.5, method = "br")$coefficients
  expect_warning(cv <- .rq(".rqn_cov")(X, y, b0, drop(y - X %*% b0), 0.5, "boot", "br", TRUE, R, 5L),
                 sprintf("%d of %d bootstrap designs were singular and dropped.", sum(sing), R), fixed = TRUE)
  B <- t(vapply(which(!sing), function(k) {
    i <- U[, k]
    suppressWarnings(quantreg::rq.fit(X[i, ], y[i], tau = 0.5, method = "br")$coefficients)
  }, numeric(3)))
  expect_equal(unname(cv$cov), unname(stats::cov(B)), tolerance = 1e-6)
  expect_error(.rq(".rqn_cov")(X, y, b0, drop(y - X %*% b0), 0.5, "xx", "br", TRUE, R, 5L),
               "unknown se type.", fixed = TRUE)
})

test_that("weights follow the rows that survive na.omit, and bad sizes or weights are refused", {
  skip_if_not_installed("quantreg")
  d <- .rq_data(40)
  d$y[c(3, 17)] <- NA
  set.seed(2)
  w <- stats::runif(40, 0.5, 2)
  fit <- morie_rq(y ~ x, d, method = "br", weights = w)
  ref <- quantreg::rq(y ~ x, data = d, tau = 0.5, weights = w, method = "br")
  expect_equal(unname(coef(fit)), unname(coef(ref)), tolerance = 1e-8)
  expect_equal(fit$weights, w[-c(3, 17)])
  w[5] <- -1
  expect_error(morie_rq(y ~ x, d, weights = w), "weights must be finite, non-negative, one per observation.",
               fixed = TRUE)
  expect_error(morie_rq(y ~ x, d, weights = w[1:10]), "weights must be finite, non-negative, one per observation.",
               fixed = TRUE)
  expect_error(morie_rq(y ~ x, d[1:2, ]), "need more observations than coefficients.", fixed = TRUE)
})

test_that("vcov and predict for one tau are quantreg's", {
  # the methods are called by name as well, so a run that sources this file sees them
  vc <- .rq("vcov.morie_rq")
  pr <- .rq("predict.morie_rq")
  skip_if_not_installed("quantreg")
  d <- .rq_data()
  fit <- morie_rq(y ~ x, d, tau = 0.4, method = "br", se = "iid")
  ref <- quantreg::rq(y ~ x, data = d, tau = 0.4, method = "br")
  expect_identical(vc(fit), vcov(fit))
  expect_equal(unname(vc(fit)), unname(summary(ref, se = "iid", covariance = TRUE)$cov), tolerance = 1e-8)
  expect_equal(unname(pr(fit)), unname(stats::fitted(ref)), tolerance = 1e-8)
  nd <- data.frame(x = c(0, 0.25, 0.9))
  p1 <- pr(fit, nd)
  expect_identical(p1, predict(fit, nd))
  # two taus: a list of covariances and a prediction matrix, one column per tau (taus whose
  # sparsity fits are unique; at 0.25 that inner fit has tied optima and either vertex is valid)
  fit2 <- morie_rq(y ~ x, d, tau = c(0.4, 0.75), method = "br", se = "iid")
  ref2 <- quantreg::rq(y ~ x, data = d, tau = c(0.4, 0.75), method = "br")
  v2 <- vc(fit2)
  expect_length(v2, 2L)
  for (k in 1:2) {
    rk <- quantreg::rq(y ~ x, data = d, tau = c(0.4, 0.75)[k], method = "br")
    expect_equal(unname(v2[[k]]), unname(summary(rk, se = "iid", covariance = TRUE)$cov), tolerance = 1e-8)
  }
  p2 <- pr(fit2, nd)
  expect_identical(dim(p2), c(3L, 2L))
  expect_equal(unname(p2), unname(predict(ref2, nd)), tolerance = 1e-8)
  expect_null(dim(p1))
  expect_equal(unname(p1), unname(predict(ref, nd)), tolerance = 1e-8)
})
