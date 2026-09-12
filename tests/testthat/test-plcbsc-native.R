# Synthetic control with in-space placebo inference (Abadie, Diamond &
# Hainmueller 2015).
#
# Anchors outside the module: a treated unit built as an exact convex
# combination of two donors, whose weights and zero pre-treatment fit are
# therefore known in advance; the gap and error formulas written longhand;
# and the permutation p-value's arithmetic floor of 1/(J+1), which the
# module states in its own note.

# Five donors on independent random paths. They have to be linearly
# independent over the pre-period: donors that are all straight lines span
# only two dimensions, and then the convex combination reproducing the
# target is a continuum rather than a point, so no particular weight vector
# is recoverable.
make_case <- function(effect = 0, Tn = 20, t0 = 12) {
  set.seed(11)
  base <- lapply(1:5, function(j) {
    cumsum(rnorm(Tn, mean = 0.1 * j, sd = 0.6)) + j
  })
  # the treated unit is 0.6 of donor 2 plus 0.4 of donor 4, plus an effect
  # applied only after t0
  y1 <- 0.6 * base[[2]] + 0.4 * base[[4]]
  y1[(t0 + 1):Tn] <- y1[(t0 + 1):Tn] + effect
  list(y1 = y1, donors = base, t0 = t0, Tn = Tn,
       true_w = c(0, 0.6, 0, 0.4, 0))
}

test_that("the simplex projection lands on the simplex", {
  for (v in list(c(0.2, 0.5, 0.9), c(-1, 2, 3), c(5, 5, 5), c(-3, -2, -1),
                 c(1, 0, 0))) {
    p <- .plcbsc_simplex_project(v)
    expect_equal(sum(p), 1)
    expect_true(all(p >= -1e-12))
  }
  # a point already on the simplex is left alone
  expect_equal(.plcbsc_simplex_project(c(0.25, 0.25, 0.5)),
               c(0.25, 0.25, 0.5))
  # the projection is the nearest such point, so a random direction cannot
  # be beaten by any other simplex point
  set.seed(4)
  v <- rnorm(6)
  p <- .plcbsc_simplex_project(v)
  for (i in 1:200) {
    q <- as.numeric(rgamma(6, 1))
    q <- q / sum(q)
    expect_true(sum((v - p)^2) <= sum((v - q)^2) + 1e-9)
  }
  expect_length(.plcbsc_simplex_project(numeric(0)), 0L)
})

test_that("an exact convex combination is recovered", {
  d <- make_case()
  x1 <- d$y1[seq_len(d$t0)]
  xd <- lapply(d$donors, function(o) o[seq_len(d$t0)])
  fit <- .plcbsc_synthetic_control(x1, xd, max_iter = 20000, tol = 1e-14)
  # the weights sit on the simplex
  expect_equal(sum(fit$weights), 1)
  expect_true(all(fit$weights >= -1e-12))
  # a perfect fit exists, so the loss is driven to zero and the fitted
  # predictors equal the treated ones
  expect_equal(fit$loss, 0, tolerance = 1e-6)
  expect_equal(fit$fitted, x1, tolerance = 1e-3)
  # and the recovered weights are the ones used to build the unit
  expect_equal(fit$weights, d$true_w, tolerance = 0.05)
  expect_true(fit$n_iter >= 1L)

  # predictor weights are honoured: zeroing out all but one predictor
  # leaves only that one to match
  v <- c(1, rep(0, d$t0 - 1))
  fv <- .plcbsc_synthetic_control(x1, xd, v = v, max_iter = 5000)
  expect_equal(sum(fv$weights), 1)
  expect_equal(fv$fitted[1], x1[1], tolerance = 1e-3)

  expect_error(.plcbsc_synthetic_control(x1, list()), "donor pool is empty")
  expect_error(.plcbsc_synthetic_control(x1, list(c(1, 2))),
               "same predictors as the treated unit")
  expect_error(.plcbsc_synthetic_control(x1, xd, v = c(1, 2)),
               "one non-negative weight per predictor")
  expect_error(.plcbsc_synthetic_control(x1, xd, v = rep(-1, d$t0)),
               "one non-negative weight per predictor")
  expect_error(.plcbsc_synthetic_control(x1, xd, v = rep(0, d$t0)),
               "some positive weight")
})

test_that("gaps and errors are their formulas", {
  y1 <- c(5, 6, 7)
  don <- list(c(1, 2, 3), c(9, 8, 7))
  w <- c(0.5, 0.5)
  expect_equal(.plcbsc_gaps(y1, don, w), y1 - (0.5 * don[[1]] + 0.5 * don[[2]]))
  # a unit that is exactly its synthetic twin has no gap
  expect_equal(.plcbsc_gaps(0.5 * don[[1]] + 0.5 * don[[2]], don, w),
               rep(0, 3))
  expect_equal(.plcbsc_rmspe(c(3, 4)), sqrt((9 + 16) / 2))
  expect_equal(.plcbsc_rmspe(c(0, 0)), 0)
  expect_true(is.nan(.plcbsc_rmspe(numeric(0))))
  # the error is scale-equivariant
  expect_equal(.plcbsc_rmspe(2 * c(3, 4)), 2 * .plcbsc_rmspe(c(3, 4)))
})

test_that("the effect statistic averages the post-period gaps", {
  gaps <- c(0, 0, 0, 2, 4, 6)
  expect_equal(.plcbsc_effect(gaps, 3, "effect"), mean(c(2, 4, 6)))
  # the ratio statistic compares post-period error to pre-period error
  pre <- .plcbsc_rmspe(c(0, 0, 0))
  expect_equal(pre, 0)
  # with a perfect pre-period fit the ratio is unbounded, and it says so
  expect_equal(.plcbsc_effect(gaps, 3, "rmspe_ratio"), Inf)
  g2 <- c(1, -1, 1, 2, 4, 6)
  expect_equal(.plcbsc_effect(g2, 3, "rmspe_ratio"),
               .plcbsc_rmspe(c(2, 4, 6)) / .plcbsc_rmspe(c(1, -1, 1)))
  # an explicit pre-period gap vector overrides the leading gaps
  expect_equal(.plcbsc_effect(g2, 3, "rmspe_ratio", pre_gaps = c(2, 2, 2)),
               .plcbsc_rmspe(c(2, 4, 6)) / 2)
  # no post-period leaves nothing to average
  expect_true(is.nan(.plcbsc_effect(gaps, 6, "effect")))
})

test_that("a known treatment effect is recovered with a small p-value", {
  d <- make_case(effect = 8)
  r <- morie_plcbsc(d$y1, d$donors, d$t0, statistic = "effect",
                    max_iter = 20000, tol = 1e-14)
  # the estimate is the average post-period gap, which is the effect built in
  expect_equal(r$estimate, 8, tolerance = 0.5)
  # the pre-period fit is near perfect because a convex combination exists
  expect_true(r$rmspe_pre < 0.05)
  expect_true(r$rmspe_post > 5)
  # the weights are the ones used to build the unit
  expect_equal(r$weights, d$true_w, tolerance = 0.05)
  # the recomputed errors match the reported ones
  expect_equal(r$rmspe_pre, .plcbsc_rmspe(r$gaps[seq_len(d$t0)]))
  expect_equal(r$rmspe_post,
               .plcbsc_rmspe(r$gaps[(d$t0 + 1):d$Tn]))
  expect_equal(r$estimate, mean(r$gaps[(d$t0 + 1):d$Tn]))
  # a real effect ranks first among the placebos, at the arithmetic floor
  expect_equal(r$rank, 1L)
  expect_equal(r$pvalue, 1 / (r$n_donors + 1))
  expect_equal(r$n_donors, 5L)
  expect_length(r$placebo, 5L)
  expect_equal(r$t0, d$t0)
  expect_equal(r$statistic, "effect")
  expect_match(r$method, "Abadie, Diamond & Hainmueller")
  expect_match(r$note, "1/\\(J\\+1\\)")
})

test_that("no treatment effect gives no significant estimate", {
  d <- make_case(effect = 0)
  r <- morie_plcbsc(d$y1, d$donors, d$t0, statistic = "effect",
                    max_iter = 20000, tol = 1e-14)
  # the treated unit is exactly its synthetic twin throughout
  expect_equal(r$estimate, 0, tolerance = 0.05)
  expect_true(r$rmspe_pre < 0.05)
  expect_true(r$rmspe_post < 0.05)
  # so it does not stand out from the donor placebos
  expect_true(r$pvalue > 1 / (r$n_donors + 1))
  expect_true(r$rank > 1L)
})

test_that("the permutation p-value cannot go below one over J plus one", {
  d <- make_case(effect = 50)
  r <- morie_plcbsc(d$y1, d$donors, d$t0, max_iter = 5000)
  # five donors means six units in the permutation, so the floor is 1/6
  expect_equal(r$pvalue, 1 / 6)
  expect_true(r$pvalue >= 1 / (r$n_donors + 1) - 1e-12)
  # the p-value is a proportion
  expect_true(r$pvalue > 0 && r$pvalue <= 1)
  # the rank is a position among the six statistics
  expect_true(r$rank >= 1L && r$rank <= r$n_donors + 1L)
})

test_that("the ratio statistic is also available", {
  d <- make_case(effect = 8)
  r <- morie_plcbsc(d$y1, d$donors, d$t0, statistic = "rmspe_ratio",
                    max_iter = 20000, tol = 1e-14)
  expect_equal(r$statistic, "rmspe_ratio")
  # the pre-period fit is nearly exact, so the ratio is very large
  expect_true(r$estimate > 50 || is.infinite(r$estimate))
  expect_equal(r$rank, 1L)
})

test_that("separate predictors may be supplied", {
  d <- make_case(effect = 5)
  # match on the pre-period means rather than the whole pre-period path
  x1 <- mean(d$y1[seq_len(d$t0)])
  xd <- lapply(d$donors, function(o) mean(o[seq_len(d$t0)]))
  r <- morie_plcbsc(d$y1, d$donors, d$t0, x_treated = x1, x_donors = xd,
                    max_iter = 5000)
  expect_equal(sum(r$weights), 1)
  expect_length(r$gaps, d$Tn)
  expect_true(is.finite(r$estimate))
})

test_that("the in-time placebo moves the intervention date", {
  d <- make_case(effect = 8)
  # pretending the intervention happened earlier, while no effect was
  # present then, should find little
  p <- .plcbsc_in_time_placebo(d$y1, d$donors, d$t0, fake_t0 = 6,
                               max_iter = 5000)
  expect_true(is.list(p))
  expect_true(any(c("estimate", "gaps") %in% names(p)))
})

test_that("plcbsc validates its arguments", {
  d <- make_case()
  expect_error(morie_plcbsc(d$y1, list(), d$t0), "donor pool is empty")
  expect_error(morie_plcbsc(d$y1, list(c(1, 2)), d$t0),
               "same number of periods")
  # the intervention date has to leave periods on both sides
  expect_error(morie_plcbsc(d$y1, d$donors, 0), "at least one pre- and one")
  expect_error(morie_plcbsc(d$y1, d$donors, d$Tn), "at least one pre- and one")
  expect_error(morie_plcbsc(d$y1, d$donors, d$t0, statistic = "tstat"),
               "statistic must be")
  expect_type(.plcbsc_cheatsheet(), "character")
})
