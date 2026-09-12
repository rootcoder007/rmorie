# One-step TMLE along a universal least favorable submodel (van der Laan &
# Rose 2018, Ch. 5).
#
# Anchors outside the module:
#   * With a constant clever covariate the score equation reduces to
#     mean(Y) - mean(Q) = 0, so the targeted fit must have mean(Q*) equal
#     to the empirical mean of Y. That fixes the answer arithmetically.
#   * A fit that already solves the score equation must not move at all.
#   * The submodel's defining property is that the path's derivative equals
#     the clever covariate times the logistic Jacobian at every point, not
#     only at the origin, and that a local submodel loses it once it moves.
#   * The one-step and the iterative routes solve the same equation and
#     must agree.

set.seed(3)
N <- 20
YB <- rbinom(N, 1, 0.6)
Q0 <- plogis(rnorm(N, 0, 0.5))          # an initial fit strictly inside (0,1)

test_that("the logit and its inverse round-trip", {
  # both are scalar helpers; the module applies them element by element
  p <- c(0.01, 0.3, 0.5, 0.7, 0.99)
  rt <- vapply(p, function(x) .tl1step_expit(.tl1step_logit(x)), numeric(1))
  expect_equal(rt, p)
  expect_equal(.tl1step_logit(0.5), 0)
  expect_equal(.tl1step_expit(0), 0.5)
  # the inverse is monotone and stays inside the unit interval
  z <- c(-8, -1, 0, 1, 8)
  e <- vapply(z, .tl1step_expit, numeric(1))
  expect_true(all(diff(e) > 0))
  expect_true(all(e > 0 & e < 1))
  # and it is the odd-symmetric logistic
  expect_equal(.tl1step_expit(-2), 1 - .tl1step_expit(2))
  expect_equal(.tl1step_logit(0.75), -.tl1step_logit(0.25))
})

test_that("the submodel path is built in both directions and sorted", {
  H <- function(q) rep(1, length(q))
  b <- .tl1step_build_ulfm(Q0, H, YB, eps_max = 1, steps = 20L)
  # one entry per step each way, plus the origin
  expect_length(b$path, 2L * 20L + 1L)
  expect_equal(b$steps, 20L)
  expect_equal(b$d_epsilon, 1 / 20)
  eps <- vapply(b$path, function(p) p$eps, numeric(1))
  # sorted, symmetric about zero, and containing the untouched fit
  expect_true(all(diff(eps) > 0))
  expect_equal(min(eps), -1, tolerance = 1e-12)
  expect_equal(max(eps), 1, tolerance = 1e-12)
  zero <- which.min(abs(eps))
  expect_equal(eps[zero], 0)
  expect_equal(b$path[[zero]]$q, Q0)
  # every point on the path is a valid probability vector
  for (p in b$path) expect_true(all(p$q > 0 & p$q < 1))
  # a positive direction raises the fit and a negative one lowers it
  expect_true(all(b$path[[length(b$path)]]$q > Q0))
  expect_true(all(b$path[[1L]]$q < Q0))
  expect_match(b$note, "least favorable EVERYWHERE")
  expect_error(.tl1step_build_ulfm(Q0, H, YB[1:5]), "fits but .* outcomes")
  expect_error(.tl1step_build_ulfm(Q0, H, YB, steps = 0L),
               "steps must be at least 1")
})

test_that("the submodel is universal and a local one is not", {
  # a clever covariate that genuinely depends on the current fit, so the
  # distinction between recomputing the direction and freezing it bites
  H <- function(q) 1 - 2 * q
  u <- .tl1step_is_universal(Q0, H, eps = 0.3)
  # the path's derivative equals H(Q_eps) times the logistic Jacobian
  expect_true(u$universal)
  expect_true(u$max_deviation < 1e-3)
  expect_equal(u$epsilon, 0.3)
  # and a submodel that keeps its initial direction has drifted away from
  # the gradient by the time it gets there, which is the whole point
  expect_true(u$local_submodel_direction_drift > 1e-6)
  expect_match(u$note, "LOCAL submodel")
  # a constant clever covariate cannot drift, since H does not depend on Q
  uc <- .tl1step_is_universal(Q0, function(q) rep(1, length(q)), eps = 0.3)
  expect_true(uc$universal)
  expect_equal(uc$local_submodel_direction_drift, 0)
})

test_that("a constant clever covariate targets the mean of the outcome", {
  # the score is mean(Y - Q), so the solution has mean(Q*) = mean(Y)
  H <- function(q) rep(1, length(q))
  r <- .tl1step_one_step_tmle(Q0, H, YB, eps_max = 4, steps = 400L)
  expect_equal(r$estimate, mean(YB), tolerance = 1e-3)
  expect_equal(r$psi, r$estimate)
  expect_equal(r$estimate, mean(r$Q_star))
  # the score equation is solved, and recomputing it from the returned fit
  # independently confirms that
  expect_true(r$abs_score < 1e-3)
  expect_equal(abs(mean(H(r$Q_star) * (YB - r$Q_star))), r$abs_score,
               tolerance = 1e-12)
  # the fit really moved, and stayed a probability
  expect_false(isTRUE(all.equal(r$Q_star, Q0)))
  expect_true(all(r$Q_star > 0 & r$Q_star < 1))
  expect_equal(r$iterations, 1)
  expect_equal(r$path_steps, 400L)
  expect_match(r$method, "one-step TMLE")
  expect_match(r$note, "no iteration")
  # the public entry point is the same routine
  expect_equal(morie_tl1step(Q0, H, YB, eps_max = 4, steps = 400L), r)
})

test_that("a fit that already solves the score equation does not move", {
  # Q constant at the mean of Y with a constant clever covariate gives a
  # score of exactly zero at epsilon = 0
  H <- function(q) rep(1, length(q))
  Qs <- rep(mean(YB), N)
  expect_equal(mean(H(Qs) * (YB - Qs)), 0)
  r <- .tl1step_one_step_tmle(Qs, H, YB, eps_max = 1, steps = 50L)
  expect_equal(r$epsilon, 0)
  expect_equal(r$Q_star, Qs)
  expect_equal(r$estimate, mean(YB))
  expect_equal(r$abs_score, 0)
})

test_that("the treatment-weighted covariate targets the weighted mean", {
  # the clever covariate for E[Y(1)] is A / g(W); the score equation then
  # sets the weighted average of the residuals to zero
  set.seed(7)
  A <- rbinom(N, 1, 0.5)
  g <- rep(0.5, N)
  H <- function(q) A / g
  r <- .tl1step_one_step_tmle(Q0, H, YB, eps_max = 4, steps = 400L)
  expect_true(r$abs_score < 1e-2)
  # recomputed from the returned fit
  expect_equal(abs(mean((A / g) * (YB - r$Q_star))), r$abs_score,
               tolerance = 1e-12)
  # the estimate is the plug-in mean of the targeted fit
  expect_equal(r$estimate, mean(r$Q_star))
  expect_true(all(r$Q_star > 0 & r$Q_star < 1))
})

test_that("the one-step and iterative routes agree", {
  # both solve the same efficient score equation, which is the module's
  # premise for replacing the iteration with a single move
  H <- function(q) rep(1, length(q))
  one <- .tl1step_one_step_tmle(Q0, H, YB, eps_max = 4, steps = 400L)
  it <- .tl1step_iterative_tmle(Q0, H, YB)
  expect_equal(it$estimate, one$estimate, tolerance = 1e-2)
  expect_true(abs(it$abs_score) < 1e-2 || it$abs_score < 1e-2)
  # the iterative route reports how many steps it took
  expect_true(it$iterations >= 1)
  # and both land on the empirical mean of the outcome
  expect_equal(it$estimate, mean(YB), tolerance = 1e-2)
})

test_that("a wider path can reach a solution a narrow one cannot", {
  # a fit far from the solution needs room to move; capping epsilon too
  # tightly leaves the score unsolved, and the routine reports the best it
  # reached rather than claiming success
  H <- function(q) rep(1, length(q))
  far <- rep(0.02, N)
  narrow <- .tl1step_one_step_tmle(far, H, YB, eps_max = 0.05, steps = 20L)
  wide <- .tl1step_one_step_tmle(far, H, YB, eps_max = 6, steps = 600L)
  expect_true(narrow$abs_score > wide$abs_score)
  expect_true(wide$abs_score < 1e-2)
  expect_equal(wide$estimate, mean(YB), tolerance = 1e-2)
  # the narrow run pushed as far as it was allowed
  expect_equal(abs(narrow$epsilon), 0.05, tolerance = 1e-9)
})
