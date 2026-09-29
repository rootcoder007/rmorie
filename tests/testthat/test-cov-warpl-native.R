# WARP (Weston, Bengio & Usunier 2010): alpha weights, L(r), sampled rank
# floor((Y-1)/N), the violation sampler replayed on the shared stream,
# and the weighted hinge step.

test_that("alpha weights and L(rank)", {
  expect_equal(morie_warpL_alpha_weights(4), 1 / (1:4))
  expect_equal(morie_warpL_alpha_weights(3, "uniform"), rep(1, 3))
  expect_equal(morie_warpL_alpha_weights(3, "top1"), c(1, 0, 0))
  expect_error(morie_warpL_alpha_weights(0), "at least 1")
  expect_error(morie_warpL_alpha_weights(3, "log"), "scheme must be")
  a <- morie_warpL_alpha_weights(5)
  expect_equal(morie_warpL_rank_weight(3, a), 1 + 1 / 2 + 1 / 3, tolerance = 1e-12)
  expect_equal(morie_warpL_rank_weight(9, a), sum(a), tolerance = 1e-12)
  expect_identical(morie_warpL_rank_weight(0, a), 0)
  expect_error(morie_warpL_rank_weight(-1, a), "negative")
})

test_that("the rank estimate is floor((Y - 1) / N)", {
  expect_identical(morie_warpL_estimate_rank(1, 101), 100L)
  expect_identical(morie_warpL_estimate_rank(3, 101), 33L)
  expect_identical(morie_warpL_estimate_rank(200, 101), 0L)
  expect_error(morie_warpL_estimate_rank(0, 10), "at least one draw")
  expect_error(morie_warpL_estimate_rank(1, 1), "two labels")
})

test_that("the violation sampler draws uniform negatives until one violates", {
  neg <- c(0.1, 0.9, 0.2, 1.8, 0.0, 0.4)
  Y <- length(neg) + 1L
  v <- morie_warpL_sample_violation(2, function(j) neg[j + 1], Y, .ghc_rng(3), margin = 1)
  e <- .ghc_rng(3)
  found <- FALSE
  for (t in seq_len(Y - 1)) {
    j <- as.integer(.ghc_unif(e, 1) * (Y - 1)) %% (Y - 1)
    if (neg[j + 1] > 2 - 1) {
      found <- TRUE
      break
    }
  }
  expect_identical(v$violated, found)
  expect_identical(v$draws, as.integer(t))
  if (found) {
    expect_identical(v$negative, as.integer(j))
    expect_identical(v$estimated_rank, as.integer((Y - 1) %/% t))
  }
  cap <- morie_warpL_sample_violation(10, function(j) neg[j + 1], Y, .ghc_rng(3), max_draws = 4)
  expect_false(cap$violated)
  expect_true(cap$capped)
  expect_identical(cap$draws, 4L)
  expect_identical(cap$estimated_rank, 0L)
  expect_error(morie_warpL_sample_violation(1, identity, Y, .ghc_rng(1), max_draws = 0),
               "at least one draw")
})

test_that("the WARP loss is L(rank) times the margin hinge", {
  a <- morie_warpL_alpha_weights(10)
  l <- morie_warpL_warp_loss(0.5, 0.2, 4, a, margin = 1)
  expect_equal(l$hinge, 0.7, tolerance = 1e-12)
  expect_equal(l$loss, sum(a[1:4]) * 0.7, tolerance = 1e-12)
  expect_identical(morie_warpL_warp_loss(3, 0, 4, a)$loss, 0)
})

test_that("a WARP step moves the user along P - negative by lr L(rank)", {
  u <- c(0.2, -0.1, 0.4)
  P <- c(1, 0, 0.5)
  negs <- list(c(0, 1, 0), c(0.5, 0.5, 0.5), c(-1, 0, 0), c(0.3, 0.2, 1))
  a <- morie_warpL_alpha_weights(4)
  r <- morie_warpL_warp_step(P, negs, u, .ghc_rng(9), a, lr = 0.1)
  sp <- sum(u * P)
  v <- morie_warpL_sample_violation(sp, function(j) sum(u * negs[[j + 1]]), 5, .ghc_rng(9))
  expect_true(r$updated)
  expect_identical(r$negative, v$negative)
  w <- sum(a[seq_len(min(v$estimated_rank, 4))])
  expect_equal(r$rank_weight, w, tolerance = 1e-12)
  expect_equal(r$user, u + 0.1 * w * (P - negs[[v$negative + 1]]), tolerance = 1e-12)
  expect_equal(r$loss, w * max(0, 1 - sp + sum(u * negs[[v$negative + 1]])), tolerance = 1e-12)
  big <- morie_warpL_warp_step(P * 100, negs, c(1, 1, 1), .ghc_rng(9), a)
  expect_false(big$updated)
  expect_identical(big$loss, 0)
  expect_identical(morie_warpL_warp(P, negs, u, .ghc_rng(9), a, lr = 0.1)$user, r$user)
  expect_match(morie_warpL_cheatsheet(), "SAMPLING", fixed = TRUE)
})
