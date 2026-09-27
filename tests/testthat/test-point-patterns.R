pp_pattern <- function() {
  U <- .morie_random_uniform(120, seed = 21, stream = 0)
  cbind(2 * U[seq(1, 119, 2)], U[seq(2, 120, 2)])
}

test_that("Ripk corrections match spatstat Kest", {
  k <- Ripk(pp_pattern(), c(0, 2, 0, 1), c(0.05, 0.1, 0.15, 0.2))
  expect_lt(max(abs(k$k * 60 / 59 - c(0.003389831, 0.018266978, 0.053770178, 0.108408179))), 1e-9)
  expect_lt(max(abs(k$k_trans * 60 / 59 - c(0.003492317, 0.018165883, 0.052690170, 0.103961817))), 1e-9)
  expect_lt(max(abs(k$k_border - c(0.002515723, 0.015833333, 0.048888889, 0.088))), 1e-9)
})

test_that("RipG border estimate matches Gest reduced sample", {
  g <- RipG(pp_pattern(), c(0, 2, 0, 1), c(0.05, 0.1, 0.15, 0.2))
  expect_lt(max(abs(g$g_border - c(4 / 53, 0.4, 13 / 15, 1))), 1e-12)
})

test_that("SpaceTimeK matches splancs stkhat", {
  tt <- 10 * .morie_random_uniform(60, seed = 22, stream = 0)
  r <- SpaceTimeK(pp_pattern(), tt, c(0, 2, 0, 1), c(0, 10), c(0.05, 0.1, 0.2, 0.3), c(0.5, 1, 2, 3))
  expect_lt(max(abs(r$kst[3, ] - c(0.092752860557, 0.15307957042, 0.360335316901, 0.677008530433))), 1e-9)
  expect_lt(max(abs(r$D - (r$kst - outer(r$ks, r$kt)))), 1e-15)
})
