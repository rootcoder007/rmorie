test_that("attention L-moments equal lmom::samlmu", {
  skip_if_not_installed("lmom")
  u <- .morie_random_uniform(300, seed = 8)
  ser <- lapply(0:9, function(k) 5 + 20 * u[25 * k + 1:25])
  r <- AttentionPunctuation(ser)
  expect_equal(r$l_moments, unname(lmom::samlmu(r$changes)), tolerance = 1e-12)
})
