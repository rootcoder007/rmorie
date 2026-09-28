test_that("AccuracyAssessment kappa equals irr::kappa2", {
  skip_if_not_installed("irr")
  u <- .morie_random_uniform(200, seed = 19, stream = 0)
  ref <- floor(u[1:100] * 4) + 1
  pred <- ifelse(u[101:200] < 0.7, ref, floor(u[101:200] * 40) %% 4 + 1)
  expect_equal(AccuracyAssessment(ref, pred)$kappa, irr::kappa2(cbind(ref, pred))$value, tolerance = 1e-14)
})
