test_that("PrewhitenedMannKendall matches modifiedmk", {
  skip_if_not_installed("modifiedmk")
  x <- 0.05 * (0:39) + .morie_random_uniform(40, seed = 5) + 0.3 * .morie_random_uniform(40, seed = 6)
  a <- PrewhitenedMannKendall(x, "pw")
  b <- modifiedmk::pwmk(x)
  expect_equal(unname(c(a$Z, a$sen_slope, a$S, a$var_S, a$tau, a$p_value)),
               unname(b[c("Z-Value", "Sen's Slope", "S", "Var(S)", "Tau", "P-value")]), tolerance = 1e-9)
  a <- PrewhitenedMannKendall(x, "tfpw")
  b <- modifiedmk::tfpwmk(x)
  expect_equal(unname(c(a$Z, a$sen_slope, a$old_sen_slope, a$S, a$var_S, a$tau, a$p_value)),
               unname(b[c("Z-Value", "Sen's Slope", "Old Sen's Slope", "S", "Var(S)", "Tau", "P-value")]), tolerance = 1e-9)
})

test_that("FleissKappa matches irr", {
  skip_if_not_installed("irr")
  C <- rbind(c(3, 1, 0), c(0, 2, 2), c(4, 0, 0), c(1, 1, 2), c(0, 0, 4), c(2, 2, 0))
  ratings <- t(apply(C, 1, function(r) rep(seq_along(r), r)))
  b <- irr::kappam.fleiss(ratings)
  a <- FleissKappa(C)
  expect_equal(a$kappa, b$value, tolerance = 1e-12)
  expect_equal(a$z, unname(b$statistic), tolerance = 1e-9)
})
