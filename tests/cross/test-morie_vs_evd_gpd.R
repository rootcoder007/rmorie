.libPaths(c("~/tmp/Rlib", .libPaths()))
skip_if_not_installed("evd")

test_that("Gpfit equals evd::fpot", {
  ii <- 0:59
  x <- -log(1 - ((ii * 37) %% 59 + 0.5) / 60) * (1 + ((ii * 7) %% 5) / 4)
  u <- 0.8
  f <- evd::fpot(x, u, std.err = TRUE, control = list(reltol = 1e-14, maxit = 5000))
  r <- Gpfit(x, u)
  # evd stops its optimiser near 1e-5; Gpfit polishes the score, so its likelihood is at least as high
  expect_equal(c(r$scale, r$shape), unname(f$estimate), tolerance = 1e-4)
  expect_equal(c(r$se_sigma, r$se_xi), unname(f$std.err), tolerance = 1e-3)
  expect_gte(r$loglik, -f$deviance / 2 - 1e-10)
})
