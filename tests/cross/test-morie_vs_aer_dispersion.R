.libPaths(c("~/tmp/Rlib", .libPaths()))
skip_if_not_installed("AER")

test_that("Scdisp equals AER::dispersiontest", {
  ii <- 0:39
  x <- ((ii * 7) %% 13) / 6
  y <- round(exp(0.3 + 0.8 * x) * exp(((ii * 11) %% 9) / 3 - 1.3))
  m <- glm(y ~ x, family = poisson, control = glm.control(epsilon = 1e-15, maxit = 100))
  for (tr in list(NULL, 2)) {
    a <- AER::dispersiontest(m, trafo = tr)
    r <- Scdisp(y, cbind(1, x), tr)
    expect_equal(r$statistic, unname(a$statistic), tolerance = 1e-8)
    expect_equal(r$p_value, a$p.value, tolerance = 1e-8)
    expect_equal(r$dispersion, unname(a$estimate), tolerance = 1e-8)
  }
})
