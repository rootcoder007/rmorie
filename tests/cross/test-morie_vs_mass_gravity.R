.libPaths(c("~/tmp/Rlib", .libPaths()))
skip_if_not_installed("MASS")

.grv_case <- function() {
  ii <- 0:29
  mo <- 2 + (ii %% 5) * 3
  md <- 1 + ((ii * 7) %% 6) * 2
  d <- 1 + ((ii * 11) %% 9) / 2
  fl <- round(exp(0.5 + 0.8 * log(mo) + 0.6 * log(md) - 1.1 * log(d)) * exp(((ii * 13) %% 17) / 4 - 2))
  data.frame(fl = fl, mo = mo, md = md, d = d, o = LETTERS[1 + ii %% 5], de = letters[1 + (ii * 7) %% 6])
}

test_that("Igravnb equals MASS::glm.nb", {
  cs <- .grv_case()
  f <- MASS::glm.nb(fl ~ log(mo) + log(md) + log(d), cs, control = glm.control(epsilon = 1e-14, maxit = 200))
  r <- Igravnb(cs$fl, cs$mo, cs$md, cs$d)
  expect_equal(r$coefficients, unname(coef(f)), tolerance = 1e-7)
  expect_equal(r$theta, f$theta, tolerance = 1e-6)
  expect_equal(r$se, unname(sqrt(diag(vcov(f)))), tolerance = 1e-6)
  expect_equal(r$loglik, as.numeric(logLik(f)), tolerance = 1e-8)
})

test_that("Igravfe and Igravcl equal Poisson glm fits", {
  cs <- .grv_case()
  g <- glm(fl ~ 0 + o + de + log(d), poisson, cs, control = glm.control(epsilon = 1e-15, maxit = 100))
  r <- Igravfe(cs$fl, cs$o, cs$de, cs$d)
  expect_equal(r$statistic, unname(coef(g)["log(d)"]), tolerance = 1e-9)
  expect_equal(r$se, unname(sqrt(diag(vcov(g)))["log(d)" == names(coef(g))]), tolerance = 1e-8)
  h <- glm(fl ~ log(d) + offset(log(mo * md)), poisson, cs, control = glm.control(epsilon = 1e-15, maxit = 100))
  expect_equal(Igravcl(cs$fl, cs$mo, cs$md, cs$d)$statistic, -unname(coef(h)[2]), tolerance = 1e-9)
})
