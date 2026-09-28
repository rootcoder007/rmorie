test_that("Krige and KrigeCV equal gstat::krige and krige.cv", {
  skip_if_not_installed("gstat")
  skip_if_not_installed("sp")
  u <- .morie_random_uniform(60, seed = 17, stream = 0)
  P <- cbind(3 * u[1:12], 3 * u[13:24])
  z <- sin(P[, 1]) + 0.3 * P[, 2] + u[25:36]
  Q <- cbind(3 * u[37:40], 3 * u[41:44])
  d <- data.frame(x = P[, 1], y = P[, 2], z = z)
  sp::coordinates(d) <- ~ x + y
  nd <- data.frame(x = Q[, 1], y = Q[, 2])
  sp::coordinates(nd) <- ~ x + y
  gm <- gstat::vgm(0.9, "Sph", 1.8, 0.15)
  ours <- list(list(model = "Nug", psill = 0.15), list(model = "Sph", psill = 0.9, range = 1.8))
  q <- function(...) suppressWarnings(gstat::krige(..., debug.level = 0))
  for (case in list(list(f = z ~ 1, a = list()), list(f = z ~ 1, a = list(beta = 1)),
                    list(f = z ~ x + y, a = list(X = cbind(1, P), X0 = cbind(1, Q))),
                    list(f = z ~ 1, a = list(nmax = 5), g = list(nmax = 5)))) {
    g <- do.call(q, c(list(case$f, d, nd, gm), if (!is.null(case$a$beta)) list(beta = 1), case$g))
    r <- do.call(Krige, c(list(z, P, Q, ours), case$a))
    expect_equal(r$prediction, g$var1.pred, tolerance = 1e-12)
    expect_equal(r$variance, g$var1.var, tolerance = 1e-12)
  }
  g <- q(z ~ 1, d, nd, gm, block = c(0.5, 0.3))
  r <- Krige(z, P, Q, ours, block = BlockDiscretize(c(0.5, 0.3)))
  # gstat keeps its Gauss block weights in single precision
  expect_equal(r$variance, g$var1.var, tolerance = 5e-8)
  cv <- suppressWarnings(gstat::krige.cv(z ~ 1, d, gm, nfold = rep(1:4, 3), verbose = FALSE))
  expect_equal(KrigeCV(z, P, ours, folds = rep(1:4, 3))$prediction, cv$var1.pred, tolerance = 1e-12)
})
