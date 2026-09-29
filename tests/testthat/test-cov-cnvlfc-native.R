# Coverage for convergent cross mapping (Sugihara et al. 2012): delay
# embedding against stats::embed, simplex cross-map prediction of X_t from
# the E + 1 nearest neighbours on Y's shadow manifold with weights
# exp(-d / d_1) (independent implementation, including the Theiler
# window), the coupled logistic map, and the CCM curves and verdict.

.xmap <- function(X, Y, E, tau = 1, ex = 0) {
  n <- length(Y)
  t0 <- (E - 1) * tau + 1
  tt <- t0:n
  M <- t(vapply(tt, function(t) Y[t - (0:(E - 1)) * tau], numeric(E)))
  M <- matrix(M, ncol = E)
  pred <- vapply(seq_along(tt), function(a) {
    cand <- which(seq_along(tt) != a & abs(tt - tt[a]) > ex)
    d <- sqrt(colSums((t(M[cand, , drop = FALSE]) - M[a, ])^2))
    o <- order(d)[1:(E + 1)]
    w <- if (d[o[1]] > 0) exp(-d[o] / d[o[1]]) else c(1, rep(0, E))
    sum(w / sum(w) * X[tt[cand[o]]])
  }, 1)
  list(obs = X[tt], pred = pred, rho = stats::cor(X[tt], pred))
}

test_that("delay embedding matches stats::embed and honours tau", {
  y <- c(3, 1, 4, 1, 5, 9, 2, 6)
  e <- cnvlfc_embed(y, E = 3)
  expect_equal(do.call(rbind, e$points), stats::embed(y, 3))
  expect_identical(e$index, 2:7)
  e2 <- cnvlfc_embed(y, E = 2, tau = 3)
  expect_equal(e2$points[[1]], c(1, 3))
  expect_identical(morie_cnvlfc, cnvlfc_embed)
  expect_error(cnvlfc_embed(y, E = 0), "at least 1")
  expect_error(cnvlfc_embed(y, tau = 0), "delay must be at least 1")
  expect_error(cnvlfc_embed(1:4, E = 3, tau = 2), "cannot support")
})

test_that("the coupled logistic map iterates x(r - r x - b y)", {
  s <- cnvlfc_coupled_logistic(5, burn = 2)
  x <- 0.4
  y <- 0.2
  X <- Y <- numeric(0)
  for (i in 1:7) {
    xn <- x * (3.8 - 3.8 * x)
    y <- y * (3.5 - 3.5 * y - 0.1 * x)
    x <- xn
    if (i > 2) {
      X <- c(X, x)
      Y <- c(Y, y)
    }
  }
  expect_equal(s$x, X, tolerance = 1e-12)
  expect_equal(s$y, Y, tolerance = 1e-12)
  expect_error(cnvlfc_coupled_logistic(5, rx = 50, x0 = 5), "diverged")
})

test_that("cross mapping predicts X_t from Y's manifold at the same t", {
  s <- cnvlfc_coupled_logistic(80)
  for (E in 1:3) {
    r <- cnvlfc_cross_map(s$x, s$y, E = E)
    ref <- .xmap(s$x, s$y, E)
    expect_equal(r$observed, ref$obs, tolerance = 1e-12)
    expect_equal(r$predicted, ref$pred, tolerance = 1e-12)
    expect_equal(r$rho, ref$rho, tolerance = 1e-12)
  }
  th <- cnvlfc_cross_map(s$x, s$y, E = 2, tau = 2, exclude = 3)
  expect_equal(th$predicted, .xmap(s$x, s$y, 2, 2, 3)$pred, tolerance = 1e-12)
  lb <- cnvlfc_cross_map(s$x, s$y, E = 2, library = 20, seed = 4)
  expect_identical(lb$library, 20L)
  expect_error(cnvlfc_cross_map(s$x, s$y[-1]), "80 and 79 points")
  expect_error(cnvlfc_cross_map(s$x, s$y, library = 3), "cannot supply 3 neighbours")
  expect_error(cnvlfc_cross_map(s$x, s$y, library = 500), "only 79 are embeddable")
})

test_that("CCM curves are cross-map skills at each library size", {
  s <- cnvlfc_coupled_logistic(150, byx = 0.3)
  r <- cnvlfc_ccm(s$x, s$y, lib_sizes = c(10, 40, 149), seed = 2)
  xy <- vapply(c(10, 40, 149), function(L) cnvlfc_cross_map(s$x, s$y, library = L, seed = 2)$rho, 1)
  yx <- vapply(c(10, 40, 149), function(L) cnvlfc_cross_map(s$y, s$x, library = L, seed = 2)$rho, 1)
  expect_equal(vapply(r$x_causes_y$curve, `[[`, 1, "rho"), xy, tolerance = 1e-12)
  expect_equal(vapply(r$y_causes_x$curve, `[[`, 1, "rho"), yx, tolerance = 1e-12)
  cx <- xy[3] - xy[1] > 0.05 && xy[3] > 0.3
  cy <- yx[3] - yx[1] > 0.05 && yx[3] > 0.3
  expect_identical(r$x_causes_y$converges, cx)
  expect_identical(r$verdict, if (cx && cy) r$verdict else if (cx) "x drives y" else if (cy) "y drives x" else
    "no convergent cross mapping in either direction")
  # x forces y and not back, so recovering X from Y beats the reverse
  expect_gt(r$estimate$x_causes_y, r$estimate$y_causes_x)
  d <- cnvlfc_convergent_cross_mapping(s$x, s$y, seed = 2)
  expect_identical(d$lib_sizes, as.integer(sort(unique(pmax(5L, as.integer(149 * c(0.05, 0.1, 0.25, 0.5, 1)))))))
})
