# Coverage for the continuous-dose MSM, proximity bands, k-means CV folds,
# GP sampling, the Geron tape relu, tree prediction and the JSON writer;
# expectations are recomputed with base R in the test body.

make_dose <- function() {
  set.seed(5)
  n <- 60
  H <- cbind(rnorm(n), runif(n))
  A <- 1 + 0.8 * H[, 1] - 0.5 * H[, 2] + rnorm(n, sd = 0.7)
  y <- 2 + 1.5 * A + H[, 1] + rnorm(n)
  list(y = y, A = A, H = H, n = n)
}

test_that("morie_gentmt weight method is WLS with stabilized normal-density weights", {
  d <- make_dose()
  r <- morie_gentmt(d$y, d$A, d$H, method = "weight")
  tm <- lm(d$A ~ d$H)
  s2 <- sum(residuals(tm)^2) / (d$n - 3)
  w <- dnorm(d$A, mean(d$A), sd(d$A)) / dnorm(d$A, fitted(tm), sqrt(s2))
  expect_equal(r$weights, unname(w), tolerance = 1e-9)
  wf <- lm(d$y ~ d$A, weights = w)
  expect_equal(r$estimate, unname(coef(wf)[2]), tolerance = 1e-9)
  expect_equal(r$se, unname(summary(wf)$coefficients[2, 2]), tolerance = 1e-9)
  expect_equal(r$crude, unname(coef(lm(d$y ~ d$A))[2]), tolerance = 1e-9)
  expect_equal(r$effective_sample_size, sum(w)^2 / sum(w^2), tolerance = 1e-9)
  expect_equal(r$variance_ratio, s2 / var(d$A), tolerance = 1e-9)

  rt <- morie_gentmt(d$y, d$A, d$H, method = "weight", trim = 0.1)
  wt <- pmin(pmax(w, quantile(w, 0.1)), quantile(w, 0.9))
  expect_equal(rt$weights, unname(wt), tolerance = 1e-9)

  expect_error(morie_gentmt(d$y, d$A, d$H, method = "nope"), "method must be")
  expect_error(morie_gentmt(d$y, rep(0:1, 30), d$H), "distinct values")
  expect_error(morie_gentmt(d$y, d$A[-1], d$H), "outcomes but")
  expect_error(morie_gentmt(d$y, d$A, d$H, degree = 0), "degree")
})

test_that("morie_gentmt subclassify pools per-stratum OLS slopes", {
  d <- make_dose()
  r <- morie_gentmt(d$y, d$A, d$H, method = "subclassify", n_strata = 4)
  mu <- fitted(lm(d$A ~ d$H))
  o <- order(mu)
  ed <- round((0:4) * d$n / 4)
  sl <- se <- sz <- numeric(4)
  for (j in 1:4) {
    idx <- o[(ed[j] + 1):ed[j + 1]]
    f <- summary(lm(d$y[idx] ~ d$A[idx]))$coefficients
    sl[j] <- f[2, 1]
    se[j] <- f[2, 2]
    sz[j] <- length(idx)
  }
  expect_equal(r$stratum_slopes, sl, tolerance = 1e-9)
  expect_equal(r$estimate, sum(sl * sz) / sum(sz), tolerance = 1e-9)
  expect_equal(r$se, sqrt(sum((sz / sum(sz))^2 * se^2)), tolerance = 1e-9)
  expect_error(morie_gentmt(d$y, d$A, d$H, method = "subclassify", n_strata = 1), "at least 2")
  expect_error(morie_gentmt(d$y, d$A, d$H, method = "subclassify", n_strata = 20), "cannot support")
})

test_that("morie_gentmt doseresponse averages the Hirano-Imbens regression", {
  d <- make_dose()
  doses <- c(0, 1, 2.5)
  r <- morie_gentmt(d$y, d$A, d$H, method = "doseresponse", doses = doses)
  tm <- lm(d$A ~ d$H)
  s2 <- sum(residuals(tm)^2) / (d$n - 3)
  mu <- fitted(tm)
  dens <- dnorm(d$A, mu, sqrt(s2))
  b <- coef(lm(d$y ~ d$A + dens + I(dens^2) + I(d$A * dens)))
  curve <- vapply(doses, function(a) {
    g <- dnorm(a, mu, sqrt(s2))
    mean(b[1] + b[2] * a + b[3] * g + b[4] * g^2 + b[5] * a * g)
  }, 0)
  expect_equal(r$coef, unname(b), tolerance = 1e-9)
  expect_equal(r$curve, curve, tolerance = 1e-9)
  expect_equal(r$slopes, diff(curve) / diff(doses), tolerance = 1e-9)
  expect_equal(r$estimate, mean(diff(curve) / diff(doses)), tolerance = 1e-9)
  expect_length(morie_gentmt(d$y, d$A, d$H, method = "doseresponse")$doses, 21)
})

test_that("ProximityBands classifies grid cells by distance to points and segments", {
  x <- c(0, 1, 2, 3)
  y <- c(0, 1, 2)
  pts <- c(0, 2)
  seg <- rbind(c(1, 0), c(3, 0))
  r <- ProximityBands(x, y, breaks = c(0.5, 1.5), points = pts, lines = list(seg))
  segd <- function(px, py) {
    t <- max(0, min(1, (px - 1) / 2))
    sqrt((px - 1 - 2 * t)^2 + py^2)
  }
  D <- outer(y, x, Vectorize(function(py, px) min(sqrt(px^2 + (py - 2)^2), segd(px, py))))
  B <- (D >= 0.5) + (D >= 1.5)
  expect_equal(r$distance, D, tolerance = 1e-12)
  expect_equal(r$band, matrix(as.integer(B), 3))
  expect_equal(r$cells_per_band, as.integer(tabulate(B + 1, 3)))
  expect_equal(r$area_per_band, tabulate(B + 1, 3) * 1 * 1)
})

test_that("KmeansCvFolds returns a k-means fixed point", {
  P <- rbind(c(0, 0), c(0.2, 0.1), c(0.1, 0.3), c(5, 5), c(5.2, 4.8),
             c(4.9, 5.1), c(10, 0), c(10.3, 0.2), c(9.8, -0.1))
  r <- KmeansCvFolds(P, 3, seed = 4)
  expect_setequal(unique(r$fold), 0:2)
  lab <- apply(P, 1, function(p) which.min(colSums((t(r$centers) - p)^2)))
  expect_equal(match(lab, unique(lab)) - 1L, r$fold)
  for (j in 1:3) {
    expect_equal(r$centers[j, ], colMeans(P[lab == j, , drop = FALSE]), tolerance = 1e-12)
  }
  r1 <- KmeansCvFolds(P, 1)
  expect_equal(r1$fold, rep(0L, 9))
  expect_equal(r1$centers[1, ], colMeans(P), tolerance = 1e-12)
})

test_that("GpSample draws mu + L z from the prior and the posterior", {
  X <- matrix(c(0, 0.4, 1.1, 2), ncol = 1)
  K <- outer(X[, 1], X[, 1], function(a, b) 2 * exp(-0.5 * (a - b)^2 / 0.7^2))
  r <- GpSample(X, "se", nsim = 2, seed = 3, variance = 2, lengthscale = 0.7)
  expect_equal(r$cov, K, tolerance = 1e-12)
  L <- t(chol(K + diag(1e-10, 4)))
  for (s in 0:1) {
    z <- .morie_random_normal(4, seed = 3, stream = s)
    expect_equal(r$samples[s + 1, ], as.numeric(L %*% z), tolerance = 1e-12)
  }
  P <- matrix(c(0.2, 1.5), ncol = 1)
  yd <- c(1, -0.5)
  kf <- function(a, b) exp(-0.5 * outer(a, b, "-")^2)
  post <- GpSample(X, "se", nsim = 1, seed = 1, data = list(P, yd), noise = 0.01)
  Kp <- kf(P[, 1], P[, 1]) + diag(0.01, 2)
  Ks <- kf(P[, 1], X[, 1])
  expect_equal(post$mean, as.numeric(t(Ks) %*% solve(Kp, yd)), tolerance = 1e-9)
  expect_equal(post$cov, kf(X[, 1], X[, 1]) - t(Ks) %*% solve(Kp, Ks), tolerance = 1e-9)
  expect_error(GpSample(X, "nope"), "kernel must be")
})

test_that("morie_gvar_relu passes the gradient only on the positive side", {
  g <- morie_geron_autograd(function(p) morie_gvar_relu(p[[1]] * p[[2]] - 1), c(2, 3))
  expect_equal(g$value, max(2 * 3 - 1, 0))
  expect_equal(g$grad, c(3, 2))
  g0 <- morie_geron_autograd(function(p) morie_gvar_relu(p[[1]] - p[[2]]) + p[[2]] * 2, c(1, 4))
  expect_equal(g0$value, 8)
  expect_equal(g0$grad, c(0, 2))
})

test_that("morie_geron_predict_tree walks a hand-built tree", {
  leaf <- function(v) list(leaf = TRUE, value = v)
  tree <- list(leaf = FALSE, feature = 0L, threshold = 1.5,
               left = leaf(10),
               right = list(leaf = FALSE, feature = 1L, threshold = 0,
                            left = leaf(20), right = leaf(30)))
  X <- rbind(c(1, 5), c(1.5, -1), c(2, -1), c(2, 0.5))
  expect_equal(morie_geron_predict_tree(tree, X),
               ifelse(X[, 1] <= 1.5, 10, ifelse(X[, 2] <= 0, 20, 30)))
  expect_equal(morie_geron_predict_tree(tree, c(3, 3)), 30)
})

test_that("jsonlite_toJSON_or_stub writes JSON that parses back", {
  skip_if_not_installed("jsonlite")
  x <- list(a = 1.5, b = c(1, 2, 3), s = "txt")
  back <- jsonlite::fromJSON(jsonlite_toJSON_or_stub(x))
  expect_equal(back$a, 1.5)
  expect_equal(back$b, c(1, 2, 3))
  expect_equal(back$s, "txt")
})
