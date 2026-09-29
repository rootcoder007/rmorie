# Cross tests: Rips persistence against TDAstats, possibilistic c-means against ppclust, REML against nlme.

test_that("RipsPersistence equals TDAstats::calculate_homology (finite pairs)", {
  skip_if_not_installed("TDAstats")
  i <- 0:17
  pts <- cbind(cos(2 * pi * i / 18) * (1 + 0.15 * (i %% 4)), sin(2 * pi * i / 18) * (1 + 0.1 * (i %% 3)) + 0.03 * i)
  pts <- rbind(pts, cbind(3.5 + 0.2 * sin(0:5), 0.3 * (0:5)))
  ref <- TDAstats::calculate_homology(pts, dim = 1)
  got <- RipsPersistence(pts)$diagram
  got <- got[is.finite(got[, 3]), , drop = FALSE]
  ref <- ref[ref[, 3] < max(ref[, 3]) | ref[, 1] == 1, , drop = FALSE]
  key <- function(m) m[order(m[, 1], m[, 2], m[, 3]), , drop = FALSE]
  common <- key(unname(got))
  expect_equal(key(unname(ref))[seq_len(nrow(common)), ], common, tolerance = 1e-12)
})

test_that("PossibilisticCmeans equals ppclust::pcm with given prototypes and omega", {
  skip_if_not_installed("ppclust")
  j <- 0:29
  X <- cbind(sin(j * 1.3) + ifelse(j %% 3 == 0, 3, 0), cos(j * 0.7) + ifelse(j %% 3 == 1, 2, 0))
  ce <- rbind(c(0, 0), c(3, 0.5), c(0.5, 2))
  ref <- ppclust::pcm(X, centers = ce, omega = c(1, 1.5, 0.8))
  got <- PossibilisticCmeans(X, ce, omega = c(1, 1.5, 0.8))
  expect_equal(got$centers, unname(ref$v), tolerance = 1e-10)
  expect_equal(got$typicality, unname(ref$t), tolerance = 1e-10)
})

test_that("nested and crossed REML components equal nlme::lme", {
  skip_if_not_installed("nlme")
  j <- 0:35
  a <- j %/% 12
  b <- (j %/% 3) %% 4
  y <- 2 + c(-1, 0.5, 1.2)[a + 1] + 0.6 * sin(3 * (a * 4 + b)) + 0.3 * sin(j * 1.9)
  df <- data.frame(y = y, a = factor(a), b = factor(b), c1 = factor(j %% 4), c2 = factor(j %% 3), g = 1)
  m <- nlme::lme(y ~ 1, random = ~ 1 | a / b, data = df, method = "REML", control = nlme::lmeControl(tolerance = 1e-10))
  got <- NestedRandomEffects(y, a, b)
  vc <- as.numeric(nlme::VarCorr(m)[c(2, 4, 5), 1])
  expect_equal(got$variances, vc, tolerance = 1e-5)
  expect_equal(got$reml_loglik, as.numeric(logLik(m)), tolerance = 1e-8)
  m2 <- nlme::lme(y ~ 1, random = list(g = nlme::pdBlocked(list(nlme::pdIdent(~ c1 - 1), nlme::pdIdent(~ c2 - 1)))),
                  data = df, method = "REML")
  got2 <- CrossedRandomEffects(y, j %% 4, j %% 3)
  expect_equal(got2$reml_loglik, as.numeric(logLik(m2)), tolerance = 1e-8)
})
