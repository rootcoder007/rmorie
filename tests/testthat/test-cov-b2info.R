# Coverage tests for R/b2info.R (Cover and Thomas 2006; Shannon 1948):
# entropies, mutual information, KL chain rule, data processing and
# coding redundancy, recomputed from their sums.

H2 <- function(p) {
  p <- p[p > 0]
  -sum(p * log2(p))
}

pxy <- rbind(c(0.1, 0.2, 0.05), c(0.25, 0.1, 0.3))
pxyz <- array(c(0.05, 0.1, 0.15, 0.02, 0.08, 0.1, 0.2, 0.3), dim = c(2, 2, 2))

test_that("Shannon, joint, conditional entropy and mutual information", {
  y <- c(3, 1, 0, 4)
  s <- Shanent(y)
  expect_equal(s$estimate, H2(y / 8), tolerance = 1e-12)
  expect_equal(s$evenness, H2(y / 8) / 2, tolerance = 1e-12)
  expect_equal(Shanent(c(1, 1), base = NULL)$estimate, log(2), tolerance = 1e-12)
  expect_error(Shanent(c(-1, 2)), "non-negative")
  expect_error(Shanent(c(1, 1), base = 1), "not 1")
  j <- Jntent(pxy)
  expect_equal(j$estimate, H2(pxy), tolerance = 1e-12)
  expect_equal(j$hx, H2(rowSums(pxy)), tolerance = 1e-12)
  ce <- Cndent(pxy)
  hyx <- -sum(pxy * log2(pxy / rowSums(pxy)))
  expect_equal(ce$estimate, hyx, tolerance = 1e-12)
  mi <- Mutinf(pxy)
  expect_equal(mi$estimate, sum(pxy * log2(pxy / outer(rowSums(pxy), colSums(pxy)))), tolerance = 1e-12)
  expect_equal(Mutinf(list(c(1, 2), c(3, 4)))$estimate, Mutinf(rbind(1:2, 3:4))$estimate)
  expect_equal(Mutinf(outer(c(0.3, 0.7), c(0.4, 0.6)))$estimate, 0, tolerance = 1e-12)
  expect_error(Jntent(rbind(c(1, -1))), "non-negative")
})

test_that("conditional mutual information and the data-processing inequality", {
  cm <- Cndmi(pxyz)
  p <- pxyz / sum(pxyz)
  pz <- apply(p, 3, sum)
  pxz <- apply(p, c(1, 3), sum)
  pyz <- apply(p, c(2, 3), sum)
  ref <- 0
  for (i in 1:2) for (j in 1:2) for (k in 1:2) {
    ref <- ref + p[i, j, k] * log2(p[i, j, k] * pz[k] / (pxz[i, k] * pyz[j, k]))
  }
  expect_equal(cm$estimate, ref, tolerance = 1e-12)
  # a Markov chain X -> Y -> Z: p(x, y, z) = p(x) p(y|x) p(z|y)
  px <- c(0.3, 0.7)
  pyx <- rbind(c(0.9, 0.1), c(0.2, 0.8))
  pzy <- rbind(c(0.6, 0.4), c(0.25, 0.75))
  mk <- array(0, c(2, 2, 2))
  for (i in 1:2) for (j in 1:2) for (k in 1:2) mk[i, j, k] <- px[i] * pyx[i, j] * pzy[j, k]
  d <- Dpineq(mk)
  expect_equal(d$markov_gap, 0, tolerance = 1e-12)
  expect_true(d$holds)
  expect_equal(d$ixy, Mutinf(apply(mk, c(1, 2), sum))$estimate, tolerance = 1e-12)
  expect_equal(d$estimate, d$ixy - Mutinf(apply(mk, c(1, 3), sum))$estimate, tolerance = 1e-12)
  lst <- list(list(c(0.05, 0.08), c(0.15, 0.2)), list(c(0.1, 0.1), c(0.02, 0.3)))
  expect_equal(Cndmi(lst)$estimate, cm$estimate, tolerance = 1e-12)
  expect_error(Cndmi(list(list(1:2, 1), list(1:2, 1:2))), "ragged")
})

test_that("KL chain rule and cross entropy", {
  qxy <- rbind(c(0.2, 0.1, 0.1), c(0.2, 0.2, 0.2))
  k <- Klchain(pxy, qxy)
  kl <- function(a, b) sum(a[a > 0] * log2(a[a > 0] / b[a > 0]))
  expect_equal(k$estimate, kl(pxy, qxy), tolerance = 1e-12)
  expect_equal(k$marginal, kl(rowSums(pxy), rowSums(qxy)), tolerance = 1e-12)
  expect_equal(k$residual, 0, tolerance = 1e-12)
  z <- qxy
  z[1, 1] <- 0
  expect_true(is.nan(Klchain(pxy, z)$residual))
  expect_error(Klchain(pxy, qxy[, 1:2]), "same shape")
  p <- c(0.5, 0.3, 0.2)
  q <- c(0.2, 0.5, 0.3)
  cr <- Crsent(p, q)
  expect_equal(cr$estimate, -sum(p * log2(q)), tolerance = 1e-12)
  expect_equal(cr$kl, kl(p, q), tolerance = 1e-12)
  expect_equal(Crsent(p, c(0, 0.5, 0.5))$estimate, Inf)
  expect_error(Crsent(p, q[1:2]), "same length")
})

test_that("coding with the wrong model, redundancy, surprisal, compositions", {
  model <- c(0.5, 0.25, 0.25)
  data <- c(0, 0, 1, 2, 1, 0, 2, 2)
  pc <- Predcomp(model, data)
  ph <- tabulate(data + 1, 3) / 8
  expect_equal(pc$estimate, -mean(log2(model[data + 1])), tolerance = 1e-12)
  expect_equal(pc$estimate, H2(ph) + sum(ph * log2(ph / model)), tolerance = 1e-12)
  expect_equal(pc$upper, pc$entropy + pc$kl + 1, tolerance = 1e-12)
  expect_error(Predcomp(model, 3), "outside the model alphabet")
  r <- Redund(c(0.7, 0.1, 0.1, 0.1))
  expect_equal(r$estimate, 1 - H2(c(0.7, 0.1, 0.1, 0.1)) / 2, tolerance = 1e-12)
  expect_error(Redund(1), "at least two")
  s <- Surpris(c(1, 1, 2), c(2, 0))
  expect_equal(s$values, c(1, 2))
  expect_equal(s$estimate, 1.5)
  expect_equal(Surpris(c(1, 0), 1)$estimate, Inf)
  cs <- Compshan(c(20, 30, 50))
  expect_equal(cs$closure, 100)
  expect_equal(cs$estimate, H2(c(0.2, 0.3, 0.5)), tolerance = 1e-12)
  expect_equal(cs$evenness, H2(c(0.2, 0.3, 0.5)) / log2(3), tolerance = 1e-12)
})

test_that("differential entropy by the trapezoid rule", {
  x <- seq(0, 4, length.out = 5)
  d <- Difent(rep(0.25, 5), x)
  expect_equal(d$estimate, log2(4), tolerance = 1e-12)
  expect_equal(d$mass, 1, tolerance = 1e-12)
  g <- seq(-10, 10, length.out = 4001)
  dn <- Difent(dnorm(g), g, base = NULL)
  # a fine grid over +-10 sd: trapezoid error far below 1e-8
  expect_equal(dn$estimate, 0.5 * log(2 * pi * exp(1)), tolerance = 1e-8)
  expect_equal(Difent(c(1, 1, 1))$estimate, 0)
  expect_error(Difent(1), "two grid points")
  expect_error(Difent(c(1, -1)), "non-negative")
})
