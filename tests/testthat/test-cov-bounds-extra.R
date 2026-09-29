# Coverage for the remaining bound files (bns*.R): each bound is rebuilt
# from the worst-case arm formula m p + y (1 - p) or the Imbens-Manski
# interval it wraps.

bx_y <- c(3, 7, 2, 9, 5, 4, 8, 6, 1, 10)
bx_d <- c(1, 0, 1, 1, 0, 1, 0, 1, 0, 1)
bx_g <- c("a", "a", "b", "b", "a", "b", "a", "b", "a", "b")
arm <- function(y, d, lo, hi) {
  p <- mean(d)
  m <- if (any(d == 1)) mean(y[d == 1]) else 0
  c(m * p + lo * (1 - p), m * p + hi * (1 - p))
}

test_that("Bnscbo bounds the ATE of a weighted compound outcome", {
  M <- cbind(bx_y, rev(bx_y))
  r <- Bnscbo(M, bx_d, c(0.5, 2))
  comp <- as.numeric(M %*% c(0.5, 2))
  a1 <- arm(comp, bx_d, min(comp), max(comp))
  a0 <- arm(comp, 1 - bx_d, min(comp), max(comp))
  expect_equal(c(r$lower, r$upper), c(a1[1] - a0[2], a1[2] - a0[1]), tolerance = 1e-12)
  expect_error(Bnscbo(matrix(numeric(0), 0, 2), numeric(0), 1:2), "empty")
  expect_error(Bnscbo(M, bx_d, 1), "one weight per component")
})

test_that("Bnscom bounds the ATE from the complier LATE", {
  z <- c(1, 1, 1, 0, 0, 1, 0, 1, 0, 0)
  d <- c(1, 1, 0, 0, 1, 1, 0, 1, 0, 0)
  r <- Bnscom(bx_y, d, z)
  pc <- mean(d[z == 1]) - mean(d[z == 0])
  e1 <- (mean((bx_y * d)[z == 1]) - mean((bx_y * d)[z == 0])) / pc
  e0 <- (mean((bx_y * (1 - d))[z == 0]) - mean((bx_y * (1 - d))[z == 1])) / pc
  late <- e1 - e0
  expect_equal(r$late, late, tolerance = 1e-12)
  expect_equal(r$late, (mean(bx_y[z == 1]) - mean(bx_y[z == 0])) / pc, tolerance = 1e-12)
  expect_equal(c(r$lower, r$upper), pc * late + (1 - pc) * c(-9, 9), tolerance = 1e-12)
  expect_error(Bnscom(numeric(0), numeric(0), numeric(0)), "empty")
  expect_error(Bnscom(bx_y, d[-1], z), "same length")
  expect_error(Bnscom(bx_y, d + 1, z), "0/1")
  expect_error(Bnscom(bx_y, d, rep(1, 10)), "only one value")
  expect_error(Bnscom(bx_y, d, 1 - z), "monotonicity")
})

test_that("Bnscnf, Bnscrf and Bnstst wrap the Imbens-Manski machinery", {
  lo <- c(1.2, 0.8, 1.1, 1.4, 0.9, 1)
  hi <- c(2.1, 2.5, 1.9, 2.2, 2.6, 2.3)
  a <- Bnscnf(seq(0, 3, by = 0.5), cbind(lo, hi), alpha = 0.1)
  b <- Bndinf(seq(0, 3, by = 0.5), cbind(lo, hi), alpha = 0.1)
  expect_equal(a$lower, mean(lo) - qnorm(0.9) * sd(lo) / sqrt(6), tolerance = 1e-12)
  expect_equal(a[c("lower", "upper", "grid_lower", "grid_upper")],
               b[c("lower", "upper", "grid_lower", "grid_upper")])
  cr <- Bnscrf(lo, hi, alpha = 0.1)
  sh <- sqrt(6) * (mean(hi) - mean(lo)) / max(sd(lo), sd(hi))
  cv <- if (pnorm(qnorm(0.9) + sh) - 0.1 >= 0.9) qnorm(0.9) else
    stats::uniroot(function(c) pnorm(c + sh) - pnorm(-c) - 0.9, c(qnorm(0.9), qnorm(0.95)),
                   tol = 1e-14)$root
  expect_equal(cr$upper, mean(hi) + cv * sd(hi) / sqrt(6), tolerance = 1e-9)
  st <- Bnstst(lo, hi, se = 1.5, cdf = 0.1)
  expect_equal(c(st$lower, st$upper), c(cr$lower, cr$upper), tolerance = 1e-12)
  expect_equal(c(st$covers, st$reject), c(1, 0))
  expect_equal(Bnstst(lo, hi, se = 10)$reject, 1)
})

test_that("Bnsiii evaluates the moment-inequality criteria on a grid", {
  yl <- c(1.2, 0.8, 1.1, 1.4, 0.9, 1)
  yu <- c(2.1, 2.5, 1.9, 2.2, 2.6, 2.3)
  g <- c(0, 1, 1.5, 3)
  r <- Bnsiii(yl, yu, g)
  a <- sqrt(6) * (mean(yl) - g) / sd(yl)
  b <- sqrt(6) * (g - mean(yu)) / sd(yu)
  q <- pmax(a, 0)^2 + pmax(b, 0)^2
  expect_equal(c(r$lower, r$upper), c(mean(yl), mean(yu)), tolerance = 1e-12)
  expect_equal(r$q_min, min(q), tolerance = 1e-12)
  expect_equal(r$n_in_set, sum(q <= 0))
  expect_equal(r$q_max_stat, max(pmax(a, b, 0)), tolerance = 1e-12)
  expect_error(Bnsiii(1:3, 1:2, 1), "same length")
  expect_error(Bnsiii(yl, yu, numeric(0)), "empty")
})

test_that("Bnssel and Bnstvr stratify the worst-case bounds", {
  r <- Bnssel(bx_y, bx_d, bx_g)
  obs <- bx_y[bx_d == 1]
  wa <- arm(bx_y[bx_g == "a"], bx_d[bx_g == "a"], min(obs), max(obs))
  wb <- arm(bx_y[bx_g == "b"], bx_d[bx_g == "b"], min(obs), max(obs))
  expect_equal(c(r$lower, r$upper), 0.5 * wa + 0.5 * wb, tolerance = 1e-12)
  expect_error(Bnssel(bx_y, bx_d, 1:3), "one value per unit")
  expect_error(Bnssel(bx_y, rep(0, 10), bx_g), "no observed")
  tv <- Bnstvr(bx_y, bx_d, bx_g)
  a1 <- rbind(arm(bx_y[bx_g == "a"], bx_d[bx_g == "a"], 1, 10),
              arm(bx_y[bx_g == "b"], bx_d[bx_g == "b"], 1, 10))
  a0 <- rbind(arm(bx_y[bx_g == "a"], 1 - bx_d[bx_g == "a"], 1, 10),
              arm(bx_y[bx_g == "b"], 1 - bx_d[bx_g == "b"], 1, 10))
  lo1 <- max(a1[, 1])
  hi1 <- min(a1[, 2])
  lo0 <- max(a0[, 1])
  hi0 <- min(a0[, 2])
  expect_equal(c(tv$lower, tv$upper), c(lo1 - hi0, hi1 - lo0), tolerance = 1e-12)
  expect_equal(tv$refuted, as.numeric(lo1 > hi1 || lo0 > hi0))
  expect_error(Bnstvr(bx_y, bx_d, 1:3), "one value per unit")
})
