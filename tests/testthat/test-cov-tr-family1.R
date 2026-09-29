# Coverage for transfen .. ts_forecast exports. Every expectation is
# recomputed in the test body.

test_that("Transent is the plug-in transfer entropy over lag-L triples", {
  x <- c("a", "b", "a", "a", "b", "b", "a", "b", "a", "a", "b", "a")
  y <- c("u", "u", "v", "u", "v", "v", "u", "v", "u", "u", "v", "u")
  te <- function(src, dst, L) {
    n <- length(dst)
    i <- seq(L, n - 2) + 1
    tr <- data.frame(n1 = dst[i + 1], d0 = dst[i - L + 1], s0 = src[i - L + 1])
    p3 <- table(tr) / nrow(tr)
    p2 <- apply(p3, c(2, 3), sum)
    pd <- apply(p3, c(1, 2), sum)
    p1 <- apply(p3, 2, sum)
    tot <- 0
    for (a in dimnames(p3)[[1]]) for (b in dimnames(p3)[[2]]) for (c in dimnames(p3)[[3]]) {
      p <- p3[a, b, c]
      if (p > 0) tot <- tot + p * log((p / p2[b, c]) / (pd[a, b] / p1[b]))
    }
    unname(tot)
  }
  r <- Transent(x, y)
  expect_equal(r$te_xy, te(x, y, 1), tolerance = 1e-12)
  expect_equal(r$te_yx, te(y, x, 1), tolerance = 1e-12)
  expect_equal(r$bits, r$te_xy / log(2), tolerance = 1e-12)
  r2 <- Transent(x, y, lag = 2)
  expect_equal(r2$te_xy, te(x, y, 2), tolerance = 1e-12)
  expect_true(is.nan(Transent(c("a", "b"), c("u", "v"))$te_xy))
})

test_that("Trcwgt and Trmwgt truncate weights at the type-7 quantile", {
  a <- c(1.2, 0.8, 3.5, 1.0, 0.9, 6.0, 1.1, 0.7)
  c2 <- c(1.0, 1.5, 1.2, 0.9, 2.0, 1.4, 0.8, 1.0)
  r <- Trcwgt(a, c2, quantile = 0.8)
  pr <- a * c2
  cut <- stats::quantile(pr, 0.8, type = 7, names = FALSE)
  expect_equal(r$cut, cut, tolerance = 1e-12)
  expect_equal(r$weights, pmin(pr, cut), tolerance = 1e-12)
  expect_equal(r$n_truncated, sum(pr > cut))
  expect_equal(r$sd, stats::sd(pmin(pr, cut)), tolerance = 1e-12)
  expect_error(Trcwgt(a, -c2), "must be positive")
  expect_error(Trcwgt(a, c2, quantile = 0), "must lie in")
  m <- Trmwgt(a, quantile = 0.75)
  cq <- stats::quantile(a, 0.75, type = 7, names = FALSE)
  tw <- pmin(a, cq)
  expect_equal(m$estimate, cq, tolerance = 1e-12)
  expect_equal(m$rescaled, tw * sum(a) / sum(tw), tolerance = 1e-12)
  expect_equal(m$cv_after, stats::sd(tw) / mean(tw), tolerance = 1e-12)
  expect_error(Trmwgt(-a), "non-negative")
})

test_that("Treedepth, TripNoRep and Trimit", {
  d <- c(3, 5, 10, 7, 10, 2)
  r <- Treedepth(d, max_depth = 10)
  expect_equal(r$saturated, 2 / 6)
  expect_equal(r$total_leapfrog, sum(2^d))
  expect_equal(r$warn, 1)
  expect_error(Treedepth(-1), "non-negative")
  t <- TripNoRep(7)
  expect_equal(t$no_repeat, 7 * 6 * 5)
  expect_equal(t$with_repeat, 7^3 - 7 * 6 * 5)
  expect_error(TripNoRep(-1), "non-negative")
  y <- c(3, 5, 2, 8, 6, 4)
  w <- c(1, 6, 1, 1, 2, 1)
  tm <- Trimit(y, w, threshold = 2.5)
  cur <- w
  repeat {
    over <- sum(pmax(cur - 2.5, 0))
    if (over <= 1e-15) break
    cur <- pmin(cur, 2.5)
    i <- cur < 2.5
    cur[i] <- cur[i] + over * cur[i] / sum(cur[i])
  }
  expect_equal(tm$weights, cur, tolerance = 1e-12)
  expect_equal(sum(tm$weights), sum(w), tolerance = 1e-12)
  expect_equal(tm$estimate, sum(cur * y) / sum(cur), tolerance = 1e-12)
  expect_equal(tm$deff_after, 6 * sum(cur^2) / sum(cur)^2, tolerance = 1e-12)
  expect_error(Trimit(y, w, threshold = 1), "below the mean weight")
})

test_that("Trpostep and Trsub solve trust-region steps", {
  adv <- c(0.5, -0.2, 1.1, 0.3)
  ratio <- c(1.1, 0.9, 1.05, 0.95)
  g <- c(0.4, -0.3)
  F <- rbind(c(2, 0.3), c(0.3, 1))
  r <- Trpostep(NULL, kl_max = 0.02, ratio = ratio, adv = adv, kl = c(0.01, 0.015), g = g, F = F)
  x <- solve(F, g)
  expect_equal(r$surrogate, mean(ratio * adv), tolerance = 1e-12)
  expect_equal(r$step_size, sqrt(0.04 / sum(x * (F %*% x))), tolerance = 1e-9)
  expect_equal(r$step, r$step_size * x, tolerance = 1e-9)
  expect_true(r$feasible)
  B <- rbind(c(1, 0.5, 0), c(0.5, -2, 0.3), c(0, 0.3, 3))
  gg <- c(1, -0.5, 0.25)
  s <- Trsub(gg, B, delta = 0.8)
  lam <- s$lam
  expect_equal(as.numeric((B + lam * diag(3)) %*% s$s), -gg, tolerance = 1e-9)
  expect_equal(sqrt(sum(s$s^2)), 0.8, tolerance = 1e-9)
  expect_gte(min(eigen(B + lam * diag(3), symmetric = TRUE)$values), -1e-9)
  expect_equal(s$estimate, -(sum(gg * s$s) + 0.5 * sum(s$s * (B %*% s$s))), tolerance = 1e-12)
  inner <- Trsub(gg, diag(c(4, 5, 6)), delta = 10)
  expect_equal(inner$s, -gg / c(4, 5, 6), tolerance = 1e-10)
  expect_equal(inner$lam, 0)
  expect_false(inner$boundary)
})

test_that("morie_trupek minimises Rosenbrock with every subproblem", {
  f <- function(x) 100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2
  gr <- function(x) c(-400 * x[1] * (x[2] - x[1]^2) - 2 * (1 - x[1]), 200 * (x[2] - x[1]^2))
  he <- function(x) rbind(c(1200 * x[1]^2 - 400 * x[2] + 2, -400 * x[1]), c(-400 * x[1], 200))
  for (sp in c("steihaug", "cauchy", "dogleg", "exact")) {
    r <- morie_trupek(f, gr, he, c(-1.2, 1), subproblem = sp, max_iter = if (sp == "cauchy") 20000L else 200L)
    if (r$converged) {
      expect_equal(r$x, c(1, 1), tolerance = 1e-8)
      expect_lte(r$gnorm, 1e-10)
    }
    expect_equal(r$fval, f(r$x), tolerance = 1e-12)
    expect_equal(r$accepted + r$rejected, length(r$history) - (r$exit_reason != "iteration limit"))
  }
  st <- morie_trupek(f, gr, he, c(-1.2, 1))
  expect_true(st$converged)
  one <- morie_trupek_trust_region(f, gr, he, c(-1.2, 1), max_iter = 1L)
  expect_equal(one$exit_reason, "iteration limit")
  expect_error(morie_trupek(f, gr, he, c(0, 0), subproblem = "lbfgs"), "expected one of")
})

test_that("morie_ts_accuracy and morie_ts_decompose", {
  fc <- c(10, 12, 11, 13)
  ac <- c(11, 12.5, 10, 14)
  a <- morie_ts_accuracy(fc, ac)
  e <- ac - fc
  expect_equal(unname(a), c(sqrt(mean(e^2)), mean(abs(e)), 100 * mean(abs(e / ac)), mean(e)), tolerance = 1e-12)
  x <- stats::ts(10 + 0.2 * (1:36) + 3 * sin(2 * pi * (1:36) / 12) + cos(1:36) / 3, frequency = 12)
  s <- morie_ts_decompose(x)
  ref <- stats::stl(x, s.window = "periodic")$time.series
  expect_equal(as.numeric(s$seasonal), as.numeric(ref[, "seasonal"]), tolerance = 1e-12)
  cl <- morie_ts_decompose(x, "classical")
  expect_equal(as.numeric(cl$trend), as.numeric(stats::decompose(x)$trend), tolerance = 1e-12)
  expect_error(morie_ts_decompose(stats::ts(1:10)), "frequency >= 2")
})
