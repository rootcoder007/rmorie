# Coverage for wquan .. yarn_native exports. Every expectation is
# recomputed in the test body.

test_that("wquan is the weighted Harrell-Davis quantile", {
  y <- c(3.1, 1.2, 5.4, 2.2, 4.0, 2.9)
  w <- c(1, 2, 1, 1.5, 0.5, 1)
  r <- wquan(y, w, p = 0.3)
  o <- order(y)
  k <- c(0, cumsum(w[o]) / sum(w))
  k[7] <- 1
  wt <- diff(stats::pbeta(k, 0.3 * 7, 0.7 * 7))
  expect_equal(r$hd, sum(wt * y[o]), tolerance = 1e-12)
  expect_equal(r$ecdf, y[o][which(cumsum(w[o]) / sum(w) >= 0.3)[1]])
  expect_equal(sum(r$w), 1, tolerance = 1e-12)
  u <- morie_weighted_quantile(y, p = 0.5)
  ku <- (0:6) / 6
  expect_equal(u$hd, sum(diff(stats::pbeta(ku, 3.5, 3.5)) * sort(y)), tolerance = 1e-12)
})

test_that("Wsdt2d finds the optimal Wasserstein assignment", {
  A <- rbind(c(0, 0), c(1, 0), c(0, 2), c(3, 1))
  B <- rbind(c(2.9, 1.2), c(0.1, 1.8), c(1.1, 0.2), c(0.2, -0.1))
  r <- Wsdt2d(A, B, p = 2)
  C <- outer(1:4, 1:4, Vectorize(function(i, j) sum((A[i, ] - B[j, ])^2)))
  perms <- as.matrix(expand.grid(1:4, 1:4, 1:4, 1:4))
  perms <- perms[apply(perms, 1, function(v) length(unique(v)) == 4), ]
  costs <- apply(perms, 1, function(pm) sum(C[cbind(1:4, pm)]))
  expect_equal(r$wpp, min(costs) / 4, tolerance = 1e-12)
  expect_equal(r$assignment + 1, unname(perms[which.min(costs), ]))
  expect_equal(r$estimate, sqrt(min(costs) / 4), tolerance = 1e-12)
  r1 <- Wsdt2d(A, B, p = 1)
  C1 <- sqrt(C)
  expect_equal(r1$wpp, min(apply(perms, 1, function(pm) sum(C1[cbind(1:4, pm)]))) / 4, tolerance = 1e-12)
})

test_that("Wasserman's classical tests match stats", {
  x <- c(0.2, 1.4, -0.3, 2.2, 0.9, 1.1, -0.8, 0.5)
  y <- c(1.9, 2.4, 0.8, 3.1, 1.6, 2.7, 1.2)
  k <- Kstest1(x, y)
  kt <- suppressWarnings(stats::ks.test(x, y, exact = FALSE))
  expect_equal(k$statistic, unname(kt$statistic), tolerance = 1e-12)
  expect_equal(k$p_value, kt$p.value, tolerance = 1e-10)
  expect_error(Kstest1(numeric(0), y), "non-empty")
  l <- Lrtest(-10.2, -14.9, 2)
  expect_equal(l$p_value, stats::pchisq(9.4, 2, lower.tail = FALSE), tolerance = 1e-12)
  expect_error(Lrtest(-15, -10, 1), "not nested")
  expect_error(Lrtest(-1, -2, 0), "df must be at least 1")
  p <- Pearsonr(x, y[c(1:7, 1)], level = 0.9)
  ct <- stats::cor.test(x, y[c(1:7, 1)], conf.level = 0.9)
  expect_equal(p$estimate, unname(ct$estimate), tolerance = 1e-12)
  expect_equal(p$p_value, ct$p.value, tolerance = 1e-12)
  expect_equal(c(p$ci_lower, p$ci_upper), as.numeric(ct$conf.int), tolerance = 1e-12)
  expect_error(Pearsonr(1:2, 1:2), "at least 3")
  s <- Scoretest(14, 40, 0.5)
  pt <- stats::prop.test(14, 40, 0.5, correct = FALSE)
  expect_equal(s$chisq, unname(pt$statistic), tolerance = 1e-12)
  expect_equal(s$p_value, pt$p.value, tolerance = 1e-12)
  expect_error(Scoretest(41, 40), "0..n")
  sg <- Sgntest(c(x, 0.5), md = 0.5)
  expect_equal(sg$n_ties, 2)
  expect_equal(sg$p_value, stats::binom.test(sum(c(x, 0.5) > 0.5), 7)$p.value, tolerance = 1e-12)
  expect_error(Sgntest(c(1, 1), md = 1), "vacuous")
  st <- Suffstat(x)
  expect_equal(c(st$T1, st$T2, st$mle_sigma2), c(mean(x), stats::sd(x), mean((x - mean(x))^2)), tolerance = 1e-12)
  expect_equal(Suffstat(c(1, 0, 1, 1), "bernoulli")$mle_mu, 0.75)
  expect_error(Suffstat(c(1, 2), "bernoulli"), "must be 0/1")
  expect_error(Suffstat(x, "poisson"), "family must be")
  wd <- Waldstat(1.3, 0.5, theta0 = 0.2, level = 0.9)
  expect_equal(wd$statistic, 2.2, tolerance = 1e-12)
  expect_equal(wd$p_value, 2 * stats::pnorm(-2.2), tolerance = 1e-12)
  expect_equal(wd$reject, 1)
  expect_error(Waldstat(1, 0), "must be positive")
  rs <- Ranksum(c(x, 1.1), y)
  wt <- suppressWarnings(stats::wilcox.test(c(x, 1.1), y, exact = FALSE, correct = TRUE))
  expect_equal(rs$U, unname(wt$statistic))
  expect_equal(rs$p_value, wt$p.value, tolerance = 1e-12)
  expect_error(Ranksum(c(1, 1), c(1, 1)), "every value is tied")
})

test_that("Wvar and Wvltdb", {
  y <- c(2.1, 3.4, 1.8, 4.0, 2.9)
  w <- c(1, 2, 1.5, 0.5, 1)
  r <- Wvar(y, w)
  mu <- sum(w * y) / 6
  s2 <- sum(w * (y - mu)^2) / 5
  expect_equal(r$estimate, s2, tolerance = 1e-12)
  expect_equal(r$se, sqrt(s2 * sum(w^2) / 36), tolerance = 1e-12)
  expect_equal(Wvar(y)$estimate, stats::var(y), tolerance = 1e-12)
  expect_error(Wvar(y, -w), "non-negative")
  x <- c(1.0, 3.0, 2.0, 5.0, 4.0, 0.5, 2.5, 1.5)
  d <- Wvltdb(x, level = 1)
  h <- c(1 + sqrt(3), 3 + sqrt(3), 3 - sqrt(3), 1 - sqrt(3)) / (4 * sqrt(2))
  a1 <- vapply(0:3, function(i) sum(h * x[(2 * i + 0:3) %% 8 + 1]), 0)
  expect_equal(d$approximation, a1, tolerance = 1e-12)
  full <- Wvltdb(x, level = 2)
  expect_lt(full$reconstruction_error, 1e-12)
  expect_equal(full$approximation_energy + sum(full$energies), sum(x^2), tolerance = 1e-12)
  hr <- Wvltdb(x, wavelet = "haar")
  expect_equal(hr$approximation, sum(x) / sqrt(8), tolerance = 1e-12)
  expect_error(Wvltdb(x[1:6]), "power of two")
  expect_error(Wvltdb(x, level = 4), "level must lie")
  expect_error(Wvltdb(x, wavelet = "db3"), "filter is longer")
})

test_that("morie_xpehh1 integrates site EHH on both sides of the core", {
  hA <- rbind(c(0, 1, 1, 0, 1), c(0, 1, 1, 0, 1), c(1, 1, 1, 0, 1), c(0, 1, 0, 1, 1), c(1, 1, 1, 0, 0), c(0, 1, 1, 0, 1))
  hB <- rbind(c(0, 1, 0, 1, 1), c(1, 0, 1, 0, 0), c(0, 1, 1, 1, 0), c(1, 1, 0, 0, 1), c(0, 0, 1, 1, 1), c(1, 1, 1, 0, 0))
  pos <- c(0, 1.5, 3, 4, 6)
  ehh <- function(H, j) {
    lo <- min(j, 3)
    hi <- max(j, 3)
    keys <- apply(H[, lo:hi, drop = FALSE], 1, paste, collapse = "")
    tb <- table(keys)
    sum(choose(tb, 2)) / choose(nrow(H), 2)
  }
  ihh <- function(H) {
    e <- vapply(1:5, function(j) ehh(H, j), 0)
    side <- function(idx) {
      a <- 0
      pp <- pos[3]
      pe <- e[3]
      for (j in idx) {
        a <- a + abs(pos[j] - pp) * (e[j] + pe) / 2
        pp <- pos[j]
        pe <- e[j]
        if (e[j] < 0.05) break
      }
      a
    }
    side(2:1) + side(4:5)
  }
  r <- morie_xpehh1(hA, hB, core = 2, positions = pos)
  expect_equal(r$I_A, ihh(hA), tolerance = 1e-12)
  expect_equal(r$I_B, ihh(hB), tolerance = 1e-12)
  expect_equal(r$xpehh_unstandardized, log(ihh(hA) / ihh(hB)), tolerance = 1e-12)
  s <- morie_xpehh1(hA, hB, core = 2, positions = pos, standardize = c(0.1, 0.5))
  expect_equal(s$estimate, (r$xpehh_unstandardized - 0.1) / 0.5, tolerance = 1e-12)
  expect_equal(morie_xpehh1(hB, hA, 2, pos)$estimate, -r$estimate, tolerance = 1e-12)
  expect_error(morie_xpehh1(hA, hB, 2, pos, standardize = c(0, 0)), "sd must be positive")
})

test_that("morie_yangr is the Yang realised relationship matrix", {
  M <- rbind(c(0, 1, 2, 1), c(2, 1, 0, 1), c(1, 1, 1, 2), c(0, 2, 2, 0))
  r <- morie_yangr(M)
  p <- colMeans(M) / 2
  v <- 2 * p * (1 - p)
  Z <- sweep(M, 2, 2 * p)
  A <- (Z %*% diag(1 / v) %*% t(Z)) / 4
  expect_equal(r$A, A, tolerance = 1e-12)
  y <- morie_yangr(M, yang_diagonal = TRUE)
  dg <- 1 + rowSums(sweep(sweep(M^2 - sweep(M, 2, 1 + 2 * p, "*"), 2, 2 * p^2, "+"), 2, v, "/")) / 4
  expect_equal(diag(y$A), dg, tolerance = 1e-12)
  expect_equal(y$A[1, 2], A[1, 2], tolerance = 1e-12)
})

test_that("Yarn ramps RoPE frequencies between interpolation and extrapolation", {
  r <- Yarn(10000, s = 4, d = 8, L = 2048, beta_fast = 32, beta_slow = 1)
  th <- 10000^(-2 * (0:3) / 8)
  rot <- 2048 * th / (2 * pi)
  g <- pmin(pmax((rot - 1) / 31, 0), 1)
  expect_equal(r$theta, th, tolerance = 1e-12)
  expect_equal(r$gamma, g, tolerance = 1e-12)
  expect_equal(r$theta_new, (1 - g) * th / 4 + g * th, tolerance = 1e-12)
  expect_equal(r$temperature, 1 / (0.1 * log(4) + 1)^2, tolerance = 1e-12)
  f <- Yarn(c(1, 0.5, 0.1, 0.01), s = 2, d = 8, L = 100)
  expect_equal(f$theta, c(1, 0.5, 0.1, 0.01))
  expect_error(Yarn(10000, 4, 7, 2048), "positive even width")
  expect_error(Yarn(10000, 4, 8, 2048, beta_fast = 1, beta_slow = 2), "beta_slow < beta_fast")
  expect_error(Yarn(1, 4, 8, 2048), "must exceed 1")
  expect_error(Yarn(c(1, 2), 4, 8, 2048), "d/2 = 4")
})
