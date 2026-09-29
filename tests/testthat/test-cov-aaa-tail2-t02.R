# Coverage for the tail-2 batch: Bray-Curtis, Cronbach's alpha, Dixon and
# Grubbs outlier tests (against the outliers package), Csiszar
# f-divergences, the mean-excess plot, the Woolf odds-ratio interval and
# the Rousseeuw-Croux Qn / Sn scales (against robustbase where present).

test_that("Bray-Curtis dissimilarity, closed and raw", {
  x <- c(10, 0, 5, 3)
  y <- c(4, 2, 5, 9)
  r <- BrayCurt(x, y, close = FALSE)
  expect_equal(r$bc, sum(abs(x - y)) / sum(x + y), tolerance = 1e-12)
  xc <- x / sum(x)
  yc <- y / sum(y)
  rc <- BrayCurt(x, y)
  expect_equal(rc$bc, sum(abs(xc - yc)) / 2, tolerance = 1e-12)
  expect_equal(rc$similarity, 1 - rc$bc, tolerance = 1e-12)
  expect_error(BrayCurt(c(1, -1), c(1, 1)), "non-negative")
  expect_error(BrayCurt(0, 0, close = FALSE), "undefined")
})

test_that("Cronbach's alpha and alpha-if-item-deleted", {
  X <- rbind(c(3, 4, 3, 5), c(2, 2, 3, 2), c(4, 5, 4, 4), c(1, 2, 1, 3), c(5, 4, 5, 5), c(3, 3, 2, 4))
  a <- CttAlpha(X)
  k <- 4
  expect_equal(a$alpha, k / (k - 1) * (1 - sum(apply(X, 2, stats::var)) / stats::var(rowSums(X))), tolerance = 1e-12)
  am <- CttAlphaMax(X)
  drop <- vapply(1:4, function(j) {
    Y <- X[, -j]
    3 / 2 * (1 - sum(apply(Y, 2, stats::var)) / stats::var(rowSums(Y)))
  }, 1)
  expect_equal(am$alpha_dropped, drop, tolerance = 1e-12)
  expect_identical(am$argmax_alpha, which.max(drop) - 1L)
  expect_equal(am$delta, drop - a$alpha, tolerance = 1e-12)
  expect_error(CttAlpha(X[, 1, drop = FALSE]), "at least two items")
  expect_error(CttAlphaMax(X[, 1:2]), "at least three items")
})

test_that("Dixon and Grubbs statistics agree with the outliers package", {
  skip_if_not_installed("outliers")
  x <- c(10.2, 9.8, 10.1, 10.4, 9.9, 12.9, 10.0, 10.3)
  for (ty in c(10, 11, 21)) {
    d <- DixonQ(x, type = ty)
    ref <- outliers::dixon.test(x, type = ty)
    expect_equal(d$statistic, unname(ref$statistic[1]), tolerance = 1e-12, info = ty)
  }
  expect_identical(DixonQ(x)$side, "max")
  expect_identical(DixonQ(x, opposite = TRUE)$side, "min")
  expect_error(DixonQ(x, type = 13), "type must be one of")
  g <- GrubbsT(x)
  gr <- outliers::grubbs.test(x, type = 10)
  expect_equal(g$statistic, unname(gr$statistic[1]), tolerance = 1e-12)
  s2 <- g$statistic^2 * 8 * (2 - 8) / (g$statistic^2 * 8 - 49)
  expect_equal(g$p_value, 8 * stats::pt(sqrt(s2), 6, lower.tail = FALSE), tolerance = 1e-12)
  # outliers forms 1 - pt(), which loses ~1e-12 relative in the tail
  expect_equal(g$p_value, gr$p.value, tolerance = 1e-9)
  expect_identical(g$index, 5L)
  tq <- stats::qt(0.05 / 8, 6)
  expect_equal(g$critical_value, 7 / sqrt(8) * sqrt(tq^2 / (6 + tq^2)), tolerance = 1e-12)
  expect_error(GrubbsT(c(1, 1, 1)), "constant")
})

test_that("Csiszar f-divergences reduce to their named forms", {
  p <- c(0.1, 0.4, 0.3, 0.2)
  q <- c(0.25, 0.25, 0.3, 0.2)
  expect_equal(FDiverg(p, q)$divergence, sum(p * log(p / q)), tolerance = 1e-12)
  expect_equal(FDiverg(p, q, "rkl")$divergence, sum(q * log(q / p)), tolerance = 1e-12)
  expect_equal(FDiverg(p, q, "tv")$divergence, 0.5 * sum(abs(p - q)), tolerance = 1e-12)
  expect_equal(FDiverg(p, q, "chi2")$divergence, sum((p - q)^2 / q), tolerance = 1e-12)
  expect_equal(FDiverg(p, q, "hellinger")$divergence, sum((sqrt(p) - sqrt(q))^2), tolerance = 1e-12)
  m <- (p + q) / 2
  expect_equal(FDiverg(p, q, "js")$divergence, sum(p * log(p / m)) + sum(q * log(q / m)), tolerance = 1e-12)
  expect_equal(FDiverg(2 * p, 3 * q, "tv")$divergence, 0.5 * sum(abs(p - q)), tolerance = 1e-12)
  expect_identical(FDiverg(c(0.5, 0.5), c(1, 0))$divergence, Inf)
  expect_equal(FDiverg(c(0.5, 0.5), c(1, 0), "tv")$divergence, 0.5, tolerance = 1e-12)
  expect_equal(FDiverg(p, q, function(t) (t - 1)^2)$divergence, sum((p - q)^2 / q), tolerance = 1e-12)
  expect_error(FDiverg(p, q, function(t) t), "f\\(1\\) = 0")
  expect_error(FDiverg(p, q, "renyi"), "unknown generator")
})

test_that("the mean-excess function and its normal interval", {
  x <- c(1.2, 3.5, 0.7, 5.1, 2.2, 8.4, 4.3, 2.9)
  me <- MeanExc(x, u_grid = c(1, 3, 5, 9))
  for (i in 1:3) {
    ex <- x[x > c(1, 3, 5)[i]] - c(1, 3, 5)[i]
    expect_equal(me$e[i], mean(ex), tolerance = 1e-12)
    expect_equal(me$se[i], stats::sd(ex) / sqrt(length(ex)), tolerance = 1e-12)
    expect_equal(me$ci_lower[i], mean(ex) - stats::qnorm(0.975) * stats::sd(ex) / sqrt(length(ex)), tolerance = 1e-12)
  }
  expect_true(is.nan(me$e[4]))
  expect_identical(me$n_exceed, c(7L, 4L, 2L, 0L))
  expect_equal(MeanExc(x)$u, sort(x)[-8])
  expect_error(MeanExc(c(2, 2, 2)), "constant")
})

test_that("Woolf odds-ratio interval and its zero-cell guard", {
  r <- OddsRat(12, 5, 7, 20)
  lor <- log(12 * 20 / (5 * 7))
  se <- sqrt(1 / 12 + 1 / 5 + 1 / 7 + 1 / 20)
  expect_equal(r$estimate, 240 / 35, tolerance = 1e-12)
  expect_equal(c(r$ci_lower, r$ci_upper), exp(lor + c(-1, 1) * stats::qnorm(0.975) * se), tolerance = 1e-12)
  expect_equal(r$p_value, 2 * stats::pnorm(-abs(lor / se)), tolerance = 1e-12)
  h <- OddsRat(12, 0, 7, 20, correction = 0.5)
  expect_equal(h$estimate, 12.5 * 20.5 / (0.5 * 7.5), tolerance = 1e-12)
  z <- OddsRat(12, 0, 7, 20)
  expect_identical(z$estimate, Inf)
  expect_true(is.nan(z$ci_lower))
  expect_error(OddsRat(1, 2, 3, 4, conf_level = 1), "strictly between")
})

test_that("Qn and Sn follow Rousseeuw & Croux (1993)", {
  x <- c(2.1, 3.7, 1.9, 5.5, 2.8, 4.1, 3.3, 9.9, 2.6, 3.0, 4.4)
  n <- length(x)
  q <- QnScale(x, finite_corr = FALSE)
  h <- n %/% 2 + 1
  dd <- sort(as.numeric(stats::dist(x)))
  expect_equal(q$raw, dd[choose(h, 2)], tolerance = 1e-12)
  expect_equal(q$estimate, 2.21914 * q$raw, tolerance = 1e-12)
  s <- SnScale(x, finite_corr = FALSE)
  inner <- vapply(x, function(v) sort(abs(v - x))[n %/% 2 + 1], 1)
  expect_equal(s$raw, sort(inner)[(n + 1) %/% 2], tolerance = 1e-12)
  expect_equal(s$estimate, 1.1926 * s$raw, tolerance = 1e-12)
  expect_error(QnScale(1), "at least two")
  skip_if_not_installed("robustbase")
  # robustbase applies its finite-sample factors only with the default
  # constant, so the defaults are compared; its C Qn0 returns the order
  # statistic in single precision (0.7 comes back as 0.69999998808), so
  # Qn agrees to float32 accuracy only
  expect_equal(QnScale(x)$estimate, robustbase::Qn(x), tolerance = 1e-7)
  expect_equal(SnScale(x)$estimate, robustbase::Sn(x), tolerance = 1e-10)
  x2 <- c(x, 7.2, 1.1, 5.9)
  expect_equal(QnScale(x2)$estimate, robustbase::Qn(x2), tolerance = 1e-7)
  expect_equal(SnScale(x2)$estimate, robustbase::Sn(x2), tolerance = 1e-10)
  expect_equal(QnScale(x, finite_corr = FALSE)$estimate, robustbase::Qn(x, constant = 2.21914), tolerance = 1e-7)
})
