# Coverage for sasRec .. scfd exports. Every expectation is recomputed in
# the test body.

test_that("SASRec causal mask, attention span and next-item ranking", {
  expect_equal(causal_mask(4), 1 * lower.tri(diag(4), diag = TRUE))
  expect_error(causal_mask(0), "non-empty")
  W <- rbind(c(1, 0, 0), c(0.3, 0.7, 0), c(0.1, 0.2, 0.7))
  a <- attention_span(W)
  expect_equal(a$mean_lookback, sum((3 - 1:3) * W[3, ]), tolerance = 1e-12)
  expect_equal(a$mass_on_last, 0.7)
  expect_equal(attention_span(W, position = 1)$mean_lookback, 0.3, tolerance = 1e-12)
  expect_error(attention_span(matrix(0, 2, 2)), "no mass")
  s <- c(0.5, -0.2, 1)
  E <- rbind(c(1, 0, 0), c(0, 1, 0), c(0.2, 0.1, 0.9), c(-1, 0, 0.5), c(0.3, 0.3, 0.3))
  sc <- as.numeric(E %*% s)
  p <- predict_next(s, E, top_k = 3, exclude = 2)
  sc2 <- sc
  sc2[3] <- NA
  ord <- order(-sc2, na.last = NA)[1:3]
  expect_equal(unname(p$ranking[, "index"]), ord)
  expect_equal(unname(p$ranking[, "score"]), sc[ord], tolerance = 1e-12)
  expect_equal(p$n_scored, 4L)
})

test_that("SAX words, breakpoints and MINDIST", {
  bp <- morie_sax_breakpoints(4)
  expect_equal(bp, stats::qnorm(c(0.25, 0.5, 0.75)), tolerance = 1e-12)
  expect_error(morie_sax_breakpoints(1), ">= 2")
  x <- c(1, 2, 5, 7, 3, 2, 8, 9, 4, 1, 0, 2)
  r <- morie_saxR(x, window = 4, alphabet = 4)
  z <- (x - mean(x)) / sqrt(mean((x - mean(x))^2))
  paa <- colMeans(matrix(z, 3))
  expect_equal(r$paa, paa, tolerance = 1e-12)
  sym <- vapply(paa, function(v) sum(v >= bp), 0)
  expect_equal(r$symbols, sym)
  expect_equal(r$word, paste(letters[sym + 1], collapse = ""))
  expect_equal(morie_saxR(rep(3, 8), 4, 5)$word, "cccc")
  expect_error(morie_saxR(x, window = 5, alphabet = 4), "divide")
  d <- morie_sax_mindist("adbc", "bcda", n = 12, alphabet = 4)
  cell <- function(r, c) if (abs(r - c) <= 1) 0 else bp[max(r, c) - 1] - bp[min(r, c)]
  s1 <- match(strsplit("adbc", "")[[1]], letters)
  s2 <- match(strsplit("bcda", "")[[1]], letters)
  expect_equal(d, sqrt(12 / 4) * sqrt(sum(mapply(cell, s1, s2)^2)), tolerance = 1e-12)
  expect_error(morie_sax_mindist("ab", "abc", 6, 4), "equal length")
  expect_error(morie_sax_mindist("ae", "ab", 6, 4), "outside alphabet")
})

test_that("Sbcrank builds the SBC rank histogram and chi-square", {
  prior <- c(0.1, -0.5, 1.2, 0.3, -1.0, 0.8)
  post <- rbind(c(0.0, 0.5, -0.2), c(-0.1, 0.3, -0.9), c(1.5, 0.9, 1.1), c(0.4, 0.2, 0.25),
                c(-0.3, -1.2, 0.1), c(0.7, 0.9, 1.0))
  r <- Sbcrank(prior, post)
  rk <- rowSums(post < prior)
  expect_equal(r$rank, rk)
  h <- tabulate(rk + 1, 4)
  expect_equal(r$histogram, h)
  expect_equal(r$statistic, sum((h - 1.5)^2 / 1.5), tolerance = 1e-12)
  expect_equal(r$p_value, stats::pchisq(r$statistic, 3, lower.tail = FALSE), tolerance = 1e-12)
  r2 <- Sbcrank(prior, post, bins = 2)
  expect_equal(r2$histogram, tabulate(pmin(1, rk %/% 2) + 1, 2))
  expect_error(Sbcrank(prior, post, bins = 3), "divide L \\+ 1")
  expect_error(Sbcrank(prior[-1], post), "one row of posterior")
})

test_that("stochastic blockmodel likelihoods", {
  A <- matrix(0, 6, 6)
  ed <- rbind(c(1, 2), c(1, 3), c(2, 3), c(4, 5), c(5, 6), c(4, 6), c(3, 4))
  A[ed] <- 1
  A[ed[, 2:1]] <- 1
  bl <- c("a", "a", "a", "b", "b", "b")
  s <- sbmest(A, bl)
  e_ab <- 1
  expect_equal(s$probabilities, rbind(c(3 / 3, 1 / 9), c(1 / 9, 3 / 3)), tolerance = 1e-12)
  # pairs with p in {0, 1} contribute nothing; only the between-block pairs remain
  expect_equal(s$estimate, e_ab * log(1 / 9) + 8 * log(8 / 9), tolerance = 1e-12)
  expect_same_function(morie_sbmest, sbmest)
  d <- sbmdg2(A, bl)
  m <- rbind(c(6, 1), c(1, 6))
  k <- c(7, 7)
  expect_equal(d$m_rs, m)
  expect_equal(d$kappa, k)
  expect_equal(d$estimate, sum(m * log(m / outer(k, k))), tolerance = 1e-12)
  expect_equal(d$uncorrected, sum(m * log(m / outer(c(3, 3), c(3, 3)))), tolerance = 1e-12)
  expect_equal(d$n_edges, 7)
  expect_same_function(morie_sbmdg2, sbmdg2)
})

test_that("Stickpost is the stick-breaking posterior mean", {
  r <- Stickpost(c(5, 3, 2), alpha = 1.5)
  cn <- c(5, 3, 2)
  tail <- c(5, 2, 0)
  V <- (1 + cn) / (1 + cn + 1.5 + tail)
  expect_equal(r$V, V, tolerance = 1e-12)
  expect_equal(r$pi, V * cumprod(c(1, 1 - V[1:2])), tolerance = 1e-12)
  expect_equal(r$remainder, prod(1 - V), tolerance = 1e-12)
  # a label vector is converted to cluster counts
  expect_equal(Stickpost(c(2, 1, 1, 2, 2, 3), 1)$counts, c(2, 3, 1))
})

test_that("Spbrown projects reliability", {
  r <- Spbrown(0.6, 3, target = 0.9)
  expect_equal(r$estimate, 1.8 / 2.2, tolerance = 1e-12)
  # solving k r / (1 + (k - 1) r) = t for k
  expect_equal(Spbrown(0.6, r$k_needed)$estimate, 0.9, tolerance = 1e-12)
  expect_true(is.nan(Spbrown(0.6, 3)$k_needed))
})

test_that("Scbsft is the covariate-adjusted AIPW-style effect", {
  X <- c(0.2, 1.1, -0.5, 0.8, 1.9, -1.0, 0.3, 0.6, 1.4, -0.2)
  bl <- c(1, 2, 0.5, 1.5, 3, 0, 1, 1.2, 2.5, 0.8)
  D <- c(1, 0, 1, 1, 0, 0, 1, 0, 1, 0)
  y <- 1 + D + 0.5 * X + 0.3 * bl + c(0.1, -0.2, 0.05, 0.15, -0.1, 0.2, -0.05, 0, 0.1, -0.1)
  r <- Scbsft(y, D, X, bl)
  f1 <- stats::lm(y ~ X + bl, subset = D == 1)
  f0 <- stats::lm(y ~ X + bl, subset = D == 0)
  m1 <- stats::predict(f1, data.frame(X = X, bl = bl))
  m0 <- stats::predict(f0, data.frame(X = X, bl = bl))
  psi <- mean(m1 - m0)
  ic <- D * (y - m1) / 0.5 - (1 - D) * (y - m0) / 0.5 + m1 - m0 - psi
  expect_equal(r$estimate, psi, tolerance = 1e-10)
  expect_equal(r$se, stats::sd(ic) / sqrt(10), tolerance = 1e-10)
  expect_equal(r$shift, mean(bl[D == 1]) - mean(bl[D == 0]), tolerance = 1e-12)
})

test_that("Scfd is scalar-on-function regression by basis expansion", {
  tg <- seq(0, 1, length.out = 6)
  X <- rbind(sin(tg), cos(tg), tg^2, 1 - tg, sin(2 * tg), exp(-tg), tg * (1 - tg))
  B <- cbind(1, tg)
  y <- c(0.5, 1.2, 0.3, 0.9, 0.7, 1.0, 0.2)
  r <- Scfd(X, y, B)
  trap <- function(v) sum(diff(tg) * (v[-1] + v[-6]) / 2)
  J <- t(apply(X, 1, function(x) c(trap(B[, 1] * x), trap(B[, 2] * x))))
  f <- stats::lm(y ~ J)
  expect_equal(r$J, J, tolerance = 1e-12)
  expect_equal(c(r$alpha, r$coef), unname(stats::coef(f)), tolerance = 1e-9)
  expect_equal(r$beta, as.numeric(B %*% r$coef), tolerance = 1e-12)
  expect_equal(r$r2, summary(f)$r.squared, tolerance = 1e-9)
  expect_equal(r$df, 4L)
  expect_error(Scfd(X, y[-1], B), "one value per curve")
  expect_error(Scfd(X, y, B[-1, ]), "one row per argument")
  expect_error(Scfd(X[1:2, ], y[1:2], B), "more curves than basis")
  expect_error(Scfd(X, y, B, t = 1:3), "t must match")
})
