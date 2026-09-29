# Coverage for chasym.R, chipsq_native.R, chnntp.R, chrbnd.R, chrnff.R,
# chwfst.R, chzlt.R, clbuvc.R, clcrp.R, clipsi.R and clipxi.R: formulas
# recomputed in base R, Blahut-Arimoto against the Z-channel capacity,
# stochastic routines replayed on their documented streams.

test_that("Chasym compares the influence-function and bootstrap variances", {
  y <- c(2.1, 3.4, 1.9, 5.6, 2.8, 4.1, 3.3, 2.5)
  w <- c(1, 2, 1, 0.5, 1.5, 1, 2, 1)
  r <- Chasym(y, H = w, B = 6, seed = 5)
  th <- sum(w * y) / sum(w)
  vif <- sum((w * (y - th))^2) / sum(w)^2
  st <- 5
  u <- function() {
    st <<- (st * 48271) %% 2147483647
    (st - 1) / 2147483646
  }
  reps <- vapply(1:6, function(b) {
    k <- vapply(1:8, function(i) min(floor(u() * 8) + 1, 8), 0)
    sum(w[k] * y[k]) / sum(w[k])
  }, 0)
  expect_equal(r$theta, th, tolerance = 1e-12)
  expect_equal(r$var_if, vif, tolerance = 1e-12)
  expect_equal(r$var_boot, var(reps), tolerance = 1e-12)
  expect_equal(r$ess, sum(w)^2 / sum(w^2), tolerance = 1e-12)
  expect_error(Chasym(1), "two observations")
  expect_error(Chasym(y, H = w[-1]), "same length")
  expect_error(Chasym(y, H = -w), "non-negative")
  expect_error(Chasym(y, A = 1:3), "same length")
  expect_error(Chasym(y, H = w * 0), "sum to zero")
  expect_error(Chasym(y, B = 1), "at least 2")
})

test_that("Chipsq is the MACS local-lambda Poisson tail", {
  r <- Chipsq(c(12, 3, 0), width = 200, lambda_bg = 2, count_1k = c(20, 5, 1),
              count_10k = c(60, 40, 30))
  lam <- pmax(2, c(20, 5, 1) * 200 / 1000, c(60, 40, 30) * 200 / 10000)
  expect_equal(r$lambda_local, lam, tolerance = 1e-12)
  expect_equal(r$pvalue[1:2], ppois(c(11, 2), lam[1:2], lower.tail = FALSE), tolerance = 1e-12)
  expect_equal(r$pvalue[3], 1)
  expect_equal(r$fold_enrichment, c(12, 3, 0) / lam, tolerance = 1e-12)
  expect_equal(Chipsq(5, 100, 2, count_1k = 50, use_1k = FALSE)$lambda_local, 2)
  expect_error(Chipsq(c(1, 2), c(1, 2, 3), 1), "match count length")
})

test_that("Chancap runs Blahut-Arimoto to the channel capacity", {
  P <- rbind(c(1, 0), c(0.5, 0.5))
  r <- Chancap(P, iters = 400)
  # Z channel with crossover 1/2: C = log2(1 + (1 - p) p^(p / (1 - p)))
  expect_equal(r$capacity_bits, log2(1.25), tolerance = 1e-9)
  bsc <- Chancap(rbind(c(0.9, 0.1), c(0.1, 0.9)), iters = 5)
  expect_equal(bsc$capacity_bits, 1 + 0.9 * log2(0.9) + 0.1 * log2(0.1), tolerance = 1e-12)
  expect_equal(bsc$input_dist, c(0.5, 0.5), tolerance = 1e-12)
  expect_error(Chancap(-P), "non-negative")
  expect_error(Chancap(P * 2), "sum to 1")
})

test_that("Chrbnd is the CLR intersection upper bound", {
  y <- c(3.1, 2.8, 3.5, 2.9, 5.2, 4.8, 5.5, 5.1, 3.0, 3.3, 2.7, 3.2)
  cell <- rep(c("a", "b", "c"), each = 4)
  r <- Chrbnd(y, instrument = cell, alpha = 0.1, gamma = 0.8)
  m <- tapply(y, cell, mean)[c("a", "b", "c")]
  s <- tapply(y, cell, sd)[c("a", "b", "c")] / 2
  kg <- qnorm(0.8^(1 / 3))
  con <- which(m <= min(m + kg * s) + 2 * kg * s)
  ka <- qnorm(0.9^(1 / length(con)))
  expect_equal(r$bound, unname(min((m + ka * s)[con])), tolerance = 1e-12)
  expect_equal(r$contact_set, unname(con) - 1L)
  expect_equal(r$naive_min, unname(min(m)), tolerance = 1e-12)
  expect_error(Chrbnd(numeric(0)), "empty")
  expect_error(Chrbnd(y, alpha = 1), "strictly")
  expect_error(Chrbnd(y, instrument = cell[-1]), "same length")
  expect_error(Chrbnd(y, instrument = c(cell[-12], "z")), "two observations")
  expect_error(Chrbnd(y, gamma = 1), "strictly")
})

test_that("Chernbnd minimises exp(-s a) M(s) over the grid", {
  mgf <- function(s) exp(s^2 / 2)
  g <- seq(0.1, 4, by = 0.1)
  r <- Chernbnd(mgf, a = 2, s_grid = g)
  v <- exp(-g * 2 + g^2 / 2)
  expect_equal(r$bound, min(v), tolerance = 1e-12)
  expect_equal(r$s, g[which.min(v)])
  expect_false(r$at_boundary)
  expect_equal(Chernbnd(mgf, 2)$s, 0.01 * 1.05^(which.min(exp(-0.01 * 1.05^(0:140) * 2 +
    (0.01 * 1.05^(0:140))^2 / 2)) - 1), tolerance = 1e-12)
  expect_error(Chernbnd(mgf, 1, s_grid = c(0, 1)), "positive")
  expect_error(Chernbnd(function(s) Inf, 1, s_grid = 1), "overflowed")
})

test_that("Chowfc is the Chow forecast F test", {
  x <- c(0.5, 1.2, -0.3, 2.1, 1.7, 0.1, 2.8, 0.9, 1.9, -0.6, 2.4, 1.1)
  y <- 1 + 2 * x + c(0.3, -0.2, 0.1, 0.5, -0.4, 0.2, -0.1, 0.6, -0.3, 2, 2.5, 1.8)
  r <- Chowfc(y, x, split = 9)
  r1 <- sum(resid(lm(y[1:9] ~ x[1:9]))^2)
  rc <- sum(resid(lm(y ~ x))^2)
  expect_equal(r$statistic, ((rc - r1) / 3) / (r1 / 7), tolerance = 1e-10)
  expect_equal(r$p_value, pf(r$statistic, 3, 7, lower.tail = FALSE), tolerance = 1e-12)
  expect_error(Chowfc(y, x, split = 12), "n2")
})

test_that("Chzlt is the Cinelli-Hazlett omitted-variable-bias adjustment", {
  d <- c(0, 1, 0, 1, 1, 0, 1, 0, 1, 0)
  x <- c(0.3, -0.2, 0.5, 0.1, -0.4, 0.2, 0.6, -0.1, 0.0, 0.4)
  y <- 1 + 0.8 * d + 0.5 * x + c(0.1, -0.2, 0.15, -0.05, 0.2, -0.1, 0.05, 0.1, -0.15, 0.02)
  r <- Chzlt(y, d, cov = x, R2_yu = 0.1, R2_du = 0.2)
  f <- summary(lm(y ~ d + x))$coefficients
  tau <- f["d", 1]
  se <- f["d", 2]
  bias <- se * sqrt(7) * sqrt(0.1 * 0.2 / 0.8)
  expect_equal(r$tau, tau, tolerance = 1e-9)
  expect_equal(r$se, se, tolerance = 1e-9)
  expect_equal(r$estimate, tau - sign(tau) * bias, tolerance = 1e-9)
  fq <- abs(tau / se) / sqrt(7)
  expect_equal(r$rv_q, min(0.5 * (sqrt(fq^4 + 4 * fq^2) - fq^2), 1), tolerance = 1e-9)
  expect_equal(r$adjusted_se, se * sqrt(0.9 / 0.8) * sqrt(7 / 6), tolerance = 1e-9)
  expect_error(Chzlt(numeric(0), numeric(0)), "empty")
  expect_error(Chzlt(y, d[-1]), "same length")
  expect_error(Chzlt(y, d, cov = x[-1]), "one row")
  expect_error(Chzlt(y, d, R2_du = 1), "R2_du")
})

test_that("Clbuvc is the Gaussian CLUB mutual-information bound", {
  x <- c(0.3, 1.2, -0.5, 2.2, 0.8, 1.9, -1.1, 0.1)
  y <- 0.5 + 0.8 * x + c(0.2, -0.3, 0.1, 0.4, -0.2, 0.3, -0.1, 0.05)
  r <- Clbuvc(x, y)
  f <- lm(y ~ x)
  s2 <- mean(resid(f)^2)
  lp <- function(yy, xx) dnorm(yy, coef(f)[1] + coef(f)[2] * xx, sqrt(s2), log = TRUE)
  pos <- mean(lp(y, x))
  neg <- mean(outer(x, y, function(a, b) lp(b, a)))
  expect_equal(r$club, pos - neg, tolerance = 1e-10)
  rho <- cor(x, y)
  expect_equal(r$mi_gauss, -0.5 * log(1 - rho^2), tolerance = 1e-12)
  q <- Clbuvc(x, y, q = c(0, 1, 2))
  expect_equal(q$positive, mean(dnorm(y, x, sqrt(2), log = TRUE)), tolerance = 1e-12)
  expect_error(Clbuvc(1:2, 1:2), "three")
  expect_error(Clbuvc(x, y[-1]), "same length")
  expect_error(Clbuvc(rep(1, 3), 1:3), "zero variance")
  expect_error(Clbuvc(x, y, q = 1:2), "\\(a, b, sigma2\\)")
  expect_error(Clbuvc(x, y, q = c(0, 1, 0)), "strictly positive")
})

test_that("Clcrp links customers by the distance-dependent CRP", {
  pos <- c(0, 0.2, 0.3, 5, 5.1, 9)
  D <- abs(outer(pos, pos, "-"))
  y <- c(1, 2, 3, 10, 11, 20)
  r <- Clcrp(y, D, alpha = 0.5, decay = 0.7, seed = 4)
  e <- .ghc_rng(4)
  links <- vapply(1:6, function(i) {
    w <- exp(-D[i, ] / 0.7)
    w[i] <- 0.5
    which(.ghc_unif(e, 1L) * sum(w) <= cumsum(w))[1]
  }, 0)
  expect_equal(r$links, as.integer(links - 1))
  G <- matrix(FALSE, 6, 6)
  G[cbind(1:6, links)] <- TRUE
  G <- G | t(G) | diag(6) > 0
  for (k in 1:6) G <- G | (G %*% G > 0)
  comp <- apply(G, 1, function(r) min(which(r)))
  lab <- match(comp, unique(comp))
  expect_equal(r$z, as.integer(lab - 1))
  expect_equal(r$cluster_mean, as.numeric(tapply(y, lab, mean)), tolerance = 1e-12)
  expect_error(Clcrp(numeric(0), matrix(0, 0, 0)), "empty")
  expect_error(Clcrp(y, D[-1, ]), "n x n")
  expect_error(Clcrp(y, D, alpha = 0), "strictly positive")
  expect_error(Clcrp(y, D, decay = 0), "strictly positive")
})

test_that("Clipsi and Clipxi: CLIP similarities and the ViT image encoder", {
  I <- rbind(c(1, 0.2, 0), c(0.1, 1, 0.3), c(0.2, 0.1, 1))
  Tx <- rbind(c(0.9, 0.3, 0.1), c(0.3, 0.2, 1), c(0, 1, 0.2))
  r <- Clipsi(I, Tx, tau = 0.05)
  cs <- (I / sqrt(rowSums(I^2))) %*% t(Tx / sqrt(rowSums(Tx^2)))
  expect_equal(r$cosine, cs, tolerance = 1e-12)
  expect_equal(r$logits, cs / 0.05, tolerance = 1e-12)
  expect_equal(r$retrieved, apply(cs, 1, which.max) - 1L)
  expect_equal(r$accuracy, mean(apply(cs, 1, which.max) == 1:3))
  expect_equal(Clipsi(cbind(c(1, 2)), cbind(c(3, -1)))$cosine, rbind(c(1, -1), c(1, -1)))
  expect_error(Clipsi(I[0, ], Tx[0, ]), "no rows")
  expect_error(Clipsi(I, Tx[-1, ]), "same number of rows")
  expect_error(Clipsi(I, Tx[, 1:2]), "share a dimension")
  expect_error(Clipsi(I, Tx, tau = 0), "strictly positive")
  img <- outer(1:32, 1:32, function(a, b) sin(a / 5) + cos(b / 7))
  x <- Clipxi(img, backbone = "vit-b/32", seed = 2)
  e <- .ghc_rng(2)
  proj <- matrix(.ghc_norm(e, 1024 * 64), 1024, 64, byrow = TRUE) / 32
  posm <- matrix(.ghc_norm(e, 2 * 64), 2, 64, byrow = TRUE) * 0.02
  head <- matrix(.ghc_norm(e, 64 * 32), 64, 32, byrow = TRUE) / 8
  tok <- as.numeric(as.numeric(t(img)) %*% proj) + posm[2, ]
  cls <- posm[1, ] + tok
  cls <- (cls - mean(cls)) / sqrt(mean((cls - mean(cls))^2))
  emb <- as.numeric(cls %*% head)
  expect_equal(x$embedding, emb / sqrt(sum(emb^2)), tolerance = 1e-10)
  expect_equal(x$n_patches, 1L)
  expect_error(Clipxi(img, backbone = "resnet"), "backbone must be")
  expect_error(Clipxi(img[1:30, ], backbone = "vit-b/32"), "multiples of the patch")
  expect_error(Clipxi(matrix(numeric(0), 0, 32)), "no rows")
})
