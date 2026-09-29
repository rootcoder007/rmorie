n <- 9
B <- 1 * (abs(outer(1:n, 1:n, "-")) == 1)
B[1, 9] <- B[9, 1] <- B[3, 7] <- B[7, 3] <- 1
W <- B / rowSums(B)
y <- c(1, 2.4, 1.3, 3.1, 1.9, 2.2, 0.7, 2.8, 1.6)
x2 <- c(0.5, 0.9, 0.2, 1.4, 0.8, 1.1, 0.3, 1.2, 0.6)
z <- y - mean(y)

test_that("Lacgear, Lacgetg and Lactest follow the Cliff-Ord moments", {
  S0 <- sum(W)
  S1 <- 0.5 * sum((W + t(W))^2)
  S2 <- sum((rowSums(W) + colSums(W))^2)
  C <- (n - 1) * sum(W * outer(y, y, "-")^2) / (2 * S0 * sum(z^2))
  expect_equal(Lacgear(y, W)$statistic, C, tolerance = 1e-13)
  expect_equal(Lacgear(y, W, FALSE)$variance, ((2 * S1 + S2) * (n - 1) - 4 * S0^2) / (2 * (n + 1) * S0^2),
               tolerance = 1e-13)
  G <- sum(B * outer(y, y)) / (sum(outer(y, y)) - sum(y^2))
  expect_equal(Lacgetg(y, B)$statistic, G, tolerance = 1e-14)
  expect_equal(Lacgetg(y, B)$expected, sum(B) / (n * (n - 1)))
  I <- n / S0 * sum(z * W %*% z) / sum(z^2)
  expect_equal(Lactest(y, W)$statistic, I, tolerance = 1e-13)
  expect_null(Lactest(y - 2, W)$getis_ord)
})

test_that("Laclisa, Laclihh, Laclihl and Lacscat", {
  m2 <- sum(z^2) / n
  lz <- as.vector(W %*% z)
  Ii <- z / m2 * lz
  E <- -(z^2 * rowSums(W)) / ((n - 1) * m2)
  V <- (z / m2)^2 * n / (n - 2) * (rowSums(W^2) - rowSums(W)^2 / (n - 1)) * (m2 - z^2 / (n - 1))
  P <- 2 * pnorm(-abs((Ii - E) / sqrt(V)))
  r <- Laclisa(y, W)
  expect_equal(r$local_values, Ii, tolerance = 1e-13)
  expect_equal(r$variance, V, tolerance = 1e-13)
  expect_equal(r$p_value, P, tolerance = 1e-12)
  expect_equal(Laclihh(y, W, 0.5)$indices, which(z > 0 & lz > 0 & P < 0.5) - 1L)
  expect_equal(Laclihl(y, W, 1)$indices, which(z > 0 & lz <= 0 & P < 1) - 1L)
  s <- Lacscat(y, W)
  expect_equal(s$statistic, sum(z * lz) / sum(z^2), tolerance = 1e-14)
  expect_equal(s$counts, c(sum(z > 0 & lz > 0), sum(z <= 0 & lz > 0), sum(z <= 0 & lz <= 0), sum(z > 0 & lz <= 0)))
})

test_that("Lacgmc and Laclimc permute with Philox uniforms", {
  gc <- function(v) (n - 1) * sum(W * outer(v, v, "-")^2) / (2 * sum(W) * sum((v - mean(v))^2))
  u <- .morie_random_uniform(29 * n, seed = 3)
  sims <- vapply(1:29, function(s) gc(y[order(u[(s - 1) * n + 1:n])]), numeric(1))
  r <- Lacgmc(y, W, nsim = 29, seed = 3)
  expect_equal(r$simulated, sims, tolerance = 1e-13)
  expect_equal(r$p_value, (1 + sum(sims <= gc(y))) / 30)
  l <- Laclimc(y, W, nsim = 19, seed = 2)
  u <- .morie_random_uniform(19 * n * (n - 1), seed = 2)
  m2 <- sum(z^2) / n
  i <- 4
  oth <- (1:n)[-i]
  s <- vapply(1:19, function(k) z[i] / m2 * sum(W[i, oth] * z[oth[order(u[((k - 1) * n + i - 1) * (n - 1) + 1:(n - 1)])]]),
              numeric(1))
  Ii <- z[i] / m2 * sum(W[i, ] * z)
  expect_equal(l$p_value[i], (1 + min(sum(s >= Ii), sum(s <= Ii))) / 20)
})

test_that("Lacscor, Lacbivl and Lacvgm", {
  wy <- as.vector(W %*% y)
  expect_equal(Lacscor(y, W)$statistic, cor(y, wy), tolerance = 1e-13)
  zx <- x2 - mean(x2)
  L <- n / sum(rowSums(W)^2) * sum((W %*% zx) * (W %*% z)) / sqrt(sum(zx^2) * sum(z^2))
  expect_equal(Lacbivl(x2, y, W)$statistic, L, tolerance = 1e-13)
  xy <- cbind(c(0, 1, 2.1, 0.1, 1.2, 2, 0.3, 1.1, 2.3), c(0, 0.2, 0, 1, 1.1, 0.9, 2.2, 1.9, 2.1))
  v <- Lacvgm(y, xy, n_lags = 4, cutoff = 2)
  d <- as.matrix(dist(xy))
  pr <- which(upper.tri(d) & d <= 2, arr.ind = TRUE)
  k <- ceiling(d[pr] / 0.5)
  g <- tapply((y[pr[, 1]] - y[pr[, 2]])^2, k, sum) / (2 * tabulate(k))[sort(unique(k))]
  expect_equal(v$gamma, unname(as.numeric(g)), tolerance = 1e-13)
})
