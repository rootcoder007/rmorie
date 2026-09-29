n <- 12
ii <- 0:(n - 1)
C <- cbind(cos(1.7 * ii) * (1 + ii / 6), sin(2.3 * ii) + 0.1 * ii)
D <- unname(as.matrix(stats::dist(C)))

test_that("point builders recompute", {
  G <- 1 * (D > 0.2 & D <= 1.1)
  diag(G) <- 0
  expect_equal(unname(swdist(C, d = 1.1, d_min = 0.2)$W), G)
  k <- swknn(C, k = 3)
  for (i in 1:n) expect_equal(sort(k$neighbours[[i]]), sort(order(D[i, ], seq_len(n))[2:4] - 1))
  V <- D^-1.5
  diag(V) <- 0
  expect_equal(unname(swinv(C, power = 1.5)$W), V, tolerance = 1e-14)
  K <- stats::dnorm(D / 0.8)
  diag(K) <- 0
  expect_equal(unname(swkern(C, bw = 0.8)$W), K, tolerance = 1e-14)
  h <- apply(D, 1, function(r) sort(r)[4])
  U <- D / h
  A <- ifelse(U >= 1, 0, 15 / 16 * (1 - U^2)^2)
  diag(A) <- 0
  expect_equal(unname(swadapt(C, k = 3, kernel = "quartic")$W), A, tolerance = 1e-14)
  g <- swgab(C)$neighbours
  for (i in 1:n) for (j in setdiff(1:n, i)) {
    m <- setdiff(1:n, c(i, j))
    expect_equal((j - 1) %in% g[[i]], !any(D[i, m]^2 + D[j, m]^2 < D[i, j]^2))
  }
  tri <- swtri(C)$neighbours
  expect_true(all(unlist(g[[1]]) %in% tri[[1]]))
})

test_that("operators and graph functions recompute", {
  W <- rbind(c(0, 0.5, 0.5, 0), c(1 / 3, 0, 1 / 3, 1 / 3), c(0.5, 0.5, 0, 0), c(0, 1, 0, 0))
  y <- c(1.5, -2, 4, 0.25)
  expect_equal(swlag(W, y), as.vector(W %*% y))
  expect_equal(swlag2(W, y), as.vector(W %*% W %*% y))
  expect_equal(swlagf(W, 0.4), solve(diag(4) - 0.4 * W))
  expect_equal(swpower(W, 3), W %*% W %*% W)
  S <- swspars(W, thr = 0.3, row_standardize = TRUE)
  expect_equal(rowSums(S), rep(1, 4))
  expect_equal(swblk(c("a", "b", "a"))$W, rbind(c(0, 0, 1), c(0, 0, 0), c(1, 0, 0)))
  R <- matrix(0, 7, 7)
  for (a in 0:5) {
    b <- (a + 1) %% 6
    R[a + 1, b + 1] <- 1
    R[b + 1, a + 1] <- 1
  }
  R[1, 4] <- 2.5
  R[4, 1] <- 2.5
  expect_equal(swpath(R, 0, 3), 1)
  expect_equal(swpath(R, 1, 4, weighted = TRUE), 3)
  expect_equal(swpath(R, 0, 6), Inf)
  lat <- c(43.65, 45.5, 49.28)
  lon <- c(-79.38, -73.57, -123.12)
  p <- lat * pi / 180
  hv <- sin((p[2] - p[1]) / 2)^2 + cos(p[1]) * cos(p[2]) * sin((lon[2] - lon[1]) * pi / 360)^2
  expect_equal(swsph(lat, lon, d = 600)$D[1, 2], 2 * 6371 * asin(sqrt(hv)), tolerance = 1e-12)
  expect_equal(swsph(lat, lon, d = 600)$neighbours, list(1L, 0L, integer(0)))
})
