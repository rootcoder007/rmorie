# Coverage for volsabr_native .. wpiece_native exports. Every expectation is
# recomputed in the test body.

test_that("morie_volsabr reduces to the ATM formula at the money", {
  r <- morie_volsabr(K = 0.03, f = 0.03, T = 2, alpha = 0.04, beta = 0.5, rho = -0.3, nu = 0.4)
  atm <- 0.04 / 0.03^0.5 * (1 + (0.25 / 24 * 0.04^2 / 0.03 + -0.3 * 0.5 * 0.04 * 0.4 / (4 * 0.03^0.5) +
                                  (2 - 3 * 0.09) / 24 * 0.16) * 2)
  expect_equal(r$estimate, atm, tolerance = 1e-12)
  expect_equal(r$atm, atm, tolerance = 1e-12)
  o <- morie_volsabr(K = 0.035, f = 0.03, T = 2, alpha = 0.04, beta = 0.5, rho = -0.3, nu = 0.4)
  lfk <- log(0.03 / 0.035)
  fkb <- (0.03 * 0.035)^0.25
  z <- 0.4 / 0.04 * fkb * lfk
  xz <- log((sqrt(1 + 0.6 * z + z^2) + z + 0.3) / 1.3)
  corr <- 1 + (0.25 / 24 * 0.04^2 / (0.03 * 0.035)^0.5 - 0.3 * 0.5 * 0.4 * 0.04 / (4 * fkb) + (2 - 0.27) / 24 * 0.16) * 2
  sig <- 0.04 / (fkb * (1 + 0.25 / 24 * lfk^2 + 0.0625 / 1920 * lfk^4)) * z / xz * corr
  expect_equal(o$estimate, sig, tolerance = 1e-12)
  expect_error(morie_volsabr(-1, 0.03, 1, 0.04, 0.5, 0, 0.4), "must be positive")
  expect_error(morie_volsabr(0.03, 0.03, 1, 0.04, 0.5, 1, 0.4), "-1 < rho < 1")
})

test_that("morie_vpc, Vrmed and Welshw", {
  expect_equal(morie_vpc(0.8)$estimate, 0.8 / (0.8 + pi^2 / 3), tolerance = 1e-12)
  expect_equal(morie_vpc(0.8, "probit")$estimate, 0.8 / 1.8, tolerance = 1e-12)
  expect_error(morie_vpc(0.8, "cloglog"), "'logit' or 'probit'")
  expect_error(morie_vpc(-1), "non-negative")
  v <- Vrmed(0.4, 0.25)
  expect_equal(v$estimate, 0.15 / 0.4, tolerance = 1e-12)
  expect_true(is.nan(Vrmed(0, 0.1)$estimate))
  y <- c(-3, -1, 0, 0.5, 4)
  w <- Welshw(y, c = 2)
  expect_equal(w$w, exp(-(y / 2)^2), tolerance = 1e-12)
  expect_equal(w$estimate, sum(2 * (1 - exp(-(y / 2)^2))), tolerance = 1e-12)
  expect_equal(w$psi, y * exp(-(y / 2)^2), tolerance = 1e-12)
})

test_that("Waicd computes lppd and the variance penalty", {
  L <- rbind(c(-1.2, -0.8, -2.1), c(-1.0, -0.9, -1.7), c(-1.4, -0.7, -2.5), c(-1.1, -1.0, -1.9))
  r <- Waicd(L)
  lppd <- sum(log(colMeans(exp(L))))
  pw <- sum(apply(L, 2, stats::var))
  expect_equal(r$lppd, lppd, tolerance = 1e-12)
  expect_equal(r$p_waic, pw, tolerance = 1e-12)
  expect_equal(r$estimate, -2 * (lppd - pw), tolerance = 1e-12)
  expect_equal(r$n_high_var, sum(apply(L, 2, stats::var) > 0.4))
})

test_that("Watstro rewires a ring lattice with the Lehmer stream", {
  r <- Watstro(8, 4, 0.3, seed = 5)
  A <- matrix(0, 8, 8)
  for (i in 1:8) for (j in 1:2) {
    t <- ((i - 1 + j) %% 8) + 1
    A[i, t] <- A[t, i] <- 1
  }
  s <- 5
  u <- function() {
    s <<- (48271 * s) %% 2147483647
    s / 2147483647
  }
  rew <- 0
  for (j in 1:2) for (i in 1:8) {
    t <- ((i - 1 + j) %% 8) + 1
    if (A[i, t] == 0) next
    if (u() < 0.3) {
      cand <- min(floor(u() * 8) + 1, 8)
      if (cand == i || A[i, cand] != 0) next
      A[i, t] <- A[t, i] <- 0
      A[i, cand] <- A[cand, i] <- 1
      rew <- rew + 1
    }
  }
  expect_equal(r$A, A)
  expect_equal(r$n_rewired, rew)
  expect_equal(r$n_edges, 16)
  expect_equal(r$estimate, 4)
})

test_that("Wlkernel counts matching Weisfeiler-Lehman labels", {
  P3 <- rbind(c(0, 1, 0), c(1, 0, 1), c(0, 1, 0))
  T3 <- rbind(c(0, 1, 1), c(1, 0, 1), c(1, 1, 0))
  r <- Wlkernel(P3, T3, K = 1)
  # iteration 0: all labels equal, 3 x 3 = 9; iteration 1: the path's
  # middle node and every triangle node see two neighbours, 1 x 3 = 3
  expect_equal(r$per_iter, c(9, 3))
  expect_equal(r$kernel, 12)
  n <- Wlkernel(P3, P3[c(2, 1, 3), c(2, 1, 3)], K = 2, normalize = TRUE)
  expect_equal(n$estimate, 1, tolerance = 1e-12)
  lab <- Wlkernel(P3, P3, K = 0, labels1 = c("a", "b", "a"), labels2 = c("a", "a", "b"))
  expect_equal(lab$kernel, 2 * 2 + 1 * 1)
})

test_that("Weisz converges to the geometric median", {
  X <- rbind(c(0, 0), c(4, 0), c(0, 3), c(5, 5))
  r <- Weisz(X)
  # for a convex quadrilateral the geometric median is where the diagonals
  # cross: y = x meets x / 4 + y / 3 = 1 at (12/7, 12/7)
  expect_equal(r$estimate, c(12, 12) / 7, tolerance = 1e-8)
  u <- sweep(X, 2, r$estimate, "-")
  expect_equal(r$cost, sum(sqrt(rowSums(u^2))), tolerance = 1e-12)
  one <- Weisz(rbind(c(1, 1), c(1, 1)))
  expect_equal(one$estimate, c(1, 1))
})

test_that("morie_wenge's three saturated strategies agree with the mediation formula", {
  E <- c(1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0)
  M <- c(1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 1, 0)
  C <- c(0, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0)
  Y <- c(2.1, 1.3, 2.8, 0.9, 1.7, 1.1, 2.4, 1.9, 1.6, 0.7, 2.2, 1.0, 1.8, 2.6, 1.5, 0.8)
  r <- morie_wenge(E, M, C, Y, strategy = "all")
  th <- 0
  for (c in 0:1) for (m in 0:1) {
    ey <- mean(Y[E == 1 & M == m & C == c])
    fm0 <- mean(M[E == 0 & C == c] == m)
    th <- th + ey * fm0 * mean(C == c)
  }
  expect_equal(r$theta_em, th, tolerance = 1e-12)
  expect_equal(r$theta_ye, th, tolerance = 1e-12)
  expect_equal(r$theta_ym, th, tolerance = 1e-12)
  fe <- ave(E, C)
  ey1 <- mean(Y * E / fe)
  ey0 <- mean(Y * (1 - E) / (1 - fe))
  expect_equal(r$ey1, ey1, tolerance = 1e-12)
  expect_equal(r$nie, ey1 - th, tolerance = 1e-12)
  expect_equal(r$nde, th - ey0, tolerance = 1e-12)
  expect_error(morie_wenge(E + 1, M, C, Y), "binary 0/1")
})

test_that("Wfrep, PTukey, Winz and Rownorm", {
  y <- c(2, 1, 2, 3, 1, 2)
  w <- c(1, 2, 1.5, 0.5, 1, 1)
  f <- Wfrep(y, w)
  expect_equal(f$levels, c("1", "2", "3"))
  expect_equal(f$freq, c(3, 3.5, 0.5))
  expect_equal(f$prop, c(3, 3.5, 0.5) / 7, tolerance = 1e-12)
  expect_equal(Wfrep(y, cells = c(1, 4))$freq, c(2, 0))
  expect_error(Wfrep(y, w[-1]), "differ in length")
  expect_equal(PTukey(3.2, 4, 20)$p, stats::ptukey(3.2, 4, 20), tolerance = 1e-12)
  expect_equal(PTukey(3.2, 4, 20, lower_tail = FALSE)$p, 1 - stats::ptukey(3.2, 4, 20), tolerance = 1e-10)
  x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 100)
  wz <- Winz(x, 0.1)
  lo <- stats::quantile(x, 0.1, type = 7, names = FALSE)
  hi <- stats::quantile(x, 0.9, type = 7, names = FALSE)
  expect_equal(wz$estimate, mean(pmin(pmax(x, lo), hi)), tolerance = 1e-12)
  expect_equal(wz$n_changed, 2L)
  W <- rbind(c(0, 1, 2), c(0, 0, 0), c(3, 1, 0))
  rn <- Rownorm(W)
  expect_equal(rn$W, rbind(c(0, 1, 2) / 3, 0, c(3, 1, 0) / 4), tolerance = 1e-12)
  expect_equal(rn$islands, 1L)
})

test_that("Wpiece learns likelihood-scored merges and segments greedily", {
  corpus <- "low lower lowest newer newest wider"
  r <- Wpiece(corpus, vocab_size = 20)
  words <- strsplit(corpus, " ")[[1]]
  sp <- lapply(words, function(w) {
    ch <- strsplit(w, "")[[1]]
    c(ch[1], if (length(ch) > 1) paste0("##", ch[-1]))
  })
  pf <- table(unlist(sp))
  pairs <- unlist(lapply(sp, function(s) if (length(s) > 1) paste(s[-length(s)], s[-1], sep = " ")))
  pq <- table(pairs)
  sc <- vapply(names(pq), function(k) {
    ab <- strsplit(k, " ")[[1]]
    pq[[k]] / (pf[[ab[1]]] * pf[[ab[2]]])
  }, 0)
  expect_equal(r$scores[1], max(sc), tolerance = 1e-12)
  expect_equal(sort(r$alphabet), sort(names(pf)))
  tk <- r$tokenize("lowest")
  expect_true(all(tk %in% r$vocab))
  expect_equal(paste(sub("^##", "", tk), collapse = ""), "lowest")
  expect_equal(r$tokenize("xyz"), "[UNK]")
  expect_lte(length(r$vocab), 20)
})
