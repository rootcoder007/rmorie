# Coverage for blipqf_native.R, bridgs_native.R, bsmed_native.R,
# bootss_native.R, blastp_native.R, egcn_native.R and ehhdec_native.R:
# Q-Former cross-attention in matrix form, the Meng-Wong bridge fixed
# point, bootstrap streams replayed (R's set.seed and SplitMix64), BLAST
# maximal segment pairs against enumeration, the EGNN layer and its
# E(n) equivariance, and EHH from pair counts.

test_that("morie_blipqf is Q-Former cross-attention", {
  Q <- rbind(c(1, 0, 0.5), c(0.2, -0.3, 1))
  Fm <- rbind(c(0.5, 1, 0), c(-1, 0.2, 0.3), c(0, 0.4, -0.6), c(1, 1, 1))
  WQ <- rbind(c(1, 0, 0.2), c(0, 1, -0.1))
  WK <- rbind(c(0.5, 0.5, 0), c(0, -1, 1))
  WV <- rbind(c(1, 1, 1), c(0.3, 0, -0.3), c(0, 2, 0))
  S <- (Q %*% t(WQ)) %*% t(Fm %*% t(WK)) / sqrt(2)
  W <- exp(S - apply(S, 1, max))
  W <- W / rowSums(W)
  r <- morie_blipqf(Q, Fm, WQ, WK, WV)
  expect_equal(r$weights, W, tolerance = 1e-12)
  expect_equal(r$output, W %*% Fm %*% t(WV), tolerance = 1e-12)
  expect_equal(r$compression, 2)
})

test_that("morie_bridgs converges to the Meng-Wong fixed point", {
  u <- (seq_len(400) - 0.5) / 400
  d1 <- qnorm(u)
  d2 <- qnorm(u, 0.3, 1.3)
  lq1 <- function(x) log(3) - x^2 / 2
  lq2 <- function(x) dnorm(x, 0.3, 1.3, log = TRUE)
  r <- morie_bridgs(d1, d2, lq1, lq2)
  l1 <- exp(lq1(d1) - lq2(d1))
  l2 <- exp(lq1(d2) - lq2(d2))
  rr <- r$ratio
  fp <- mean(l2 / (0.5 * l2 + 0.5 * rr)) / mean(1 / (0.5 * l1 + 0.5 * rr))
  expect_true(r$converged)
  expect_equal(fp, rr, tolerance = 1e-10)
  # quadrature-like draws: the estimate sits close to 3 sqrt(2 pi)
  expect_equal(rr, 3 * sqrt(2 * pi), tolerance = 1e-3)
  one <- morie_bridgs(d1, d2, lq1, lq2, max_iter = 1)
  sh <- max(log(c(l1, l2)))
  e1 <- l1 / exp(sh)
  e2 <- l2 / exp(sh)
  expect_equal(one$log_ratio, log(mean(e2 / (0.5 * e2 + 0.5)) / mean(1 / (0.5 * e1 + 0.5))) + sh,
               tolerance = 1e-12)
  expect_false(one$converged)
  expect_error(morie_bridgs(numeric(0), d2, lq1, lq2), "non-empty")
})

test_that("Bsmed replays its bootstrap on R's stream", {
  x <- c(0.5, 1.2, -0.3, 2.1, 0.8, 1.5, -1, 0.2, 1.1, 0.6)
  m <- 0.4 * x + c(0.1, -0.2, 0.3, 0, -0.1, 0.2, 0.1, -0.3, 0.05, 0.15)
  y <- 0.5 * m + 0.2 * x + c(-0.1, 0.2, 0, 0.15, -0.05, 0.1, -0.2, 0.05, 0.1, -0.1)
  ab <- function(i) {
    a <- coef(lm(m[i] ~ x[i]))[2]
    b <- coef(lm(y[i] ~ x[i] + m[i]))[3]
    unname(a * b)
  }
  r <- Bsmed(x, m, y, B = 50, alpha = 0.1, seed = 7)
  boots <- withr::with_seed(7, vapply(1:50, function(k) ab(sample.int(10, 10, replace = TRUE)), 0))
  s <- sort(boots)
  expect_equal(r$estimate, ab(1:10), tolerance = 1e-10)
  expect_equal(r$c_prime, unname(coef(lm(y ~ x + m))[2]), tolerance = 1e-10)
  expect_equal(r$boot_estimate, mean(boots), tolerance = 1e-10)
  expect_equal(r$se, sd(boots), tolerance = 1e-10)
  expect_equal(c(r$ci_lower, r$ci_upper), s[c(as.integer(2.5), as.integer(47.5) + 1L)], tolerance = 1e-10)
  expect_error(Bsmed(x, m[-1], y), "equal length")
  expect_error(Bsmed(x, m, y, B = 1), "at least 2")
})

test_that("morie_bootss replays the Rao-Wu-Yue rescaled bootstrap", {
  y <- c(3, 5, 2, 8, 6, 4, 7, 1, 9, 2)
  w <- c(10, 10, 12, 12, 8, 8, 15, 15, 20, 20)
  st <- c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2)
  cl <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
  r <- morie_bootss(y, w, st, cl, B = 4, seed = 3)
  e <- .ghc_rng(3)
  groups <- list(list(1:2, 3:4, 5:6), list(7:8, 9:10))
  reps <- numeric(4)
  for (b in 1:4) {
    wb <- w
    for (h in 1:2) {
      nh <- length(groups[[h]])
      mh <- nh - 1
      cnt <- tabulate(vapply(seq_len(mh), function(d) min(floor(.ghc_unif(e, 1) * nh), nh - 1) + 1, 0), nh)
      fac <- 1 - sqrt(mh / (nh - 1)) + sqrt(mh / (nh - 1)) * nh / mh * cnt
      for (ci in 1:nh) wb[groups[[h]][[ci]]] <- w[groups[[h]][[ci]]] * fac[ci]
    }
    reps[b] <- sum(wb * y)
  }
  expect_equal(r$replicates, reps, tolerance = 1e-12)
  expect_equal(r$variance, mean((reps - sum(w * y))^2), tolerance = 1e-12)
  mean_stat <- function(yy, ww) sum(ww * yy) / sum(ww)
  ml <- morie_bootss(y, w, st, cl, statistic = mean_stat, B = 3, m = list("1" = 1, "2" = 1))
  expect_equal(ml$estimate, sum(w * y) / sum(w), tolerance = 1e-12)
  expect_error(morie_bootss(y, w[-1], st, cl), "paired")
  expect_error(morie_bootss(y, -w, st, cl), "positive")
  expect_error(morie_bootss(y, w, st, c(1, 1, 1, 1, 1, 1, 4, 4, 4, 4)), ">= 2 clusters")
  expect_error(morie_bootss(y, w, st, cl, m = 3), "m_h")
})

test_that("morie_blastp finds the maximal segment pair", {
  q <- "GATTACAGG"
  s <- "CTTACAGAT"
  qa <- strsplit(q, "")[[1]]
  sa <- strsplit(s, "")[[1]]
  sc <- function(i, j, L) sum(ifelse(qa[i:(i + L - 1)] == sa[j:(j + L - 1)], 1, -1))
  best <- 0
  for (i in 1:9) for (j in 1:9) for (L in 1:min(10 - i, 10 - j)) best <- max(best, sc(i, j, L))
  r <- morie_blastp(q, s)
  expect_equal(r$score, best)
  expect_equal(sc(r$q_start + 1, r$s_start + 1, r$length), best)
  expect_equal(r$e_value, 0.1 * 81 * exp(-best), tolerance = 1e-12)
  expect_equal(r$p_value, 1 - exp(-r$e_value), tolerance = 1e-12)
  sm <- list("A|C" = 2, "G|G" = 5)
  w <- morie_blastp("AG", "CG", score_matrix = sm)
  expect_equal(w$score, 7)
  expect_equal(morie_blastp("AAA", "CCC")$score, 0)
  expect_error(morie_blastp("", "A"), "non-empty")
})

test_that("the EGNN aliases run the equivariant layer", {
  H <- list(c(1, 0), c(0, 1), c(0.5, 0.5))
  X <- list(c(0, 0), c(1, 0), c(0, 2))
  phi_e <- function(hi, hj, d2, a) c(sum(hi * hj) + d2, d2)
  phi_x <- function(m) 0.1 * m[1]
  phi_h <- function(h, m) h + 0.01 * m
  r <- e_gcn(H, X, 1, phi_e, phi_x, phi_h)
  Xn <- lapply(1:3, function(i) {
    acc <- X[[i]]
    for (j in setdiff(1:3, i)) {
      d2 <- sum((X[[i]] - X[[j]])^2)
      acc <- acc + 0.5 * (X[[i]] - X[[j]]) * 0.1 * (sum(H[[i]] * H[[j]]) + d2)
    }
    acc
  })
  Hn <- lapply(1:3, function(i) {
    ms <- Reduce(`+`, lapply(setdiff(1:3, i), function(j) {
      d2 <- sum((X[[i]] - X[[j]])^2)
      c(sum(H[[i]] * H[[j]]) + d2, d2)
    }))
    H[[i]] + 0.01 * ms
  })
  expect_equal(r$X, Xn, tolerance = 1e-12)
  expect_equal(r$H, Hn, tolerance = 1e-12)
  th <- 0.7
  R <- rbind(c(cos(th), -sin(th)), c(sin(th), cos(th)))
  g <- c(2, -1)
  rot <- equivariantgraphconv(H, lapply(X, function(x) as.numeric(R %*% x) + g), 2, phi_e, phi_x, phi_h)
  base <- morie_egcn(H, X, 2, phi_e, phi_x, phi_h)$estimate
  expect_equal(rot$H, base$H, tolerance = 1e-12)
  expect_equal(rot$X, lapply(base$X, function(x) as.numeric(R %*% x) + g), tolerance = 1e-12)
})

test_that("morie_ehhdec counts homozygous pairs outward from the core", {
  hap <- rbind(c(0, 1, 1, 0, 1), c(0, 1, 1, 1, 1), c(1, 1, 1, 0, 0), c(0, 0, 1, 0, 1),
               c(1, 0, 0, 1, 1), c(0, 1, 1, 0, 0))
  ehh <- function(rows, core) {
    vapply(1:5, function(j) {
      k <- apply(hap[rows, min(j, core):max(j, core), drop = FALSE], 1, paste, collapse = "")
      pr <- combn(length(rows), 2)
      mean(k[pr[1, ]] == k[pr[2, ]])
    }, 0)
  }
  r <- morie_ehhdec(hap, 2)
  car1 <- which(hap[, 3] == 1)
  expect_equal(r$ehh1, ehh(car1, 3), tolerance = 1e-12)
  expect_equal(r$ehhs, ehh(1:6, 3), tolerance = 1e-12)
  expect_true(all(is.nan(r$ehh0)))
  expect_equal(c(r$n1, r$n0), c(5L, 1L))
  expect_equal(morie_ehhdec(hap, 0, positions = c(0, 10, 20, 30, 40))$positions, c(0, 10, 20, 30, 40))
  expect_error(morie_ehhdec(hap[1, , drop = FALSE], 0), "at least 2")
  expect_error(morie_ehhdec(hap, 5), "core out of range")
  expect_error(morie_ehhdec(hap + 1, 0), "0/1")
  expect_error(morie_ehhdec(hap, 0, positions = 1:3), "length mismatch")
})
