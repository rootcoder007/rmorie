# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/mambss_native.R (Mamba's selective SSM, Gu & Dao
# 2023). ZOH discretisation Abar = exp(dA), Bbar = (exp(dA) - 1) B / A;
# the scan is re-run channel by channel; and Theorem 1 (N = 1, A = -1,
# B = 1, softplus delta) is checked against the gated RNN form.

test_that("softplus is log1p(exp(z)) with linear and exponential tails", {
  expect_equal(softplus(0), log(2), tolerance = 1e-12)
  expect_equal(softplus(2.5), log1p(exp(2.5)), tolerance = 1e-12)
  expect_identical(softplus(40), 40)
  expect_equal(softplus(-40), exp(-40))
})

test_that("discretize_zoh matches the exact ZOH integral and Euler alternative", {
  A <- c(-1, -0.5, -2)
  B <- c(1, 2, 0.5)
  d <- 0.3
  z <- discretize_zoh(d, A, B)
  expect_equal(z$Abar, exp(d * A), tolerance = 1e-12)
  expect_equal(z$Bbar, (exp(d * A) - 1) / A * B, tolerance = 1e-12)
  e <- discretize_zoh(d, A, B, rule = "euler")
  expect_equal(e$Bbar, d * B)
  expect_equal(e$Abar, exp(d * A), tolerance = 1e-12)
  # the small-|dA| branch is the second-order expansion of the same integral
  s <- discretize_zoh(1e-10, c(1), c(2))
  expect_equal(s$Bbar, 1e-10 * 2 * (1 + 0.5e-10), tolerance = 1e-24)
  expect_equal(discretize_zoh(0, A, B)$Bbar, rep(0, 3))
  expect_error(discretize_zoh(-1, A, B), "non-negative")
  expect_error(discretize_zoh(0.1, A, B[-1]), "A has 3 entries but B has 2")
  expect_error(discretize_zoh(0.1, A, B, rule = "rk4"), "rule must be zoh or euler")
})

test_that("selective_ssm_step advances h and reads out C h", {
  A <- c(-1, -0.5)
  B <- c(1, 2)
  C <- c(0.5, -1)
  h <- c(0.2, -0.3)
  d <- 0.4
  z <- discretize_zoh(d, A, B)
  s <- selective_ssm_step(1.5, h, A, B, C, d)
  expect_equal(s$h, z$Abar * h + z$Bbar * 1.5, tolerance = 1e-12)
  expect_equal(s$y, sum(C * s$h), tolerance = 1e-12)
  for (fn in list(selectivessmstep, mamba_ssm_step, mambassmstep)) {
    expect_equal(fn(1.5, h, A, B, C, d)$y, s$y, tolerance = 1e-12)
  }
  expect_error(selective_ssm_step(1, c(0), A, B, C, d), "state has 1 entries")
  expect_error(selective_ssm_step(1, h, A, B, c(1), d), "C has 1 entries")
})

test_that("selective_scan runs the input-dependent recurrence over the sequence", {
  X <- rbind(c(1, -0.5), c(0.25, 2), c(-1, 0.5))
  A <- rbind(c(-1, -0.5), c(-2, -0.25))
  WB <- matrix(c(0.5, -1, 0.25, 1), 2)
  WC <- matrix(c(1, 0, -0.5, 2), 2)
  Wd <- matrix(c(0.3, -0.2), 1)
  db <- c(0.1, -0.1)
  r <- selective_scan(X, A, WB, WC, Wd, delta_bias = db, b_delta = 0.05, D_skip = c(0.5, 0))
  h <- matrix(0, 2, 2)
  Y <- matrix(0, 3, 2)
  dl <- matrix(0, 3, 2)
  for (t in 1:3) {
    xt <- X[t, ]
    Bt <- as.numeric(WB %*% xt)
    Ct <- as.numeric(WC %*% xt)
    raw <- as.numeric(Wd %*% xt) + 0.05
    for (cc in 1:2) {
      dt <- log1p(exp(raw + db[cc]))
      dl[t, cc] <- dt
      z <- discretize_zoh(dt, A[cc, ], Bt)
      h[cc, ] <- z$Abar * h[cc, ] + z$Bbar * xt[cc]
      Y[t, cc] <- sum(Ct * h[cc, ]) + c(0.5, 0)[cc] * xt[cc]
    }
  }
  expect_equal(r$y, Y, tolerance = 1e-12)
  expect_equal(r$delta, dl, tolerance = 1e-12)
  expect_equal(r$state, h, tolerance = 1e-12)
  expect_equal(c(r$L, r$D, r$N), c(3L, 2L, 2L))
  expect_false(r$time_invariant)
  expect_equal(s6_layer(X, A, WB, WC, Wd, delta_bias = db, b_delta = 0.05, D_skip = c(0.5, 0)), Y,
               tolerance = 1e-12)
  expect_equal(morie_mambss(X, A, WB, WC, Wd, delta_bias = db, b_delta = 0.05, D_skip = c(0.5, 0))$y, Y,
               tolerance = 1e-12)
  expect_error(selective_scan(X[0, , drop = FALSE], A, WB, WC, Wd), "sequence is empty")
  expect_error(selective_scan(X, A[1, , drop = FALSE], WB, WC, Wd), "A has 1 rows for 2 channels")
  expect_error(selective_scan(X, A, WB[1, , drop = FALSE], WC, Wd), "must have N=2 rows")
  expect_error(selective_scan(X, A, WB, WC, rbind(Wd, Wd)), "exactly 1 row")
  expect_error(selective_scan(X, A, WB, WC, Wd, delta_bias = 1), "delta_bias has 1 entries")
})

test_that("Theorem 1: N = 1, A = -1, B = 1 gives the gated RNN exactly", {
  x <- c(1, -0.5, 2, 0.25)
  w <- 0.8
  b <- -0.2
  g <- gated_rnn_equivalent(x, w, b)
  expect_equal(g$g, plogis(w * x + b), tolerance = 1e-12)
  hh <- 0
  ex <- numeric(4)
  for (i in 1:4) {
    hh <- (1 - g$g[i]) * hh + g$g[i] * x[i]
    ex[i] <- hh
  }
  expect_equal(g$h, ex, tolerance = 1e-12)
  # the scan with those parameters reproduces it: delta = softplus(w x + b),
  # Abar = exp(-delta) = 1 - g and Bbar = 1 - exp(-delta) = g
  # 1 - exp(-softplus(z)) = sigmoid(z), so the S6 recurrence with
  # A = -1, B = C = 1 and delta = softplus(w x + b) IS the gate above
  sc <- selective_scan(matrix(x, ncol = 1), matrix(-1), matrix(0), matrix(0),
                       matrix(w), b_B = 1, b_C = 1, b_delta = b)
  expect_equal(as.numeric(sc$delta), log1p(exp(w * x + b)), tolerance = 1e-12)
  expect_equal(as.numeric(sc$y), ex, tolerance = 1e-12)
})

