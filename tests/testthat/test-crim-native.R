# SPDX-License-Identifier: AGPL-3.0-or-later
# Module 17: ETAS, multivariate Hawkes, Knox near-repeat, risk
# terrain -- known-truth recovery + internal invariants.

test_that("ETAS recovers a subcritical simulated process", {
  # Simulate a univariate Omori-Hawkes (thinning) with known pars.
  set.seed(101)
  mu <- 0.4
  K <- 0.15
  cc <- 0.5
  p <- 1.5
  t_max <- 300
  tt <- c()
  cur <- 0
  lam_bar <- function(hist, s) {
    mu + K * sum(( s - hist + cc)^(-p))
  }
  while (cur < t_max) {
    lb <- lam_bar(tt, cur) + K * cc^(-p)
    cur <- cur + stats::rexp(1, lb)
    if (cur >= t_max) break
    if (stats::runif(1) < lam_bar(tt, cur) / lb) tt <- c(tt, cur)
  }
  skip_if(length(tt) < 30, "simulation too sparse")
  fit <- morie_crim_etas(tt, magnitudes = rep(1, length(tt)),
                         t_max = t_max)
  expect_s3_class(fit, "morie_etas")
  expect_true(fit$converged)
  expect_lt(fit$branching_ratio, 1) # subcritical DGP
  expect_lt(abs(fit$par[["mu"]] - mu), 0.3)
})

test_that("multivariate Hawkes recovers cross-excitation structure", {
  # 2-dim: component 1 excites 2 strongly, no reverse excitation.
  set.seed(102)
  b <- 2
  t_max <- 400
  mu <- c(0.3, 0.1)
  A <- matrix(c(0.2, 0.5, 0, 0.2), 2, 2, byrow = TRUE)
  # A[j,k]: excitation of j BY k. Row 2: A[2,1] = 0.5.
  tt <- c()
  mk <- c()
  cur <- 0
  lam_j <- function(s) {
    l <- mu
    if (length(tt)) {
      for (j in 1:2) {
        l[j] <- l[j] + b * sum(A[j, mk] * exp(-b * (s - tt)))
      }
    }
    l
  }
  while (cur < t_max) {
    lb <- sum(lam_j(cur)) + b * sum(A)
    cur <- cur + stats::rexp(1, lb)
    if (cur >= t_max) break
    l <- lam_j(cur)
    if (stats::runif(1) < sum(l) / lb) {
      tt <- c(tt, cur)
      mk <- c(mk, sample(1:2, 1, prob = l))
    }
  }
  skip_if(length(tt) < 60, "simulation too sparse")
  fit <- morie_crim_hawkes_multivariate(tt, mk, t_max = t_max, beta = b)
  expect_s3_class(fit, "morie_mv_hawkes")
  expect_lt(fit$spectral_radius, 1)
  # DGP: A[1,2] = 0.5 (component 1 excited BY 2); reverse is 0.
  expect_gt(fit$A[1, 2], fit$A[2, 1])
})

test_that("Knox test detects planted space-time clustering", {
  set.seed(103)
  # Background + planted near-repeats (same place, close in time).
  n_bg <- 60
  x <- runif(n_bg)
  y <- runif(n_bg)
  tt <- runif(n_bg, 0, 100)
  seeds <- sample(n_bg, 12)
  x <- c(x, x[seeds] + rnorm(12, sd = 0.01))
  y <- c(y, y[seeds] + rnorm(12, sd = 0.01))
  tt <- c(tt, tt[seeds] + runif(12, 0, 2))
  fit <- morie_crim_near_repeat(x, y, tt, s_threshold = 0.05,
                                t_threshold = 3)
  expect_s3_class(fit, "morie_knox")
  expect_gt(fit$ratio, 1.5)
  expect_lt(fit$p.value, 0.05)

  # Null data: no signal.
  fit0 <- morie_crim_near_repeat(runif(70), runif(70),
                                 runif(70, 0, 100),
                                 s_threshold = 0.05, t_threshold = 3)
  expect_gt(fit0$p.value, 0.05)
})

test_that("risk terrain recovers the generating layer", {
  set.seed(104)
  bars <- cbind(runif(12), runif(12))
  # Incidents cluster near bars; decoy layer unrelated.
  pick <- sample(12, 150, TRUE)
  inc <- cbind(bars[pick, 1] + rnorm(150, sd = 0.05),
               bars[pick, 2] + rnorm(150, sd = 0.05))
  decoy <- cbind(runif(10), runif(10))
  fit <- morie_crim_risk_terrain(inc, list(bars = bars, decoy = decoy),
                                 n_grid = 15L)
  expect_s3_class(fit, "morie_rtm")
  expect_gt(fit$coefficients[["bars"]], fit$coefficients[["decoy"]])
  expect_gt(fit$coefficients[["bars"]], 0)
  expect_true(all(dim(fit$risk_surface) == c(15L, 15L)))
})

test_that("degenerate inputs error cleanly", {
  expect_error(morie_crim_etas(1:5), ">= 10")
  expect_error(morie_crim_near_repeat(1:5, 1:5, 1:5, 1, 1))
})
