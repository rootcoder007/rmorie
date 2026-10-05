# SPDX-License-Identifier: AGPL-3.0-or-later
# References: the authors' JAGS model for Bayesian Aldrich-McKelvey
# (asmcjr::BAM) and a JAGS encoding of the Bakker-Poole lognormal
# unfolding model.
test_that("native Bayesian Aldrich-McKelvey agrees with asmcjr::BAM (JAGS)", {
  skip_if_not_installed("rjags")
  skip_if_not_installed("asmcjr")
  set.seed(11)
  zt <- c(-1.4, -0.6, 0, 0.5, 1.5)
  N <- 150
  Z <- rnorm(N, 0, 0.8) + outer(runif(N, 0.4, 1.6) * ifelse(runif(N) < 0.1, -1, 1), zt) +
    matrix(rnorm(N * 5, 0, 0.4), N, 5)
  Z[sample(length(Z), 40)] <- NA
  colnames(Z) <- paste0("s", 1:5)
  fit <- morie_spatial_voting_bayesian_am(Z, n_samples = 2500L, burn_in = 1000L, seed = 2L)
  bp <- structure(list(stims = Z, self = rep(0, N)), class = c("bamPrep", "list"))
  set.seed(3)
  utils::capture.output(ref <- asmcjr::BAM(bp, polarity = 1, n.sample = 2500, n.chains = 2, n.adapt = 2000))
  expect_lt(max(abs(ref$zhat.ci$idealpt - fit$zeta_mean)), 0.01)
  expect_lt(max(abs(ref$zhat.ci$sd - fit$zeta_sd)), 0.005)
})

test_that("native Bayesian unfolding agrees with the JAGS lognormal model", {
  skip_if_not_installed("rjags")
  set.seed(31)
  n <- 150
  m <- 10
  Zt <- matrix(runif(m * 2, -0.55, 0.55), m, 2)
  Xt <- matrix(runif(n * 2, -0.55, 0.55), n, 2)
  D <- sqrt(rmorie:::.morie_sqdist(Xt, Zt)) * exp(matrix(rnorm(n * m, 0, 0.2), n, m))
  f <- morie_spatial_voting_bayesian_unfolding(D, n_samples = 1500L, burn_in = 1000L, seed = 1L)
  mod <- "model{
   for (i in 1:n) { for (j in 1:m) {
     D[i,j] ~ dlnorm(log(sqrt((x[i,1]-z[j,1])^2 + (x[i,2]-z[j,2])^2)), tau) } }
   for (i in 1:n) { x[i,1] ~ dnorm(0, 0.01)  x[i,2] ~ dnorm(0, 0.01) }
   z[2,1] ~ dnorm(0, 0.01) T(0,)
   for (j in 3:m) { z[j,1] ~ dnorm(0, 0.01)  z[j,2] ~ dnorm(0, 0.01) }
   tau ~ dunif(0, 10)
  }"
  zfix <- matrix(NA, m, 2)
  zfix[1, ] <- 0
  zfix[2, 2] <- 0
  F0 <- sweep(f$stimuli, 2, f$stimuli[1, ])
  th <- atan2(F0[2, 2], F0[2, 1])
  Rm <- matrix(c(cos(th), sin(th), -sin(th), cos(th)), 2)
  zi <- F0 %*% Rm
  zi[1, ] <- NA
  zi[2, 2] <- NA
  X0 <- sweep(f$ideal_points, 2, f$stimuli[1, ]) %*% Rm
  set.seed(4)
  jm <- rjags::jags.model(textConnection(mod), data = list(D = D, n = n, m = m, z = zfix),
                          inits = list(x = X0, z = zi, tau = 5), n.chains = 1, n.adapt = 1000, quiet = TRUE)
  update(jm, 1000, progress.bar = "none")
  sm <- rjags::coda.samples(jm, c("x", "z", "tau"), 3000, progress.bar = "none")[[1]]
  S <- nrow(sm)
  xs <- array(sm[, grep("^x", colnames(sm))], c(S, n, 2))
  zs <- array(sm[, grep("^z", colnames(sm))], c(S, m, 2))
  dj <- Reduce(`+`, lapply(seq_len(S), function(s) {
    sqrt(rmorie:::.morie_sqdist(matrix(xs[s, , ], n), matrix(zs[s, , ], m)))
  })) / S
  # stimuli 1 and 2 carry the JAGS gauge (and its implicit 1/d prior)
  expect_lt(max(abs(dj[, 3:m] - f$distance_mean[, 3:m])), 0.04)
  expect_lt(abs(mean(sm[, "tau"]) - f$tau), 0.02)
})
