# Coverage for the geostatistical simulation shelf (Deutsch & Journel
# 1998; Goovaerts 1997; Strebelle 2002): p-field simulation as
# mean + sd * chol(R) z, conditional LMC simulation by Gaussian
# conditioning, collocated cokriging simulation, the ensemble summaries
# (E-type, variance, type-7 quantiles), Markov-Bayes indicator simulation
# and SNESIM, each checked on cases small enough to recompute from the
# kriging equations with base R's chol / solve and the same random
# streams.

.m <- list(model = "Exp", range = 2, sill = 1.5, nugget = 0)
.cov <- function(P, Q, m) {
  H <- sqrt(outer(P[, 1], Q[, 1], "-")^2 + outer(P[, 2], Q[, 2], "-")^2)
  C <- m$sill * switch(m$model, Exp = exp(-H / m$range), Gau = exp(-(H / m$range)^2),
                       Sph = ifelse(H < m$range, 1 - 1.5 * H / m$range + 0.5 * (H / m$range)^3, 0))
  C[H == 0] <- m$sill + m$nugget
  C
}

test_that("p-field simulation is mean + sd * L z with L = chol(R)'", {
  P <- cbind(c(0, 1, 2, 0.5), c(0, 0, 1, 2))
  mu <- c(1, 2, 3, 4)
  s <- c(0.5, 1, 1.5, 2)
  f <- PfieldSimulate(mu, s, P, list(model = "Sph", range = 3, sill = 9), seed = 5)
  R <- .cov(P, P, list(model = "Sph", range = 3, sill = 1, nugget = 0))
  z <- .morie_random_normal(4, seed = 5, stream = 0)
  expect_equal(f, mu + s * as.numeric(t(chol(R)) %*% z), tolerance = 1e-12)
})

test_that("LMC simulation conditions the joint Gaussian on the observed cells", {
  comps <- list(list(matrix(c(1, 0.4, 0.4, 0.8), 2), list(model = "Exp", range = 1.5)),
                list(matrix(c(0.2, 0.05, 0.05, 0.3), 2), list(model = "Gau", range = 3)))
  D <- cbind(c(0, 2), c(0, 0))
  V <- rbind(c(1.2, NA), c(-0.3, 0.5))
  Tg <- cbind(c(1, 2), c(0, 0))
  r <- LmcConditionalSimulate(D, V, Tg, comps, means = c(0.1, -0.1), seed = 2)
  cc <- function(A, B) {
    H <- sqrt(outer(A[, 1], B[, 1], "-")^2 + outer(A[, 2], B[, 2], "-")^2)
    out <- 0
    for (cp in comps) {
      Rh <- exp(-if (cp[[2]]$model == "Exp") H / cp[[2]]$range else (H / cp[[2]]$range)^2)
      out <- out + cp[[1]][A[, 3] + 1, B[, 3] + 1, drop = FALSE] * Rh
    }
    out
  }
  Dl <- rbind(c(0, 0, 0), c(2, 0, 0), c(2, 0, 1))
  z <- c(1.2, -0.3, 0.5)
  fr <- rbind(c(1, 0, 0), c(1, 0, 1))
  mu <- c(0.1, -0.1)
  Sdd <- cc(Dl, Dl)
  Sfd <- cc(fr, Dl)
  cm <- mu[fr[, 3] + 1] + Sfd %*% solve(Sdd, z - mu[Dl[, 3] + 1])
  S <- cc(fr, fr) - Sfd %*% solve(Sdd, t(Sfd))
  sim <- as.numeric(cm + t(chol(S)) %*% .morie_random_normal(2, seed = 2, stream = 0))
  expect_equal(r$simulated[1, ], sim, tolerance = 1e-10)
  expect_equal(r$simulated[2, ], c(-0.3, 0.5))
  u <- LmcConditionalSimulate(matrix(0, 0, 2), matrix(0, 0, 2), Tg[1, , drop = FALSE], comps, seed = 3)
  S0 <- cc(fr, fr)
  expect_equal(as.numeric(u$simulated), as.numeric(t(chol(S0)) %*% .morie_random_normal(2, seed = 3, stream = 0)), tolerance = 1e-10)
})

test_that("collocated cosimulation solves the cokriging system at each node", {
  D <- cbind(c(0, 3, 1), c(0, 0, 2))
  v <- c(0.4, -1, 0.9)
  x <- c(1, 1)
  r <- CollocatedCosimulate(D, v, rbind(x, D[2, ]), secondary = c(0.7, 0), model = .m, rho = 0.6, seed = 4)
  expect_equal(r$simulated[2], -1)
  path <- .gs2_perm(2, 4, 0)
  zn <- .morie_random_normal(2, seed = 4, stream = 1)
  step <- which(path == 0)
  known <- if (step == 1) D else rbind(D, D[2, ])
  kv <- if (step == 1) v else c(v, -1)
  o <- order(sqrt(colSums((t(known) - x)^2)))
  u <- .m
  u$sill <- 1
  cx <- .cov(known[o, , drop = FALSE], rbind(x), u)[, 1]
  C <- rbind(cbind(.cov(known[o, , drop = FALSE], known[o, , drop = FALSE], u), 0.6 * cx), c(0.6 * cx, 1))
  c0 <- c(cx, 0.6)
  lam <- solve(C, c0)
  ref <- sum(lam[seq_along(o)] * kv[o]) + lam[length(lam)] * 0.7 + sqrt(max(1 - sum(lam * c0), 0)) * zn[step]
  expect_equal(r$simulated[1], ref, tolerance = 1e-10)
})

test_that("the ensemble reports E-type, population variance and type-7 quantiles", {
  D <- cbind(c(0, 3), c(0, 0))
  Tg <- cbind(c(1, 2, 3), c(1, 0, 0))
  e <- ConditionalEnsemble(D, c(1, -1), Tg, .m, n_real = 5, probs = c(0.25, 0.5), seed = 7)
  R <- t(vapply(0:4, function(r) SgsBlockSimulate(D, c(1, -1), Tg, .m, seed = 7 + r)$simulated, numeric(3)))
  expect_equal(e$realisations, R, tolerance = 1e-12)
  expect_equal(e$etype, colMeans(R), tolerance = 1e-12)
  expect_equal(e$variance, colMeans(sweep(R, 2, colMeans(R))^2), tolerance = 1e-12)
  expect_equal(e$quantiles[[1]], apply(R, 2, stats::quantile, 0.25, names = FALSE), tolerance = 1e-12)
  expect_equal(e$quantiles[[2]], apply(R, 2, stats::median), tolerance = 1e-12)
  # the data location is reproduced in every realisation
  expect_equal(R[, 3], rep(-1, 5))
})

test_that("Markov-Bayes indicator simulation krigs each class indicator", {
  D <- cbind(c(0, 2, 0), c(0, 0, 2))
  cats <- c(0L, 1L, 1L)
  props <- c(0.4, 0.6)
  mods <- list(list(model = "Exp", range = 2, sill = 0.24), list(model = "Exp", range = 2, sill = 0.24))
  x <- c(1, 1)
  r <- SisMarkovBayes(D, cats, rbind(x), props, mods, seed = 1)
  pr <- vapply(1:2, function(c) {
    lam <- solve(.cov(D, D, c(mods[[c]], nugget = 0)), .cov(D, rbind(x), c(mods[[c]], nugget = 0))[, 1])
    min(max(props[c] + sum(lam * (as.numeric(cats == c - 1) - props[c])), 0), 1)
  }, 1)
  pr <- pr / sum(pr)
  expect_equal(r$probabilities[[1]], pr, tolerance = 1e-12)
  u <- .morie_random_uniform(1, seed = 1, stream = 1)
  expect_identical(r$simulated, as.integer(which(u <= cumsum(pr))[1] - 1))
  s <- SisMarkovBayes(D, cats, rbind(x), props, mods, soft = matrix(c(0.9, 0.1), 1), B = c(0.5, 0.5), seed = 1)
  expect_equal(sum(s$probabilities[[1]]), 1, tolerance = 1e-12)
  expect_false(isTRUE(all.equal(s$probabilities[[1]], pr)))
  k <- SisMarkovBayes(D, cats, D[2, , drop = FALSE], props, mods)
  expect_identical(k$simulated, 1L)
  expect_identical(k$probabilities[[1]], c(0, 1))
})

test_that("SNESIM draws from training-image conditional frequencies", {
  TI <- rbind(c(0, 0, 1, 1), c(0, 0, 1, 1), c(1, 1, 0, 0))
  cond <- rbind(c(0, 0, 1))
  s <- SnesimSimulate(TI, 2, 1, list(c(-1, 0)), conditioning = cond, seed = 3)
  expect_identical(s$categories, c(0, 1))
  expect_equal(s$grid[1, 1], 1)
  # pattern "left neighbour is 1": centres (ti >= 1) whose left cell is 1
  nb <- TI[, 1:3] == 1
  ctr <- TI[, 2:4][nb]
  pr <- c(mean(ctr == 0), mean(ctr == 1))
  u <- .morie_random_uniform(1, seed = 3, stream = 1)
  expect_equal(s$grid[1, 2], c(0, 1)[which(u <= cumsum(pr))[1]])
  m <- SnesimSimulate(TI, 3, 2, list(c(-1, 0), c(0, -1)), seed = 1)
  expect_true(all(m$grid %in% c(0, 1)))
  expect_false(anyNA(m$grid))
})
