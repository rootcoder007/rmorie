# Coverage for ABC samplers, robust weight functions and sequence
# alignment (abcrej_native.R, abcsmc.R, abcnnt_native.R, andrew.R,
# andrews.R, cauchw.R, alnnw.R, alnsw.R, areN.R): samplers replayed on
# their documented streams, M-estimation functions from their formulas,
# alignments by an independent dynamic programme.

vdc_b <- function(i, b) {
  f <- 1
  r <- 0
  k <- i + 1
  while (k > 0) {
    f <- f / b
    r <- r + f * (k %% b)
    k <- k %/% b
  }
  r
}

test_that("morie_abcrej accepts prior draws within eps", {
  sim <- function(th, e) c(th[1] + th[2], th[1] - th[2])
  obs <- c(1, 0.2)
  r <- morie_abcrej(sim, obs, eps = 0.5, prior = list(c(0, 1), c(-1, 1)), n_draws = 40, seed = 3)
  e <- .ghc_rng(3)
  acc <- list()
  for (k in 1:40) {
    th <- c(.ghc_unif(e, 1L, 0, 1), .ghc_unif(e, 1L, -1, 1))
    if (sqrt(sum((sim(th) - obs)^2)) <= 0.5) acc[[length(acc) + 1]] <- th
  }
  expect_equal(r$n_accepted, length(acc))
  expect_equal(r$samples, acc, tolerance = 1e-12)
  expect_equal(r$posterior_mean, colMeans(do.call(rbind, acc)), tolerance = 1e-12)
  none <- morie_abcrej(sim, c(100, 100), eps = 0.1, prior = list(c(0, 1), c(0, 1)), n_draws = 5)
  expect_true(all(is.nan(none$posterior_mean)))
  expect_error(morie_abcrej(sim, obs, 0, list(c(0, 1))), "positive")
  expect_error(morie_abcrej(sim, obs, 1, list(c(1, 0))), "low < high")
  expect_error(morie_abcrej(function(th, e) 1, obs, 1, list(c(0, 1))), "matching obs")
})

test_that("Abcsmc runs population Monte Carlo on its van der Corput design", {
  model <- function(th) c(2 * th[1], th[1]^2)
  S <- c(1, 0.25)
  r <- Abcsmc(model, S, priors = list(c(0, 1)), n_particles = 6, schedule = c(1, 0.4),
              kernel_sd = 0.2)
  N <- 6
  theta <- matrix(vapply(0:5, vdc_b, 0, b = 2), N, 1)
  w <- rep(1 / N, N)
  for (eps in c(1, 0.4)) {
    nt <- list()
    nw <- numeric(0)
    tries <- 0
    i <- 0
    while (length(nt) < N && tries < 20 * N) {
      src <- theta[(i %% nrow(theta)) + 1, ]
      cand <- min(max(src + (vdc_b(tries, 2) - 0.5) * 2 * 0.2, 0), 1)
      d <- sqrt(sum((model(cand) - S)^2))
      tries <- tries + 1
      i <- i + 1
      if (d <= eps) {
        den <- sum(w * dnorm((cand - theta[, 1]) / 0.2) / 0.2)
        nt[[length(nt) + 1]] <- cand
        nw <- c(nw, 1 / den)
      }
    }
    theta <- matrix(unlist(nt), ncol = 1)
    w <- nw / sum(nw)
  }
  expect_equal(r$theta, theta, tolerance = 1e-12)
  expect_equal(r$weights, w, tolerance = 1e-12)
  expect_equal(r$estimate, sum(w * theta[, 1]), tolerance = 1e-12)
  expect_equal(r$ess, 1 / sum(w^2), tolerance = 1e-12)
  # a round that fills fewer than n_particles must not break the next one
  tight <- Abcsmc(model, S, priors = list(c(0, 1)), n_particles = 6, schedule = c(0.02, 0.001),
                  kernel_sd = 0.2)
  expect_equal(sum(tight$weights), 1, tolerance = 1e-12)
  expect_lt(nrow(tight$theta), 6L)
})

test_that("sequential_neural_likelihood (tiny run) keeps its bookkeeping consistent", {
  sim <- function(th, e) c(th[1] + 0.1)
  lp <- function(th) if (abs(th[1]) > 3) -Inf else -0.5 * th[1]^2
  r <- sequential_neural_likelihood(sim, x_o = 0.5, log_prior = lp, theta0 = 0,
                                    n_rounds = 2L, n_per_round = 4L, n_layers = 1L, hidden = 2L,
                                    epochs = 1L, mcmc_burn = 2L, n_posterior = 6L, seed = 1L)
  expect_equal(r$n_simulations, 8L)
  expect_equal(vapply(r$D, function(p) p[[2]], 0), vapply(r$D, function(p) p[[1]] + 0.1, 0),
               tolerance = 1e-12)
  expect_equal(r$posterior_mean, colMeans(r$posterior_samples), tolerance = 1e-12)
  expect_equal(r$posterior_sd, apply(r$posterior_samples, 2, sd), tolerance = 1e-12)
  expect_length(r$history, 2L)
  expect_error(sequential_neural_likelihood(sim, numeric(0), lp, 0), "non-empty")
  expect_error(sequential_neural_likelihood(sim, 1, lp, 0, n_rounds = 0L), "positive")
})

test_that("Andrews sine, Cauchy weights and asymptotic relative efficiency", {
  r <- c(-5, -1, 0, 0.5, 2, 4.5)
  a <- Andrewswt(r, A = 1.2)
  expect_equal(a$weight, ifelse(abs(r) > 1.2 * pi, 0, ifelse(r == 0, 1, sin(r / 1.2) / (r / 1.2))),
               tolerance = 1e-12)
  expect_equal(a$rejected, sum(abs(r) > 1.2 * pi))
  expect_error(Andrewswt(r, 0), "positive")
  p <- Andrewspsi(r, c = 1.2)
  ins <- abs(r) <= 1.2 * pi
  expect_equal(p$psi, ifelse(ins, 1.2 * sin(r / 1.2), 0), tolerance = 1e-12)
  expect_equal(p$rho, ifelse(ins, 1.44 * (1 - cos(r / 1.2)), 2 * 1.44), tolerance = 1e-12)
  expect_equal(p$psi[r != 0], (a$weight * r)[r != 0], tolerance = 1e-12)
  expect_error(Andrewspsi(r, -1), "positive")
  cw <- Cauchw(r, c = 2)
  expect_equal(cw$weights, 1 / (1 + (r / 2)^2), tolerance = 1e-12)
  expect_equal(cw$objective, sum(2 * log(1 + (r / 2)^2)), tolerance = 1e-12)
  expect_equal(cw$psi, r * cw$weights, tolerance = 1e-12)
  expect_error(Cauchw(r, 0), "positive finite")
  expect_error(Cauchw(numeric(0)), "empty")
  ar <- Areratio(2, 1.5, n1 = 10, n2 = 20)
  expect_equal(ar$are, (2 / 10) / (1.5 / 20), tolerance = 1e-12)
  expect_equal(c(ar$normalmedian, ar$normalhl), c(2 / pi, 3 / pi))
  expect_error(Areratio(0, 1), "strictly positive")
  expect_error(Areratio(1, 1, n1 = 0), "strictly positive")
})

test_that("Alnnw and Alnsw are global and local alignment scores", {
  s1 <- "GATTACA"
  s2 <- "GCATGCU"
  sc <- function(x, y) if (x == y) 1 else -1
  a <- strsplit(s1, "")[[1]]
  b <- strsplit(s2, "")[[1]]
  F <- matrix(0, 8, 8)
  F[, 1] <- -(0:7)
  F[1, ] <- -(0:7)
  H <- matrix(0, 8, 8)
  for (i in 1:7) for (j in 1:7) {
    F[i + 1, j + 1] <- max(F[i, j] + sc(a[i], b[j]), F[i, j + 1] - 1, F[i + 1, j] - 1)
    H[i + 1, j + 1] <- max(0, H[i, j] + sc(a[i], b[j]), H[i, j + 1] - 1, H[i + 1, j] - 1)
  }
  g <- Alnnw(s1, s2)
  expect_equal(g$score, F[8, 8])
  al <- function(u, v) sum(ifelse(u == "-" | v == "-", -1, ifelse(u == v, 1, -1)))
  expect_equal(al(strsplit(g$aligned1, "")[[1]], strsplit(g$aligned2, "")[[1]]), g$score)
  expect_equal(gsub("-", "", g$aligned1), s1)
  l <- Alnsw(s1, s2)
  expect_equal(l$score, max(H))
  expect_equal(al(strsplit(l$aligned1, "")[[1]], strsplit(l$aligned2, "")[[1]]), l$score)
  M <- matrix(-2, 4, 4)
  diag(M) <- 3
  alpha <- sort(unique(c(a, b)))
  M <- matrix(-2, length(alpha), length(alpha))
  diag(M) <- 3
  gm <- Alnnw(s1, s2, sub_matrix = M, gap = 2)
  Fm <- matrix(0, 8, 8)
  Fm[, 1] <- -2 * (0:7)
  Fm[1, ] <- -2 * (0:7)
  for (i in 1:7) for (j in 1:7) {
    Fm[i + 1, j + 1] <- max(Fm[i, j] + if (a[i] == b[j]) 3 else -2, Fm[i, j + 1] - 2, Fm[i + 1, j] - 2)
  }
  expect_equal(gm$score, Fm[8, 8])
  expect_error(Alnnw("", "A"), "may be empty")
  expect_error(Alnnw("A", "C", gap = -1), "non-negative")
  expect_error(Alnnw("A", "C", sub_matrix = diag(3)), "square over the symbol alphabet")
  expect_error(Alnsw("", "A"), "may be empty")
  expect_error(Alnsw("A", "C", gap = -1), "non-negative")
})
