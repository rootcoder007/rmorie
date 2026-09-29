# Probabilistic LSA (Hofmann 1999): the aspect-model joint, the E step as
# Bayes over z, the M step as normalised expected counts, and EM's
# monotone log-likelihood from a replayed initialisation.

pl_N <- rbind(c(3, 0, 1, 2), c(0, 4, 2, 0), c(1, 1, 0, 5))
pl_Pz <- c(0.6, 0.4)
pl_Pd <- rbind(c(0.5, 0.2, 0.3), c(0.1, 0.6, 0.3))
pl_Pw <- rbind(c(0.4, 0.1, 0.2, 0.3), c(0.1, 0.5, 0.3, 0.1))

test_that("the joint is sum_z P(z) P(d|z) P(w|z)", {
  P <- joint_probability(pl_Pz, pl_Pd, pl_Pw)
  ref <- t(pl_Pd) %*% diag(pl_Pz) %*% pl_Pw
  expect_equal(P, ref, tolerance = 1e-15)
  expect_equal(sum(P), 1, tolerance = 1e-15)
})

test_that("the E step is Bayes over z for every observed (d, w)", {
  post <- e_step(pl_N, pl_Pz, pl_Pd, pl_Pw)
  for (d in 1:3) for (w in 1:4) {
    if (pl_N[d, w] == 0) {
      expect_equal(post[d, w, ], c(0, 0))
    } else {
      num <- pl_Pz * pl_Pd[, d] * pl_Pw[, w]
      expect_equal(post[d, w, ], num / sum(num), tolerance = 1e-15)
    }
  }
  expect_error(e_step(-pl_N, pl_Pz, pl_Pd, pl_Pw), "non-negative")
  expect_error(e_step(pl_N * 0, pl_Pz, pl_Pd, pl_Pw), "corpus is empty")
})

test_that("the M step normalises the expected counts", {
  post <- e_step(pl_N, pl_Pz, pl_Pd, pl_Pw)
  m <- m_step(pl_N, post, 2)
  R <- sweep(post, c(1, 2), pl_N, "*")
  expect_equal(m$Pw_z, t(apply(R, c(2, 3), sum)) / colSums(apply(R, c(2, 3), sum)), tolerance = 1e-12)
  expect_equal(m$Pd_z, t(apply(R, c(1, 3), sum)) / colSums(apply(R, c(1, 3), sum)), tolerance = 1e-12)
  expect_equal(m$Pz, apply(R, 3, sum) / sum(pl_N), tolerance = 1e-12)
})

test_that("EM raises the log-likelihood from the replayed initialisation", {
  r <- morie_plsa(pl_N, 2, iters = 60, tol = 1e-12, seed = 3)
  e <- .ghc_rng(3)
  t0 <- 0.5 + .ghc_unif(e, 2L)
  Pz <- t0 / sum(t0)
  Pd <- t(vapply(1:2, function(z) {
    v <- 0.5 + .ghc_unif(e, 3L)
    v / sum(v)
  }, numeric(3)))
  Pw <- t(vapply(1:2, function(z) {
    v <- 0.5 + .ghc_unif(e, 4L)
    v / sum(v)
  }, numeric(4)))
  post <- e_step(pl_N, Pz, Pd, Pw)
  m <- m_step(pl_N, post, 2)
  expect_equal(r$loglik_history[1], log_likelihood(pl_N, m$Pz, m$Pd_z, m$Pw_z), tolerance = 1e-12)
  expect_true(all(diff(r$loglik_history) > -1e-10))
  expect_equal(r$final_loglik, log_likelihood(pl_N, r$P_z, r$P_d_given_z, r$P_w_given_z),
               tolerance = 1e-12)
  expect_identical(r$n_parameters, 2L * (3L + 4L) + 2L)
  expect_equal(probabilisticlsa(pl_N, 2, 60, 1e-12, 3)$P_z, r$P_z)
  expect_equal(perplexity(pl_N, r$P_z, r$P_d_given_z, r$P_w_given_z),
               exp(-r$final_loglik / sum(pl_N)), tolerance = 1e-12)
  expect_error(morie_plsa(pl_N, 0), "at least 1")
})
