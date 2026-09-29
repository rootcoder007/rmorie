# Coverage for the generative-chemistry sampler: the Gaussian KL to N(0, I)
# and the beta-ELBO, reparameterised latent sampling (regenerated from the
# counter generator), central-difference gradient ascent (against the
# analytic gradient of a quadratic), SMILES validity by parsing, and the
# VAE / latent-diffusion routes with a toy decoder.

test_that("KL(N(mu, diag e^lv) || N(0, I)) and the beta-ELBO", {
  mu <- c(0.5, -1, 0)
  lv <- c(0, log(2), -1)
  kl <- 0.5 * sum(mu^2 + exp(lv) - 1 - lv)
  expect_equal(morie_genmol_kl(mu, lv), kl, tolerance = 1e-12)
  expect_identical(morie_genmol_kl(c(0, 0), c(0, 0)), 0)
  expect_equal(morie_genmol_elbo(-3, mu, lv, beta = 0.5), -3 - 0.5 * kl, tolerance = 1e-12)
  expect_error(morie_genmol_kl(mu, lv[-1]), "one log-variance")
})

test_that("samples are mu + T sigma eps from the counter generator", {
  mu <- c(1, -2)
  lv <- c(0, log(4))
  s <- morie_genmol_sample(mu, lv, n = 3, temperature = 0.5, seed = 6)
  e <- .ghc_rng(6)
  ref <- lapply(1:3, function(i) mu + 0.5 * c(1, 2) * .ghc_norm(e, 2))
  expect_equal(s, ref, tolerance = 1e-12)
  expect_equal(morie_genmol_sample(mu, lv, n = 2, temperature = 0), list(mu, mu))
  expect_error(morie_genmol_sample(mu, lv, temperature = -1), "negative temperature")
  expect_error(morie_genmol_sample(mu, lv[1]), "one log-variance")
})

test_that("gradient ascent by central differences on a quadratic", {
  f <- function(z) -sum((z - c(1, 2))^2)
  r <- morie_genmol_optimise(c(0, 0), f, steps = 5, lr = 0.1)
  z <- c(0, 0)
  for (i in 1:5) z <- z + 0.1 * (-2 * (z - c(1, 2)))
  # central differences are exact for a quadratic up to rounding
  expect_equal(r$z, z, tolerance = 1e-8)
  expect_length(r$trajectory, 6L)
  expect_equal(r$values, vapply(r$trajectory, f, 1), tolerance = 1e-12)
})

test_that("validity parses SMILES", {
  expect_identical(morie_genmol_validity(c("CCO", "c1ccccc1", "C1CC", "C(C")), c(TRUE, TRUE, FALSE, FALSE))
})

test_that("VAE and diffusion routes decode latents and score the batch", {
  dec <- function(z) if (z[1] > 0) "CCO" else if (z[2] > 0) "CC" else "C1CC"
  model <- list(mu = c(0.2, 0.1), logvar = c(0, 0), decoder = dec)
  r <- morie_genmol(model, 6, conditions = list(training_set = "CC"), seed = 3)
  zs <- morie_genmol_sample(model$mu, model$logvar, 6, 1, 3)
  sm <- vapply(zs, dec, "")
  ok <- sm != "C1CC"
  expect_identical(r$smiles, sm)
  expect_identical(r$valid, ok)
  expect_equal(r$validity, mean(ok))
  u <- sort(unique(sm[ok]))
  expect_identical(r$unique, u)
  expect_identical(r$novel, setdiff(u, "CC"))
  expect_equal(r$uniqueness, length(u) / sum(ok))
  expect_equal(r$elbo, mean(ok) - morie_genmol_kl(model$mu, model$logvar), tolerance = 1e-12)
  sc <- morie_alfrf2_schedule(4)
  e <- .ghc_rng(9)
  ref <- lapply(1:2, function(q) {
    x <- .ghc_norm(e, 2)
    for (t in 4:1) {
      ab <- sc$abar
      c1 <- sqrt(ab[t]) * sc$betas[t + 1] / (1 - ab[t + 1])
      c2 <- sqrt(sc$alphas[t + 1]) * (1 - ab[t]) / (1 - ab[t + 1])
      sd <- sqrt(sc$betas[t + 1] * (1 - ab[t]) / (1 - ab[t + 1]))
      z <- .ghc_norm(e, 2)
      x <- c1 * model$mu + c2 * x + if (t > 1) 0.7 * sd * z else 0
    }
    x
  })
  d <- morie_genmol(list(mu = model$mu, logvar = model$logvar), 2, route = "diffusion", T = 4, seed = 9, temperature = 0.7)
  expect_equal(d$latents, ref, tolerance = 1e-12)
  expect_false(d$has_decoder)
  expect_match(d$reason, "no decoder")
  p <- morie_genmol(model, 2, conditions = list(property = function(z) -sum(z^2)), steps = 3, seed = 1)
  expect_length(p$trajectory[[1]], 4L)
  expect_equal(p$latents[[2]], p$trajectory[[2]][[4]])
  expect_error(morie_genmol(model, 1, route = "gan"), "vae or diffusion")
  expect_error(morie_genmol(model, 0), "sample of nothing")
  expect_match(morie_genmol_cheatsheet(), "generative chemistry")
})
