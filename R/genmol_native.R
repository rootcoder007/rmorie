# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of genmol -- generative chemistry in a latent space. Mirrors
# src/morie/fn/genmol.py operation for operation, on the diffusion
# schedule in R/alfrf2_native.R, the SMILES parser in
# R/avalon_native.R, the shared RNG in R/aaa_helpers_ghc_rng.R and the
# shared numerics in R/aaa_helpers_w3num.R.
#
# The idea that made this field is Gomez-Bombarelli's. Chemical space is
# discrete -- you cannot take half a bond -- so it cannot be searched
# with anything that needs a gradient. A variational autoencoder buys
# its way out by learning a CONTINUOUS coordinate system in which every
# point decodes to a molecule: now you can move smoothly, follow a
# gradient, interpolate between two compounds, and optimise a property
# directly in the latent space instead of enumerating candidates.
#
#   THE SAMPLER. Draw a latent, decode it. The draw is the
#   reparameterisation of Kingma and Welling -- z = mu + sigma times a
#   standard normal -- with a temperature multiplying sigma, so
#   temperature zero returns the mean exactly and larger temperatures
#   trade validity for diversity. There is also a diffusion route, which
#   runs the reverse DDPM process in the latent space using the schedule
#   already written for the protein module rather than a second copy.
#
#   THE OPTIMISER. Gradient ascent on a property, in the latent space.
#   The gradient is by central differences on the caller's own property
#   function, which means this works with any property that can be
#   computed -- including one that is not differentiable, which is most
#   of the interesting ones.
#
# WHAT IS ARITHMETIC AND WHAT IS A WEIGHT. Every formula here is exact
# and published. The DECODER is a trained network and there is none
# here. Called without one the module says so and declines, because a
# generative model with no decoder generates nothing, and returning
# latent vectors while calling them molecules would be the worst kind of
# quiet failure.
#
# VALIDITY IS EXECUTED, NOT ASSERTED. A decoded string is valid if it
# PARSES as a molecule. Uniqueness and novelty are counted the same way,
# against the training set the caller supplies; a model that reproduces
# its training set has a validity of one and a novelty of zero, and
# reporting only the first would be flattering it.
#
# References
#   Gomez-Bombarelli, R. et al. (2018) "Automatic chemical design using
#     a data-driven continuous representation of molecules." ACS
#     Central Science 4(2), 268-276. doi:10.1021/acscentsci.7b00572.
#   Sanchez-Lengeling, B. and Aspuru-Guzik, A. (2018) "Inverse
#     molecular design using machine learning." Science 361(6400),
#     360-365. doi:10.1126/science.aat2663.
#   Kingma, D.P. and Welling, M. (2014) "Auto-encoding variational
#     Bayes." ICLR.
#   Ho, J., Jain, A. and Abbeel, P. (2020) "Denoising diffusion
#     probabilistic models." NeurIPS 33, 6840-6851.
#   Polykovskiy, D. et al. (2020) "Molecular sets (MOSES): a
#     benchmarking platform for molecular generation models."
#     Frontiers in Pharmacology 11, 565644.

.genmol_routes <- c("vae", "diffusion")

#' The Gaussian KL to a standard normal, in closed form
#'
#' Half the sum of the squared mean, plus the variance, minus one, minus
#' the log variance. Exactly zero at mu zero and logvar zero, which is
#' what makes it usable as a regulariser: the penalty is nothing when
#' the posterior already is the prior.
#'
#' @param mu The posterior means.
#' @param logvar The posterior log-variances.
#' @return A non-negative number.
#' @export
morie_genmol_kl <- function(mu, logvar) {
  if (length(mu) != length(logvar))
    stop("one log-variance per latent dimension")
  m <- as.numeric(mu); lv <- as.numeric(logvar)
  0.5 * .w3_csum(m * m + exp(lv) - 1 - lv)
}

#' The evidence lower bound: reconstruction minus the divergence
#'
#' Beta weights the divergence. At one this is the bound; above one it
#' is the beta-VAE trade, which buys a more disentangled latent space at
#' the cost of reconstruction, and it is a parameter because that trade
#' is the caller's to make.
#'
#' @param reconstruction The reconstruction term.
#' @param mu,logvar The posterior parameters.
#' @param beta The weight on the divergence.
#' @return The bound.
#' @export
morie_genmol_elbo <- function(reconstruction, mu, logvar, beta = 1)
  as.numeric(reconstruction) -
    as.numeric(beta) * morie_genmol_kl(mu, logvar)

#' Reparameterised draws: z = mu + temperature * sigma * epsilon
#'
#' At temperature zero every draw is exactly the mean -- not
#' approximately, exactly -- which is the check that the noise really is
#' entering through this one multiplication and nowhere else.
#'
#' @param mu,logvar The posterior parameters.
#' @param n How many draws.
#' @param temperature The multiplier on the standard deviation.
#' @param seed The random stream.
#' @return A list of latent vectors.
#' @export
morie_genmol_sample <- function(mu, logvar, n = 1L, temperature = 1,
                                seed = 0) {
  d <- length(mu)
  if (length(logvar) != d)
    stop("one log-variance per latent dimension")
  t <- as.numeric(temperature)
  if (t < 0) stop("a negative temperature is not a temperature")
  e <- .ghc_rng(seed)
  s <- exp(0.5 * as.numeric(logvar))
  lapply(seq_len(as.integer(n)), function(q)
    as.numeric(mu) + t * s * .ghc_norm(e, d))
}

#' Gradient ascent on a property, in the latent space
#'
#' The gradient is a central difference, so the property function need
#' not be differentiable or even continuous -- which matters, because
#' the properties worth optimising in chemistry are things like
#' synthetic accessibility and predicted binding, and those come from
#' code, not from formulas.
#'
#' The trajectory is returned, not just the endpoint: a latent
#' optimisation that ran off to infinity looks exactly like a successful
#' one if you only report where it stopped.
#'
#' @param z0 The starting latent.
#' @param property_fn The property to maximise.
#' @param steps How many ascent steps.
#' @param lr The step size.
#' @param eps The finite-difference spacing.
#' @return A list with the endpoint, the trajectory and the values.
#' @export
morie_genmol_optimise <- function(z0, property_fn, steps = 20L,
                                  lr = 0.1, eps = 1e-4) {
  z <- as.numeric(z0)
  traj <- list(z)
  vals <- as.numeric(property_fn(z))
  for (it in seq_len(as.integer(steps))) {
    g <- numeric(length(z))
    for (i in seq_along(z)) {
      up <- z; dn <- z
      up[i] <- up[i] + eps; dn[i] <- dn[i] - eps
      g[i] <- (as.numeric(property_fn(up)) -
                 as.numeric(property_fn(dn))) / (2 * eps)
    }
    z <- z + lr * g
    traj[[length(traj) + 1L]] <- z
    vals <- c(vals, as.numeric(property_fn(z)))
  }
  list(z = z, trajectory = traj, values = vals)
}

#' Which decoded strings are molecules, by running the parser
#'
#' Validity here is not a heuristic and not a regular expression: a
#' string is valid when the SMILES parser accepts it, which is the same
#' standard the generative chemistry literature reports.
#'
#' @param smiles_list The decoded strings.
#' @return A logical vector.
#' @export
morie_genmol_validity <- function(smiles_list)
  vapply(as.character(smiles_list), function(s)
    tryCatch({ morie_avalon_parse(s); TRUE },
             error = function(z) FALSE), logical(1),
    USE.NAMES = FALSE)

#' Sample molecules from a latent model and score what came back
#'
#' @param model A list with mu and logvar for the latent prior, and
#'   decoder, a function from a latent vector to a SMILES string.
#'   Without a decoder nothing is generated and the reason says so.
#' @param n_samples How many to draw.
#' @param conditions A list with property to optimise each latent
#'   against before decoding, and training_set to measure novelty
#'   against.
#' @param route Either vae or diffusion.
#' @param temperature The multiplier on the noise.
#' @param seed The random stream.
#' @param steps Ascent steps when a property is given.
#' @param T Diffusion steps for the diffusion route.
#' @param beta The weight on the divergence in the bound.
#' @return A list with the latents, the decoded strings, and validity,
#'   uniqueness and novelty as fractions of the sample.
#' @export
morie_genmol <- function(model, n_samples, conditions = NULL,
                         route = "vae", temperature = 1, seed = 0,
                         steps = 20L, T = 10L, beta = 1) {
  if (!(route %in% .genmol_routes)) stop("the route is vae or diffusion")
  n <- as.integer(n_samples)
  if (n < 1L) stop("a sample of nothing is not a sample")
  mu <- as.numeric(model$mu); logvar <- as.numeric(model$logvar)
  prop <- if (is.null(conditions)) NULL else conditions$property
  train <- if (is.null(conditions) || is.null(conditions$training_set))
    character(0) else as.character(conditions$training_set)
  dec <- model$decoder

  if (route == "vae") {
    zs <- morie_genmol_sample(mu, logvar, n, temperature, seed)
  } else {
    sc <- morie_alfrf2_schedule(T)
    betas <- sc$betas; alphas <- sc$alphas; abar <- sc$abar
    T <- as.integer(T)
    e <- .ghc_rng(seed)
    d <- length(mu)
    zs <- vector("list", n)
    for (q in seq_len(n)) {
      x <- .ghc_norm(e, d)
      for (tt in seq(T, 1L)) {
        # No trained denoiser either, so the prediction of the clean
        # latent is the prior mean. That makes the route the prior
        # sampled through the diffusion posterior -- correct arithmetic,
        # honestly labelled, and not a claim to be anybody's generative
        # model.
        x0 <- mu
        c1 <- sqrt(abar[tt]) * betas[tt + 1L] / (1 - abar[tt + 1L])
        c2 <- sqrt(alphas[tt + 1L]) * (1 - abar[tt]) / (1 - abar[tt + 1L])
        sd <- sqrt(betas[tt + 1L] * (1 - abar[tt]) / (1 - abar[tt + 1L]))
        z <- .ghc_norm(e, d)
        x <- c1 * x0 + c2 * x
        if (tt > 1L) x <- x + temperature * sd * z
      }
      zs[[q]] <- x
    }
  }

  trajs <- list(); props <- list()
  if (!is.null(prop)) {
    moved <- vector("list", n)
    for (q in seq_len(n)) {
      r <- morie_genmol_optimise(zs[[q]], prop, steps)
      moved[[q]] <- r$z
      trajs[[q]] <- r$trajectory
      props[[q]] <- r$values
    }
    zs <- moved
  }

  if (is.null(dec))
    return(list(
      latents = zs, smiles = character(0), valid = logical(0),
      reason = paste0("the model carries no decoder, so there is ",
                      "nothing to turn a latent vector into a ",
                      "molecule: a decoder is a trained network and ",
                      "none is shipped here. Pass one as the ",
                      "model's decoder."),
      n_samples = n, n_valid = 0L, n_unique = 0L, n_novel = 0L,
      validity = 0, uniqueness = 0, novelty = 0,
      kl = morie_genmol_kl(mu, logvar), elbo = NULL,
      property = props, trajectory = trajs, route = route,
      temperature = as.numeric(temperature), beta = as.numeric(beta),
      n_latent = length(mu), has_decoder = FALSE,
      method = "latent generative chemistry sampler"))

  smiles <- vapply(zs, function(z) as.character(dec(z)), character(1))
  flags <- morie_genmol_validity(smiles)
  good <- smiles[flags]
  uniq <- sort(unique(good), method = "radix")
  novel <- uniq[!(uniq %in% train)]
  nv <- sum(flags)
  list(latents = zs, smiles = smiles, valid = flags, reason = "",
       n_samples = n, n_valid = nv, n_unique = length(uniq),
       n_novel = length(novel), validity = nv / n,
       uniqueness = if (nv > 0L) length(uniq) / nv else 0,
       novelty = if (length(uniq)) length(novel) / length(uniq) else 0,
       unique = uniq, novel = novel,
       kl = morie_genmol_kl(mu, logvar),
       elbo = morie_genmol_elbo(nv / n, mu, logvar, beta),
       property = props, trajectory = trajs, route = route,
       temperature = as.numeric(temperature), beta = as.numeric(beta),
       n_latent = length(mu), has_decoder = TRUE,
       method = "latent generative chemistry sampler")
}

#' One-line summary of the genmol module
#'
#' @return A character scalar.
#' @export
morie_genmol_cheatsheet <- function()
  paste0("genmol: generative chemistry. Reparameterised latent ",
         "sampling or a latent diffusion, gradient ascent on any ",
         "property by central differences, validity checked by parsing")
