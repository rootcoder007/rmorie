# Metropolis-coupled MCMC over phylogenies (Ronquist & Huelsenbeck 2003,
# MrBayes 3).
#
# The decisive anchor is topology recovery on an alignment whose true tree
# is known by construction: two pairs of identical sequences must resolve
# as the split separating the pairs. The pairing is varied so the claim can
# fail, rather than being satisfied by any fixed answer. Everything else is
# recomputed from the returned samples.

# A and B identical, C and D identical: the true split is {A,B} | {C,D}.
ALN_AB <- list(
  A = "AAAACCCCGGGGTTTTAAAACCCC",
  B = "AAAACCCCGGGGTTTTAAAACCCC",
  C = "TTTTGGGGCCCCAAAATTTTGGGG",
  D = "TTTTGGGGCCCCAAAATTTTGGGG"
)
# the same four names, paired the other way: the true split is {A,C} | {B,D}
ALN_AC <- list(
  A = "AAAACCCCGGGGTTTTAAAACCCC",
  C = "AAAACCCCGGGGTTTTAAAACCCC",
  B = "TTTTGGGGCCCCAAAATTTTGGGG",
  D = "TTTTGGGGCCCCAAAATTTTGGGG"
)

test_that("the heating schedule is the MrBayes geometric ladder", {
  for (lam in c(0, 0.1, 0.2, 1)) {
    for (j in 0:4) {
      expect_equal(morie_phylby_chain_temperature(j, lam), 1 / (1 + lam * j))
    }
  }
  # the cold chain samples the posterior itself
  expect_equal(morie_phylby_chain_temperature(0, 0.2), 1)
  # hotter chains are flatter, and a zero heating parameter gives no ladder
  temps <- vapply(0:4, morie_phylby_chain_temperature, numeric(1), lam = 0.2)
  expect_true(all(diff(temps) < 0))
  expect_true(all(temps > 0 & temps <= 1))
  expect_equal(vapply(0:4, morie_phylby_chain_temperature, numeric(1),
                      lam = 0), rep(1, 5))
  expect_error(morie_phylby_chain_temperature(0, -1),
               "heating parameter must be >= 0")
  expect_error(morie_phylby_chain_temperature(-1, 0.2),
               "chain index must be >= 0")
})

test_that("the true split is recovered, whichever pair it separates", {
  r1 <- morie_phylby(ALN_AB, n_iter = 150, n_chains = 2, n_runs = 2,
                     sample_every = 5, seed = 1)
  expect_equal(r1$map_topology, "A,B")
  expect_true(r1$map_probability > 0.9)
  # repairing the alignment moves the answer, so the recovery is real and
  # not an artefact of taxon ordering
  r2 <- morie_phylby(ALN_AC, n_iter = 150, n_chains = 2, n_runs = 2,
                     sample_every = 5, seed = 1)
  expect_equal(r2$map_topology, "A,C")
  expect_true(r2$map_probability > 0.9)
  # the credibility of the recovered clade is high in each case
  expect_true(r1$clade_credibility[["A,B"]] > 0.9)
  expect_true(r2$clade_credibility[["A,C"]] > 0.9)
  expect_equal(r1$estimate, r1$clade_credibility)
})

test_that("the reported diagnostics are recomputable from the samples", {
  r <- morie_phylby(ALN_AB, n_iter = 150, n_chains = 2, n_runs = 2,
                    sample_every = 5, seed = 3)
  # the posterior probability of the MAP tree is its share of the samples
  expect_equal(r$map_probability,
               r$topology_counts[[r$map_topology]] / r$n_samples)
  expect_equal(r$n_samples, length(r$samples))
  expect_equal(sum(unlist(r$topology_counts)), r$n_samples)
  # every clade credibility is a probability
  expect_true(all(vapply(r$clade_credibility,
                         function(p) p >= 0 && p <= 1, logical(1))))
  # acceptance and swap rates are proportions
  expect_true(r$acceptance >= 0 && r$acceptance <= 1)
  expect_true(r$swap_rate >= 0 && r$swap_rate <= 1)
  # the temperature ladder has one entry per chain
  expect_length(r$temperatures, 2L)
  expect_equal(r$temperatures,
               vapply(0:1, morie_phylby_chain_temperature, numeric(1),
                      lam = 0.2))
  expect_equal(r$n_chains, 2L)
  expect_equal(r$n_runs, 2L)
  expect_length(r$runs, 2L)
  # the between-run split-frequency spread is non-negative, and is zero when
  # the independent runs agree on everything
  expect_true(r$asdsf >= 0)
  expect_equal(r$asdsf, 0)
  expect_match(r$method, "Ronquist & Huelsenbeck")
})

test_that("a single chain and a single run still sample", {
  r <- morie_phylby(ALN_AB, n_iter = 100, n_chains = 1, n_runs = 1,
                    sample_every = 5, seed = 5)
  expect_equal(r$n_chains, 1L)
  expect_equal(r$n_runs, 1L)
  expect_length(r$temperatures, 1L)
  expect_equal(r$temperatures, 1)
  # with one run there is no between-run comparison to make
  expect_equal(r$asdsf, 0)
  expect_true(r$n_samples > 0)
  expect_equal(r$map_topology, "A,B")
  # an explicit burnin is honoured
  rb <- morie_phylby(ALN_AB, n_iter = 100, burnin = 10, n_chains = 1,
                     n_runs = 1, sample_every = 5, seed = 5)
  expect_true(rb$n_samples >= r$n_samples)
})

test_that("site partitions are accepted per site", {
  n_site <- nchar(ALN_AB$A)
  part <- rep(c("first", "second"), each = n_site / 2)
  r <- morie_phylby(ALN_AB, n_iter = 100, n_chains = 1, n_runs = 1,
                    sample_every = 5, partitions = part, seed = 7)
  expect_equal(r$map_topology, "A,B")
  expect_true(r$n_samples > 0)
  # one label per site is required
  expect_error(morie_phylby(ALN_AB, n_iter = 50, partitions = c("a", "b")),
               "one partition label per site")
})

test_that("phylby refuses alignments it cannot analyse", {
  # fewer than four taxa leaves no unrooted topology free to vary
  expect_error(morie_phylby(ALN_AB[1:3], n_iter = 50),
               "at least four taxa")
  # ragged or empty sequences are not an alignment
  bad <- ALN_AB
  bad$D <- "AAAA"
  expect_error(morie_phylby(bad, n_iter = 50), "must be aligned")
  empt <- lapply(ALN_AB, function(z) "")
  expect_error(morie_phylby(empt, n_iter = 50), "non-empty")
  expect_error(morie_phylby(ALN_AB, n_iter = 0), "must be positive")
  expect_error(morie_phylby(ALN_AB, n_iter = 50, n_chains = 0),
               "must be positive")
  expect_error(morie_phylby(ALN_AB, n_iter = 50, n_runs = 0),
               "must be positive")
  expect_error(morie_phylby(ALN_AB, n_iter = 50, swap_every = 0),
               "swap_every and sample_every must be positive")
  expect_error(morie_phylby(ALN_AB, n_iter = 50, sample_every = 0),
               "swap_every and sample_every must be positive")
  # the burnin has to leave something behind
  expect_error(morie_phylby(ALN_AB, n_iter = 50, burnin = 50),
               "burnin must be less than n_iter")
  expect_error(morie_phylby(ALN_AB, n_iter = 50, burnin = -1),
               "burnin must be less than n_iter")
})
