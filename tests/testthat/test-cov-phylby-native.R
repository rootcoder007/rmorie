# MrBayes 3 pieces (Ronquist & Huelsenbeck 2003): bipartitions, topology
# keys, NNI neighbourhoods, the exponential branch prior, partitioned
# likelihoods and the Metropolis-coupling swap rule.

ph_tree <- list(list("A", 0.1, "B", 0.2), 0.05, list("C", 0.15, "D", 0.3), 0.05)
ph_seqs <- list(A = "ACGTACGTAA", B = "ACGTACGTTA", C = "ACGAACCTTA", D = "TCGAACCTTG")

test_that("splits and topology keys describe the unrooted bipartitions", {
  expect_identical(morie_phylby_splits_of(ph_tree), list(c("A", "B")))
  t5 <- list(list(list("A", 1, "B", 1), 1, "C", 1), 1, list("D", 1, "E", 1), 1)
  sp <- morie_phylby_splits_of(t5)
  keys <- sort(vapply(sp, function(s) paste(sort(s), collapse = ","), ""))
  expect_identical(keys, c("A,B", "A,B,C"))
  expect_identical(morie_phylby_topology_key(t5), list(c("A", "B"), c("A", "B", "C")))
  # rotating children does not change the topology
  rot <- list(list("D", 0.3, "C", 0.15), 0.05, list("B", 0.2, "A", 0.1), 0.05)
  expect_identical(morie_phylby_topology_key(rot), morie_phylby_topology_key(ph_tree))
  expect_identical(morie_phylby_topology_key(list("A", 1, "B", 1)), list())
})

test_that("NNI from a four-taxon tree reaches the two other topologies", {
  nb <- morie_phylby_nni_neighbours(ph_tree)
  ks <- sort(vapply(nb, function(t) paste(unlist(morie_phylby_topology_key(t)), collapse = ","), ""))
  expect_identical(ks, c("A,C", "A,D"))
})

test_that("the log posterior adds the exponential branch prior to the likelihood", {
  lp <- morie_phylby_log_posterior(ph_tree, ph_seqs, branch_prior_mean = 0.2, temperature = 0.5)
  br <- c(0.1, 0.2, 0.05, 0.15, 0.3, 0.05)
  expect_equal(lp$logprior, sum(stats::dexp(br, rate = 5, log = TRUE)), tolerance = 1e-12)
  expect_equal(lp$loglik, morie_phylml(ph_tree, ph_seqs, NULL, 1)$log_likelihood, tolerance = 1e-12)
  expect_equal(lp$logpost, 0.5 * (lp$loglik + lp$logprior), tolerance = 1e-12)
  parts <- rep(c("x", "y"), each = 5)
  pp <- morie_phylby_log_posterior(ph_tree, ph_seqs, partitions = parts,
                                   rates = list(x = 0.5, y = 2))
  sub <- function(k) lapply(ph_seqs, function(s) substr(s, 5 * k - 4, 5 * k))
  ll <- morie_phylml(ph_tree, sub(1), NULL, 0.5)$log_likelihood +
    morie_phylml(ph_tree, sub(2), NULL, 2)$log_likelihood
  expect_equal(pp$loglik, ll, tolerance = 1e-12)
  expect_equal(pp$logprior, sum(stats::dexp(br, rate = 10, log = TRUE)) - 2.5, tolerance = 1e-12)
  expect_error(morie_phylby_log_posterior(ph_tree, ph_seqs, branch_prior_mean = 0), "prior mean")
  expect_error(morie_phylby_log_posterior(ph_tree, ph_seqs, rate = 0), "substitution rate")
  bad <- list("A", -1, "B", 1, "C", 1, "D", 1)
  expect_error(morie_phylby_log_posterior(bad, ph_seqs), "non-negative")
})

test_that("clade credibility is the split frequency over samples", {
  alt <- list(list("A", 0.1, "C", 0.2), 0.05, list("B", 0.15, "D", 0.3), 0.05)
  cc <- morie_phylby_clade_credibility(list(ph_tree, ph_tree, alt, ph_tree))
  expect_equal(cc[["A,B"]], 0.75)
  expect_equal(cc[["A,C"]], 0.25)
  expect_error(morie_phylby_clade_credibility(list()), "no samples")
})

test_that("chain swaps are accepted with min(1, exp((b_j - b_k)(l_k - l_j)))", {
  expect_equal(morie_phylby_swap_acceptance(1, 0.5, -10, -12), exp(0.5 * -2), tolerance = 1e-15)
  expect_identical(morie_phylby_swap_acceptance(1, 0.5, -12, -10), 1)
  expect_identical(morie_phylby_swap_acceptance(1, 0, 0, 1e6), 1)
  expect_equal(morie_phylby_chain_temperature(3, 0.2), 1 / 1.6, tolerance = 1e-15)
  expect_match(morie_phylby_cheatsheet(), "Metropolis coupling", fixed = TRUE)
})
