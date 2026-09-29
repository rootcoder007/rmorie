# HMMER3 acceleration (Eddy 2011; Farrar 2007): striped layout, the MSV
# sum of ungapped segments on a hand-built profile, the Gumbel tail,
# sparse rescaling and the filter pipeline.

ph_prof <- rbind(c(5, -1), c(-1, 2), c(2, -1), c(-1, 2))

test_that("the striped layout visits position j q + i lane by lane", {
  s <- phmmsr_striped_layout(10, 4)
  q <- 3
  ref <- unlist(lapply(0:(q - 1), function(i) {
    p <- (0:3) * q + i
    p[p < 10]
  }))
  expect_identical(s$order, as.integer(ref))
  expect_identical(s$segments, 3L)
  expect_identical(sort(s$order), 0:9)
  expect_error(phmmsr_striped_layout(0), "positive")
})

test_that("MSV sums ungapped segments, each entered at log(tau)", {
  tau <- 0.02
  one <- phmmsr_msv_score(c(0, 1, 0, 1), ph_prof, tau = tau)$score
  # entering at position 1 costs log(tau) and pays 5 + 2 + 2 + 2
  expect_equal(one, 11 + log(tau), tolerance = 1e-14)
  two <- phmmsr_msv_score(c(0, 1, 0, 1, 0, 1, 0, 1), ph_prof, tau = tau)$score
  expect_equal(two, 2 * (11 + log(tau)), tolerance = 1e-14)
  # no positive emission anywhere: the score never leaves zero
  expect_identical(phmmsr_msv_score(c(0, 1), -abs(ph_prof))$score, 0)
  expect_error(phmmsr_msv_score(c(0, 1), matrix(numeric(0), 0, 2)), "empty")
})

test_that("the Gumbel tail is 1 - exp(-exp(-lambda (x - mu)))", {
  for (x in c(5, 10, 14)) {
    expect_equal(phmmsr_gumbel_pvalue(x, 10, 0.7), 1 - exp(-exp(-0.7 * (x - 10))), tolerance = 1e-15)
  }
  expect_identical(phmmsr_gumbel_pvalue(-2000, 10, 0.7), 1)
  expect_error(phmmsr_gumbel_pvalue(1, 0, 0), "lambda must be positive")
})

test_that("sparse rescaling fires only below the floor", {
  v <- c(1e-35, 4e-34, 2e-36)
  r <- phmmsr_sparse_rescale(v)
  expect_true(r$rescaled)
  expect_equal(r$values, v / 4e-34, tolerance = 1e-15)
  expect_equal(r$log_offset, log(1 / 4e-34), tolerance = 1e-15)
  k <- phmmsr_sparse_rescale(c(0.2, 0.5))
  expect_false(k$rescaled)
  expect_error(phmmsr_sparse_rescale(c(0, 0)), "underflowed")
  expect_error(phmmsr_sparse_rescale(numeric(0)), "nothing to rescale")
})

test_that("the pipeline passes sequences whose MSV p-value clears the threshold", {
  seqs <- list(c(0, 1, 0, 1, 0, 1, 0, 1), c(1, 1, 1), c(0, 1, 0, 1))
  r <- phmmsr_search_pipeline(seqs, ph_prof, msv_threshold = 0.3, mu = 5, lam = 0.7,
                              full_score = function(s, p) length(s))
  sc <- vapply(seqs, function(s) phmmsr_msv_score(s, ph_prof)$score, 1)
  pv <- 1 - exp(-exp(-0.7 * (sc - 5)))
  expect_identical(r$passed, which(pv <= 0.3) - 1L)
  expect_equal(vapply(r$msv_scores, `[[`, 1, 3), pv, tolerance = 1e-15)
  expect_equal(r$survivor_fraction, mean(pv <= 0.3))
  expect_equal(unlist(r$full_scores), vapply(seqs[pv <= 0.3], length, 1L), ignore_attr = TRUE)
  expect_identical(profile_hmm_search(seqs, ph_prof, 0.3, 5, 0.7)$passed, r$passed)
  expect_match(phmmsr_cheatsheet(), "P-VALUE", fixed = TRUE)
})
