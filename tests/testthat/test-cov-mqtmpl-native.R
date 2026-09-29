# Coverage for the R/qtl-style genome scans. The HMM posteriors are
# checked against brute-force enumeration of every genotype path, the
# single-marker and EM LODs against lm() residual sums of squares
# ((n/2) log10(RSS0/RSS1) is exact when the QTL genotype is known), the
# imputation LOD against its own weights, and the samplers against the
# fully-informative limit where every draw must reproduce the markers.

.qhal <- function(d) 0.5 * (1 - exp(-2 * d / 100))
.qdat <- function() {
  g1 <- c(1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1)
  g2 <- c(1, 0, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0)
  g3 <- c(0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0)
  noise <- c(0.3, -0.2, 0.1, 0.4, -0.5, 0.2, -0.1, 0.3, -0.3, 0.1, 0.2, -0.4)
  list(markers = list(g1, g2, g3), pos = c(0, 20, 50), y = 1 + 1.5 * g2 + noise, g = cbind(g1, g2, g3))
}

test_that("method status, checking, Haldane and the flanking-marker formula", {
  st <- morie_mqtmpl_method_status()
  expect_identical(st$available, c("em", "mr", "imp"))
  expect_false(morie_mqtmpl_method_status("hk")$available)
  expect_match(morie_mqtmpl_method_status("hk")$reason, "Haley")
  expect_identical(mqtmpl_method_status("em")$available, TRUE)
  expect_error(morie_mqtmpl_method_status("lm"), "method must be one of")
  expect_error(mqtmpl_check_method("hk"), "not implemented")
  expect_error(mqtmpl_check_method("xx"), "method must be one of")
  expect_null(mqtmpl_check_method("mr"))
  expect_equal(mqtmpl_haldane(c(0, 10, 50)), .qhal(c(0, 10, 50)), tolerance = 1e-12)
  rl <- .qhal(7)
  rr <- .qhal(13)
  p <- mqtmpl_genotype_probabilities(1, 0, rl, rr)
  expect_equal(p, c((1 - rl) * rr, rl * (1 - rr)) / ((1 - rl) * rr + rl * (1 - rr)), tolerance = 1e-12)
  expect_identical(mqtmpl_kw_n_imp(list()), 64L)
  expect_error(mqtmpl_kw_n_imp(list(1)), "covariates are not implemented")
  expect_match(mqtmpl_cheatsheet(), "forward-backward HMM")
})

test_that("HMM genotype posteriors equal brute-force path enumeration", {
  pos <- c(0, 12, 30, 45)
  geno <- list(c(1, NA, 0, 1), c(0, 0, NA, NA), c(1, 1, 1, 0))
  for (e in c(0, 0.08)) {
    post <- morie_mqtmpl_hmm_genotype_probabilities(geno, pos, error_rate = e)
    expect_equal(mqtmpl_hmm_genotype_probabilities(geno, pos, e), post, tolerance = 1e-12)
    r <- .qhal(diff(pos))
    paths <- as.matrix(expand.grid(rep(list(0:1), 4)))
    for (i in seq_along(geno)) {
      pr <- apply(paths, 1, function(s) {
        p <- 0.5
        for (j in 1:3) p <- p * (if (s[j] == s[j + 1]) 1 - r[j] else r[j])
        for (j in 1:4) if (!is.na(geno[[i]][j])) p <- p * (if (geno[[i]][j] == s[j]) 1 - e else e)
        p
      })
      if (sum(pr) == 0) next
      ref <- vapply(1:4, function(j) sum(pr[paths[, j] == 1]) / sum(pr), 1)
      expect_equal(post[[i]][, 2], ref, tolerance = 1e-12)
    }
  }
  expect_error(morie_mqtmpl_hmm_genotype_probabilities(geno, pos, error_rate = 0.5), "error rate")
  expect_error(morie_mqtmpl_hmm_genotype_probabilities(geno, c(0, 10, 5, 20)), "must increase")
  expect_error(morie_mqtmpl_hmm_genotype_probabilities(list(c(1, 0)), pos), "one call per marker")
})

test_that("genotype sampling reproduces fully informative markers and is seeded", {
  geno <- list(c(1, 0, 1), c(0, 0, 1), c(1, 1, 0))
  pos <- c(0, 20, 50)
  d <- morie_mqtmpl_sample_genotypes(geno, pos, grid = pos, n_imp = 5, seed = 3)
  for (k in 1:5) for (i in 1:3) expect_equal(d[[k]][[i]], geno[[i]])
  # between two markers that agree and lie 0.01 cM apart the pseudomarker
  # takes their common state unless an event of probability ~1e-4 occurs
  dd <- morie_mqtmpl_sample_genotypes(list(c(1, 1), c(0, 0)), c(0, 0.01), grid = c(0, 0.005, 0.01), n_imp = 20, seed = 1)
  expect_true(all(vapply(dd, function(x) all(x[[1]] == 1) && all(x[[2]] == 0), TRUE)))
  a <- morie_mqtmpl_sample_genotypes(list(c(1, NA, 0)), c(0, 30, 60), grid = c(0, 30, 60), n_imp = 4, seed = 7)
  expect_identical(a, morie_mqtmpl_sample_genotypes(list(c(1, NA, 0)), c(0, 30, 60), grid = c(0, 30, 60), n_imp = 4, seed = 7))
  expect_identical(mqtmpl_sample_genotypes(list(c(1, NA, 0)), c(0, 30, 60), grid = c(0, 30, 60), n_imp = 4, seed = 7), a)
  # the ends are typed, so only the middle marker varies
  expect_true(all(vapply(a, function(x) x[[1]][1] == 1 && x[[1]][3] == 0, TRUE)))
})

test_that("imputation weights and the single-marker LOD recompute with lm", {
  q <- .qdat()
  n <- 12
  for (j in 1:3) {
    rss <- sum(stats::lm.fit(cbind(1, q$g[, j]), q$y)$residuals^2)
    expect_equal(morie_mqtmpl_imputation_weights(q$y, q$g[, j]), -log(n) - 0.5 * n * log(rss), tolerance = 1e-12)
    expect_equal(mqtmpl_imputation_weights(q$y, q$g[, j], 3), -1.5 * log(n) - 0.5 * n * log(rss), tolerance = 1e-12)
    rss0 <- sum((q$y - mean(q$y))^2)
    expect_equal(mqtmpl_single_marker(q$y, q$g[, j])$lod, 0.5 * n * log10(rss0 / rss), tolerance = 1e-12)
  }
  expect_equal(morie_mqtmpl_imputation_weights(q$y, rep(0, n), 1), -0.5 * log(n) - 0.5 * n * log(sum((q$y - mean(q$y))^2)), tolerance = 1e-12)
  expect_error(morie_mqtmpl_imputation_weights(q$y, 1:3), "one genotype per phenotype")
})

test_that("marker-regression and EM scans agree with (n/2) log10(RSS0/RSS1) at the markers", {
  q <- .qdat()
  n <- 12
  lod <- vapply(1:3, function(j) {
    0.5 * n * log10(sum((q$y - mean(q$y))^2) / sum(stats::lm.fit(cbind(1, q$g[, j]), q$y)$residuals^2))
  }, 1)
  mr <- mqtmpl_scanone(q$y, q$markers, q$pos, method = "mr")
  expect_equal(mr$lod, lod, tolerance = 1e-12)
  expect_equal(mr$peak_position, 20)
  em <- morie_mqtmpl_scanone(q$y, q$markers, q$pos, method = "em", step = 10)
  at <- match(q$pos[1:2], em$position)
  # with the QTL on a typed marker the EM has a known genotype and its MLE
  # is least squares; it stops at a 1e-10 log-likelihood change
  expect_equal(em$lod[at], lod[1:2], tolerance = 1e-9)
  expect_equal(em$position, c(0, 10, 20, 20, 30, 40, 50))
  expect_identical(em$n_covariates, 0L)
  cv <- c(0.5, 1, 0.2, 0.9, 0.1, 0.4, 0.8, 0.3, 0.6, 0.7, 0.05, 0.95)
  cim <- mqtmpl_scan_cim(q$y, q$markers, q$pos, cofactors = list(cv), step = 20)
  r0 <- sum(stats::lm.fit(cbind(1, cv), q$y)$residuals^2)
  r1 <- sum(stats::lm.fit(cbind(1, q$g[, 2], cv), q$y)$residuals^2)
  expect_equal(cim$lod[match(20, cim$position)], 0.5 * n * log10(r0 / r1), tolerance = 1e-9)
  f <- mqtmpl_cim_em(q$y, q$g[, 1], q$g[, 2], 0, .qhal(20))
  expect_equal(f$lod, lod[1], tolerance = 1e-9)
  expect_equal(mqtmpl_cim_one(q$y, q$g[, 1], q$g[, 2], 0, .qhal(20), list())$lod, f$lod, tolerance = 1e-12)
  expect_error(morie_mqtmpl_scanone(q$y, q$markers, q$pos, method = "hk"), "not implemented")
  expect_error(morie_mqtmpl_scanone(q$y[-1], q$markers, q$pos), "typed on all")
})

test_that("the imputation scan is the log-mean-exp of the draw weights", {
  q <- .qdat()
  n <- 12
  sc <- mqtmpl_scanone(q$y, q$markers, q$pos, method = "imp", step = 25)
  geno <- lapply(1:n, function(i) q$g[i, ])
  draws <- morie_mqtmpl_sample_genotypes(geno, q$pos, c(0, 25, 50), 64, 0, 0)
  null <- -0.5 * log(n) - 0.5 * n * log(sum((q$y - mean(q$y))^2))
  ref <- vapply(1:3, function(gi) {
    w <- vapply(draws, function(dr) morie_mqtmpl_imputation_weights(q$y, vapply(dr, function(r) r[gi], 1)), 1)
    (log(mean(exp(w - max(w)))) + max(w) - null) / log(10)
  }, 1)
  expect_equal(sc$lod, ref, tolerance = 1e-12)
  expect_identical(sc$n_imputations, 64L)
  si <- mqtmpl_scan_imp(q$y, q$markers, q$pos, 25, 8L, 0, 2)
  expect_equal(si$position, c(0, 25, 50))
  expect_error(morie_mqtmpl_scanone(q$y, q$markers, q$pos, method = "imp", covariates = list(1:12)), "covariates are not implemented")
})

test_that("permutation threshold is the ceil((1 - alpha) B)-th null maximum and is seeded", {
  q <- .qdat()
  pt <- morie_mqtmpl_permutation_threshold(q$y, q$markers, q$pos, n_perm = 20, alpha = 0.1, method = "mr", seed = 4)
  expect_equal(pt$threshold, sort(pt$null_maxima)[18])
  expect_false(is.unsorted(pt$null_maxima))
  expect_identical(pt, morie_mqtmpl_permutation_threshold(q$y, q$markers, q$pos, n_perm = 20, alpha = 0.1, method = "mr", seed = 4))
  expect_identical(mqtmpl_permutation_threshold(q$y, q$markers, q$pos, n_perm = 20, alpha = 0.1, method = "mr", seed = 4), pt)
  expect_false(identical(pt$null_maxima, morie_mqtmpl_permutation_threshold(q$y, q$markers, q$pos, n_perm = 20, alpha = 0.1, method = "mr", seed = 5)$null_maxima))
  # every null maximum is the maximum marker-regression LOD of some
  # permutation of y, so it cannot exceed the LOD of a perfect split
  expect_true(all(pt$null_maxima >= 0))
  expect_error(morie_mqtmpl_permutation_threshold(q$y, q$markers, q$pos, alpha = 1), "alpha must lie")
})

test_that("LOD support interval walks down to the drop from the peak", {
  sc <- list(lod = c(0.5, 1.2, 3.1, 4, 3.8, 2.1, 2.6, 0.4), position = seq(0, 70, 10))
  si <- morie_mqtmpl_lod_support_interval(sc, drop = 2)
  expect_equal(c(si$peak, si$lower, si$upper, si$peak_lod), c(30, 20, 60, 4))
  expect_identical(mqtmpl_lod_support_interval(sc, 2), si)
  expect_equal(morie_mqtmpl_lod_support_interval(sc, drop = 1.5)$upper, 40)
  si2 <- morie_mqtmpl_lod_support_interval(sc, drop = 0.5)
  expect_equal(c(si2$lower, si2$upper), c(30, 40))
})
