# Coverage for BLAST maximal segment pairs (Altschul et al. 1990) and the
# Karlin-Altschul statistics. The exact MSP is compared with a
# brute-force search over every diagonal segment, word hits with direct
# enumeration, X-drop extension with its X = Inf closed form, lambda with
# stats::uniroot, the p-value with stats::ppois, and the Gumbel fit with
# a least-squares refit of its own simulated scores.

.sc <- function(a, b) if (a == b) 5 else -4
.brute_msp <- function(q, s) {
  qa <- strsplit(q, "")[[1]]
  sa <- strsplit(s, "")[[1]]
  best <- 0
  for (i in seq_along(qa)) for (j in seq_along(sa)) {
    run <- 0
    k <- 0
    while (i + k <= length(qa) && j + k <= length(sa)) {
      run <- run + .sc(qa[i + k], sa[j + k])
      best <- max(best, run)
      k <- k + 1
    }
  }
  best
}

test_that("the exact MSP equals a brute-force search over diagonal segments", {
  for (pr in list(c("ACGTTGCA", "TTACGTTGA"), c("AAAA", "CCCC"), c("GATTACA", "ATTAC"))) {
    r <- morie_msp_exact(pr[1], pr[2])
    expect_equal(r$score, .brute_msp(pr[1], pr[2]))
    if (r$score > 0) {
      qs <- substr(pr[1], r$qstart + 1, r$qstart + r$length)
      ss <- substr(pr[2], r$sstart + 1, r$sstart + r$length)
      expect_equal(sum(mapply(.sc, strsplit(qs, "")[[1]], strsplit(ss, "")[[1]])), r$score)
    }
  }
  M <- matrix(c(2, -1, -1, 3), 2)
  mm <- morie_msp_exact("ABBA", "BBAB", matrix = M, alphabet = "AB")
  expect_equal(mm$score, 3 + 3 + 2)
})

test_that("word hits enumerate exact and neighbourhood matches", {
  h <- morie_word_hits("ACGTAC", "TACGT", 3)
  ref <- list()
  for (i in 1:4) for (j in 1:3) if (substr("ACGTAC", i, i + 2) == substr("TACGT", j, j + 2)) ref[[length(ref) + 1]] <- c(i, j)
  expect_setequal(lapply(h, as.numeric), lapply(ref, as.numeric))
  nb <- morie_word_hits("ACG", "ACT", 2, mode = "neighborhood", threshold = 1)
  expect_setequal(lapply(nb, as.numeric), list(c(1, 1), c(2, 2)))
  expect_length(morie_word_hits("AC", "ACGT", 3), 0L)
  expect_error(morie_word_hits("ACG", "ACG", 2, mode = "spaced"), "exact' or 'neighborhood")
  expect_error(morie_word_hits("ACG", "ACG", 2, mode = "neighborhood"), "needs a threshold")
})

test_that("X-drop extension reaches the best prefix sums when X is infinite", {
  q <- strsplit("GGACGTTTAGCC", "")[[1]]
  s <- strsplit("CGACGTTTACCC", "")[[1]]
  e <- extend_one(q, s, 2, 2, 4, .sc, Inf)
  sc <- mapply(.sc, q, s)
  word <- sum(sc[3:6])
  right <- max(0, cumsum(sc[7:12]))
  left <- max(0, cumsum(sc[2:1]))
  expect_equal(e$score, word + right + left)
  e0 <- extend_one(q, s, 2, 2, 4, .sc, 0)
  expect_lte(e0$score, e$score)
})

test_that("BLAST finds a planted segment and scores it by Karlin-Altschul", {
  seg <- "ACGTTGCATGCA"
  r <- morie_blstn(paste0("TTTT", seg, "GGG"), c(paste0("CCCCC", seg, "AA"), "GGGGGGGGGGGG"), w = 8)
  expect_identical(r$n_hsps, 1L)
  h <- r$hsps[[1]]
  expect_identical(h$subject, 1L)
  expect_equal(h$score, 5 * 12)
  expect_equal(c(h$qstart, h$sstart, h$length, h$identities), c(4, 5, 12, 12))
  expect_equal(h$pvalue, morie_blast_pvalue(60, 19, 19, r$lam, r$K), tolerance = 1e-12)
  expect_identical(morie_blast(paste0("TTTT", seg, "GGG"), paste0("CCCCC", seg, "AA"), w = 8)$best_score, 60)
  expect_identical(morie_blast_nucleotide, morie_blstn)
  expect_error(morie_blstn("", "ACGT"), "query must be non-empty")
  expect_error(morie_blstn("ACGT", "ACGT", X = -1), "X must be >= 0")
})

test_that("score distribution, lambda, lattice span and Karlin-Altschul K", {
  d <- morie_score_distribution(5, -4)
  expect_equal(d[["5"]], 0.25, tolerance = 1e-12)
  expect_equal(d[["-4"]], 0.75, tolerance = 1e-12)
  d2 <- morie_score_distribution(1, -2, letter_probs = c(0.1, 0.2, 0.3, 0.4))
  expect_equal(d2[["1"]], sum(c(0.1, 0.2, 0.3, 0.4)^2), tolerance = 1e-12)
  lam <- lambda_star(d)
  ref <- stats::uniroot(function(l) 0.25 * exp(5 * l) + 0.75 * exp(-4 * l) - 1, c(1e-6, 5), tol = 1e-15)$root
  expect_equal(lam, ref, tolerance = 1e-10)
  expect_equal(0.25 * exp(5 * lam) + 0.75 * exp(-4 * lam), 1, tolerance = 1e-12)
  expect_error(lambda_star(list("1" = 0.6, "-1" = 0.4)), "must be negative")
  expect_identical(lattice_check(4.0000000001), 4L)
  expect_error(lattice_check(2.5), "integer lattice")
  expect_equal(gcd_span(c(6, -4, 10)), 2)
  expect_equal(gcd_span(integer(0)), 1)
  ka <- morie_karlin_altschul(match = 5, mismatch = -4)
  expect_equal(ka$lam, lam, tolerance = 1e-12)
  expect_equal(ka$delta, 1)
  x <- ka$lam * ka$delta
  expect_equal(ka$K_upper, ka$C * x / (1 - exp(-x)), tolerance = 1e-12)
  expect_equal(ka$K_lower, ka$C * x / (exp(x) - 1), tolerance = 1e-12)
  expect_equal(ka$K_upper / ka$K_lower, exp(x), tolerance = 1e-12)
  expect_equal(morie_karlin_altschul(match = 5, mismatch = -4, bound = "mid")$K, (ka$K_upper + ka$K_lower) / 2, tolerance = 1e-12)
  expect_equal(ka$mean_score, 5 * 0.25 - 4 * 0.75, tolerance = 1e-12)
  k2 <- morie_karlin_altschul(match = 10, mismatch = -8)
  expect_equal(k2$lam, lam / 2, tolerance = 1e-10)
  expect_equal(k2$delta, 2)
  expect_error(morie_karlin_altschul(bound = "exact"), "bound must be")
})

test_that("the BLAST p-value is the Poisson tail and the Gumbel fit is least squares", {
  y <- 0.3 * 100 * 200 * exp(-0.2 * 60)
  expect_equal(morie_blast_pvalue(60, 100, 200, 0.2, 0.3), 1 - exp(-y), tolerance = 1e-12)
  expect_equal(morie_blast_pvalue(60, 100, 200, 0.2, 0.3, c = 3), stats::ppois(2, y, lower.tail = FALSE), tolerance = 1e-12)
  expect_error(morie_blast_pvalue(1, 1, 1, 0, 1), "must be positive")
  # a highly significant hit keeps its tiny p-value instead of rounding to 0
  expect_equal(morie_blast_pvalue(400, 100, 200, 0.2, 0.3), 6000 * exp(-80), tolerance = 1e-12)
  g <- morie_estimate_gumbel(20, 20, c(1, 1, 1, 1), n_sim = 60, seed = 3)
  s <- g$scores
  expect_false(is.unsorted(s))
  r <- 12:54
  fit <- stats::lm(log(-log(r / 61)) ~ s[r])
  expect_equal(g$lam, -unname(stats::coef(fit)[2]), tolerance = 1e-10)
  expect_equal(g$K, exp(unname(stats::coef(fit)[1])) / 400, tolerance = 1e-10)
  expect_identical(g, morie_estimate_gumbel(20, 20, c(1, 1, 1, 1), n_sim = 60, seed = 3))
  expect_error(morie_estimate_gumbel(5, 5, c(0, 0)), "must be positive")
})
