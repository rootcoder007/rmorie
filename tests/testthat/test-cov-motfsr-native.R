# Coverage for MEME/MM motif discovery (Bailey & Elkan 1994). One EM
# iteration is replayed from its equations (E-step eq 4, lambda eq 5,
# letter frequencies eq 13 with pseudo-counts), the overlap
# normalisation, log-odds, Bayes threshold and scoring are recomputed,
# and the full search is checked on a planted motif.

.mseqs <- c("GCTATAAGCG", "ATTATACCGA", "CGGCTATAGT", "TATACGCATG")

test_that("alphabet, preparation, background and starting matrices", {
  expect_identical(motfsr_alphabet_of(.mseqs, NULL), c("A", "C", "G", "T"))
  expect_identical(motfsr_alphabet_of(.mseqs, c("T", "A", "C", "G")), c("T", "A", "C", "G"))
  expect_error(motfsr_alphabet_of(.mseqs, c("A", "A")), "repeated")
  pr <- motfsr_prepare(.mseqs, 4, NULL)
  expect_length(pr$starts, 4 * 7)
  expect_equal(pr$coded[[1]], match(strsplit(.mseqs[1], "")[[1]], c("A", "C", "G", "T")) - 1L)
  expect_error(motfsr_prepare(.mseqs, 11, NULL), "at least w = 11")
  expect_error(motfsr_prepare("ACGX", 2, c("A", "C", "G", "T")), "not in the alphabet")
  tab <- table(factor(strsplit(paste(.mseqs, collapse = ""), "")[[1]], levels = c("A", "C", "G", "T")))
  mu <- motfsr_mu(pr$coded, 4)
  expect_equal(mu, as.numeric(tab) / 40, tolerance = 1e-12)
  u <- motfsr_uniform_theta(3, 4, mu)
  expect_length(u, 4L)
  expect_true(all(vapply(u, function(r) isTRUE(all.equal(r, mu)), TRUE)))
  th <- motfsr_theta_from_subsequence(pr$coded, 1, 3, 4, 4, mu, 0.7)
  # the 1-based window starting at letter 3 of "GCTATAAGCG" is "TATA"
  expect_equal(th[[2]], c(0.1, 0.1, 0.1, 0.7), tolerance = 1e-12)
  expect_equal(th[[3]], c(0.7, 0.1, 0.1, 0.1), tolerance = 1e-12)
  expect_equal(motfsr_log_component(th, pr$coded, 1, 3, 4, 1L), 4 * log(0.7), tolerance = 1e-12)
  expect_equal(motfsr_log_component(th, pr$coded, 1, 3, 4, 2L), sum(log(mu[c(4, 1, 4, 1)])), tolerance = 1e-12)
  expect_identical(motfsr_log_component(list(mu, c(0, 1, 0, 0)), pr$coded, 1, 1, 1, 1L), -Inf)
})

test_that("overlap normalisation rescales the worst window to sum 1", {
  z <- list(c(0.6, 0.7, 0.2), c(0.3, 0.1))
  r <- motfsr_normalise_windows(z, 2)
  expect_equal(r[[1]], c(0.6 / 1.3, 0.7 / 1.3, 0.2), tolerance = 1e-12)
  expect_equal(r[[2]], z[[2]])
  expect_identical(motfsr_normalise_windows(z, 1), z)
  set.seed(1)
  zz <- list(stats::runif(9), stats::runif(6))
  rr <- motfsr_normalise_windows(zz, 3)
  for (row in rr) for (j in seq_len(length(row) - 2)) expect_lte(sum(row[j:(j + 2)]), 1 + 1e-12)
})

test_that("one EM iteration replays eqs (4), (5) and (13)", {
  w <- 3
  pr <- motfsr_prepare(.mseqs, w, NULL)
  mu <- motfsr_mu(pr$coded, 4)
  th0 <- motfsr_theta_from_subsequence(pr$coded, 1, 3, w, 4, mu, 0.6)
  lam <- 0.1
  fit <- morie_motfsr_mm_fit(.mseqs, w, NULL, th0, lam, beta = 0.05, max_iter = 1, normalize_overlaps = FALSE)
  z <- list()
  ll <- 0
  cnt <- matrix(0, w + 1, 4)
  for (s in pr$starts) {
    x <- pr$coded[[s[1]]][s[2] + 0:(w - 1)] + 1
    p1 <- lam * prod(vapply(1:w, function(t) th0[[t + 1]][x[t]], 1))
    p0 <- (1 - lam) * prod(mu[x])
    zi <- p1 / (p1 + p0)
    z[[length(z) + 1]] <- zi
    ll <- ll + log(p1 + p0)
    for (t in 1:w) {
      cnt[t + 1, x[t]] <- cnt[t + 1, x[t]] + zi
      cnt[1, x[t]] <- cnt[1, x[t]] + 1 - zi
    }
  }
  expect_equal(unlist(fit$z), unlist(z), tolerance = 1e-12)
  expect_equal(fit$log_likelihood, ll, tolerance = 1e-12)
  expect_equal(fit$lambda1, mean(unlist(z)), tolerance = 1e-12)
  newth <- lapply(1:(w + 1), function(r) (cnt[r, ] + 0.05 * mu) / (sum(cnt[r, ]) + 0.05))
  expect_equal(fit$theta, newth, tolerance = 1e-12)
  expect_equal(fit$background, newth[[1]], tolerance = 1e-12)
  rs <- motfsr_mm_fit(.mseqs, w, NULL, th0, lam, beta = 0.05, max_iter = 1, normalize_overlaps = FALSE)
  expect_equal(rs$theta, fit$theta, tolerance = 1e-12)
  er <- lapply(pr$coded, function(r) rep(0.5, length(r)))
  fe <- morie_motfsr_mm_fit(.mseqs, w, NULL, th0, lam, beta = 0.05, erasing = er, max_iter = 1, normalize_overlaps = FALSE)
  cnt2 <- cnt
  cnt2[-1, ] <- cnt[-1, ] * 0.5
  expect_equal(fe$motif, lapply(2:(w + 1), function(r) (cnt2[r, ] + 0.05 * mu) / (sum(cnt2[r, ]) + 0.05)), tolerance = 1e-12)
  fs <- morie_motfsr_mm_fit(.mseqs, w, NULL, th0, lam, beta = 0.05, erasing = er, max_iter = 1, normalize_overlaps = FALSE, erase_by = "start")
  expect_equal(fs$motif, fe$motif, tolerance = 1e-12)
  full <- morie_motfsr_mm_fit(.mseqs, w, NULL, th0, lam, max_iter = 500, tol = 1e-10)
  expect_true(full$converged)
  expect_equal(vapply(full$theta, sum, 1), rep(1, w + 1), tolerance = 1e-12)
  expect_error(morie_motfsr_mm_fit(.mseqs, w, lambda0 = 1), "lambda0 must lie")
  expect_error(morie_motfsr_mm_fit(.mseqs, w, erase_by = "site"), "erase_by")
  expect_error(motfsr_mm_fit(.mseqs, w, theta0 = list(1)), "theta0 must be")
})

test_that("log-odds, Bayes threshold, scoring and the lambda grid", {
  mo <- list(c(0.7, 0.1, 0.1, 0.1), c(0, 0.5, 0.25, 0.25))
  bg <- c(0.25, 0.25, 0.5, 0)
  lo <- morie_motfsr_log_odds_matrix(mo, bg)
  expect_equal(lo[[1]], c(log(0.7 / 0.25), log(0.1 / 0.25), log(0.1 / 0.5), Inf))
  expect_equal(lo[[2]][1:3], c(-Inf, log(2), log(0.5)))
  expect_equal(motfsr_log_odds_matrix(mo, bg), lo)
  expect_equal(morie_motfsr_bayes_threshold(0.2), log(4), tolerance = 1e-12)
  loss <- list(c(0, 3), c(2, 0))
  expect_equal(morie_motfsr_bayes_threshold(0.2, loss), log(4) + log(3 / 2), tolerance = 1e-12)
  expect_equal(motfsr_bayes_threshold(0.2, loss), log(4) + log(3 / 2), tolerance = 1e-12)
  expect_error(morie_motfsr_bayes_threshold(0), "lambda1 must lie")
  expect_error(morie_motfsr_bayes_threshold(0.2, list(c(0, 0), c(1, 0))), "r12 > r22")
  spec <- list(c(1, -1, 0.5, 0), c(0.2, 0.3, -2, 1))
  sc <- morie_motfsr_score_sequence(spec, "ACGT", c("A", "C", "G", "T"))
  expect_equal(sc, c(1 + 0.3, -1 - 2, 0.5 + 1), tolerance = 1e-12)
  h <- morie_motfsr_score_sequence(spec, "ACGT", c("A", "C", "G", "T"), threshold = 1)
  expect_identical(h$hits, c(0L, 2L))
  expect_equal(motfsr_score_sequence(spec, "ACGT", c("A", "C", "G", "T")), sc)
  expect_error(morie_motfsr_score_sequence(spec, "ACGU", c("A", "C", "G", "T")), "not in the alphabet")
  g <- motfsr_lambda_grid(100, 4, 5, NULL)
  expect_equal(g, c(0.02, 0.04, 0.08, 0.1), tolerance = 1e-12)
  expect_identical(motfsr_lambda_grid(100, 4, 5, 0.3), 0.3)
})

test_that("the full search finds a planted TATA motif and erases it between passes", {
  seqs <- c(.mseqs, "AC")
  r <- morie_motfsr(seqs, 4, n_motifs = 2, max_starts = 20)
  expect_identical(r$motifs[[1]]$consensus, "TATA")
  sc <- r$motifs[[1]]$sites
  expect_true(all(vapply(sc, function(s) s[3] >= r$motifs[[1]]$threshold, TRUE)))
  expect_false(is.unsorted(-vapply(sc, function(s) s[3], 1)))
  expect_false(anyNA(unlist(r$erasing)))
  expect_equal(r$erasing[[5]], c(1, 1))
  expect_true(any(unlist(r$erasing) < 1))
  rr <- motfsr_run(seqs, 4, n_motifs = 2, max_starts = 20)
  expect_identical(rr$motifs[[1]]$consensus, "TATA")
  expect_false(anyNA(unlist(rr$erasing)))
  expect_identical(morie_motfsr_motif_meme, morie_motfsr)
  un <- morie_motfsr(.mseqs, 4, starts = "uniform", start_scoring = "none", max_iter = 50)
  expect_length(un$motifs, 1L)
  expect_error(morie_motfsr(.mseqs, 4, starts = "random"), "starts must be")
  expect_error(morie_motfsr(.mseqs, 4, start_weight = 1), "start_weight")
  expect_match(motfsr_cheatsheet(), "Bailey & Elkan")
})
