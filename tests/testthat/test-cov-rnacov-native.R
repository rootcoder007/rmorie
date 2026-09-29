# RNA covariance scoring (Eddy & Durbin 1994; Rivas et al. 2017):
# column-pair counts and mutual information in bits with the
# Miller-Madow correction, dot-bracket parsing, and Nussinov folding
# checked against an independent first-base recursion.

rc_aln <- c("GGACUUCGUCC", "GAACUUCGUUC", "GCACUUCGUGC", "GUACU-CGUAC", "AGACUUCGUCU")
rc_pairs <- function(a, b) paste0(a, b) %in% c("AU", "UA", "CG", "GC", "GU", "UG")
rc_best <- function(s, ml = 3) {
  ch <- strsplit(s, "")[[1]]
  memo <- new.env()
  f <- function(i, j) {
    if (j - i <= ml) return(0)
    key <- paste(i, j)
    if (!is.null(memo[[key]])) return(memo[[key]])
    best <- f(i + 1, j)
    for (k in (i + ml + 1):j) {
      if (rc_pairs(ch[i], ch[k])) {
        inner <- if (k - 1 > i + 1) f(i + 1, k - 1) else 0
        rest <- if (k < j) f(k + 1, j) else 0
        best <- max(best, 1 + inner + rest)
      }
    }
    memo[[key]] <- best
    best
  }
  f(1, length(ch))
}

test_that("counts skip gaps and MI is the plug-in estimate in bits", {
  cc <- morie_rnacov_counts(rc_aln, 2, 10)
  a <- substr(rc_aln, 2, 2)
  b <- substr(rc_aln, 10, 10)
  expect_identical(cc$n, 5L)
  expect_identical(sum(cc$joint), 5L)
  tab <- table(a, b)
  p <- tab / sum(tab)
  pa <- rowSums(p)
  pb <- colSums(p)
  mi <- sum(ifelse(p > 0, p * log2(p / outer(pa, pb)), 0))
  expect_equal(morie_rnacov_mi(rc_aln, 2, 10)$mi, mi, tolerance = 1e-14)
  mm <- morie_rnacov_mi(rc_aln, 2, 10, "miller_madow")
  expect_equal(mm$mi, mi - (sum(tab > 0) - sum(pa > 0) - sum(pb > 0) + 1) / (2 * 5 * log(2)),
               tolerance = 1e-14)
  # a constant column carries no information whatever the other does
  expect_equal(morie_rnacov_mi(rc_aln, 4, 2)$mi, 0, tolerance = 1e-15)
  expect_identical(morie_rnacov_counts(rc_aln, 6, 1)$n, 4L)
  expect_identical(morie_rnacov_mi(c("-A", "-C"), 1, 2)$n, 0L)
  expect_error(morie_rnacov_mi(rc_aln, 2, 10, "jackknife"), "none or miller_madow")
})

test_that("dot-bracket parsing pairs matching brackets", {
  expect_identical(morie_rnacov_parse("((..<..>..))"), cbind(c(0L, 1L, 4L), c(11L, 10L, 7L)))
  expect_identical(dim(morie_rnacov_parse("....")), c(0L, 2L))
  expect_error(morie_rnacov_parse("(()"), "never closed")
  expect_error(morie_rnacov_parse("())"), "nothing to close")
  expect_error(morie_rnacov_parse("(x)"), "not dot-bracket")
})

test_that("Nussinov folding reaches the maximum number of non-crossing pairs", {
  for (s in c("GGGAAAUCC", "GCAUCUAUGC", "ACGUACGUACGU", "GGGGAAAACCCCUUUU")) {
    r <- morie_rnacov_nussinov(s)
    expect_equal(r$total, rc_best(s))
    ch <- strsplit(s, "")[[1]]
    P <- r$pairs + 1
    expect_identical(nrow(P), as.integer(r$total))
    if (nrow(P)) {
      expect_true(all(rc_pairs(ch[P[, 1]], ch[P[, 2]])))
      expect_true(all(P[, 2] - P[, 1] > 3))
      for (a in seq_len(nrow(P))) for (b in seq_len(nrow(P))) {
        crossing <- P[a, 1] < P[b, 1] && P[b, 1] < P[a, 2] && P[a, 2] < P[b, 2]
        expect_false(crossing)
      }
    }
  }
  expect_identical(morie_rnacov_nussinov("GAAC")$total, 0L)
  expect_identical(morie_rnacov_nussinov("GAAAC")$total, 1L)
})

test_that("morie_rnacov scores the pairs of a given or folded structure", {
  st <- "(((.....)))"
  r <- morie_rnacov(rc_aln, st)
  pr <- morie_rnacov_parse(st)
  mis <- vapply(seq_len(nrow(pr)), function(k) morie_rnacov_mi(rc_aln, pr[k, 1] + 1, pr[k, 2] + 1)$mi, 1)
  expect_equal(r$mutual_information, mis, tolerance = 1e-15)
  expect_equal(r$estimate, mean(mis), tolerance = 1e-15)
  expect_identical(r$covarying, which(mis > 0) - 1L)
  f <- morie_rnacov(rc_aln, mode = "nussinov")
  expect_identical(f$folded_pairs, morie_rnacov_nussinov(rc_aln[1])$total)
  expect_error(morie_rnacov(rc_aln, mode = "rnafold"), "mode must be")
  expect_error(morie_rnacov(rc_aln), "needs a structure")
  expect_error(morie_rnacov(c("ACGU", "ACG"), "(..)"), "same length")
  expect_error(morie_rnacov(rc_aln, "(.........)..)"), "never closed|nothing to close|outside")
  expect_match(morie_rnacov_cheatsheet(), "mutual information in bits", fixed = TRUE)
})
