# Coverage for the native MAFFT (Katoh et al. 2002). Residue vectors and
# the similarity matrix are rebuilt from their equations, the FFT
# correlation against its direct circular sum, the JTT PAM matrix against
# Matrix::expm of its generator, the guide tree against stats::hclust
# (average linkage), and the alignments against the invariants every
# alignment must keep (degapping recovers the input, WSP is recomputed).

.vhat <- function() {
  v <- c(A = 31, R = 124, N = 56, D = 54, C = 55, Q = 85, E = 83, G = 3, H = 96, I = 111,
         L = 111, K = 119, M = 105, F = 132, P = 32.5, S = 32, T = 61, W = 170, Y = 136, V = 84)
  (v - mean(v)) / sqrt(mean((v - mean(v))^2))
}
.phat <- function() {
  p <- c(A = 8.1, R = 10.5, N = 11.6, D = 13, C = 5.5, Q = 10.5, E = 12.3, G = 9, H = 10.4, I = 5.2,
         L = 4.9, K = 11.3, M = 5.7, F = 5.2, P = 8, S = 9.2, T = 8.6, W = 5.4, Y = 6.2, V = 5.9)
  (p - mean(p)) / sqrt(mean((p - mean(p))^2))
}
.aa <- strsplit("ARNDCQEGHILKMFPSTWYV", "")[[1]]

test_that("residue_vectors are weighted Grantham volume/polarity (aa) or base counts (nt)", {
  g <- c("ACD-W", "RC-EW")
  w <- c(0.3, 0.7)
  r <- residue_vectors(g, w)
  vh <- .vhat()
  ph <- .phat()
  ch <- strsplit(g, "")
  val <- function(x, tab) if (x %in% names(tab)) tab[[x]] else 0
  expect_equal(r$vol, vapply(1:5, function(n) 0.3 * val(ch[[1]][n], vh) + 0.7 * val(ch[[2]][n], vh), 1), tolerance = 1e-12)
  expect_equal(r$pol, vapply(1:5, function(n) 0.3 * val(ch[[1]][n], ph) + 0.7 * val(ch[[2]][n], ph), 1), tolerance = 1e-12)
  nt <- residue_vectors(c("ACGT", "AAGT"), seq_type = "nt")
  expect_equal(nt$A, c(1, 0.5, 0, 0))
  expect_equal(nt$C, c(0, 0.5, 0, 0))
  expect_error(residue_vectors(c("AC", "A")), "same length")
  expect_error(residue_vectors("AC", weights = c(1, 2)), "one weight")
})

test_that("FFT correlation equals the direct circular sum of eq (2)", {
  a <- c(0.5, -1, 2, 0.3)
  b <- c(1, 0.2, -0.7, 0.4, 1.5)
  f <- mafft_xcorr_fft(a, b)
  expect_identical(f$size, 16L)
  ref <- vapply(0:15, function(k) {
    j <- (0:3 + k) %% 16
    sum(ifelse(j < 5, a * b[pmin(j, 4) + 1], 0))
  }, 1)
  expect_equal(f$c, ref, tolerance = 1e-12)
  expect_equal(mafft_xcorr_direct(a, b, 16L), ref, tolerance = 1e-12)
  cr <- correlation(c("ACDEF"), c("CDEFG"))
  cd <- correlation(c("ACDEF"), c("CDEFG"), method = "direct")
  expect_equal(cr$c, cd$c, tolerance = 1e-12)
  expect_equal(cr$lags, c(0:7, -8:-1))
  v1 <- residue_vectors("ACDEF")
  v2 <- residue_vectors("CDEFG")
  expect_equal(cr$c[2], sum(v1$vol[1:4] * v2$vol[2:5]) + sum(v1$pol[1:4] * v2$pol[2:5]), tolerance = 1e-12)
  expect_equal(mafft_peaks(cr$lags, cr$c, 3), cr$lags[order(-cr$c)][1:3])
  expect_error(correlation("A", "C", method = "ntt"), "fft' or 'direct")
})

test_that("jtt_matrix is exp(Q t) of the normalised JTT generator", {
  skip_if_not_installed("Matrix")
  ex <- mafft_jtt_exchangeability()
  expect_equal(ex$S, t(ex$S))
  expect_equal(ex$S[2, 1], 247 / (400 * 0.051 * 0.077), tolerance = 1e-12)
  expect_equal(ex$S[20, 19], 42 / (400 * 0.066 * 0.032), tolerance = 1e-12)
  j <- jtt_matrix(pam = 120)
  f <- ex$f
  Q <- ex$S * rep(f, each = 20)
  diag(Q) <- 0
  diag(Q) <- -rowSums(Q)
  Q <- Q / (-sum(f * diag(Q)) * 100)
  expect_equal(j$Q, Q, tolerance = 1e-12)
  expect_equal(j$rate, 0.01, tolerance = 1e-12)
  P <- as.matrix(Matrix::expm(Matrix::Matrix(Q * 120)))
  expect_equal(j$P, P, tolerance = 1e-10)
  expect_equal(rowSums(j$P), rep(1, 20), tolerance = 1e-12)
  expect_equal(f * j$P, t(f * j$P), tolerance = 1e-12)
  expect_equal(j$matrix[["W|R"]], 10 * log10(j$P[18, 2] / f[2]), tolerance = 1e-12)
  expect_error(jtt_matrix(0), "pam must be positive")
})

test_that("normalized_similarity_matrix implements eq (7) and the all-positive control", {
  raw <- list()
  for (a in c("A", "C", "G", "T")) for (b in c("A", "C", "G", "T")) raw[[paste(a, b, sep = "|")]] <- if (a == b) 2 else if (a < b) -1 else -0.5
  fr <- c(A = 0.1, C = 0.2, G = 0.3, T = 0.4)
  r <- normalized_similarity_matrix(raw, as.list(fr), s_a = 0.06, seq_type = "nt")
  M <- matrix(unlist(raw), 4, 4, byrow = TRUE)
  a1 <- sum(fr * diag(M))
  a2 <- sum(outer(fr, fr) * M)
  expect_equal(c(r$average1, r$average2), c(a1, a2), tolerance = 1e-12)
  Mn <- matrix(unlist(r$matrix), 4, 4, byrow = TRUE)
  expect_equal(Mn, (M - a2) / (a1 - a2) + 0.06, tolerance = 1e-12)
  # a random pair scores S_a on average, an identity 1 + S_a
  expect_equal(sum(outer(fr, fr) * Mn), 0.06, tolerance = 1e-12)
  expect_equal(sum(fr * diag(Mn)), 1.06, tolerance = 1e-12)
  ap <- normalized_similarity_matrix(raw, as.list(fr), seq_type = "nt", mode = "all_positive")
  expect_equal(min(unlist(ap$matrix)), 0, tolerance = 1e-12)
  expect_equal(ap$s_a, -min((M - a2) / (a1 - a2)), tolerance = 1e-12)
  aa <- normalized_similarity_matrix()
  jf <- unlist(jtt_matrix(200)$freqs, use.names = FALSE)
  expect_equal(unlist(aa$freqs, use.names = FALSE), jf / sum(jf), tolerance = 1e-12)
  flat <- lapply(raw, function(v) 1)
  expect_error(normalized_similarity_matrix(flat, seq_type = "nt"), "no signal")
  expect_error(normalized_similarity_matrix(raw[-1], seq_type = "nt"), "missing")
  expect_error(normalized_similarity_matrix(mode = "raw"), "mode must be one of")
})

test_that("mafft_default_raw, mafft_lookup and mafft_site_score", {
  nt <- mafft_default_raw("nt")
  expect_length(nt$M, 16L)
  expect_identical(mafft_lookup(nt$M, "G", "G"), 1)
  expect_identical(mafft_lookup(nt$M, "G", "T"), -1)
  expect_identical(mafft_lookup(nt$M, "G", "X"), 0)
  gr <- mafft_default_raw("aa", "grantham")
  vh <- .vhat()
  ph <- .phat()
  expect_length(gr$M, 400L)
  expect_equal(mafft_lookup(gr$M, "W", "G"), -((vh[["W"]] - vh[["G"]])^2 + (ph[["W"]] - ph[["G"]])^2), tolerance = 1e-12)
  jt <- mafft_default_raw("aa")
  expect_equal(mafft_lookup(jt$M, "L", "I"), jtt_matrix(200)$matrix[["L|I"]], tolerance = 1e-12)
  sc <- normalized_similarity_matrix()
  ga <- c("AC-", "WCD")
  gb <- c("A-D", "GCD", "ACE")
  wa <- c(0.4, 0.6)
  wb <- c(0.2, 0.3, 0.5)
  ref <- 0
  for (p in 1:2) for (q in 1:3) {
    x <- substr(ga[p], 2, 2)
    y <- substr(gb[q], 2, 2)
    if (x != "-" && y != "-") ref <- ref + wa[p] * wb[q] * sc$matrix[[paste(x, y, sep = "|")]]
  }
  expect_equal(mafft_site_score(sc$matrix, ga, gb, wa, wb, 2, 2), ref, tolerance = 1e-12)
})

test_that("gap profiles, degapping, weights and cleaning", {
  g <- c("A--CD", "AB-C-")
  gp <- mafft_gap_profiles(g, c(0.25, 0.75))
  gs <- ge <- numeric(6)
  for (k in 1:2) {
    z <- as.numeric(strsplit(g[k], "")[[1]] == "-")
    for (x in 1:5) {
      gs[x] <- gs[x] + c(0.25, 0.75)[k] * (1 - z[x]) * (if (x < 5) z[x + 1] else 0)
      ge[x] <- ge[x] + c(0.25, 0.75)[k] * (if (x > 1) z[x - 1] else 0) * (1 - z[x])
    }
  }
  expect_equal(gp$gs, gs, tolerance = 1e-12)
  expect_equal(gp$ge, ge, tolerance = 1e-12)
  expect_equal(unname(mafft_degap(c("A-C-", "B--D"))), c("AC-", "B-D"))
  expect_equal(unname(mafft_degap(c("--", "--"))), c("", ""))
  expect_equal(mafft_weights(4), rep(0.25, 4))
  cl <- mafft_clean(c("acgt", "aNgu"))
  expect_identical(cl$seq_type, "nt")
  expect_identical(cl$seqs, c("ACGT", "ANGU"))
  expect_identical(mafft_clean(c("acdw"))$seq_type, "aa")
  expect_error(mafft_clean(c("AC", "")), "empty sequence")
  expect_error(mafft_clean("AC", seq_type = "rna"), "seq_type must be")
})

test_that("group alignment: a single deletion opens one gap and degapping restores the input", {
  sc <- normalized_similarity_matrix()
  g <- group_align("ACDEFGHIK", "ACEFGHIK", sc)
  expect_equal(unname(g$out1), "ACDEFGHIK")
  expect_equal(unname(g$out2), "AC-EFGHIK")
  nw <- mafft_nw("ACDEFGHIK", "ACEFGHIK", sc$matrix, 1, 1, 2.4)
  expect_equal(unname(unlist(nw$out2)), "AC-EFGHIK")
  pr <- group_align(c("ACDEF", "ACDEW"), c("CDEF"), sc)
  expect_equal(gsub("-", "", unname(pr$out1)), c("ACDEF", "ACDEW"))
  expect_equal(gsub("-", "", unname(pr$out2)), "CDEF")
  expect_identical(length(unique(nchar(c(pr$out1, pr$out2)))), 1L)
  an <- group_align("ACDEFGHIK", "ACEFGHIK", sc, anchors = list(c(5L, 4L)))
  expect_equal(unname(an$out2), "AC-EFGHIK")
  expect_error(group_align("AC", "AC", sc, anchors = list(c(2L, 0L), c(1L, 2L))), "anchors cross")
  expect_error(group_align(c("AC", "A"), "AC", sc), "one length")
})

test_that("homologous segments, their arrangement and anchors", {
  sc <- normalized_similarity_matrix()
  s <- "MKTAYIAKQRQISFVKSHFSRQ"
  segs <- find_homologous_segments(s, s, sc, window = 8L, n_peaks = 1L, threshold = 0.5)
  expect_length(segs, 1L)
  expect_equal(segs[[1]][1:3], c(0L, 0L, 22L))
  expect_error(find_homologous_segments(s, s, sc, window = 0L), "must be positive")
  sg <- list(c(0L, 0L, 5L, 2L, 0L), c(3L, 1L, 4L, 3L, -2L), c(6L, 7L, 3L, 1L, 1L), c(10L, 12L, 4L, 2L, 2L))
  ch <- arrange_segments(sg)
  compat <- function(a, b) a[1] + a[3] <= b[1] && a[2] + a[3] <= b[2]
  best <- -Inf
  for (m in 1:15) {
    pick <- sg[which(bitwAnd(m, 2^(0:3)) > 0)]
    ok <- length(pick) < 2 || all(vapply(seq_len(length(pick) - 1), function(i) compat(pick[[i]], pick[[i + 1]]), TRUE))
    if (ok) best <- max(best, sum(vapply(pick, function(p) p[3] * p[4], 1)))
  }
  expect_equal(sum(vapply(ch, function(p) p[3] * p[4], 1)), best)
  expect_length(arrange_segments(list()), 0L)
  am <- mafft_anchors_from(rbind(c(0L, 2L, 5L), c(10L, 12L, 4L), c(-1L, 3L, 2L)))
  expect_equal(am[1:2], list(c(2L, 4L), c(12L, 14L)))
  expect_null(am[[3]])
  expect_length(mafft_anchors_from(matrix(integer(0), 0, 3)), 0L)
})

test_that("six-tuple distance and the average-linkage guide tree", {
  seqs <- c("MKTAYIAKQRQISF", "MKTAYLAKQRQISF", "GGGSTPGGGSTPAA", "MKSAYIAKQRAISF")
  D <- sixtuple_distance(seqs)
  grp <- c("AGPST", "C", "DENQ", "FWY", "HKR", "ILMV")
  code <- function(s) paste(vapply(strsplit(s, "")[[1]], function(ch) letters[which(vapply(grp, function(g) grepl(ch, g, fixed = TRUE), TRUE))], ""), collapse = "")
  tup <- function(t) table(substring(t, 1:(nchar(t) - 5), 6:nchar(t)))
  sh <- function(x, y) {
    k <- intersect(names(x), names(y))
    sum(pmin(x[k], y[k]))
  }
  tb <- lapply(seqs, function(s) tup(code(s)))
  for (i in 1:4) for (j in 1:4) {
    if (i == j) next
    expect_equal(D[i, j], 1 - sh(tb[[i]], tb[[j]]) / min(sum(tb[[i]]), sum(tb[[j]])), tolerance = 1e-12)
  }
  tr <- guide_tree(D)
  expect_length(tr, 3L)
  Dd <- matrix(c(0, 0.1, 0.9, 0.3, 0.1, 0, 0.8, 0.35, 0.9, 0.8, 0, 0.7, 0.3, 0.35, 0.7, 0), 4)
  gt <- guide_tree(Dd)
  hc <- stats::hclust(stats::as.dist(Dd), method = "average")
  mem <- function(k) if (k < 0) -k else unlist(lapply(hc$merge[k, ], mem))
  for (s in 1:3) expect_setequal(unlist(gt[[s]]$members), mem(s))
  expect_error(guide_tree(matrix(0, 1, 1)), "at least two")
})

test_that("WSP score, progressive and iterative alignment, and the mafft driver", {
  sc <- normalized_similarity_matrix()
  aln <- c("AC-DE", "ACWDE", "-CWD-")
  w <- rep(1 / 3, 3)
  tot <- 0
  for (i in 1:2) for (j in (i + 1):3) {
    a <- strsplit(aln[i], "")[[1]]
    b <- strsplit(aln[j], "")[[1]]
    open <- FALSE
    for (r in 1:5) {
      if (a[r] == "-" || b[r] == "-") {
        if (!open) tot <- tot - w[i] * w[j] * 2.4
        open <- TRUE
      } else {
        open <- FALSE
        tot <- tot + w[i] * w[j] * sc$matrix[[paste(a[r], b[r], sep = "|")]]
      }
    }
  }
  expect_equal(wsp_score(aln, sc), tot, tolerance = 1e-12)
  expect_error(wsp_score(c("AC", "A"), sc), "rectangular")
  seqs <- c("MKTAYIAKQR", "MKTYIAKQR", "MKTAYIQR")
  for (m in c("NW-NS-1", "NW-NS-2", "FFT-NS-2", "FFT-NS-i")) {
    r <- mafft_alignment(seqs, method = m, window = 5L)
    expect_equal(unname(gsub("-", "", r$alignment)), seqs)
    expect_identical(length(unique(nchar(r$alignment))), 1L)
    expect_equal(r$score, wsp_score(r$alignment, sc), tolerance = 1e-12)
  }
  expect_identical(mafftalignment, mafft_alignment)
  pa <- progressive_align(seqs, sc, use_fft = FALSE)
  expect_equal(unname(gsub("-", "", pa)), seqs)
  it <- iterative_refine(pa, sc, use_fft = FALSE, max_iterate = 3L)
  expect_gte(it$score, wsp_score(pa, sc) - 1e-12)
  expect_equal(it$score, wsp_score(it$aln, sc), tolerance = 1e-12)
  expect_error(iterative_refine(pa, sc, max_iterate = 0L), "at least 1")
  expect_error(mafft_alignment(seqs, method = "L-INS-i"), "method must be one of")
  expect_error(mafft_alignment("ACD"), "at least two")
  expect_identical(morie_mafft("wsp_score", aln, sc), wsp_score(aln, sc))
  expect_identical(morie_mafft("sixtuple_distance", seqs), sixtuple_distance(seqs))
  expect_match(morie_mafft("cheatsheet")$cheatsheet, "Katoh")
  expect_error(morie_mafft("nope"), "unknown op")
  expect_error(morie_mafft(), "op must be one of")
})
