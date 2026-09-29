# Coverage for text metrics and related helpers (bleuS.R, bertS.R,
# chrF_native.R, bm25.R, bpetk.R, alammar_llm_native.R, e_div_native.R):
# each score is recomputed from its published definition by explicit
# n-gram counting in the test.

ngrams <- function(tok, n) {
  if (length(tok) < n) return(character(0))
  vapply(seq_len(length(tok) - n + 1), function(i) paste(tok[i:(i + n - 1)], collapse = " "), "")
}

test_that("BleuS is clipped n-gram precision with the brevity penalty", {
  cand <- "the cat sat on the mat today"
  refs <- c("the cat is on the mat", "there is a cat on the mat today")
  r <- BleuS(cand, refs, max_n = 2)
  ct <- strsplit(cand, " ")[[1]]
  rt <- lapply(refs, function(s) strsplit(s, " ")[[1]])
  p <- vapply(1:2, function(n) {
    cg <- table(ngrams(ct, n))
    mx <- vapply(names(cg), function(g) max(vapply(rt, function(t) sum(ngrams(t, n) == g), 0)), 0)
    sum(pmin(cg, mx)) / sum(cg)
  }, 0)
  expect_equal(r$p_n, p, tolerance = 1e-12)
  expect_equal(r$bp, 1)
  expect_equal(r$bleu, exp(mean(log(p))), tolerance = 1e-12)
  short <- BleuS("the cat", "the cat sat on the mat", max_n = 1)
  expect_equal(short$bp, exp(1 - 6 / 2), tolerance = 1e-12)
  expect_equal(BleuS("a b", "c d", max_n = 1)$bleu, 0)
  expect_equal(BleuS(c("A", "b"), list(c("a", "b")), max_n = 1)$bleu, 1)
  expect_error(BleuS(cand, refs, max_n = 0), "at least one")
  expect_error(BleuS("", refs), "candidate is empty")
  expect_error(BleuS(cand, list()), "no references")
  expect_error(BleuS(cand, c("x", "")), "reference is empty")
})

test_that("Bertscore greedily matches cosine similarities", {
  X <- rbind(c(1, 0.2, 0), c(0.1, 1, 0.3), c(0.5, 0.5, 0.5))
  Y <- rbind(c(0.9, 0.1, 0.1), c(0, 0.2, 1))
  r <- Bertscore(X, Y, idf = c(1, 2, 1))
  S <- (X / sqrt(rowSums(X^2))) %*% t(Y / sqrt(rowSums(Y^2)))
  R <- sum(c(1, 2, 1) * apply(S, 1, max)) / 4
  P <- mean(apply(S, 2, max))
  expect_equal(c(r$R, r$P), c(R, P), tolerance = 1e-12)
  expect_equal(r$F, 2 * P * R / (P + R), tolerance = 1e-12)
  expect_error(Bertscore(X, Y[, 1:2]), "dimension")
  expect_error(Bertscore(rbind(X, 0), Y), "non-zero")
  expect_error(Bertscore(X, Y, idf = 1), "one weight per reference")
  expect_error(Bertscore(X, Y, idf = c(0, 0, 0)), "not all be zero")
})

test_that("morie_chrF is the character n-gram F-beta score", {
  h <- "the cat sat"
  ref <- "the cat sits"
  r <- morie_chrF(h, ref, n_char = 3, beta = 2)
  hc <- strsplit(gsub(" ", "", h), "")[[1]]
  rc <- strsplit(gsub(" ", "", ref), "")[[1]]
  pr <- vapply(1:3, function(n) {
    a <- table(ngrams(hc, n))
    b <- table(ngrams(rc, n))
    m <- sum(vapply(names(a), function(g) min(a[[g]], if (g %in% names(b)) b[[g]] else 0), 0))
    c(m / sum(a), m / sum(b))
  }, numeric(2))
  P <- mean(pr[1, ])
  R <- mean(pr[2, ])
  expect_equal(c(r$chrP, r$chrR), c(P, R), tolerance = 1e-12)
  expect_equal(r$chrf, 5 * P * R / (4 * P + R), tolerance = 1e-12)
  w <- morie_chrF(h, ref, n_char = 2, word_order = 1)
  wp <- 2 / 3
  cp <- vapply(1:2, function(n) {
    a <- table(ngrams(hc, n))
    b <- table(ngrams(rc, n))
    m <- sum(vapply(names(a), function(g) min(a[[g]], if (g %in% names(b)) b[[g]] else 0), 0))
    c(m / sum(a), m / sum(b))
  }, numeric(2))
  expect_equal(w$chrP, mean(c(cp[1, ], wp)), tolerance = 1e-12)
  best <- morie_chrF(h, c("zzz", ref), n_char = 3)
  expect_equal(best$chrf, r$chrf, tolerance = 1e-12)
  expect_error(morie_chrF(h, ref, n_char = 0), "at least 1")
  expect_error(morie_chrF(h, ref, beta = 0), "positive")
  expect_error(morie_chrF(h, ref, word_order = -1), ">= 0")
})

test_that("Bm25 is Okapi BM25 with Robertson-Sparck Jones IDF", {
  docs <- c("the quick brown fox", "the lazy dog", "quick quick fox jumps over the dog")
  r <- Bm25(docs, "quick fox dog", k1 = 1.5, b = 0.6)
  toks <- lapply(docs, function(s) strsplit(s, " ")[[1]])
  len <- lengths(toks)
  avg <- mean(len)
  terms <- sort(c("quick", "fox", "dog"))
  nt <- vapply(terms, function(t) sum(vapply(toks, function(d) t %in% d, TRUE)), 0)
  idf <- log((3 - nt + 0.5) / (nt + 0.5))
  sc <- vapply(1:3, function(i) sum(vapply(seq_along(terms), function(k) {
    f <- sum(toks[[i]] == terms[k])
    idf[k] * f * 2.5 / (f + 1.5 * (0.4 + 0.6 * len[i] / avg))
  }, 0)), 0)
  expect_equal(r$scores, sc, tolerance = 1e-12)
  expect_equal(r$ranking, order(-sc) - 1L)
  expect_equal(r$idf_smooth, unname(log(1 + (3 - nt + 0.5) / (nt + 0.5))), tolerance = 1e-12)
  expect_error(Bm25(character(0), "q"), "empty")
  expect_error(Bm25(docs, ""), "query is empty")
  expect_error(Bm25(docs, "q", k1 = -1), "non-negative")
  expect_error(Bm25(docs, "q", b = 2), "\\[0, 1\\]")
  expect_error(Bm25(c("", ""), "q"), "every document is empty")
})

test_that("Bpetrain learns merges by pair frequency", {
  corpus <- c("low", "low", "lower", "newest", "newest", "widest")
  r <- Bpetrain(corpus, vocab_size = 3)
  seqs <- list(c("l", "o", "w", "</w>"), c("l", "o", "w", "e", "r", "</w>"),
               c("n", "e", "w", "e", "s", "t", "</w>"), c("w", "i", "d", "e", "s", "t", "</w>"))
  fr <- c(2, 1, 2, 1)
  merges <- character(0)
  for (it in 1:3) {
    pc <- list()
    for (k in seq_along(seqs)) {
      s <- seqs[[k]]
      for (j in seq_len(length(s) - 1)) {
        key <- paste(s[j], s[j + 1], sep = "|")
        pc[[key]] <- (if (is.null(pc[[key]])) 0 else pc[[key]]) + fr[k]
      }
    }
    v <- unlist(pc)
    best <- names(v)[which.max(v)]
    merges <- c(merges, best)
    ab <- strsplit(best, "|", fixed = TRUE)[[1]]
    seqs <- lapply(seqs, function(s) {
      out <- character(0)
      j <- 1
      while (j <= length(s)) {
        if (j < length(s) && s[j] == ab[1] && s[j + 1] == ab[2]) {
          out <- c(out, paste0(ab, collapse = ""))
          j <- j + 2
        } else {
          out <- c(out, s[j])
          j <- j + 1
        }
      }
      out
    })
  }
  expect_equal(r$merges, merges)
  expect_equal(r$vocab, sort(unique(unlist(seqs))))
  wc <- Bpetrain(c("ab", "cd"), vocab_size = 5, word_counts = c(1, 1))
  expect_equal(wc$estimate, 0)
})

test_that("morie_alammar_cosine_similarity_loss is the squared cosine error", {
  A <- rbind(c(1, 0, 1), c(0.5, 0.2, -0.1))
  B <- rbind(c(0.8, 0.1, 1.2), c(-0.3, 0.4, 0.2))
  y <- c(0.9, -0.2)
  r <- morie_alammar_cosine_similarity_loss(A, B, y)
  cs <- rowSums(A * B) / sqrt(rowSums(A^2) * rowSums(B^2))
  expect_equal(r$similarities, cs, tolerance = 1e-12)
  expect_equal(r$estimate, mean((cs - y)^2), tolerance = 1e-12)
  expect_error(morie_alammar_cosine_similarity_loss(A, B[1, , drop = FALSE], y), "matched pairs")
  expect_error(morie_alammar_cosine_similarity_loss(A, B, c(2, 0)), "\\[-1, 1\\]")
})

test_that("morie_e_div maximises the energy two-sample statistic", {
  x <- c(0.1, -0.2, 0.3, 0.0, 0.2, 3.1, 2.8, 3.3, 2.9, 3.0)
  r <- morie_e_div(x, sig = 0.1, R = 19L, max_cp = 1)
  D <- abs(outer(x, x, "-"))
  qhat <- function(a, tau, k) {
    X <- (a + 1):tau
    Y <- (tau + 1):k
    n <- length(X)
    m <- length(Y)
    e <- 2 * mean(D[X, Y]) - sum(D[X, X]) / (n * (n - 1)) - sum(D[Y, Y]) / (m * (m - 1))
    n * m / (n + m) * e
  }
  best <- -Inf
  for (tau in 2:8) for (k in (tau + 2):10) {
    q <- qhat(0, tau, k)
    if (q > best) {
      best <- q
      bt <- tau
    }
  }
  expect_equal(r$q_stats[1], best, tolerance = 1e-12)
  expect_equal(r$changepoints, bt)
  expect_lte(r$p_values[1], 0.1)
  expect_error(morie_e_div(1:3), "too short")
  expect_error(morie_e_div(x, alpha = 2), "\\(0, 2\\)")
})
